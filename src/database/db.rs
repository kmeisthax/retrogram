//! Implementation of core database type

use crate::analysis::Mappable;
use crate::arch::Architecture;
use crate::cli::Nameable;
use crate::database::AnyDatabase;
use crate::{analysis, ast, memory};
use serde::de::{self, Deserializer, MapAccess, SeqAccess, Visitor};
use serde::{Deserialize, Serialize};
use std::any::Any;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;
use std::marker::PhantomData;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Symbol<P>(ast::Label, memory::Pointer<P>)
where
    P: Mappable + Nameable;

impl<P> Symbol<P>
where
    P: Mappable + Nameable,
{
    pub fn as_label(&self) -> &ast::Label {
        &self.0
    }

    pub fn set_label(&mut self, new_label: ast::Label) {
        self.0 = new_label;
    }

    pub fn as_pointer(&self) -> &memory::Pointer<P> {
        &self.1
    }

    pub fn set_pointer(&mut self, new_pointer: memory::Pointer<P>) {
        self.1 = new_pointer;
    }
}

/// A repository of information obtained from the program under analysis.
#[derive(Clone, Debug, Serialize)]
pub struct Database<AR>
where
    AR: Architecture,
{
    symbols: Vec<Symbol<AR::PtrVal>>,

    #[serde(skip)]
    was_deserialized: bool,

    /// A list of program regions.
    blocks: Vec<analysis::Block<AR>>,

    /// A list of all cross-references in the program.
    xrefs: Vec<analysis::Reference<AR::PtrVal>>,

    /// A list of all labels in the program.
    #[serde(skip)]
    label_symbols: HashMap<ast::Label, usize>,

    /// A list of all pointer values in the program which have a label.
    #[serde(skip)]
    pointer_symbols: HashMap<memory::Pointer<AR::PtrVal>, usize>,

    /// A list of crossreferences sorted by source address.
    #[serde(skip)]
    xref_source_index: BTreeMap<memory::Pointer<AR::PtrVal>, HashSet<usize>>,

    /// A list of crossreferences sorted by target address.
    #[serde(skip)]
    xref_target_index: BTreeMap<memory::Pointer<AR::PtrVal>, HashSet<usize>>,
}

impl<AR> AnyDatabase for Database<AR>
where
    AR: Architecture + 'static,
{
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_mut_any(&mut self) -> &mut dyn Any {
        self
    }
}

#[derive(Deserialize)]
#[serde(field_identifier, rename_all = "lowercase")]
enum DatabaseField {
    Symbols,
    Blocks,
    Xrefs,
}

struct DatabaseVisitor<AR>(PhantomData<AR>)
where
    AR: Architecture;

impl<'dw, AR> Visitor<'dw> for DatabaseVisitor<AR>
where
    AR: Architecture,
{
    type Value = Database<AR>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("struct Database")
    }

    fn visit_seq<V>(self, mut seq: V) -> Result<Database<AR>, V::Error>
    where
        V: SeqAccess<'dw>,
    {
        let mut db = Database::new();

        db.symbols = seq
            .next_element()?
            .ok_or_else(|| de::Error::invalid_length(0, &self))?;
        db.blocks = seq
            .next_element()?
            .ok_or_else(|| de::Error::invalid_length(1, &self))?;
        db.xrefs = seq
            .next_element()?
            .ok_or_else(|| de::Error::invalid_length(2, &self))?;

        Ok(db)
    }

    fn visit_map<V>(self, mut map: V) -> Result<Database<AR>, V::Error>
    where
        V: MapAccess<'dw>,
    {
        let mut db = Database::new();

        db.was_deserialized = true;

        while let Some(key) = map.next_key()? {
            match key {
                DatabaseField::Symbols => {
                    if !db.symbols.is_empty() {
                        return Err(de::Error::duplicate_field("symbols"));
                    }

                    db.symbols = map.next_value()?;
                }
                DatabaseField::Blocks => {
                    if !db.blocks.is_empty() {
                        return Err(de::Error::duplicate_field("blocks"));
                    }

                    db.blocks = map.next_value()?;
                }
                DatabaseField::Xrefs => {
                    if !db.xrefs.is_empty() {
                        return Err(de::Error::duplicate_field("xrefs"));
                    }

                    db.xrefs = map.next_value()?;
                }
            }
        }

        Ok(db)
    }
}

impl<'dw, AR> Deserialize<'dw> for Database<AR>
where
    AR: Architecture,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'dw>,
    {
        let mut mine = deserializer.deserialize_struct(
            "Database",
            &["symbols", "blocks", "xrefs"],
            DatabaseVisitor::<AR>(PhantomData),
        )?;

        mine.update_indexes();

        Ok(mine)
    }
}

impl<AR> Default for Database<AR>
where
    AR: Architecture,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<AR> Database<AR>
where
    AR: Architecture,
{
    pub fn new() -> Self {
        Database {
            symbols: Vec::new(),
            was_deserialized: false,
            blocks: Vec::new(),
            xrefs: Vec::new(),
            label_symbols: HashMap::new(),
            pointer_symbols: HashMap::new(),
            xref_source_index: BTreeMap::new(),
            xref_target_index: BTreeMap::new(),
        }
    }

    /// Regenerate internal indexes that aren't serialized to disk.
    ///
    /// This is automatically called by `Deserialize`. It is not necessary to
    /// call it outside of that context.
    fn update_indexes(&mut self) {
        if self.was_deserialized {
            for (id, Symbol(lbl, ptr)) in self.symbols.iter().enumerate() {
                self.label_symbols.insert(lbl.clone(), id);
                self.pointer_symbols.insert(ptr.clone(), id);
            }

            for (id, xref) in self.xrefs.iter().enumerate() {
                let source = xref.as_source();
                let source_bukkit = self
                    .xref_source_index
                    .entry(source.clone())
                    .or_insert_with(HashSet::new);

                source_bukkit.insert(id);

                let target = xref.as_target();
                if let Some(target) = target {
                    let target_bukkit = self
                        .xref_target_index
                        .entry(target.clone())
                        .or_insert_with(HashSet::new);

                    target_bukkit.insert(id);
                }
            }

            self.was_deserialized = false;
        }
    }

    /// Create a new symbol association.
    pub fn insert_symbol(&mut self, label: ast::Label, ptr: memory::Pointer<AR::PtrVal>) {
        let id = self.symbols.len();

        self.symbols.push(Symbol(label.clone(), ptr.clone()));
        self.label_symbols.insert(label, id);
        self.pointer_symbols.insert(ptr, id);
    }

    /// Change a given symbol record's label or pointer association.
    ///
    /// Fields provided as None will be left as it is.
    pub fn update_symbol(
        &mut self,
        sym_id: usize,
        new_label: Option<ast::Label>,
        new_pointer: Option<memory::Pointer<AR::PtrVal>>,
    ) {
        if let Some(sym) = self.symbols.get_mut(sym_id) {
            if let Some(new_label) = new_label {
                self.label_symbols.remove(sym.as_label());
                sym.set_label(new_label.clone());
                self.label_symbols.insert(new_label, sym_id);
            }

            if let Some(new_pointer) = new_pointer {
                self.pointer_symbols.remove(sym.as_pointer());
                sym.set_pointer(new_pointer.clone());
                self.pointer_symbols.insert(new_pointer, sym_id);
            }
        }
    }

    /// Ensure a symbol exists within the database with a given label and
    /// pointer.
    ///
    /// This function takes some precautions to avoid inserting duplicate labels
    /// into the symbol table. Specifically, if the label already exists, we
    /// repoint it to the new pointer. Furthermore, if a placeholder label
    /// already refers to the same location, we change that symbol's label to
    /// match the request.
    pub fn upsert_symbol(
        &mut self,
        new_label: ast::Label,
        for_pointer: memory::Pointer<AR::PtrVal>,
    ) {
        if let Some(sym_id) = self.label_symbol(&new_label) {
            if let Some(sym) = self.symbol(sym_id) {
                if *sym.as_pointer() == for_pointer {
                    return;
                }
            }

            self.update_symbol(sym_id, None, Some(for_pointer));
        } else if let Some(sym_id) = self.pointer_symbol(&for_pointer) {
            if let Some(sym) = self.symbol(sym_id) {
                if *sym.as_label() == new_label {
                    return;
                }

                if !sym.as_label().is_placeholder() {
                    return self.insert_symbol(new_label, for_pointer);
                }
            }

            self.update_symbol(sym_id, Some(new_label), None);
        } else {
            self.insert_symbol(new_label, for_pointer);
        }
    }

    /// Create a label for a location that is not named in the database.
    pub fn insert_placeholder_label(
        &mut self,
        ptr: memory::Pointer<AR::PtrVal>,
        kind: analysis::ReferenceKind,
    ) -> ast::Label {
        let mut name = format!("{}", kind);

        for (_, key, cval) in ptr.iter_contexts() {
            name = match cval.into_concrete() {
                Some(cval) => format!("{}_{:X}", name, cval),
                _ => format!("{}_{}??", name, key),
            };
        }

        name = format!("{}_{:X}", name, ptr.as_pointer());

        self.upsert_symbol(ast::Label::new_placeholder(&name, None), ptr);

        ast::Label::new(&name, None)
    }

    pub fn insert_crossreference(&mut self, myref: analysis::Reference<AR::PtrVal>) {
        let id = self.xrefs.len();
        let source_bucket = self
            .xref_source_index
            .entry(myref.as_source().clone())
            .or_insert_with(HashSet::new);

        for other_id in source_bucket.iter() {
            if let Some(other) = self.xrefs.get(*other_id) {
                if other.as_source() == myref.as_source() && other.as_target() == myref.as_target()
                {
                    return;
                }
            }
        }

        source_bucket.insert(id);
        if let Some(target) = myref.as_target() {
            self.xref_target_index
                .entry(target.clone())
                .or_insert_with(HashSet::new)
                .insert(id);
        }

        self.xrefs.push(myref);
    }

    /// Indicate that some number of traces passed through a given block.
    pub fn insert_trace_counts(&mut self, block_ids: HashSet<usize>, count: u32) {
        for block_id in block_ids.iter() {
            if let Some(block) = self.blocks.get_mut(*block_id) {
                block.add_traces(count);
            }
        }
    }

    pub fn pointer_symbol(&self, ptr: &memory::Pointer<AR::PtrVal>) -> Option<usize> {
        self.pointer_symbols.get(ptr).copied()
    }

    pub fn label_symbol(&self, label: &ast::Label) -> Option<usize> {
        self.label_symbols.get(label).copied()
    }

    pub fn symbol(&self, symbol_id: usize) -> Option<&Symbol<AR::PtrVal>> {
        self.symbols.get(symbol_id)
    }

    pub fn block(&self, block_id: usize) -> Option<&analysis::Block<AR>> {
        self.blocks.get(block_id)
    }

    pub fn xref(&self, xref_id: usize) -> Option<&analysis::Reference<AR::PtrVal>> {
        self.xrefs.get(xref_id)
    }

    /// Find which block of the program's control-flow graph contains this
    /// pointer.
    pub fn find_block_membership(&self, ptr: &memory::Pointer<AR::PtrVal>) -> Option<usize> {
        //TODO: This needs to be way higher performance than a linear scan
        for (i, ref block) in self.blocks.iter().enumerate() {
            if block.is_ptr_within_block(ptr) {
                return Some(i);
            }
        }

        None
    }

    /// Add a block to the database.
    ///
    /// If the block already exists (or at least, there is one at the start of
    /// this block) then insertion will silently fail.
    pub fn insert_block(&mut self, block: analysis::Block<AR>) {
        if self.find_block_membership(block.as_start()).is_some() {
            return;
        }

        self.blocks.push(block)
    }

    /// Given a block number and a split point, split the block and return the
    /// second block's location.
    ///
    /// If the new block size would cause the current block to shrink, a new
    /// block is created and it's id is returned. Otherwise, None is returned.
    ///
    /// If an invalid block ID is given, this function also returns None.
    pub fn split_block(&mut self, block_id: usize, new_size: AR::Offset) -> Option<usize> {
        let block = self.blocks.get_mut(block_id);

        if let Some(block) = block {
            let new_block = block.split_block(new_size)?;
            let id = self.blocks.len();
            self.blocks.push(new_block);

            return Some(id);
        }

        None
    }

    /// Given a contextualized memory range, return all xref IDs originating
    /// from that memory range.
    pub fn find_xrefs_from<'a>(
        &'a self,
        from_start: &memory::Pointer<AR::PtrVal>,
        from_length: AR::Offset,
    ) -> impl Iterator<Item = usize> + 'a {
        let from_end = from_start.clone() + from_length;

        self.xref_source_index
            .range(from_start..&from_end.into_ptr())
            .map(|(_, v)| v)
            .flatten()
            .copied()
    }

    /// Given a contextualized memory range, return all xref IDs targeting that
    /// memory range.
    pub fn find_xrefs_to<'a>(
        &'a self,
        to_start: &memory::Pointer<AR::PtrVal>,
        to_length: AR::Offset,
    ) -> impl Iterator<Item = usize> + 'a {
        let to_end = to_start.clone() + to_length;

        self.xref_target_index
            .range(to_start..&to_end.into_ptr())
            .map(|(_, v)| v)
            .flatten()
            .copied()
    }

    /// Search for all crossreferences whose static target has not yet been
    /// analyzed.
    pub fn unanalyzed_static_xrefs(&self) -> Vec<usize> {
        let mut xref_ids = Vec::new();

        for (id, xref) in self.xrefs.iter().enumerate() {
            if let Some(target) = xref.as_target() {
                // TODO: Ouch. O(mn) complexity. Can we do better?
                if self.find_block_membership(target) == None {
                    xref_ids.push(id);
                }
            }
        }

        xref_ids
    }

    /// Search for all 'undertraced' blocks which should be traced to find
    /// static crossreferences.
    ///
    /// A block is considered 'undertraced' if the number of traces that have
    /// run through it's constituent block are less than the number of dynamic
    /// crossreferences in that block. This would mean that a block with no
    /// dynamic flow behavior is never considered for tracing.
    pub fn undertraced_blocks(&self) -> Vec<usize> {
        let mut blocks = Vec::new();

        for (i, block) in self.blocks.iter().enumerate() {
            let mut dynamism_score = 0;
            for xref_id in self.find_xrefs_from(block.as_start(), block.as_length().clone()) {
                if let Some(xref) = self.xref(xref_id) {
                    if xref.is_dynamic() {
                        dynamism_score += 1;
                    }
                }
            }

            if dynamism_score > block.traces() {
                blocks.push(i);
            }
        }

        blocks
    }
}
