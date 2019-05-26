//! Analysis database - Allows accumulation of program facts as disassembly
//! passes run on the program.

use std::{fs, io};
use std::collections::{HashSet, HashMap};
use std::fmt::UpperHex;
use serde::{Serialize, Deserialize};
use crate::retrogram::{ast, memory, project, analysis};

fn gimme_a_ptr<P>() -> HashMap<memory::Pointer<P>, ast::Label> where P: analysis::Mappable {
    HashMap::new()
}

fn gimme_a_lbl<P>() -> HashMap<ast::Label, memory::Pointer<P>> where P: analysis::Mappable {
    HashMap::new()
}

fn im_stale() -> bool {
    true
}

/// A repository of information obtained from the program under analysis.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Database<P, S> where P: analysis::Mappable {
    symbol_list: Vec<(ast::Label, memory::Pointer<P>)>,

    #[serde(skip, default="im_stale")]
    was_deserialized: bool,

    /// A list of program regions.
    blocks: Vec<analysis::Block<P, S>>,

    /// A list of all cross-references in the program.
    xrefs: Vec<analysis::Reference<P>>,

    /// A list of all labels in the program.
    #[serde(skip, default="gimme_a_lbl")]
    labels: HashMap<ast::Label, memory::Pointer<P>>,
    
    /// A list of all pointer values in the program which have a label.
    #[serde(skip, default="gimme_a_ptr")]
    pointers: HashMap<memory::Pointer<P>, ast::Label>
}

impl<P, S> Database<P, S> where P: analysis::Mappable {
    pub fn new() -> Self {
        Database {
            symbol_list: Vec::new(),
            was_deserialized: false,
            blocks: Vec::new(),
            xrefs: Vec::new(),
            labels: HashMap::new(),
            pointers: HashMap::new()
        }
    }

    /// Regenerate internal indexes that aren't serialized to disk
    /// 
    /// TODO: Find a way to get rid of this and do it alongside deserialization
    pub fn update_indexes(&mut self) {
        if self.was_deserialized {
            for (lbl, ptr) in self.symbol_list.iter() {
                self.labels.insert(lbl.clone(), ptr.clone());
                self.pointers.insert(ptr.clone(), lbl.clone());
            }

            self.was_deserialized = false;
        }
    }

    pub fn import_symbols<PS>(&mut self, prog: &project::Program, parse_symbol_file: PS) -> io::Result<()>
        where PS: Fn(io::BufReader<fs::File>, &mut Database<P, S>) -> io::Result<()> {

        for symbol_file in prog.iter_symbol_files() {
            if let Ok(file) = fs::File::open(symbol_file) {
                parse_symbol_file(io::BufReader::new(file), self)?;
            }
        }

        Ok(())
    }

    pub fn insert_label(&mut self, label: ast::Label, ptr: memory::Pointer<P>) {
        self.symbol_list.push((label.clone(), ptr.clone()));
        self.labels.insert(label.clone(), ptr.clone());
        self.pointers.insert(ptr, label);
    }

    /// Create a label for a location that is not named in the database.
    pub fn insert_placeholder_label(&mut self, ptr: memory::Pointer<P>, kind: analysis::ReferenceKind) -> ast::Label
        where P: UpperHex {
        let mut name = format!("{}", kind);

        for (_, key, cval) in ptr.iter_contexts() {
            name = match cval.into_concrete() {
                Some(cval) => format!("{}_{:X}", name, cval),
                _ => format!("{}_{}??", name, key)
            };
        }

        name = format!("{}_{:X}", name, ptr.as_pointer());

        self.insert_label(ast::Label::new_placeholder(&name, None), ptr);

        ast::Label::new(&name, None)
    }

    pub fn insert_crossreference(&mut self, myref: analysis::Reference<P>) {
        self.xrefs.push(myref);
    }

    pub fn pointer_label(&self, ptr: &memory::Pointer<P>) -> Option<&ast::Label> {
        self.pointers.get(ptr)
    }

    pub fn label_pointer(&self, label: &ast::Label) -> Option<&memory::Pointer<P>> {
        self.labels.get(label)
    }

    pub fn insert_block(&mut self, block: analysis::Block<P, S>) {
        self.blocks.push(block)
    }

    pub fn block(&self, block_id: usize) -> Option<&analysis::Block<P, S>> {
        self.blocks.get(block_id)
    }
}

impl<P, S> Database<P, S> where P: analysis::Mappable + memory::PtrNum<S>, S: memory::Offset<P> {
    pub fn find_block_membership(&mut self, ptr: &memory::Pointer<P>) -> Option<usize> {
        //TODO: This needs to be way higher performance than a linear scan
        for (i, ref block) in self.blocks.iter().enumerate() {
            if block.is_ptr_within_block(ptr) {
                return Some(i);
            }
        }

        None
    }

    /// Given a block number and a split point, split the block and return the
    /// second block's location.
    /// 
    /// If the new block size would cause the current block to shrink, a new
    /// block is created and it's id is returned. Otherwise, None is returned.
    /// 
    /// If an invalid block ID is given, this function also returns None.
    pub fn split_block(&mut self, block_id: usize, new_size: S) -> Option<usize> {
        let block = self.blocks.get_mut(block_id);

        if let Some(block) = block {
            if block.as_length() < &new_size {
                let remaining_size = block.as_length().clone() - new_size.clone();

                if let Ok(remaining_size) = S::try_from(remaining_size) {
                    let new_block_start = block.as_start().contextualize(P::from(block.as_start().as_pointer().clone() + new_size));

                    *block = analysis::Block::from_parts(block.as_start().clone(), remaining_size.clone());
                    let new_block = analysis::Block::from_parts(new_block_start, remaining_size);

                    let id = self.blocks.len();
                    self.blocks.push(new_block);

                    return Some(id);
                }
            }
        }

        None
    }
}