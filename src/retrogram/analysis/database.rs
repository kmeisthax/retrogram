//! Analysis database - Allows accumulation of program facts as disassembly
//! passes run on the program.

use std::{fs, io};
use std::collections::{HashSet, HashMap};
use std::fmt::UpperHex;
use crate::retrogram::{ast, memory, project, analysis};

#[derive(Clone, Debug)]
struct Block<P, S> where P: analysis::Mappable {
    start: memory::Pointer<P>,
    length: S,
    exits: HashSet<Option<memory::Pointer<P>>>
}

/// A repository of information obtained from the program under analysis.
#[derive(Clone, Debug)]
pub struct Database<P, S> where P: analysis::Mappable {
    /// A list of all labels in the program.
    labels: HashMap<ast::Label, memory::Pointer<P>>,
    
    /// A list of all pointer values in the program which have a label.
    pointers: HashMap<memory::Pointer<P>, ast::Label>,

    /// A list of program regions.
    blocks: Vec<Block<P, S>>,

    /// A list of all cross-references in the program.
    xrefs: Vec<analysis::Reference<P>>
}

impl<P, S> Database<P, S> where P: analysis::Mappable {
    pub fn new() -> Self {
        Database {
            labels: HashMap::new(),
            pointers: HashMap::new(),
            blocks: Vec::new(),
            xrefs: Vec::new()
        }
    }

    pub fn for_program<PS>(prog: &project::Program, parse_symbol_file: PS) -> io::Result<Self>
        where PS: Fn(io::BufReader<fs::File>, &mut Database<P, S>) -> io::Result<()> {
        let mut db = Self::new();

        for symbol_file in prog.iter_symbol_files() {
            if let Ok(file) = fs::File::open(symbol_file) {
                parse_symbol_file(io::BufReader::new(file), &mut db)?;
            }
        }

        Ok(db)
    }

    pub fn insert_label(&mut self, label: ast::Label, ptr: memory::Pointer<P>) {
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

        self.insert_label(ast::Label::new(&name, None), ptr);

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
}