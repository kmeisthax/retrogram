//! Analysis database - Allows accumulation of program facts as disassembly
//! passes run on the program.

use std::{fs, io};
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result, UpperHex};
use crate::retrogram::{ast, memory, project, analysis};

#[derive(Copy, Clone, Debug)]
pub enum ReferenceKind {
    Unknown,
    Data,
    Code,
    Subroutine
}

impl Display for ReferenceKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        //write!(f, "({}, {})", self.x, self.y)
        match self {
            ReferenceKind::Unknown => write!(f, "UNK"),
            ReferenceKind::Data => write!(f, "DAT"),
            ReferenceKind::Code => write!(f, "LOC"),
            ReferenceKind::Subroutine => write!(f, "FUN")
        }
    }
}

/// A repository of information obtained from the program under analysis.
#[derive(Clone, Debug)]
pub struct Database<P> where P: analysis::Mappable {
    /// A list of all labels in the program.
    labels: HashMap<ast::Label, memory::Pointer<P>>,
    
    /// A list of all pointer values in the program which have a label.
    pointers: HashMap<memory::Pointer<P>, ast::Label>
}

impl<P> Database<P> where P: analysis::Mappable {
    pub fn new() -> Self {
        Database {
            labels: HashMap::new(),
            pointers: HashMap::new()
        }
    }

    pub fn for_program<PS>(prog: &project::Program, parse_symbol_file: PS) -> io::Result<Self>
        where PS: Fn(io::BufReader<fs::File>, &mut Database<P>) -> io::Result<()> {
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
    pub fn insert_placeholder_label(&mut self, ptr: memory::Pointer<P>, kind: ReferenceKind) -> ast::Label
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

    pub fn pointer_label(&self, ptr: &memory::Pointer<P>) -> Option<&ast::Label> {
        self.pointers.get(ptr)
    }

    pub fn label_pointer(&self, label: &ast::Label) -> Option<&memory::Pointer<P>> {
        self.labels.get(label)
    }
}