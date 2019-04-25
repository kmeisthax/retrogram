//! Analysis database - Allows accumulation of program facts as disassembly
//! passes run on the program.

use std::{fs, io};
use std::collections::HashMap;
use std::hash::Hash;
use std::ops::{Add, Sub};
use std::fmt::{Display, Formatter, Result, UpperHex};
use crate::retrogram::{ast, memory, project};

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
pub struct Database<P> where P: Eq + Hash {
    /// A list of all labels in the program.
    labels: HashMap<ast::Label, memory::Pointer<P>>,
    
    /// A list of all pointer values in the program which have a label.
    pointers: HashMap<memory::Pointer<P>, ast::Label>
}

impl<P> Database<P> where P: Clone + Eq + Hash {
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

/// Given an operand, replace all Pointer literals with Label operands obtained
/// from the Database.
pub fn replace_operand_with_label<I, F, P, AMV, AS, AIO>(src_operand: ast::Operand<I, F, P>, db: &mut Database<P>, start_addr: &memory::Pointer<P>, memory: &memory::Memory<P, AMV, AS, AIO>, refkind: ReferenceKind) -> ast::Operand<I, F, P>
    where P: memory::PtrNum<AS> + Clone + UpperHex + Eq + Hash,
        AS: memory::Offset<P> + Clone,
        I: Clone,
        F: Clone {
    match src_operand {
        ast::Operand::Literal(ast::Literal::Pointer(pt)) => {
            let mut cpt = start_addr.contextualize(pt.clone());
            cpt = memory.minimize_context(cpt);

            if let Some(lbl) = db.pointer_label(&cpt) {
                ast::Operand::Label(lbl.clone())
            } else {
                ast::Operand::Label(db.insert_placeholder_label(cpt, refkind))
            }
        },
        ast::Operand::DataReference(op) => ast::Operand::DataReference(Box::new(replace_operand_with_label(*op, db, start_addr, memory, ReferenceKind::Data))),
        ast::Operand::CodeReference(op) => ast::Operand::CodeReference(Box::new(replace_operand_with_label(*op, db, start_addr, memory, ReferenceKind::Code))),
        ast::Operand::Indirect(op) => ast::Operand::Indirect(Box::new(replace_operand_with_label(*op, db, start_addr, memory, refkind))),
        ast::Operand::Add(opl, opr) => ast::Operand::Add(Box::new(replace_operand_with_label(*opl, db, start_addr, memory, refkind)), Box::new(replace_operand_with_label(*opr, db, start_addr, memory, refkind))),
        _ => src_operand
    }
}

/// Given an Assembly, create a new Assembly with all pointers replaced with
/// their equivalent labels in the database.
/// 
/// If a given pointer has no matching label, then a temporary label will be
/// automatically generated and added to the database.
pub fn replace_labels<I, F, P, AMV, AS, AIO>(src_assembly: ast::Assembly<I, F, P>, db: &mut Database<P>, memory: &memory::Memory<P, AMV, AS, AIO>) -> ast::Assembly<I, F, P>
    where P: memory::PtrNum<AS> + Clone + UpperHex + Eq + Hash,
        AS: memory::Offset<P> + Clone,
        I: Clone,
        F: Clone {
    let mut dst_assembly = ast::Assembly::new();

    for line in src_assembly.iter_lines() {
        if let Some(instr) = line.instr() {
            let mut new_operands = Vec::new();

            for operand in instr.iter_operands() {
                new_operands.push(replace_operand_with_label(operand.clone(), db, &line.source_address(), memory, ReferenceKind::Unknown));
            }

            let new_instr = ast::Instruction::new(instr.opcode(), new_operands);
            let (label, _old_instr, comment, src_addr) = line.clone().into_parts();
            dst_assembly.append_line(ast::Line::new(label, Some(new_instr), comment, src_addr));
        } else {
            dst_assembly.append_line(line.clone());
        }
    }

    dst_assembly
}

/// Given an Assembly, create a new Assembly with all labels inserted from the
/// database.
pub fn inject_labels<I, F, P>(src_assembly: ast::Assembly<I, F, P>, db: &Database<P>) -> ast::Assembly<I, F, P>
    where P: Clone + Eq + Hash, I: Clone, F: Clone {
    let mut dst_assembly = ast::Assembly::new();
    
    for line in src_assembly.iter_lines() {
        if let None = line.label() {
            let (_label, old_instr, comment, src_addr) = line.clone().into_parts();

            if let Some(new_label) = db.pointer_label(&src_addr) {
                dst_assembly.append_line(ast::Line::new(Some(new_label.clone()), old_instr, comment, src_addr));
            } else {
                dst_assembly.append_line(line.clone());
            }
        } else {
            dst_assembly.append_line(line.clone());
        }
    }

    dst_assembly
}