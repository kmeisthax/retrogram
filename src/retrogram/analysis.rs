//! Analysis database - Allows accumulation of program facts as disassembly
//! passes run on the program.

use std::collections::HashMap;
use std::hash::Hash;
use crate::retrogram::{ast, memory};

/// A repository of information obtained from the program under analysis.
pub struct Database<P> {
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

    pub fn insert_label(&mut self, label: ast::Label, ptr: memory::Pointer<P>) {
        self.labels.insert(label.clone(), ptr.clone());
        self.pointers.insert(ptr, label);
    }

    pub fn pointer_label(&self, ptr: memory::Pointer<P>) -> Option<&ast::Label> {
        self.pointers.get(&ptr)
    }
}

/// Given an operand, replace all Pointer literals with Label operands obtained
/// from the Database.
pub fn replace_operand_with_label<I, F, P>(src_operand: &ast::Operand<ast::Literal<I, F, P>>, db: &Database<P>) -> ast::Operand<ast::Literal<I, F, P>> where P: Clone + Eq + Hash, ast::Literal<I, F, P>: Clone {
    match src_operand.clone() {
        ast::Operand::Literal(ast::Literal::Pointer(pt)) => {
            //TODO: How do we retain pointer context?
            if let Some(lbl) = db.pointer_label(memory::Pointer::from(pt)) {
                ast::Operand::Label(lbl.clone())
            } else {
                src_operand.clone()
            }
        },
        ast::Operand::DataReference(op) => ast::Operand::DataReference(Box::new(replace_operand_with_label(op.as_ref(), db))),
        ast::Operand::CodeReference(op) => ast::Operand::CodeReference(Box::new(replace_operand_with_label(op.as_ref(), db))),
        ast::Operand::Indirect(op) => ast::Operand::Indirect(Box::new(replace_operand_with_label(op.as_ref(), db))),
        ast::Operand::Add(opl, opr) => ast::Operand::Add(Box::new(replace_operand_with_label(opl.as_ref(), db)), Box::new(replace_operand_with_label(opr.as_ref(), db))),
        _ => src_operand.clone()
    }
}

pub fn replace_labels<I, F, P>(src_assembly: ast::Assembly<ast::Literal<I, F, P>>, db: &Database<P>) -> ast::Assembly<ast::Literal<I, F, P>> where P: Clone + Eq + Hash, ast::Literal<I, F, P>: Clone {
    let mut dst_assembly = ast::Assembly::new();

    for line in src_assembly.iter_lines() {
        if let Some(instr) = line.instr() {
            let mut new_operands = Vec::new();

            for operand in instr.iter_operands() {
                //replace the operand...

                new_operands.push(replace_operand_with_label(operand, db));
            }

            let new_instr = ast::Instruction::new(instr.opcode(), new_operands);
            let (label, old_instr, comment) = line.clone().into_parts();
            dst_assembly.append_line(ast::Line::new(label, Some(new_instr), comment));
        } else {
            dst_assembly.append_line(line.clone());
        }
    }

    dst_assembly
}