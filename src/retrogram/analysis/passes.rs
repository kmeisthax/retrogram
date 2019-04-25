//! Analysis passes responsible for transforming AST output from the
//! disassemblers.

use std::{fs, io};
use std::collections::HashMap;
use std::hash::Hash;
use std::ops::{Add, Sub};
use std::fmt::{Display, Formatter, Result, UpperHex};
use crate::retrogram::analysis::{Database, ReferenceKind};
use crate::retrogram::{ast, memory, project, analysis};

/// Given an operand, replace all Pointer literals with Label operands obtained
/// from the Database.
pub fn replace_operand_with_label<I, F, P, AMV, AS, AIO>(src_operand: ast::Operand<I, F, P>, db: &mut Database<P>, start_addr: &memory::Pointer<P>, memory: &memory::Memory<P, AMV, AS, AIO>, refkind: ReferenceKind) -> ast::Operand<I, F, P>
    where P: memory::PtrNum<AS> + analysis::Mappable + Clone + UpperHex,
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
    where P: memory::PtrNum<AS> + analysis::Mappable + Clone + UpperHex,
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
    where P: analysis::Mappable, I: Clone, F: Clone {
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