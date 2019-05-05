//! Analysis passes responsible for transforming AST output from the
//! disassemblers.

use std::{fmt, io};
use std::collections::HashSet;
use std::fmt::UpperHex;
use crate::retrogram::analysis::{Database, ReferenceKind};
use crate::retrogram::{ast, memory, analysis};

/// Given memory and a pointer, disassemble a basic block of instructions and
/// return them.
/// 
/// A basic block consists of an unbroken string of instructions with the
/// following properties:
/// 
///  * Every instruction in the block naturally "follows" one another, according
///    to the offsets provided by the disassembler function.
///  * Disassembly continues until the program unconditionally jumps to another
///    location, returns, or executes an invalid instruction.
pub fn disassemble_block<I, SI, F, P, MV, S, IO, DIS>(start_pc: memory::Pointer<P>, plat: &memory::Memory<P, MV, S, IO>, disassemble: &DIS) -> io::Result<(ast::Section<I, SI, F, P>, HashSet<Option<memory::Pointer<P>>>)>
    where P: memory::PtrNum<S> + analysis::Mappable + fmt::Display, S: memory::Offset<P>,
        DIS: Fn(&memory::Pointer<P>, &memory::Memory<P, MV, S, IO>) -> (Option<ast::Instruction<I, SI, F, P>>, S, bool, Vec<Option<memory::Pointer<P>>>) {
    let mut pc = start_pc;
    let mut asm = ast::Section::new(&format!("Untitled Section at {}", pc.as_pointer()), &pc);
    let mut targets = HashSet::new();

    loop {
        match disassemble(&pc, &plat) {
            (Some(instr), size, is_nonfinal, instr_targets) => {
                asm.append_line(ast::Line::new(None, Some(instr), None, pc.clone().into_ptr()));
                pc = pc.contextualize(P::from(pc.as_pointer().clone() + size));

                for target in instr_targets {
                    targets.insert(target);
                }

                if !is_nonfinal {
                    return Ok((asm, targets));
                }
            },
            (None, _, _, _) => return Ok((asm, targets))
        }
    }
}

/// Given an operand, replace all Pointer literals with Label operands obtained
/// from the Database.
pub fn replace_operand_with_label<I, S, F, P, AMV, AS, AIO>(src_operand: ast::Operand<I, S, F, P>, db: &mut Database<P, AS>, start_addr: &memory::Pointer<P>, memory: &memory::Memory<P, AMV, AS, AIO>, refkind: ReferenceKind) -> ast::Operand<I, S, F, P>
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
pub fn replace_labels<I, S, F, P, AMV, AS, AIO>(src_assembly: ast::Section<I, S, F, P>, db: &mut Database<P, AS>, memory: &memory::Memory<P, AMV, AS, AIO>) -> ast::Section<I, S, F, P>
    where P: memory::PtrNum<AS> + analysis::Mappable + Clone + UpperHex,
        AS: memory::Offset<P> + Clone,
        I: Clone,
        S: Clone,
        F: Clone {
    let mut dst_assembly = ast::Section::new(src_assembly.section_name(), src_assembly.section_loc());

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
pub fn inject_labels<I, S, F, P, MS>(src_assembly: ast::Section<I, S, F, P>, db: &Database<P, MS>) -> ast::Section<I, S, F, P>
    where P: analysis::Mappable, I: Clone, S: Clone, F: Clone {
    let mut dst_assembly = ast::Section::new(src_assembly.section_name(), src_assembly.section_loc());
    
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