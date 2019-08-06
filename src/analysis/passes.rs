//! Analysis passes responsible for transforming AST output from the
//! disassemblers.

use std::{fmt, io};
use std::collections::HashSet;
use std::fmt::UpperHex;
use crate::analysis::ReferenceKind;
use crate::database::Database;
use crate::{ast, memory, analysis};

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
/// 
/// This function also returns the offset to the end of the last instruction.
/// 
/// # Return value
/// 
/// This function returns the following values:
/// 
///  * An AST representation of the disassembled code
///  * A list of references generated by the disassembly
///  * How many bytes were disassembled
///  * A list of blocks generated by the disassembly
///  * Whether or not the disassembly ended due to an invalid instruction
pub fn disassemble_block<I, SI, F, P, MV, S, IO, DIS>(start_pc: memory::Pointer<P>,
    plat: &memory::Memory<P, MV, S, IO>, disassemble: &DIS) ->
        io::Result<(ast::Section<I, SI, F, P, MV, S>, HashSet<analysis::Reference<P>>, Option<S>, Vec<analysis::Block<P, S>>, bool)>
    where P: memory::PtrNum<S> + analysis::Mappable + fmt::Display + fmt::UpperHex,
        S: memory::Offset<P> + fmt::Display,
        DIS: analysis::Disassembler<I, SI, F, P, MV, S, IO>,
        ast::Instruction<I, SI, F, P>: Clone {
    
    let mut pc = start_pc.clone();
    let mut asm = ast::Section::new(&format!("Untitled Section at {}", pc.as_pointer()));
    let mut targets = HashSet::new();
    let mut blocks = Vec::new();
    let mut cur_block_pc = start_pc.clone();
    let mut cur_blk_size = S::zero();

    loop {
        match disassemble(&pc, &plat) {
            Ok(disasm) => {
                asm.append_directive(disasm.directive(), pc.clone());
                pc = pc.contextualize(P::from(pc.as_pointer().clone() + disasm.next_offset()));
                cur_blk_size = S::try_from(cur_blk_size + disasm.next_offset()).map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Could not increase size of block by instruction offset"))?;

                for target in disasm.iter_targets() {
                    targets.insert(target.clone());
                }

                if !disasm.flow().is_nonbranching() {
                    blocks.push(analysis::Block::from_parts(cur_block_pc.clone(), cur_blk_size));
                    cur_block_pc = pc.clone();
                    cur_blk_size = S::zero();
                }

                if !disasm.flow().is_nonfinal() {
                    if cur_blk_size > S::zero() {
                        blocks.push(analysis::Block::from_parts(cur_block_pc.clone(), cur_blk_size));
                    }

                    return Ok((asm, targets, S::try_from(pc.as_pointer().clone() - start_pc.as_pointer().clone()).ok(), blocks, false));
                }
            },
            Err(_) => {
                //TODO: Find a way to propagate the error and partial results
                if cur_blk_size > S::zero() {
                    blocks.push(analysis::Block::from_parts(cur_block_pc.clone(), cur_blk_size));
                }

                return Ok((asm, targets, S::try_from(pc.as_pointer().clone() - start_pc.as_pointer().clone()).ok(), blocks, true));
            }
        }
    }
}

/// Given an operand, replace all Pointer literals with Label operands obtained
/// from the Database.
pub fn replace_operand_with_label<I, S, F, P, AMV, AS, AIO>(src_operand: ast::Operand<I, S, F, P>, db: &mut Database<P, AS>, start_addr: &memory::Pointer<P>, memory: &memory::Memory<P, AMV, AS, AIO>, refkind: ReferenceKind) -> ast::Operand<I, S, F, P>
    where P: memory::PtrNum<AS> + analysis::Mappable + Clone + UpperHex,
        AS: memory::Offset<P> + Clone,
        ast::Operand<I, S, F, P>: Clone {
    match src_operand {
        ast::Operand::Literal(ast::Literal::Pointer(pt)) => {
            let cpt = memory.minimize_context(pt);

            if let Some(sym_id) = db.pointer_symbol(&cpt) {
                let sym = db.symbol(sym_id).expect("Database handed an invalid symbol back");
                ast::Operand::Label(sym.as_label().clone())
            } else {
                ast::Operand::Label(db.insert_placeholder_label(cpt, refkind))
            }
        },
        ast::Operand::DataReference(op) => ast::Operand::DataReference(Box::new(replace_operand_with_label(*op, db, start_addr, memory, ReferenceKind::Data))),
        ast::Operand::CodeReference(op) => ast::Operand::CodeReference(Box::new(replace_operand_with_label(*op, db, start_addr, memory, ReferenceKind::Code))),
        ast::Operand::Indirect(op) => ast::Operand::Indirect(Box::new(replace_operand_with_label(*op, db, start_addr, memory, refkind))),
        ast::Operand::Infix(opl, infix, opr) => ast::Operand::Infix(Box::new(replace_operand_with_label(*opl, db, start_addr, memory, refkind)), infix, Box::new(replace_operand_with_label(*opr, db, start_addr, memory, refkind))),
        _ => src_operand
    }
}

/// Given an Assembly, create a new Assembly with all pointers replaced with
/// their equivalent labels in the database.
/// 
/// If a given pointer has no matching label, then a temporary label will be
/// automatically generated and added to the database.
pub fn replace_labels<I, S, F, P, AMV, AS, AIO>
    (src_assembly: ast::Section<I, S, F, P, AMV, AS>,
        db: &mut Database<P, AS>,
        memory: &memory::Memory<P, AMV, AS, AIO>) -> ast::Section<I, S, F, P, AMV, AS>
    where P: memory::PtrNum<AS> + analysis::Mappable + Clone + UpperHex,
        AS: memory::Offset<P> + Clone,
        ast::Directive<I, S, F, P, AMV, AS>: Clone, ast::Operand<I, S, F, P>: Clone {
    let mut dst_assembly = ast::Section::new(src_assembly.section_name());

    for (directive, loc) in src_assembly.iter_directives() {
        match directive {
            ast::Directive::EmitInstr(instr) => {
                let mut new_operands = Vec::new();

                for operand in instr.iter_operands() {
                    new_operands.push(replace_operand_with_label(operand.clone(), db, loc, memory, ReferenceKind::Unknown));
                }

                let new_instr = ast::Instruction::new(instr.opcode(), new_operands);
                dst_assembly.append_directive(ast::Directive::EmitInstr(new_instr), loc.clone());
            },
            _ => dst_assembly.append_directive(directive.clone(), loc.clone())
        }
    }

    dst_assembly
}

/// Given an Assembly, create a new Assembly with all labels inserted from the
/// database.
pub fn inject_labels<I, SI, F, P, MV, S>
    (src_assembly: ast::Section<I, SI, F, P, MV, S>,
        db: &Database<P, S>) -> ast::Section<I, SI, F, P, MV, S>
    where P: analysis::Mappable, ast::Directive<I, SI, F, P, MV, S>: Clone {
    
    let mut dst_assembly = ast::Section::new(src_assembly.section_name());
    
    // The set of locations that already have a label
    let mut labeled_locations_set = HashSet::new();

    for (directive, loc) in src_assembly.iter_directives() {
        if let ast::Directive::DeclareLabel(_) = directive {
            labeled_locations_set.insert(loc);
        }
    }
    
    for (directive, loc) in src_assembly.iter_directives() {
        //TODO: If two symbols exist at the same location only one gets injected
        if let Some(sym_id) = db.pointer_symbol(&loc) {
            if labeled_locations_set.contains(loc) {
                labeled_locations_set.insert(loc);

                let sym = db.symbol(sym_id).expect("Database gave an invalid symbol ID instead of None!!!");

                dst_assembly.append_directive(ast::Directive::DeclareLabel(sym.as_label().clone()), loc.clone());
            }
        }

        dst_assembly.append_directive(directive.clone(), loc.clone());
    }

    dst_assembly
}