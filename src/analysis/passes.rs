//! Analysis passes responsible for transforming AST output from the
//! disassemblers.

use crate::analysis::ReferenceKind;
use crate::ast::{Instruction, Literal, Operand};
use crate::database::Database;
use crate::{analysis, ast, memory};
use std::collections::HashSet;
use std::convert::TryFrom;
use std::fmt;
use std::fmt::UpperHex;

pub type DisassmbledSection<L, P, MV, S> = (
    ast::Section<L, P, MV, S>,
    HashSet<analysis::Reference<P>>,
    Option<S>,
    Vec<analysis::Block<P, S>>,
    Option<analysis::Error<P, S>>,
);

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
///  * Any error generated during the disassembly process. If present, block is
///    incomplete.
pub fn disassemble_block<L, P, MV, S, IO, DIS>(
    start_pc: memory::Pointer<P>,
    plat: &memory::Memory<P, MV, S, IO>,
    disassemble: &DIS,
) -> DisassmbledSection<L, P, MV, S>
where
    L: Literal,
    P: memory::PtrNum<S> + analysis::Mappable + fmt::Display + fmt::UpperHex,
    S: memory::Offset<P> + fmt::Display,
    DIS: analysis::Disassembler<L, P, MV, S, IO>,
    Instruction<L>: Clone,
{
    let mut pc = start_pc.clone();
    let mut asm = ast::Section::new(&format!("Untitled Section at {}", pc.as_pointer()));
    let mut targets = HashSet::new();
    let mut blocks = Vec::new();
    let mut cur_block_pc = start_pc.clone();
    let mut cur_blk_size = S::zero();
    let mut error = None;

    loop {
        match disassemble(&pc, &plat) {
            Ok(disasm) => {
                let new_pcval = match pc.as_pointer().clone().checked_add(disasm.next_offset()) {
                    Some(new_pcval) => new_pcval,
                    None => {
                        error = Some(analysis::Error::BlockSizeOverflow);
                        break;
                    }
                };
                let new_blk_size = match cur_blk_size.clone().checked_add(disasm.next_offset()) {
                    Some(cur_blk_size) => cur_blk_size,
                    None => {
                        error = Some(analysis::Error::BlockSizeOverflow);
                        break;
                    }
                };

                asm.append_directive(disasm.directive(), pc.clone());

                for target in disasm.iter_targets() {
                    targets.insert(target.clone());
                }

                pc = pc.contextualize(new_pcval);
                cur_blk_size = new_blk_size;

                if disasm.flow().is_branching() {
                    blocks.push(analysis::Block::from_parts(
                        cur_block_pc.clone(),
                        cur_blk_size,
                    ));
                    cur_block_pc = pc.clone();
                    cur_blk_size = S::zero();
                }

                if disasm.flow().is_final() {
                    break;
                }
            }
            Err(e) => {
                error = Some(e);
                break;
            }
        }
    }

    if cur_blk_size > S::zero() {
        blocks.push(analysis::Block::from_parts(cur_block_pc, cur_blk_size));
    }

    (
        asm,
        targets,
        S::try_from(pc.as_pointer().clone() - start_pc.as_pointer().clone()).ok(),
        blocks,
        error,
    )
}

/// Given an operand, replace all Pointer literals with Label operands obtained
/// from the Database.
pub fn replace_operand_with_label<L, P, AMV, AS, AIO>(
    src_operand: Operand<L>,
    db: &mut Database<P, AS>,
    start_addr: &memory::Pointer<P>,
    memory: &memory::Memory<P, AMV, AS, AIO>,
    refkind: ReferenceKind,
) -> Operand<L>
where
    L: Literal<PtrVal = P>,
    P: memory::PtrNum<AS> + analysis::Mappable + Clone + UpperHex,
    AS: memory::Offset<P> + Clone,
    Operand<L>: Clone,
{
    match src_operand {
        ast::Operand::Literal(l) if l.is_pointer() => {
            let pt = l.into_pointer().unwrap();
            let cpt = memory.minimize_context(pt);

            if let Some(sym_id) = db.pointer_symbol(&cpt) {
                let sym = db
                    .symbol(sym_id)
                    .expect("Database handed an invalid symbol back");
                ast::Operand::Label(sym.as_label().clone())
            } else {
                ast::Operand::Label(db.insert_placeholder_label(cpt, refkind))
            }
        }
        ast::Operand::DataReference(op) => ast::Operand::DataReference(Box::new(
            replace_operand_with_label(*op, db, start_addr, memory, ReferenceKind::Data),
        )),
        ast::Operand::CodeReference(op) => ast::Operand::CodeReference(Box::new(
            replace_operand_with_label(*op, db, start_addr, memory, ReferenceKind::Code),
        )),
        ast::Operand::Indirect(op) => ast::Operand::Indirect(Box::new(replace_operand_with_label(
            *op, db, start_addr, memory, refkind,
        ))),
        ast::Operand::Infix(opl, infix, opr) => ast::Operand::Infix(
            Box::new(replace_operand_with_label(
                *opl, db, start_addr, memory, refkind,
            )),
            infix,
            Box::new(replace_operand_with_label(
                *opr, db, start_addr, memory, refkind,
            )),
        ),
        _ => src_operand,
    }
}

/// Given an Assembly, create a new Assembly with all pointers replaced with
/// their equivalent labels in the database.
///
/// If a given pointer has no matching label, then a temporary label will be
/// automatically generated and added to the database.
pub fn replace_labels<L, P, AMV, AS, AIO>(
    src_assembly: ast::Section<L, P, AMV, AS>,
    db: &mut Database<P, AS>,
    memory: &memory::Memory<P, AMV, AS, AIO>,
) -> ast::Section<L, P, AMV, AS>
where
    L: Literal<PtrVal = P>,
    P: memory::PtrNum<AS> + analysis::Mappable + Clone + UpperHex,
    AS: memory::Offset<P> + Clone,
    ast::Directive<L, P, AMV, AS>: Clone,
    Operand<L>: Clone,
{
    let mut dst_assembly = ast::Section::new(src_assembly.section_name());

    for (directive, loc) in src_assembly.iter_directives() {
        match directive {
            ast::Directive::EmitInstr(instr, offset) => {
                let mut new_operands = Vec::new();

                for operand in instr.iter_operands() {
                    new_operands.push(replace_operand_with_label(
                        operand.clone(),
                        db,
                        loc,
                        memory,
                        ReferenceKind::Unknown,
                    ));
                }

                let new_instr = ast::Instruction::new(instr.opcode(), new_operands);
                dst_assembly.append_directive(
                    ast::Directive::EmitInstr(new_instr, offset.clone()),
                    loc.clone(),
                );
            }
            _ => dst_assembly.append_directive(directive.clone(), loc.clone()),
        }
    }

    dst_assembly
}

/// Given an Assembly, create a new Assembly with all labels inserted from the
/// database.
pub fn inject_labels<L, P, MV, S>(
    src_assembly: ast::Section<L, P, MV, S>,
    db: &Database<P, S>,
) -> ast::Section<L, P, MV, S>
where
    L: Literal,
    P: analysis::Mappable,
    ast::Directive<L, P, MV, S>: Clone,
{
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
            if !labeled_locations_set.contains(loc) {
                labeled_locations_set.insert(loc);

                let sym = db
                    .symbol(sym_id)
                    .expect("Database gave an invalid symbol ID instead of None!!!");

                dst_assembly.append_directive(
                    ast::Directive::DeclareLabel(sym.as_label().clone()),
                    loc.clone(),
                );
            }
        }

        dst_assembly.append_directive(directive.clone(), loc.clone());
    }

    dst_assembly
}

/// Inject organization directives into a block.
///
/// An organization directive is necessary any time there is a discontinuity
/// between the end location of one instruction in the directive stream, and the
/// start location of the next one.
pub fn inject_orgs<L, P, AMV, AS>(
    src_assembly: ast::Section<L, P, AMV, AS>,
) -> ast::Section<L, P, AMV, AS>
where
    L: Literal,
    P: memory::PtrNum<AS> + analysis::Mappable + Clone,
    AS: memory::Offset<P> + Clone + TryFrom<usize>,
    ast::Directive<L, P, AMV, AS>: Clone,
{
    let mut dst_assembly = ast::Section::new(src_assembly.section_name());
    let mut expected_next_pc = None;

    for (dir, pc) in src_assembly.iter_directives() {
        let discontinuity_accounted_for = match dir {
            ast::Directive::DeclareOrg(org_pc) => expected_next_pc == Some(org_pc.clone()),
            _ => false,
        };

        if expected_next_pc != Some(pc.clone()) && !discontinuity_accounted_for {
            dst_assembly.append_directive(ast::Directive::DeclareOrg(pc.clone()), pc.clone());
            expected_next_pc = Some(pc.clone());
        }

        expected_next_pc = match dir {
            ast::Directive::EmitInstr(_, offset) => Some(pc.clone() + offset.clone()),
            ast::Directive::EmitData(d) => {
                if let Ok(d_len) = AS::try_from(d.len()) {
                    Some(pc.clone() + d_len)
                } else {
                    expected_next_pc
                }
            }
            ast::Directive::EmitSpace(offset) => Some(pc.clone() + offset.clone()),
            _ => expected_next_pc,
        };

        dst_assembly.append_directive(dir.clone(), pc.clone());
    }

    dst_assembly
}
