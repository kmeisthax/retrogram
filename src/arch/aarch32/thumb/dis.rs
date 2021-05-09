//! THUMB instruction set disassembly

use crate::analysis::Flow;
use crate::analysis::Reference as refr;
use crate::analysis::ReferenceKind as refkind;
use crate::arch::aarch32;
use crate::arch::aarch32::thumb::{ThumbInstruction, THUMB_STATE};
use crate::arch::aarch32::Aarch32Register as A32Reg;
use crate::arch::aarch32::{Bus, Disasm, Literal, PtrVal};
use crate::ast::{Instruction, Operand as op};
use crate::reg::Symbolic;
use crate::{memory, reg};

/// Disassemble the instruction at `p` in `mem`.
///
/// This function returns a `Disasm` if the memory location points to a valid
/// instruction, otherwise an analysis error of some kind.
#[allow(clippy::many_single_char_names)]
pub fn disassemble<L>(p: &memory::Pointer<PtrVal>, mem: &Bus) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let (instr, length) = ThumbInstruction::from_static_stream(p, mem)?;
    let (ast_instr, flow, targets) = match instr {
        ThumbInstruction::LogicalShiftLeftImmediate(rd, rm, immed8) => (
            Instruction::new(
                "LSL",
                vec![
                    op::sym(&rd.to_string()),
                    op::sym(&rm.to_string()),
                    op::lit(immed8 as u32),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::LogicalShiftRightImmediate(rd, rm, immed8) => (
            Instruction::new(
                "LSR",
                vec![
                    op::sym(&rd.to_string()),
                    op::sym(&rm.to_string()),
                    op::lit(immed8 as u32),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::ArithmeticShiftRightImmediate(rd, rm, immed8) => (
            Instruction::new(
                "ASR",
                vec![
                    op::sym(&rd.to_string()),
                    op::sym(&rm.to_string()),
                    op::lit(immed8 as u32),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::AddRegister(rd, rn, rm) => (
            Instruction::new(
                "ADD",
                vec![
                    op::sym(&rd.to_string()),
                    op::sym(&rn.to_string()),
                    op::sym(&rm.to_string()),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::SubRegister(rd, rn, rm) => (
            Instruction::new(
                "SUB",
                vec![
                    op::sym(&rd.to_string()),
                    op::sym(&rn.to_string()),
                    op::sym(&rm.to_string()),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::AddImmediate3(rd, rn, immed3) => (
            Instruction::new(
                "ADD",
                vec![
                    op::sym(&rd.to_string()),
                    op::sym(&rn.to_string()),
                    op::lit(immed3 as u32),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::SubImmediate3(rd, rn, immed3) => (
            Instruction::new(
                "SUB",
                vec![
                    op::sym(&rd.to_string()),
                    op::sym(&rn.to_string()),
                    op::lit(immed3 as u32),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::MovImmediate8(rd, immed8) => (
            Instruction::new(
                "MOV",
                vec![op::sym(&rd.to_string()), op::lit(immed8 as u32)],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::CmpImmediate8(rd, immed8) => (
            Instruction::new(
                "CMP",
                vec![op::sym(&rd.to_string()), op::lit(immed8 as u32)],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::AddImmediate8(rd, immed8) => (
            Instruction::new(
                "ADD",
                vec![op::sym(&rd.to_string()), op::lit(immed8 as u32)],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::SubImmediate8(rd, immed8) => (
            Instruction::new(
                "SUB",
                vec![op::sym(&rd.to_string()), op::lit(immed8 as u32)],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::AddImmediate8Pc(rd, immed8) => (
            Instruction::new(
                "ADD",
                vec![
                    op::sym(&rd.to_string()),
                    op::sym("PC"),
                    op::lit(immed8 as u32 * 4),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::AddImmediate8Sp(rd, immed8) => (
            Instruction::new(
                "ADD",
                vec![
                    op::sym(&rd.to_string()),
                    op::sym("SP"),
                    op::lit(immed8 as u32 * 4),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::AddSpImmediate7(immed7) => (
            Instruction::new("ADD", vec![op::sym("SP"), op::lit(immed7 as u32)]),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::SubSpImmediate7(immed7) => (
            Instruction::new("SUB", vec![op::sym("SP"), op::lit(immed7 as u32)]),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::AndRegister(rd, rm) => (
            Instruction::new(
                "AND",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::EorRegister(rd, rm) => (
            Instruction::new(
                "EOR",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::LogicalShiftLeftRegister(rd, rs) => (
            Instruction::new(
                "LSL",
                vec![op::sym(&rd.to_string()), op::sym(&rs.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::LogicalShiftRightRegister(rd, rs) => (
            Instruction::new(
                "LSR",
                vec![op::sym(&rd.to_string()), op::sym(&rs.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::ArithmeticShiftRightRegister(rd, rs) => (
            Instruction::new(
                "ASR",
                vec![op::sym(&rd.to_string()), op::sym(&rs.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::AddCarryRegister(rd, rm) => (
            Instruction::new(
                "ADC",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::RotateRightRegister(rd, rs) => (
            Instruction::new(
                "ROR",
                vec![op::sym(&rd.to_string()), op::sym(&rs.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::SubCarryRegister(rd, rm) => (
            Instruction::new(
                "SBC",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::BitTestRegister(rn, rm) => (
            Instruction::new(
                "TST",
                vec![op::sym(&rn.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::NegateRegister(rd, rm) => (
            Instruction::new(
                "NEG",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::CompareRegister(rn, rm) => (
            Instruction::new(
                "CMP",
                vec![op::sym(&rn.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::CompareNegativeRegister(rn, rm) => (
            Instruction::new(
                "CMN",
                vec![op::sym(&rn.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::OrRegister(rd, rm) => (
            Instruction::new(
                "ORR",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::MultiplyRegister(rd, rm) => (
            Instruction::new(
                "MUL",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::BitClearRegister(rd, rm) => (
            Instruction::new(
                "BIC",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::MoveNotRegister(rd, rm) => (
            Instruction::new(
                "MVN",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::AddHighRegister(rd, rm) => (
            Instruction::new(
                "ADD",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::CompareHighRegister(rd, rm) => (
            Instruction::new(
                "CMP",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::MovHighRegister(rd, rm) => (
            Instruction::new(
                "MOV",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::SoftwareInterrupt(offset) => {
            let mut swi_target = p.contextualize(0x0000_0008);

            swi_target.set_arch_context(THUMB_STATE, Symbolic::from(0));
            (
                Instruction::new("SWI", vec![op::lit(offset as u32)]),
                Flow::Calling,
                vec![refr::new_static_ref(
                    p.clone(),
                    swi_target,
                    refkind::Subroutine,
                )],
            )
        }
        ThumbInstruction::BranchConditional(cond, offset) => {
            let signed_offset = (offset as i32) << 1;
            let target = p.contextualize((signed_offset - 4 + *p.as_pointer() as i32) as PtrVal);
            (
                Instruction::new(&format!("B{}", cond), vec![]),
                Flow::Branching(true),
                vec![refr::new_static_ref(p.clone(), target, refkind::Code)],
            )
        }
        ThumbInstruction::Branch(offset) => {
            let offset = (offset as i32) << 1;
            let target = p.contextualize((offset - 4 + *p.as_pointer() as i32) as PtrVal);
            (
                Instruction::new("B", vec![op::cptr(target.clone())]),
                Flow::Branching(false),
                vec![refr::new_static_ref(p.clone(), target, refkind::Code)],
            )
        }
        ThumbInstruction::BranchLink(offset) => {
            let target = p.contextualize((*p.as_pointer() as i32 + offset as i32) as u32);

            (
                Instruction::new("BL", vec![op::cptr(target.clone())]),
                Flow::Branching(false),
                vec![refr::new_static_ref(p.clone(), target, refkind::Subroutine)],
            )
        }
        ThumbInstruction::BranchLinkExchange(offset) => {
            let mut target =
                p.contextualize((*p.as_pointer() as i32 + offset) as u32 & 0xFFFF_FFFC);
            target.set_arch_context(THUMB_STATE, Symbolic::from(0));

            (
                Instruction::new("BLX", vec![op::cptr(target.clone())]),
                Flow::Branching(false),
                vec![refr::new_static_ref(p.clone(), target, refkind::Subroutine)],
            )
        }
        ThumbInstruction::BranchExchangeDynamic(rm) => (
            Instruction::new("BX", vec![op::sym(&rm.to_string())]),
            Flow::Branching(false),
            vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
        ),
        ThumbInstruction::BranchLinkExchangeDynamic(rm) => (
            Instruction::new("BLX", vec![op::sym(&rm.to_string())]),
            Flow::Calling,
            vec![refr::new_dyn_ref(p.clone(), refkind::Subroutine)],
        ),
        ThumbInstruction::StoreWordSpRelative(rd, immed8) => (
            Instruction::new(
                "STR",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![
                            op::sym("SP"),
                            op::infix(op::lit(immed8 as u32), "*", op::lit(4)),
                        ],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::StoreWordRegisterRelative(rd, rn, rm) => (
            Instruction::new(
                "STR",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![op::sym(&rn.to_string()), op::sym(&rm.to_string())],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::StoreWordRegisterImmedOffset(rd, rn, immed5) => (
            Instruction::new(
                "STR",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![
                            op::sym(&rn.to_string()),
                            op::infix(op::lit(immed5 as u32), "*", op::lit(4)),
                        ],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::StoreHalfwordRegisterRelative(rd, rn, rm) => (
            Instruction::new(
                "STRH",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![op::sym(&rn.to_string()), op::sym(&rm.to_string())],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::StoreHalfwordRegisterImmedOffset(rd, rn, immed5) => (
            Instruction::new(
                "STRH",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![
                            op::sym(&rn.to_string()),
                            op::infix(op::lit(immed5 as u32), "*", op::lit(2)),
                        ],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::StoreByteRegisterRelative(rd, rn, rm) => (
            Instruction::new(
                "STRB",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![op::sym(&rn.to_string()), op::sym(&rm.to_string())],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::StoreByteRegisterImmedOffset(rd, rn, immed5) => (
            Instruction::new(
                "STRB",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![op::sym(&rn.to_string()), op::lit(immed5 as u32)],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::LoadWordPcRelative(rd, immed8) => (
            Instruction::new(
                "LDR",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![
                            op::sym("PC"),
                            op::infix(op::lit(immed8 as u32), "*", op::lit(4)),
                        ],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::LoadWordSpRelative(rd, immed8) => (
            Instruction::new(
                "LDR",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![
                            op::sym("SP"),
                            op::infix(op::lit(immed8 as u32), "*", op::lit(4)),
                        ],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::LoadSignedByteRegisterRelative(rd, rn, rm) => (
            Instruction::new(
                "LDRSB",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![op::sym(&rn.to_string()), op::sym(&rm.to_string())],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::LoadWordRegisterRelative(rd, rn, rm) => (
            Instruction::new(
                "LDR",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![op::sym(&rn.to_string()), op::sym(&rm.to_string())],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::LoadWordRegisterImmedOffset(rd, rn, immed5) => (
            Instruction::new(
                "LDR",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![
                            op::sym(&rn.to_string()),
                            op::infix(op::lit(immed5 as u32), "*", op::lit(4)),
                        ],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::LoadHalfwordRegisterRelative(rd, rn, rm) => (
            Instruction::new(
                "LDRH",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![op::sym(&rn.to_string()), op::sym(&rm.to_string())],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::LoadHalfwordRegisterImmedOffset(rd, rn, immed5) => (
            Instruction::new(
                "LDRH",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![
                            op::sym(&rn.to_string()),
                            op::infix(op::lit(immed5 as u32), "*", op::lit(2)),
                        ],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::LoadByteRegisterRelative(rd, rn, rm) => (
            Instruction::new(
                "LDRB",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![op::sym(&rn.to_string()), op::sym(&rm.to_string())],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::LoadByteRegisterImmedOffset(rd, rn, immed5) => (
            Instruction::new(
                "LDRB",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![op::sym(&rn.to_string()), op::lit(immed5 as u32)],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::LoadSignedHalfwordRegisterRelative(rd, rn, rm) => (
            Instruction::new(
                "LDRSH",
                vec![
                    op::sym(&rd.to_string()),
                    op::wrap(
                        "[",
                        vec![op::sym(&rn.to_string()), op::sym(&rm.to_string())],
                        "]",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::SignExtendHalfword(rd, rm) => (
            Instruction::new(
                "SXTH",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::SignExtendByte(rd, rm) => (
            Instruction::new(
                "SXTB",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::UnsignExtendHalfword(rd, rm) => (
            Instruction::new(
                "UXTH",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::UnsignExtendByte(rd, rm) => (
            Instruction::new(
                "UXTB",
                vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::ByteReverseWord(rd, rn) => (
            Instruction::new(
                "REV",
                vec![op::sym(&rd.to_string()), op::sym(&rn.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::ByteReversePackedHalfword(rd, rn) => (
            Instruction::new(
                "REV16",
                vec![op::sym(&rd.to_string()), op::sym(&rn.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::ByteReverseSignedHalfword(rd, rn) => (
            Instruction::new(
                "REVSH",
                vec![op::sym(&rd.to_string()), op::sym(&rn.to_string())],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::Push(rlist) => (
            Instruction::new(
                "PUSH",
                vec![op::wrap(
                    "{",
                    rlist.into_iter().map(|r| op::sym(&r.to_string())).collect(),
                    "}",
                )],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::Pop(rlist) => (
            Instruction::new(
                "POP",
                vec![op::wrap(
                    "{",
                    rlist.into_iter().map(|r| op::sym(&r.to_string())).collect(),
                    "}",
                )],
            ),
            if rlist.contains(A32Reg::R15) {
                Flow::Returning(false)
            } else {
                Flow::Normal
            },
            vec![],
        ),
        ThumbInstruction::Breakpoint(immed) => {
            let mut bkpt_target = p.contextualize(0x0000_000C);
            bkpt_target.set_arch_context(THUMB_STATE, reg::Symbolic::from(0));

            (
                Instruction::new("BKPT", vec![op::lit(immed as u32)]),
                Flow::Calling,
                vec![refr::new_static_ref(
                    p.clone(),
                    bkpt_target,
                    refkind::Subroutine,
                )],
            )
        }
        ThumbInstruction::StoreMultipleIncrementAfter(rn, rlist) => (
            Instruction::new(
                "STMIA",
                vec![
                    op::suff(op::sym(&rn.to_string()), "!"),
                    op::wrap(
                        "{",
                        rlist.into_iter().map(|r| op::sym(&r.to_string())).collect(),
                        "}",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
        ThumbInstruction::LoadMultipleIncrementAfter(rn, rlist) => (
            Instruction::new(
                "LDMIA",
                vec![
                    op::suff(op::sym(&rn.to_string()), "!"),
                    op::wrap(
                        "{",
                        rlist.into_iter().map(|r| op::sym(&r.to_string())).collect(),
                        "}",
                    ),
                ],
            ),
            Flow::Normal,
            vec![],
        ),
    };

    Ok(Disasm::new(ast_instr, length, flow, targets))
}
