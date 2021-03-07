use crate::analysis::{Flow, Reference, ReferenceKind};
use crate::arch::sm83;
use crate::arch::sm83::{Bus, Condition, Disasm, Instruction, Literal, PtrVal};
use crate::ast::{Instruction as inst, Operand, Operand as op};
use crate::memory;

/// Append a condition code onto an operand.
fn cond_operand<L>(cond: Option<Condition>, operand: Option<Operand<L>>) -> Vec<Operand<L>>
where
    L: Literal,
{
    if let Some(cond) = cond {
        if let Some(operand) = operand {
            vec![cond.into_operand(), operand]
        } else {
            vec![cond.into_operand()]
        }
    } else if let Some(operand) = operand {
        vec![operand]
    } else {
        vec![]
    }
}

#[derive(Clone)]
pub enum AbstractOperand {
    Symbol(&'static str),
    Indirect(&'static str),
}

impl<L> Into<Operand<L>> for AbstractOperand
where
    L: Literal,
{
    fn into(self) -> Operand<L> {
        match self {
            Self::Symbol(s) => op::sym(s),
            Self::Indirect(is) => op::indir(op::sym(is)),
        }
    }
}

lazy_static! {
    /// z80 instruction encoding uses this 3-bit enumeration to encode the target of
    /// 8-bit ALU or register transfer operations.
    pub static ref ALU_TARGET_REGS: [AbstractOperand; 8] = [AbstractOperand::Symbol("b"), AbstractOperand::Symbol("c"), AbstractOperand::Symbol("d"), AbstractOperand::Symbol("e"), AbstractOperand::Symbol("h"), AbstractOperand::Symbol("l"), AbstractOperand::Indirect("hl"), AbstractOperand::Symbol("a")];
}

/// z80 instruction encoding uses this 2-bit enumeration for memory pointers.
pub static ALU_TARGET_MEM: [&str; 4] = ["bc", "de", "hli", "hld"];

/// Disassemble the instruction at `p` in `mem`.
///
/// This function returns a `Disasm` if the memory location points to a valid
/// instruction, otherwise an analysis error of some kind.
pub fn disassemble<L>(p: &memory::Pointer<PtrVal>, mem: &Bus) -> sm83::Result<Disasm<L>>
where
    L: Literal,
{
    Ok(match Instruction::from_static_stream(p, mem)? {
        (Instruction::Nop, offset) => {
            Disasm::new(inst::new("nop", vec![]), offset, Flow::Normal, vec![])
        }
        (Instruction::LdReg8(tgt, src), offset) => Disasm::new(
            inst::new("ld", vec![tgt.into_operand(), src.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::LdHLSP(sp_offset), offset) => Disasm::new(
            inst::new(
                "ld",
                vec![op::sym("hl"), op::addop(op::sym("sp"), op::lit(sp_offset))],
            ),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::LdSPHL, offset) => Disasm::new(
            inst::new("ld", vec![op::sym("sp"), op::sym("hl")]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::LdWriteStatic(mem_addr), offset) => Disasm::new(
            inst::new(
                "ld",
                vec![op::indir(op::dptr(p.contextualize(mem_addr))), op::sym("a")],
            ),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::LdWritePtr(ptr_regpair), offset) => Disasm::new(
            inst::new(
                "ld",
                vec![op::indir(ptr_regpair.into_operand()), op::sym("a")],
            ),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::LdWriteHiStatic(hi_addr), offset) => Disasm::new(
            inst::new(
                "ldh",
                vec![
                    op::indir(op::dptr(p.contextualize(0xFF00 + hi_addr as u16))),
                    op::sym("a"),
                ],
            ),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::LdWriteHiPtr, offset) => Disasm::new(
            inst::new("ld", vec![op::indir(op::sym("c")), op::sym("a")]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::LdReadStatic(mem_addr), offset) => Disasm::new(
            inst::new(
                "ld",
                vec![op::sym("a"), op::indir(op::dptr(p.contextualize(mem_addr)))],
            ),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::LdReadPtr(ptr_regpair), offset) => Disasm::new(
            inst::new(
                "ld",
                vec![op::sym("a"), op::indir(ptr_regpair.into_operand())],
            ),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::LdReadHiStatic(hi_addr), offset) => Disasm::new(
            inst::new(
                "ldh",
                vec![
                    op::sym("a"),
                    op::indir(op::dptr(p.contextualize(0xFF00 + hi_addr as u16))),
                ],
            ),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::LdReadHiPtr, offset) => Disasm::new(
            inst::new("ld", vec![op::sym("a"), op::indir(op::sym("c"))]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::LdConst8(tgt, const8), offset) => Disasm::new(
            inst::new("ld", vec![tgt.into_operand(), op::lit(const8)]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::LdConst16(regpair, const16), offset) => Disasm::new(
            inst::new("ld", vec![regpair.into_operand(), op::lit(const16)]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::LdWriteStaticSP(sp_base), offset) => Disasm::new(
            inst::new(
                "ld",
                vec![op::indir(op::dptr(p.contextualize(sp_base))), op::sym("sp")],
            ),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Inc16(regpair), offset) => Disasm::new(
            inst::new("inc", vec![regpair.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Inc8(tgt), offset) => Disasm::new(
            inst::new("inc", vec![tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Dec16(regpair), offset) => Disasm::new(
            inst::new("dec", vec![regpair.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Dec8(tgt), offset) => Disasm::new(
            inst::new("dec", vec![tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Jump(pc_addr, cond), offset) => Disasm::new(
            inst::new(
                "jp",
                cond_operand(cond, Some(op::cptr(p.contextualize(pc_addr)))),
            ),
            offset,
            Flow::Branching(cond.is_some()),
            vec![Reference::new_static_ref(
                p.clone(),
                p.contextualize(pc_addr),
                ReferenceKind::Code,
            )],
        ),
        (Instruction::JumpRelative(pc_offset, cond), offset) => {
            let tgt_ptr = (*p.as_pointer() as i16 + pc_offset as i16) as u16;
            Disasm::new(
                inst::new(
                    "jr",
                    cond_operand(cond, Some(op::cptr(p.contextualize(tgt_ptr)))),
                ),
                offset,
                Flow::Branching(cond.is_some()),
                vec![Reference::new_static_ref(
                    p.clone(),
                    p.contextualize(tgt_ptr),
                    ReferenceKind::Code,
                )],
            )
        }
        (Instruction::JumpDynamic, offset) => Disasm::new(
            inst::new("jp", vec![op::indir(op::sym("hl"))]),
            offset,
            Flow::Branching(false),
            vec![Reference::new_dyn_ref(p.clone(), ReferenceKind::Code)],
        ),
        (Instruction::Call(pc_addr, cond), offset) => Disasm::new(
            inst::new(
                "call",
                cond_operand(cond, Some(op::cptr(p.contextualize(pc_addr)))),
            ),
            offset,
            Flow::Calling,
            vec![Reference::new_static_ref(
                p.clone(),
                p.contextualize(pc_addr),
                ReferenceKind::Subroutine,
            )],
        ),
        (Instruction::CallRst(rst_vector), offset) => Disasm::new(
            inst::new("rst", vec![op::cptr(p.contextualize(rst_vector as u16))]),
            offset,
            Flow::Calling,
            vec![Reference::new_static_ref(
                p.clone(),
                p.contextualize(rst_vector as u16),
                ReferenceKind::Subroutine,
            )],
        ),
        (Instruction::Push(regpair), offset) => Disasm::new(
            inst::new("push", vec![regpair.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Pop(regpair), offset) => Disasm::new(
            inst::new("pop", vec![regpair.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Return(condcode), offset) => Disasm::new(
            inst::new("ret", cond_operand(condcode, None)),
            offset,
            Flow::Returning(condcode.is_some()),
            vec![],
        ),
        (Instruction::ReturnFromInterrupt, offset) => Disasm::new(
            inst::new("reti", vec![]),
            offset,
            Flow::Returning(false),
            vec![],
        ),
        (Instruction::Add16(regpair), offset) => Disasm::new(
            inst::new("add", vec![op::sym("hl"), regpair.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::AddSpConst(sp_offset), offset) => Disasm::new(
            inst::new("add", vec![op::sym("sp"), op::lit(sp_offset)]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Add8(tgt), offset) => Disasm::new(
            inst::new("add", vec![op::sym("a"), tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Add8Const(val), offset) => Disasm::new(
            inst::new("add", vec![op::sym("a"), op::lit(val)]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::AddCarry8(tgt), offset) => Disasm::new(
            inst::new("adc", vec![op::sym("a"), tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::AddCarry8Const(val), offset) => Disasm::new(
            inst::new("adc", vec![op::sym("a"), op::lit(val)]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Sub8(tgt), offset) => Disasm::new(
            inst::new("sub", vec![op::sym("a"), tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Sub8Const(val), offset) => Disasm::new(
            inst::new("sub", vec![op::sym("a"), op::lit(val)]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::SubCarry8(tgt), offset) => Disasm::new(
            inst::new("sbc", vec![op::sym("a"), tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::SubCarry8Const(val), offset) => Disasm::new(
            inst::new("sbc", vec![op::sym("a"), op::lit(val)]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::And8(tgt), offset) => Disasm::new(
            inst::new("and", vec![op::sym("a"), tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::And8Const(val), offset) => Disasm::new(
            inst::new("and", vec![op::sym("a"), op::lit(val)]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Xor8(tgt), offset) => Disasm::new(
            inst::new("xor", vec![op::sym("a"), tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Xor8Const(val), offset) => Disasm::new(
            inst::new("xor", vec![op::sym("a"), op::lit(val)]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Or8(tgt), offset) => Disasm::new(
            inst::new("or", vec![op::sym("a"), tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Or8Const(val), offset) => Disasm::new(
            inst::new("or", vec![op::sym("a"), op::lit(val)]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::Cp(tgt), offset) => Disasm::new(
            inst::new("cp", vec![op::sym("a"), tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::CpConst(val), offset) => Disasm::new(
            inst::new("cp", vec![op::sym("a"), op::lit(val)]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::RotateLeftCarry(tgt), offset) => Disasm::new(
            inst::new("rlc", vec![tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::RotateRightCarry(tgt), offset) => Disasm::new(
            inst::new("rrc", vec![tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::RotateLeft(tgt), offset) => Disasm::new(
            inst::new("rl", vec![tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::RotateRight(tgt), offset) => Disasm::new(
            inst::new("rr", vec![tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::ShiftLeftArithmetic(tgt), offset) => Disasm::new(
            inst::new("sla", vec![tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::ShiftRightArithmetic(tgt), offset) => Disasm::new(
            inst::new("sra", vec![tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::NybbleSwap(tgt), offset) => Disasm::new(
            inst::new("swap", vec![tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::ShiftRightLogical(tgt), offset) => Disasm::new(
            inst::new("srl", vec![tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::BitTest(bit, tgt), offset) => Disasm::new(
            inst::new("bit", vec![op::lit(bit), tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::BitReset(bit, tgt), offset) => Disasm::new(
            inst::new("res", vec![op::lit(bit), tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::BitSet(bit, tgt), offset) => Disasm::new(
            inst::new("set", vec![op::lit(bit), tgt.into_operand()]),
            offset,
            Flow::Normal,
            vec![],
        ),
        (Instruction::RotateLeftCarryAccum, offset) => {
            Disasm::new(inst::new("rlca", vec![]), offset, Flow::Normal, vec![])
        }
        (Instruction::RotateLeftAccum, offset) => {
            Disasm::new(inst::new("rla", vec![]), offset, Flow::Normal, vec![])
        }
        (Instruction::RotateRightCarryAccum, offset) => {
            Disasm::new(inst::new("rrca", vec![]), offset, Flow::Normal, vec![])
        }
        (Instruction::RotateRightAccum, offset) => {
            Disasm::new(inst::new("rra", vec![]), offset, Flow::Normal, vec![])
        }
        (Instruction::Compliment, offset) => {
            Disasm::new(inst::new("cpl", vec![]), offset, Flow::Normal, vec![])
        }
        (Instruction::DecimalAdjust, offset) => {
            Disasm::new(inst::new("daa", vec![]), offset, Flow::Normal, vec![])
        }
        (Instruction::SetCarry, offset) => {
            Disasm::new(inst::new("scf", vec![]), offset, Flow::Normal, vec![])
        }
        (Instruction::ClearCarry, offset) => {
            Disasm::new(inst::new("ccf", vec![]), offset, Flow::Normal, vec![])
        }
        (Instruction::DisableInterrupt, offset) => {
            Disasm::new(inst::new("di", vec![]), offset, Flow::Normal, vec![])
        }
        (Instruction::EnableInterrupt, offset) => {
            Disasm::new(inst::new("ei", vec![]), offset, Flow::Normal, vec![])
        }
        (Instruction::Halt, offset) => {
            Disasm::new(inst::new("halt", vec![]), offset, Flow::Normal, vec![])
        }
        (Instruction::Stop, offset) => {
            Disasm::new(inst::new("stop", vec![]), offset, Flow::Normal, vec![])
        }
    })
}
