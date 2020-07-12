use crate::arch::sm83;
use crate::arch::sm83::{Bus, Disasm, Literal, PtrVal};
use crate::ast::{Instruction as inst, Operand, Operand as op};
use crate::{analysis, memory};
use num::One;

fn int_op16<L, IO>(p: &memory::Pointer<PtrVal>, mem: &Bus<IO>) -> Operand<L>
where
    L: Literal,
    IO: One,
{
    if let Some(val) = mem.read_leword::<u16>(p).into_concrete() {
        return op::lit(val);
    }

    op::miss()
}

fn dptr_op16<L, IO>(p: &memory::Pointer<PtrVal>, mem: &Bus<IO>) -> Operand<L>
where
    L: Literal,
    IO: One,
{
    if let Some(val) = mem.read_leword::<u16>(p).into_concrete() {
        return op::dptr(p.contextualize(val));
    }

    op::miss()
}

fn cptr_target<IO>(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus<IO>,
    kind: analysis::ReferenceKind,
) -> analysis::Reference<PtrVal>
where
    IO: One,
{
    if let Some(lobit) = mem.read_leword::<u16>(p).into_concrete() {
        return analysis::Reference::new_static_ref(p.clone() - 1, p.contextualize(lobit), kind);
    }

    analysis::Reference::new_dyn_ref(p.clone() - 1, kind)
}

fn cptr_op16<L, IO>(p: &memory::Pointer<PtrVal>, mem: &Bus<IO>) -> Operand<L>
where
    L: Literal,
    IO: One,
{
    match mem.read_leword::<u16>(p).into_concrete() {
        Some(target) => op::cptr(p.contextualize(target)),
        None => op::miss(),
    }
}

fn int_op8<L, IO>(p: &memory::Pointer<PtrVal>, mem: &Bus<IO>) -> Operand<L>
where
    L: Literal,
    IO: One,
{
    if let Some(lobit) = mem.read_unit(p).into_concrete() {
        return op::lit(lobit);
    }

    op::miss()
}

fn pcrel_target<IO>(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus<IO>,
    kind: analysis::ReferenceKind,
) -> analysis::Reference<PtrVal>
where
    IO: One,
{
    if let Some(lobit) = mem.read_unit(p).into_concrete() {
        let target = ((p.as_pointer() + 1) as i16 + (lobit as i8) as i16) as u16;
        return analysis::Reference::new_static_ref(p.clone() - 1, p.contextualize(target), kind);
    }

    analysis::Reference::new_dyn_ref(p.clone() - 1, kind)
}

fn pcrel_op8<L, IO>(p: &memory::Pointer<PtrVal>, mem: &Bus<IO>) -> Operand<L>
where
    L: Literal,
    IO: One,
{
    match mem.read_unit(p).into_concrete() {
        Some(target) => {
            op::cptr(p.contextualize(((p.as_pointer() + 1) as i16 + (target as i8) as i16) as u16))
        }
        None => op::miss(),
    }
}

fn hram_op8<L, IO>(p: &memory::Pointer<PtrVal>, mem: &Bus<IO>) -> Operand<L>
where
    L: Literal,
    IO: One,
{
    if let Some(lobit) = mem.read_unit(p).into_concrete() {
        return op::dptr(p.contextualize(0xFF00 + lobit as u16));
    }

    op::miss()
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

/// z80 instruction encoding uses this 2-bit enumeration to encode the target of
/// 16-bit ALU operations.
static ALU_TARGET_PAIRS: [&str; 4] = ["bc", "de", "hl", "sp"];

/// z80 instruction encoding uses this 2-bit enumeration to encode the target of
/// stack manipulation instructions.
static STACK_TARGET_PAIRS: [&str; 4] = ["bc", "de", "hl", "af"];

/// z80 instruction encoding uses this 2-bit enumeration for memory pointers.
pub static ALU_TARGET_MEM: [&str; 4] = ["bc", "de", "hli", "hld"];

/// z80 instruction encoding uses this 3-bit enumeration to encode most common
/// ALU operations. The source register is always A for these operations, which
/// are actually a holdover from 8080 and have been carried into SM83.
static ALU_OPS: [&str; 8] = ["add", "adc", "sub", "sbc", "and", "xor", "or", "cp"];

/// z80 instruction encoding uses this 2-bit enumeration to encode condition
/// codes for instructions that change control flow.
static ALU_CONDCODE: [&str; 4] = ["nz", "z", "nc", "c"];

/// z80 instruction encoding uses this 3-bit enumeration to encode "bitwise" ALU
/// operations, such as rotates, carry flag manipulation, and BCD adjustment
static ALU_BITOPS: [&str; 8] = ["rlca", "rrca", "rla", "rra", "daa", "cpl", "scf", "ccf"];

static NEW_ALU_BITOPS: [&str; 8] = ["rlc", "rrc", "rl", "rr", "sla", "sra", "swap", "srl"];

/// Disassemble the instruction at `p` in `mem`.
///
/// This function returns:
///
///  * A string representation of the instruction encountered, if there is a
///    valid instruction at P; otherwise `None`
///  * The offset to the next instruction, usually also the size of the current
///    instruction, except for architectures with polynomial program counters.
///  * is_nonfinal: If true, the next instruction is implicitly a target of the
///    current instruction.
///  * is_nonbranching: If true, the current instruction does not end the
///    current block.
///  * A list of pointers containing all statically known jump and call targets
///    from the instruction. Instructions with dynamic or unknown jump targets
///    must be expressed as None. The next instruction is implied as a target
///    if is_nonfinal is returned as True and does not need to be provided here.
pub fn disassemble<L, IO>(p: &memory::Pointer<PtrVal>, mem: &Bus<IO>) -> sm83::Result<Disasm<L>>
where
    L: Literal,
    IO: One,
{
    match mem.read_unit(p).into_concrete() {
        Some(0xCB) => match mem.read_unit(&(p.clone() + 1)).into_concrete() {
            Some(subop) => {
                let targetreg = ALU_TARGET_REGS[(subop & 0x07) as usize].clone().into();
                let new_bitop = NEW_ALU_BITOPS[((subop >> 3) & 0x07) as usize];

                match ((subop >> 6) & 0x03, (subop >> 3) & 0x07, subop & 0x07) {
                    (0, _, _) => Ok(Disasm::new(
                        inst::new(new_bitop, vec![targetreg]),
                        2,
                        analysis::Flow::Normal,
                        vec![],
                    )),
                    (1, bit, _) => Ok(Disasm::new(
                        inst::new("bit", vec![op::lit(bit), targetreg]),
                        2,
                        analysis::Flow::Normal,
                        vec![],
                    )),
                    (2, bit, _) => Ok(Disasm::new(
                        inst::new("res", vec![op::lit(bit), targetreg]),
                        2,
                        analysis::Flow::Normal,
                        vec![],
                    )),
                    (3, bit, _) => Ok(Disasm::new(
                        inst::new("set", vec![op::lit(bit), targetreg]),
                        2,
                        analysis::Flow::Normal,
                        vec![],
                    )),
                    _ => Err(analysis::Error::InvalidInstruction),
                }
            }
            _ => Err(analysis::Error::UnconstrainedMemory(p.clone() + 1)),
        },

        //Z80 instructions that don't fit the pattern decoder below
        Some(0x00) => Ok(Disasm::new(
            inst::new("nop", vec![]),
            1,
            analysis::Flow::Normal,
            vec![],
        )),
        Some(0x08) => Ok(Disasm::new(
            inst::new("ld", vec![op::indir(dptr_op16(p, mem)), op::sym("sp")]),
            3,
            analysis::Flow::Normal,
            vec![],
        )),
        Some(0x10) => Ok(Disasm::new(
            inst::new("stop", vec![]),
            1,
            analysis::Flow::Normal,
            vec![],
        )),
        Some(0x18) => Ok(Disasm::new(
            inst::new("jr", vec![pcrel_op8(&(p.clone() + 1), mem)]),
            2,
            analysis::Flow::Branching(false),
            vec![pcrel_target(
                &(p.clone() + 1),
                mem,
                analysis::ReferenceKind::Code,
            )],
        )),
        Some(0x76) => Ok(Disasm::new(
            inst::new("halt", vec![]),
            1,
            analysis::Flow::Normal,
            vec![],
        )), //encoded as ld [hl], [hl]

        Some(0xC3) => Ok(Disasm::new(
            inst::new("jp", vec![cptr_op16(&(p.clone() + 1), mem)]),
            3,
            analysis::Flow::Branching(false),
            vec![cptr_target(
                &(p.clone() + 1),
                mem,
                analysis::ReferenceKind::Code,
            )],
        )),
        Some(0xCD) => Ok(Disasm::new(
            inst::new("call", vec![cptr_op16(&(p.clone() + 1), mem)]),
            3,
            analysis::Flow::Normal,
            vec![cptr_target(
                &(p.clone() + 1),
                mem,
                analysis::ReferenceKind::Subroutine,
            )],
        )),

        Some(0xC9) => Ok(Disasm::new(
            inst::new("ret", vec![]),
            1,
            analysis::Flow::Returning,
            vec![],
        )),
        Some(0xD9) => Ok(Disasm::new(
            inst::new("reti", vec![]),
            1,
            analysis::Flow::Returning,
            vec![],
        )),
        Some(0xE9) => Ok(Disasm::new(
            inst::new("jp", vec![op::indir(op::sym("hl"))]),
            1,
            analysis::Flow::Branching(false),
            vec![analysis::Reference::new_dyn_ref(
                p.clone(),
                analysis::ReferenceKind::Code,
            )],
        )),
        Some(0xF9) => Ok(Disasm::new(
            inst::new("ld", vec![op::sym("sp"), op::sym("hl")]),
            1,
            analysis::Flow::Normal,
            vec![],
        )),

        Some(0xE0) => Ok(Disasm::new(
            inst::new(
                "ldh",
                vec![op::indir(hram_op8(&(p.clone() + 1), mem)), op::sym("a")],
            ),
            2,
            analysis::Flow::Normal,
            vec![],
        )),
        Some(0xE8) => Ok(Disasm::new(
            inst::new("add", vec![op::sym("sp"), int_op8(&(p.clone() + 1), mem)]),
            2,
            analysis::Flow::Normal,
            vec![],
        )),
        Some(0xF0) => Ok(Disasm::new(
            inst::new(
                "ldh",
                vec![op::sym("a"), op::indir(hram_op8(&(p.clone() + 1), mem))],
            ),
            2,
            analysis::Flow::Normal,
            vec![],
        )),
        Some(0xF8) => Ok(Disasm::new(
            inst::new(
                "ld",
                vec![
                    op::sym("hl"),
                    op::addop(op::sym("sp"), int_op8(&(p.clone() + 1), mem)),
                ],
            ),
            2,
            analysis::Flow::Normal,
            vec![],
        )),

        Some(0xE2) => Ok(Disasm::new(
            inst::new("ld", vec![op::indir(op::sym("c")), op::sym("a")]),
            1,
            analysis::Flow::Normal,
            vec![],
        )),
        Some(0xEA) => Ok(Disasm::new(
            inst::new(
                "ld",
                vec![op::indir(dptr_op16(&(p.clone() + 1), mem)), op::sym("a")],
            ),
            3,
            analysis::Flow::Normal,
            vec![],
        )),
        Some(0xF2) => Ok(Disasm::new(
            inst::new("ld", vec![op::sym("a"), op::indir(op::sym("c"))]),
            1,
            analysis::Flow::Normal,
            vec![],
        )),
        Some(0xFA) => Ok(Disasm::new(
            inst::new(
                "ld",
                vec![op::sym("a"), op::indir(dptr_op16(&(p.clone() + 1), mem))],
            ),
            3,
            analysis::Flow::Normal,
            vec![],
        )),

        Some(0xF3) => Ok(Disasm::new(
            inst::new("di", vec![]),
            1,
            analysis::Flow::Normal,
            vec![],
        )),
        Some(0xFB) => Ok(Disasm::new(
            inst::new("ei", vec![]),
            1,
            analysis::Flow::Normal,
            vec![],
        )),

        //Z80 instructions that follow a particular pattern
        Some(op) => {
            let condcode = ALU_CONDCODE[((op >> 3) & 0x03) as usize];
            let targetpair = ALU_TARGET_PAIRS[((op >> 4) & 0x03) as usize];
            let targetreg = ALU_TARGET_REGS[((op >> 3) & 0x07) as usize].clone().into();
            let targetmem = ALU_TARGET_MEM[((op >> 4) & 0x03) as usize];
            let bitop = ALU_BITOPS[((op >> 3) & 0x07) as usize];
            let targetreg2 = ALU_TARGET_REGS[(op & 0x07) as usize].clone().into();
            let aluop = ALU_OPS[((op >> 3) & 0x07) as usize];
            let stackpair = STACK_TARGET_PAIRS[((op >> 4) & 0x03) as usize];

            //decode `op` into aab?cddd. This creates a nice visual table for
            //the Z80's semiperiodic instruction encoding
            match (
                (op >> 6) & 0x03,
                (op >> 5) & 0x01,
                (op >> 3) & 0x01,
                op & 0x07,
            ) {
                (0, 0, _, 0) => Err(analysis::Error::Misinterpretation(1, false)), /* 00, 08, 10, 18 */
                (0, 1, _, 0) => Ok(Disasm::new(
                    inst::new(
                        "jr",
                        vec![op::sym(condcode), pcrel_op8(&(p.clone() + 1), mem)],
                    ),
                    2,
                    analysis::Flow::Branching(true),
                    vec![pcrel_target(
                        &(p.clone() + 1),
                        mem,
                        analysis::ReferenceKind::Code,
                    )],
                )),
                (0, _, 0, 1) => Ok(Disasm::new(
                    inst::new(
                        "ld",
                        vec![op::sym(targetpair), int_op16(&(p.clone() + 1), mem)],
                    ),
                    3,
                    analysis::Flow::Normal,
                    vec![],
                )),
                (0, _, 1, 1) => Ok(Disasm::new(
                    inst::new("add", vec![op::sym("hl"), op::sym(targetpair)]),
                    1,
                    analysis::Flow::Normal,
                    vec![],
                )),
                (0, _, 0, 2) => Ok(Disasm::new(
                    inst::new("ld", vec![op::indir(op::sym(targetmem)), op::sym("a")]),
                    1,
                    analysis::Flow::Normal,
                    vec![],
                )),
                (0, _, 1, 2) => Ok(Disasm::new(
                    inst::new("ld", vec![op::sym("a"), op::indir(op::sym(targetmem))]),
                    1,
                    analysis::Flow::Normal,
                    vec![],
                )),
                (0, _, 0, 3) => Ok(Disasm::new(
                    inst::new("inc", vec![op::sym(targetpair)]),
                    1,
                    analysis::Flow::Normal,
                    vec![],
                )),
                (0, _, 1, 3) => Ok(Disasm::new(
                    inst::new("dec", vec![op::sym(targetpair)]),
                    1,
                    analysis::Flow::Normal,
                    vec![],
                )),
                (0, _, _, 4) => Ok(Disasm::new(
                    inst::new("inc", vec![targetreg]),
                    1,
                    analysis::Flow::Normal,
                    vec![],
                )),
                (0, _, _, 5) => Ok(Disasm::new(
                    inst::new("dec", vec![targetreg]),
                    1,
                    analysis::Flow::Normal,
                    vec![],
                )),
                (0, _, _, 6) => Ok(Disasm::new(
                    inst::new("ld", vec![targetreg, int_op8(&(p.clone() + 1), mem)]),
                    2,
                    analysis::Flow::Normal,
                    vec![],
                )),
                (0, _, _, 7) => Ok(Disasm::new(
                    inst::new(bitop, vec![]),
                    1,
                    analysis::Flow::Normal,
                    vec![],
                )),
                (1, _, _, _) => Ok(Disasm::new(
                    inst::new("ld", vec![targetreg2, targetreg]),
                    1,
                    analysis::Flow::Normal,
                    vec![],
                )),
                (2, _, _, _) => Ok(Disasm::new(
                    inst::new(aluop, vec![op::sym("a"), targetreg2]),
                    1,
                    analysis::Flow::Normal,
                    vec![],
                )),
                (3, 0, _, 0) => Ok(Disasm::new(
                    inst::new("ret", vec![op::sym(condcode)]),
                    1,
                    analysis::Flow::Branching(true),
                    vec![],
                )),
                (3, 1, _, 0) => Err(analysis::Error::Misinterpretation(1, false)), /* E0, E8, F0, F8 */
                (3, _, 0, 1) => Ok(Disasm::new(
                    inst::new("pop", vec![op::sym(stackpair)]),
                    1,
                    analysis::Flow::Normal,
                    vec![],
                )),
                (3, _, 1, 1) => Err(analysis::Error::Misinterpretation(1, false)), /* C9, D9, E9, F9 */
                (3, 0, _, 2) => Ok(Disasm::new(
                    inst::new(
                        "jp",
                        vec![op::sym(condcode), cptr_op16(&(p.clone() + 1), mem)],
                    ),
                    3,
                    analysis::Flow::Branching(true),
                    vec![cptr_target(
                        &(p.clone() + 1),
                        mem,
                        analysis::ReferenceKind::Code,
                    )],
                )),
                (3, 1, _, 2) => Err(analysis::Error::Misinterpretation(1, false)), /* E2, EA, F2, FA */
                (3, _, _, 3) => Err(analysis::Error::InvalidInstruction),
                (3, 0, _, 4) => Ok(Disasm::new(
                    inst::new(
                        "call",
                        vec![op::sym(condcode), cptr_op16(&(p.clone() + 1), mem)],
                    ),
                    3,
                    analysis::Flow::Normal,
                    vec![cptr_target(
                        &(p.clone() + 1),
                        mem,
                        analysis::ReferenceKind::Subroutine,
                    )],
                )),
                (3, 1, _, 4) => Err(analysis::Error::InvalidInstruction),
                (3, _, 0, 5) => Ok(Disasm::new(
                    inst::new("push", vec![op::sym(stackpair)]),
                    1,
                    analysis::Flow::Normal,
                    vec![],
                )),
                (3, _, 1, 5) => Err(analysis::Error::InvalidInstruction),
                (3, _, _, 6) => Ok(Disasm::new(
                    inst::new(aluop, vec![op::sym("a"), int_op8(&(p.clone() + 1), mem)]),
                    2,
                    analysis::Flow::Normal,
                    vec![],
                )),
                (3, _, _, 7) => Ok(Disasm::new(
                    inst::new(
                        "rst",
                        vec![op::cptr(p.contextualize((op & 0x38) as PtrVal))],
                    ),
                    1,
                    analysis::Flow::Normal,
                    vec![analysis::Reference::new_static_ref(
                        p.clone(),
                        p.contextualize((op & 0x38) as u16),
                        analysis::ReferenceKind::Subroutine,
                    )],
                )),

                _ => Err(analysis::Error::InvalidInstruction),
            }
        }
        _ => Err(analysis::Error::UnconstrainedMemory(p.clone())),
    }
}
