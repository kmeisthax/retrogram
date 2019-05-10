//! A Z80 derivative created by SHARP for use in the Nintendo Game Boy

use std::io;
use crate::retrogram::{memory, ast};
use crate::retrogram::ast::Operand as op;
use crate::retrogram::ast::Instruction as inst;

/// Enumeration of all architectural GBZ80 registers.
/// 
/// Couple things to note:
/// 
///  * We don't consider register pairs (e.g. BC, DE, HL)
///  * F isn't considered special here
///  * SP has been treated as a register pair and split into S and P.
#[derive(Copy, Clone, Debug, Hash)]
pub enum Register {
    A, B, C, D, E, H, L, F, S, P
}

/// The type which represents a value contained in an LR35902 register.
pub type Value = u8;

/// The type which represents an LR35902 memory address.
pub type Pointer = u16;

/// The type which represents a positive memory offset.
pub type Offset = u16;

/// The type which represents a signed value contained in an LR35902 register or register pair.
pub type SignedValue = i16;

/// The type which represents data stored in memory as seen by the processor.
pub type Data = u8;

/// The compatible memory model type necessary to analyze GBz80 programs.
pub type Bus = memory::Memory<Pointer, Data, Offset>;

/// The AST type which represents a disassembled operand.
/// 
/// TODO: When ! is stable, replace the floating-point type with !.
pub type Operand = ast::Operand<Offset, SignedValue, f32, Pointer>;

/// The AST type which represents a disassembled instruction.
/// 
/// TODO: When ! is stable, replace the floating-point type with !.
pub type Instruction = ast::Instruction<Offset, SignedValue, f32, Pointer>;

/// The AST type which represents disassembled code.
/// 
/// TODO: When ! is stable, replace the floating-point type with !.
pub type Section = ast::Section<Offset, SignedValue, f32, Pointer>;

fn int_op16(p: &memory::Pointer<Pointer>, mem: &Bus) -> Operand {
    if let Some(val) = mem.read_leword::<u16>(p).into_concrete() {
        return op::int(val);
    }

    op::miss()
}

fn dptr_op16(p: &memory::Pointer<Pointer>, mem: &Bus) -> Operand {
    if let Some(val) = mem.read_leword::<u16>(p).into_concrete() {
        return op::dptr(val);
    }

    op::miss()
}

fn cptr_target(p: &memory::Pointer<Pointer>, mem: &Bus) -> Option<memory::Pointer<Pointer>> {
    if let Some(lobit) = mem.read_leword::<u16>(p).into_concrete() {
        return Some(p.contextualize(lobit));
    }

    None
}

fn cptr_op16(p: &memory::Pointer<Pointer>, mem: &Bus) -> Operand {
    match cptr_target(p, mem) {
        Some(target) => op::cptr(target.into_pointer()),
        None => op::miss()
    }
}

fn int_op8(p: &memory::Pointer<Pointer>, mem: &Bus) -> Operand {
    if let Some(lobit) = mem.read_unit(p).into_concrete() {
        return op::int(lobit);
    }

    op::miss()
}

fn pcrel_target(p: &memory::Pointer<Pointer>, mem: &Bus) -> Option<memory::Pointer<Pointer>> {
    if let Some(lobit) = mem.read_unit(p).into_concrete() {
        return Some(p.contextualize(((p.as_pointer() + 1) as i16 + (lobit as i8) as i16) as u16));
    }

    None
}

fn pcrel_op8(p: &memory::Pointer<Pointer>, mem: &Bus) -> Operand {
    match pcrel_target(p, mem) {
        Some(target) => op::cptr(target.into_pointer()),
        None => op::miss()
    }
}

fn hram_op8(p: &memory::Pointer<Pointer>, mem: &Bus) -> Operand {
    if let Some(lobit) = mem.read_unit(p).into_concrete() {
        return op::dptr(0xFF00 + lobit as u16);
    }

    op::miss()
}

lazy_static! {
    /// z80 instruction encoding uses this 3-bit enumeration to encode the target of
    /// 8-bit ALU or register transfer operations.
    static ref ALU_TARGET_REGS: [Operand; 8] = [op::sym("b"), op::sym("c"), op::sym("d"), op::sym("e"), op::sym("h"), op::sym("l"), op::indir(op::sym("hl")), op::sym("a")];
}

/// z80 instruction encoding uses this 2-bit enumeration to encode the target of
/// 16-bit ALU operations.
static ALU_TARGET_PAIRS: [&str; 4] = ["bc", "de", "hl", "sp"];

/// z80 instruction encoding uses this 2-bit enumeration to encode the target of
/// stack manipulation instructions.
static STACK_TARGET_PAIRS: [&str; 4] = ["bc", "de", "hl", "af"];

/// z80 instruction encoding uses this 2-bit enumeration for memory pointers.
static ALU_TARGET_MEM: [&str; 4] = ["bc", "de", "hli", "hld"];

/// z80 instruction encoding uses this 3-bit enumeration to encode most common
/// ALU operations. The source register is always A for these operations, which
/// are actually a holdover from 8080 and have been carried into LR35902.
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
///  * True, if execution would continue at the instruction following this one,
///    or false if the instruction terminates the current basic block
///  * A list of pointers containing all statically known jump and call targets
///    from the instruction. The list should not include the next instruction,
///    which is implied by the previous two instructions. Instructions with
///    dynamic or unknown jump targets must be expressed as None.
pub fn disassemble(p: &memory::Pointer<Pointer>, mem: &Bus) -> (Option<Instruction>, Offset, bool, Vec<Option<memory::Pointer<Pointer>>>) {
    match mem.read_unit(p).into_concrete() {
        Some(0xCB) => {
            //TODO: CB prefix
            match mem.read_unit(&(p.clone()+1)).into_concrete() {
                Some(subop) => {
                    let targetreg = ALU_TARGET_REGS[(subop & 0x07) as usize].clone();
                    let new_bitop = NEW_ALU_BITOPS[((subop >> 3) & 0x07) as usize];

                    match ((subop >> 6) & 0x03, (subop >> 3) & 0x07, subop & 0x07) {
                        (0, _, _) => (Some(inst::new(new_bitop, vec![targetreg])), 2, true, vec![]),
                        (1, bit, _) => (Some(inst::new("bit", vec![op::int(bit), targetreg])), 2, true, vec![]),
                        (2, bit, _) => (Some(inst::new("res", vec![op::int(bit), targetreg])), 2, true, vec![]),
                        (3, bit, _) => (Some(inst::new("set", vec![op::int(bit), targetreg])), 2, true, vec![]),
                        _ => (None, 0, false, vec![])
                    }
                },
                _ => (None, 0, false, vec![])
            }
        }

        //Z80 instructions that don't fit the pattern decoder below
        Some(0x00) => (Some(inst::new("nop", vec![])), 1, true, vec![]),
        Some(0x08) => (Some(inst::new("ld", vec![op::indir(dptr_op16(p, mem)), op::sym("sp")])), 3, true, vec![]),
        Some(0x10) => (Some(inst::new("stop", vec![])), 1, true, vec![]),
        Some(0x18) => (Some(inst::new("jr", vec![pcrel_op8(&(p.clone()+1), mem)])), 2, false, vec![pcrel_target(&(p.clone()+1), mem)]),
        Some(0x76) => (Some(inst::new("halt", vec![])), 1, true, vec![]), //encoded as ld [hl], [hl]

        Some(0xC3) => (Some(inst::new("jp", vec![cptr_op16(&(p.clone()+1), mem)])), 3, false, vec![cptr_target(&(p.clone()+1), mem)]),
        Some(0xCD) => (Some(inst::new("call", vec![cptr_op16(&(p.clone()+1), mem)])), 3, true, vec![cptr_target(&(p.clone()+1), mem)]),

        Some(0xC9) => (Some(inst::new("ret", vec![])), 1, false, vec![]),
        Some(0xD9) => (Some(inst::new("reti", vec![])), 1, false, vec![]),
        Some(0xE9) => (Some(inst::new("jp", vec![op::indir(op::sym("hl"))])), 1, false, vec![None]),
        Some(0xF9) => (Some(inst::new("ld", vec![op::sym("sp"), op::sym("hl")])), 1, true, vec![]),

        Some(0xE0) => (Some(inst::new("ldh", vec![op::indir(hram_op8(&(p.clone()+1), mem)), op::sym("a")])), 2, true, vec![]),
        Some(0xE8) => (Some(inst::new("add", vec![op::sym("sp"), int_op8(&(p.clone()+1), mem)])), 2, true, vec![]),
        Some(0xF0) => (Some(inst::new("ldh", vec![op::sym("a"), op::indir(hram_op8(&(p.clone()+1), mem))])), 2, true, vec![]),
        Some(0xF8) => (Some(inst::new("ld", vec![op::sym("hl"), op::add(op::sym("sp"), int_op8(&(p.clone()+1), mem))])), 2, true, vec![]),

        Some(0xE2) => (Some(inst::new("ld", vec![op::indir(op::sym("c")), op::sym("a")])), 1, true, vec![]),
        Some(0xEA) => (Some(inst::new("ld", vec![op::indir(dptr_op16(&(p.clone()+1), mem)), op::sym("a")])), 3, true, vec![]),
        Some(0xF2) => (Some(inst::new("ld", vec![op::sym("a"), op::indir(op::sym("c"))])), 1, true, vec![]),
        Some(0xFA) => (Some(inst::new("ld", vec![op::sym("a"), op::indir(dptr_op16(&(p.clone()+1), mem))])), 3, true, vec![]),

        Some(0xF3) => (Some(inst::new("di", vec![])), 1, true, vec![]),
        Some(0xFB) => (Some(inst::new("ei", vec![])), 1, true, vec![]),

        //Z80 instructions that follow a particular pattern
        Some(op) => {
            let condcode = ALU_CONDCODE[((op >> 3) & 0x03) as usize];
            let targetpair = ALU_TARGET_PAIRS[((op >> 4) & 0x03) as usize];
            let targetreg = ALU_TARGET_REGS[((op >> 3) & 0x07) as usize].clone();
            let targetmem = ALU_TARGET_MEM[((op >> 4) & 0x03) as usize];
            let bitop = ALU_BITOPS[((op >> 3) & 0x07) as usize];
            let targetreg2 = ALU_TARGET_REGS[(op & 0x07) as usize].clone();
            let aluop = ALU_OPS[((op >> 3) & 0x07) as usize];
            let stackpair = STACK_TARGET_PAIRS[((op >> 4) & 0x03) as usize];

            //decode `op` into aab?cddd. This creates a nice visual table for
            //the Z80's semiperiodic instruction encoding
            match ((op >> 6) & 0x03, (op >> 5) & 0x01, (op >> 3) & 0x01, op & 0x07) {
                (0, 0, _, 0) => panic!("Instruction shouldn't be decoded here"), /* 00, 08, 10, 18 */
                (0, 1, _, 0) => (Some(inst::new("jr", vec![op::sym(condcode), pcrel_op8(&(p.clone()+1), mem)])), 2, true, vec![pcrel_target(&(p.clone()+1), mem)]),
                (0, _, 0, 1) => (Some(inst::new("ld", vec![op::sym(targetpair), int_op16(&(p.clone()+1), mem)])), 3, true, vec![]),
                (0, _, 1, 1) => (Some(inst::new("add", vec![op::sym("hl"), op::sym(targetpair)])), 1, true, vec![]),
                (0, _, 0, 2) => (Some(inst::new("ld", vec![op::indir(op::sym(targetmem)), op::sym("a")])), 1, true, vec![]),
                (0, _, 1, 2) => (Some(inst::new("ld", vec![op::sym("a"), op::indir(op::sym(targetmem))])), 1, true, vec![]),
                (0, _, 0, 3) => (Some(inst::new("inc", vec![op::sym(targetpair)])), 1, true, vec![]),
                (0, _, 1, 3) => (Some(inst::new("dec", vec![op::sym(targetpair)])), 1, true, vec![]),
                (0, _, _, 4) => (Some(inst::new("inc", vec![targetreg])), 1, true, vec![]),
                (0, _, _, 5) => (Some(inst::new("dec", vec![targetreg])), 1, true, vec![]),
                (0, _, _, 6) => (Some(inst::new("ld", vec![targetreg, int_op8(&(p.clone()+1), mem)])), 2, true, vec![]),
                (0, _, _, 7) => (Some(inst::new(bitop, vec![])), 1, true, vec![]),
                (1, _, _, _) => (Some(inst::new("ld", vec![targetreg2, targetreg])), 1, true, vec![]),
                (2, _, _, _) => (Some(inst::new(aluop, vec![op::sym("a"), targetreg2])), 1, true, vec![]),
                (3, 0, _, 0) => (Some(inst::new("ret", vec![op::sym(condcode)])), 1, true, vec![]),
                (3, 1, _, 0) => panic!("Instruction shouldn't be decoded here"), /* E0, E8, F0, F8 */
                (3, _, 0, 1) => (Some(inst::new("pop", vec![op::sym(stackpair)])), 1, true, vec![]),
                (3, _, 1, 1) => panic!("Instruction shouldn't be decoded here"), /* C9, D9, E9, F9 */
                (3, 0, _, 2) => (Some(inst::new("jp", vec![op::sym(condcode), cptr_op16(&(p.clone()+1), mem)])), 3, true, vec![cptr_target(&(p.clone()+1), mem)]),
                (3, 1, _, 2) => panic!("Instruction shouldn't be decoded here"), /* E2, EA, F2, FA */
                (3, _, _, 3) => (None, 0, false, vec![]),
                (3, 0, _, 4) => (Some(inst::new("call", vec![op::sym(condcode), cptr_op16(&(p.clone()+1), mem)])), 3, true, vec![cptr_target(&(p.clone()+1), mem)]),
                (3, 1, _, 4) => (None, 0, false, vec![]),
                (3, _, 0, 5) => (Some(inst::new("push", vec![op::sym(stackpair)])), 1, true, vec![]),
                (3, _, 1, 5) => (None, 0, false, vec![]),
                (3, _, _, 6) => (Some(inst::new(aluop, vec![op::sym("a"), int_op8(&(p.clone()+1), mem)])), 2, true, vec![]),
                (3, _, _, 7) => (Some(inst::new("rst", vec![op::cptr(op & 0x38)])), 1, true, vec![Some(p.contextualize((op & 0x38) as u16))]),

                _ => (None, 0, false, vec![])
            }
        },
        _ => (None, 0, false, vec![])
    }
}