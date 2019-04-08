//! A Z80 derivative created by SHARP for use in the Nintendo Game Boy

use crate::retrogram::{memory, reg, ast};
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

/// The type which represents data stored in memory as seen by the processor.
pub type Data = u8;

/// The compatible memory model type necessary to analyze GBz80 programs.
pub type Bus = memory::Memory<Pointer, Data, Offset>;

fn int_op16(p: Pointer, mem: &Bus, ctx: &reg::Context) -> ast::Operand {
    if let Some(lobit) = mem.read_unit(p, ctx).into_concrete() {
        if let Some(hibit) = mem.read_unit(p+1, ctx).into_concrete() {
            return op::int(((hibit as u16) << 8 | lobit as u16));
        }
    }

    op::miss()
}

fn int_op8(p: Pointer, mem: &Bus, ctx: &reg::Context) -> ast::Operand {
    if let Some(lobit) = mem.read_unit(p, ctx).into_concrete() {
        return op::int(lobit);
    }

    op::miss()
}

fn pcrel_op8(p: Pointer, mem: &Bus, ctx: &reg::Context) -> ast::Operand {
    if let Some(lobit) = mem.read_unit(p, ctx).into_concrete() {
        return op::int(((p - 1) as i16 + (lobit as i8) as i16) as u16);
    }

    op::miss()
}

fn hram_op8(p: Pointer, mem: &Bus, ctx: &reg::Context) -> ast::Operand {
    if let Some(lobit) = mem.read_unit(p, ctx).into_concrete() {
        return op::int(0xFF00 + lobit as u16);
    }

    op::miss()
}

/// z80 instruction encoding uses this 3-bit enumeration to encode the target of
/// 8-bit ALU or register transfer operations.
static ALU_TARGET_REGS: [&str; 8] = ["b", "c", "d", "e", "h", "l", "[hl]", "a"];

/// z80 instruction encoding uses this 2-bit enumeration to encode the target of
/// 16-bit ALU operations.
static ALU_TARGET_PAIRS: [&str; 4] = ["bc", "de", "hl", "sp"];

/// z80 instruction encoding uses this 2-bit enumeration to encode the target of
/// stack manipulation instructions.
static STACK_TARGET_PAIRS: [&str; 4] = ["bc", "de", "hl", "af"];

/// z80 instruction encoding uses this 2-bit enumeration for memory pointers.
static ALU_TARGET_MEM: [&str; 4] = ["[bc]", "[de]", "[hli]", "[hld]"];

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
///  * The size of the current instruction
///  * True, if execution would continue at the instruction following this one,
///    or false if the instruction terminates the current basic block
pub fn disassemble(p: Pointer, mem: &Bus, ctx: &reg::Context) -> (Option<ast::Instruction>, Offset, bool) {
    match mem.read_unit(p, ctx).into_concrete() {
        Some(0xCB) => {
            //TODO: CB prefix
            match mem.read_unit(p+1, ctx).into_concrete() {
                Some(subop) => {
                    let targetreg = ALU_TARGET_REGS[(subop & 0x07) as usize];
                    let new_bitop = NEW_ALU_BITOPS[((subop >> 3) & 0x07) as usize];

                    match ((subop >> 6) & 0x03, (subop >> 3) & 0x07, subop & 0x07) {
                        (0, _, _) => (Some(inst::new(new_bitop, vec![op::sym(targetreg)])), 2, true),
                        (1, bit, _) => (Some(inst::new("bit", vec![op::int(bit), op::sym(targetreg)])), 2, true),
                        (2, bit, _) => (Some(inst::new("res", vec![op::int(bit), op::sym(targetreg)])), 2, true),
                        (3, bit, _) => (Some(inst::new("set", vec![op::int(bit), op::sym(targetreg)])), 2, true),
                        _ => (None, 0, false)
                    }
                },
                _ => (None, 0, false)
            }
        }

        //Z80 instructions that don't fit the pattern decoder below
        Some(0x00) => (Some(inst::new("nop", vec![])), 1, true),
        Some(0x08) => (Some(inst::new("ld", vec![int_op16(p, mem, ctx), op::sym("sp")])), 3, true),
        Some(0x10) => (Some(inst::new("stop", vec![])), 1, true),
        Some(0x18) => (Some(inst::new("jr", vec![pcrel_op8(p+1, mem, ctx)])), 2, false),
        Some(0x76) => (Some(inst::new("halt", vec![])), 1, true), //encoded as ld [hl], [hl]

        Some(0xC3) => (Some(inst::new("jp", vec![int_op16(p+1, mem, ctx)])), 3, false),
        Some(0xCD) => (Some(inst::new("call", vec![int_op16(p+1, mem, ctx)])), 3, true),

        Some(0xC9) => (Some(inst::new("ret", vec![])), 1, false),
        Some(0xD9) => (Some(inst::new("reti", vec![])), 1, false),
        Some(0xE9) => (Some(inst::new("jp", vec![op::sym("[hl]")])), 1, false),
        Some(0xF9) => (Some(inst::new("ld", vec![op::sym("sp"), op::sym("hl")])), 1, true),

        Some(0xE0) => (Some(inst::new("ldh", vec![hram_op8(p+1, mem, ctx), op::sym("a")])), 2, true),
        Some(0xE8) => (Some(inst::new("add", vec![op::sym("sp"), int_op8(p+1, mem, ctx)])), 2, true),
        Some(0xF0) => (Some(inst::new("ldh", vec![op::sym("a"), hram_op8(p+1, mem, ctx)])), 2, true),
        Some(0xF8) => (Some(inst::new("ld", vec![op::sym("hl"), op::add(op::sym("sp"), int_op8(p+1, mem, ctx))])), 2, true),

        Some(0xE2) => (Some(inst::new("ld", vec![op::sym("[c]"), op::sym("a")])), 1, true),
        Some(0xEA) => (Some(inst::new("ld", vec![int_op16(p+1, mem, ctx), op::sym("a")])), 3, true),
        Some(0xF2) => (Some(inst::new("ld", vec![op::sym("a"), op::sym("[c]")])), 1, true),
        Some(0xFA) => (Some(inst::new("ld", vec![op::sym("a"), int_op16(p+1, mem, ctx)])), 3, true),

        Some(0xF3) => (Some(inst::new("di", vec![])), 1, true),
        Some(0xFB) => (Some(inst::new("ei", vec![])), 1, true),

        //Z80 instructions that follow a particular pattern
        Some(op) => {
            let condcode = ALU_CONDCODE[((op >> 3) & 0x03) as usize];
            let targetpair = ALU_TARGET_PAIRS[((op >> 4) & 0x03) as usize];
            let targetreg = ALU_TARGET_REGS[((op >> 3) & 0x07) as usize];
            let targetmem = ALU_TARGET_MEM[((op >> 4) & 0x03) as usize];
            let bitop = ALU_BITOPS[((op >> 3) & 0x07) as usize];
            let targetreg2 = ALU_TARGET_REGS[(op & 0x07) as usize];
            let aluop = ALU_OPS[((op >> 3) & 0x07) as usize];
            let stackpair = STACK_TARGET_PAIRS[((op >> 4) & 0x03) as usize];

            //decode `op` into aab?cddd. This creates a nice visual table for
            //the Z80's semiperiodic instruction encoding
            match ((op >> 6) & 0x03, (op >> 5) & 0x01, (op >> 3) & 0x01, op & 0x07) {
                (0, 0, _, 0) => panic!("Instruction shouldn't be decoded here"), /* 00, 08, 10, 18 */
                (0, 1, _, 0) => (Some(inst::new("jr", vec![op::sym(condcode), pcrel_op8(p+1, mem, ctx)])), 2, true),
                (0, _, 0, 1) => (Some(inst::new("ld", vec![op::sym(targetpair), int_op16(p+1, mem, ctx)])), 3, true),
                (0, _, 1, 1) => (Some(inst::new("add", vec![op::sym("hl"), op::sym(targetpair)])), 1, true),
                (0, _, 0, 2) => (Some(inst::new("ld", vec![op::sym(targetmem), op::sym("a")])), 1, true),
                (0, _, 1, 2) => (Some(inst::new("ld", vec![op::sym("a"), op::sym(targetmem)])), 1, true),
                (0, _, 0, 3) => (Some(inst::new("inc", vec![op::sym(targetpair)])), 1, true),
                (0, _, 1, 3) => (Some(inst::new("dec", vec![op::sym(targetpair)])), 1, true),
                (0, _, _, 4) => (Some(inst::new("inc", vec![op::sym(targetreg)])), 1, true),
                (0, _, _, 5) => (Some(inst::new("dec", vec![op::sym(targetreg)])), 1, true),
                (0, _, _, 6) => (Some(inst::new("ld", vec![op::sym(targetreg), int_op8(p+1, mem, ctx)])), 2, true),
                (0, _, _, 7) => (Some(inst::new(bitop, vec![])), 1, true),
                (1, _, _, _) => (Some(inst::new("ld", vec![op::sym(targetreg2), op::sym(targetreg)])), 1, true),
                (2, _, _, _) => (Some(inst::new(aluop, vec![op::sym("a"), op::sym(targetreg2)])), 1, true),
                (3, 0, _, 0) => (Some(inst::new("ret", vec![op::sym(condcode)])), 1, true),
                (3, 1, _, 0) => panic!("Instruction shouldn't be decoded here"), /* E0, E8, F0, F8 */
                (3, _, 0, 1) => (Some(inst::new("pop", vec![op::sym(stackpair)])), 1, true),
                (3, _, 1, 1) => panic!("Instruction shouldn't be decoded here"), /* C9, D9, E9, F9 */
                (3, 0, _, 2) => (Some(inst::new("jp", vec![op::sym(condcode), int_op16(p+1, mem, ctx)])), 3, true),
                (3, 1, _, 2) => panic!("Instruction shouldn't be decoded here"), /* E2, EA, F2, FA */
                (3, _, _, 3) => (None, 0, false),
                (3, 0, _, 4) => (Some(inst::new("call", vec![op::sym(condcode), int_op16(p+1, mem, ctx)])), 3, true),
                (3, 1, _, 4) => (None, 0, false),
                (3, _, 0, 5) => (Some(inst::new("push", vec![op::sym(stackpair)])), 1, true),
                (3, _, 1, 5) => (None, 0, false),
                (3, _, _, 6) => (Some(inst::new(aluop, vec![op::sym("a"), int_op8(p+1, mem, ctx)])), 2, true),
                (3, _, _, 7) => (Some(inst::new("rst", vec![op::ptr(op & 0x30)])), 1, true),

                _ => (None, 0, false)
            }
        },
        _ => (None, 0, false)
    }
}
