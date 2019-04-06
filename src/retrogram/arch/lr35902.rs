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
type Value = u8;

/// The type which represents an LR35902 memory address.
type Pointer = u16;

/// The type which represents a positive memory offset.
type Offset = u16;

/// The type which represents data stored in memory as seen by the processor.
type Data = u8;

fn dis_op16(p: Pointer, mem: memory::Memory<Pointer, Data>) -> ast::Operand {
    if let Some(lobit) = mem.read_unit(p).into_concrete() {
        if let Some(hibit) = mem.read_unit(p).into_concrete() {
            return op::int((hibit as u16) << 8 | lobit as u16);
        }
    }

    op::miss()
}

fn dis_op8(p: Pointer, mem: memory::Memory<Pointer, Data>) -> ast::Operand {
    if let Some(lobit) = mem.read_unit(p).into_concrete() {
        return op::int(lobit);
    }

    op::miss()
}

/// Disassemble the instruction at `p` in `mem`.
/// 
/// This function returns:
/// 
///  * A string representation of the instruction encountered, if there is a
///    valid instruction at P; otherwise `None`
///  * The size of the current instruction
///  * If the given instruction should halt the disassembly, or if execution
///    would (eventually) continue after the next
fn disassemble(p: Pointer, mem: memory::Memory<Pointer, Data>) -> (Option<ast::Instruction>, Offset, bool) {
    match mem.read_unit(p).into_concrete() {
        Some(0x00) => (Some(inst::new("nop", vec![])), 1, true),
        Some(0x01) => (Some(inst::new("ld", vec![op::sym("bc"), dis_op16(p+1, mem)])), 3, true),
        Some(0x02) => (Some(inst::new("ld", vec![op::sym("[bc]"), op::sym("a")])), 1, true),
        Some(0x03) => (Some(inst::new("inc", vec![op::sym("bc")])), 1, true),
        Some(0x04) => (Some(inst::new("inc", vec![op::sym("b")])), 1, true),
        Some(0x05) => (Some(inst::new("dec", vec![op::sym("b")])), 1, true),
        Some(0x06) => (Some(inst::new("ld", vec![op::sym("b"), dis_op8(p+1, mem)])), 2, true),
        Some(0x07) => (Some(inst::new("rlca", vec![])), 1, true),
        Some(0x08) => (Some(inst::new("ld", vec![dis_op16(p+1, mem), op::sym("sp")])), 3, true),
        Some(0x09) => (Some(inst::new("add", vec![op::sym("hl"), op::sym("bc")])), 1, true),
        Some(0x0a) => (Some(inst::new("ld", vec![op::sym("a"), op::sym("[bc]")])), 1, true),
        Some(0x0b) => (Some(inst::new("dec", vec![op::sym("bc")])), 1, true),
        Some(0x0c) => (Some(inst::new("inc", vec![op::sym("c")])), 1, true),
        Some(0x0d) => (Some(inst::new("dec", vec![op::sym("c")])), 1, true),
        Some(0x0e) => (Some(inst::new("ld", vec![op::sym("c"), dis_op8(p+1, mem)])), 2, true),
        Some(0x0f) => (Some(inst::new("rrca", vec![])), 1, true),
        Some(0x10) => (Some(inst::new("stop", vec![])), 1, true),
        Some(0x11) => (Some(inst::new("ld", vec![op::sym("de"), dis_op16(p+1, mem)])), 3, true),
        Some(0x12) => (Some(inst::new("ld", vec![op::sym("[de]"), op::sym("a")])), 1, true),
        Some(0x13) => (Some(inst::new("inc", vec![op::sym("de")])), 1, true),
        Some(0x14) => (Some(inst::new("inc", vec![op::sym("d")])), 1, true),
        Some(0x15) => (Some(inst::new("dec", vec![op::sym("d")])), 1, true),
        Some(0x16) => (Some(inst::new("ld", vec![op::sym("d"), dis_op8(p+1, mem)])), 2, true),
        Some(0x17) => (Some(inst::new("rla", vec![])), 1, true),
        Some(0x18) => (Some(inst::new("jr", vec![dis_op8(p+1, mem)])), 2, true),
        Some(0x19) => (Some(inst::new("add", vec![op::sym("hl"), op::sym("de")])), 1, true),
        Some(0x1a) => (Some(inst::new("ld", vec![op::sym("a"), op::sym("[de]")])), 1, true),
        Some(0x1b) => (Some(inst::new("dec", vec![op::sym("de")])), 1, true),
        Some(0x1c) => (Some(inst::new("inc", vec![op::sym("e")])), 1, true),
        Some(0x1d) => (Some(inst::new("dec", vec![op::sym("e")])), 1, true),
        Some(0x1e) => (Some(inst::new("ld", vec![op::sym("e"), dis_op8(p+1, mem)])), 2, true),
        Some(0x1f) => (Some(inst::new("rra", vec![])), 1, true),
        _ => (None, 0, false)
    }
}