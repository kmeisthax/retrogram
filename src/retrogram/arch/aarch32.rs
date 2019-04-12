//! ARM, formerly an acronym of Acorn RISC Machine, and a quite popular ISA.
//! 
//! This only covers 32-bit ARM, now known as AArch32.

use crate::retrogram::{memory, ast};

enum Aarch32Register {
    R0, R1, R2, R3,
    R4, R5, R6, R7,
    R8, R9, R10, R11,
    R12, R13, R14, R15,
}

/// The type which represents a value contained in an ARM AArch32 register.
type Value = u32;

/// The type which represents an ARM AArch32 memory address.
type Pointer = u32;

/// The type which represents a positive memory offset.
type Offset = u32;

/// The type which represents data stored in memory as seen by the processor.
type Data = u8;

/// The compatible memory model type necessary to analyze AArch32 programs.
pub type Bus = memory::Memory<Pointer, Data, Offset>;

/// Disassemble the instruction at `p` in `mem`.
/// 
/// This function returns:
/// 
///  * A string representation of the instruction encountered, if there is a
///    valid instruction at P; otherwise `None`
///  * The size of the current instruction
///  * True, if execution would continue at the instruction following this one,
///    or false if the instruction terminates the current basic block
pub fn disassemble(p: &memory::Pointer<Pointer>, mem: &Bus) -> (Option<ast::Instruction>, Offset, bool) {
    (None, 0, false)
}