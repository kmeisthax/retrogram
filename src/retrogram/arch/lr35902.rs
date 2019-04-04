//! A Z80 derivative created by SHARP for use in the Nintendo Game Boy

use crate::retrogram::memory;

/// Enumeration of all architectural GBZ80 registers.
/// 
/// Couple things to note:
/// 
///  * We don't consider register pairs (e.g. BC, DE, HL)
///  * F isn't considered special here
///  * SP has been treated as a register pair and split into S and P.
#[derive(Copy, Debug, Hash)]
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

/// Disassemble the instruction at `p` in `mem`.
/// 
/// This function returns:
/// 
///  * A string representation of the instruction encountered, if there is a
///    valid instruction at P; otherwise `None`
///  * The size of the current instruction
///  * If the current instruction should have a further instruction following it
///  * A list of symbolic pointers to possible next instructions
/// 
/// The list of next instructions should be sorted by conditionality. The first
/// slot of the vector is always reserved for the instruction which should
/// appear 'next' in source order. (On some exotic architectures where PC is a
/// polynomial, rather than arithmetic counter, this is not `p` + the size of
/// the current instruction.) If no successive instruction exists, then return
/// parameter 3 should be false. The remaining next instruction pointers
/// represent jumps to other addresses.
fn disassemble(p: Pointer, mem: memory::Memory<Value, Data>) -> (Option<String>, Offset, bool, Vec<Symbolic<Pointer>>) {
    (None, 0, false, Vec::new())
}