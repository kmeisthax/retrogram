//! 65C816 CPU architecture. Extension of the 6502 to ""16-bit"" operation.
//! 
//! I swear, if this counts as 16-bit, then the god damned Sega Genesis was
//! 32-bit. Do the math.
//! 
//! Used as the main CPU for Apple ][gs and Super Famicom platforms.
//! Also used as accelerator chips for Commodore 64 and Super Famicom.

use crate::retrogram::{ast, memory, analysis};
use crate::retrogram::maths::u24;
use crate::retrogram::ast::Operand as op;
use crate::retrogram::ast::Instruction as inst;

/// Names of all 65C816 registers we care about.
/// 
/// A, X, Y, and S are variable-width: various bits of P either shorten or
/// lengthen them to 8 or 16 bits. The rest of the registers work as follows:
/// 
///  * PC is always 16 bits, and PB is always 8. They combine to form a 24-bit
///    memory address to retrieve the next instruction from.
///  * P is always 8 bits.
///  * DB is always 8 bits. It combines with the calculated address of any data
///    read or write instruction to form a 24-bit memory address to retrieve the
///    next instruction from.
/// 
/// The variable width nature of certain registers also means that we need an
/// architectural context for the widths of certain registers.
enum Register {
    /// General-purpose Accumulator.
    A,
    /// Index register, type X
    X,
    /// Index register, type Y
    Y,
    /// Stack pointer register
    S,
    /// Processor Status register
    P,
    /// Zero page pointer register
    D,
    /// Data bank
    DB,
    /// Program bank
    PB
}

/// Enum type representing a 65C816 register value.
/// 
/// Data width varies based on context, so this needs to be able to hold either
/// u8 or u16s.
#[allow(non_camel_case_types)]
pub enum Value {
    u8(u8),
    u16(u16)
}

/// The type which represents a 65C816 memory address.
pub type Pointer = u24;

/// The type which represents a positive memory offset.
pub type Offset = u24;

/// The type which represents a signed value contained in a 65C816 register.
#[allow(non_camel_case_types)]
pub enum SignedValue {
    i8(i8),
    i16(i16)
}

/// The type which represents data stored in memory as seen by the processor.
pub type Data = u8;

/// The compatible memory model type necessary to analyze 65C816 programs.
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
pub fn disassemble(p: &memory::Pointer<Pointer>, mem: &Bus) -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    match mem.read_unit(p).as_concrete() {
        Some(0x1B) => (Some(inst::new("TCS", vec![])), u24::from(1 as u8), true, true, vec![]),
        Some(0x3B) => (Some(inst::new("TSC", vec![])), u24::from(1 as u8), true, true, vec![]),
        Some(0x5B) => (Some(inst::new("TCD", vec![])), u24::from(1 as u8), true, true, vec![]),
        Some(0x7B) => (Some(inst::new("TDC", vec![])), u24::from(1 as u8), true, true, vec![]),
        Some(0x8A) => (Some(inst::new("TXA", vec![])), u24::from(1 as u8), true, true, vec![]),
        Some(0x98) => (Some(inst::new("TYA", vec![])), u24::from(1 as u8), true, true, vec![]),
        Some(0x9A) => (Some(inst::new("TXS", vec![])), u24::from(1 as u8), true, true, vec![]),
        Some(0x9B) => (Some(inst::new("TXY", vec![])), u24::from(1 as u8), true, true, vec![]),
        Some(0xA8) => (Some(inst::new("TAY", vec![])), u24::from(1 as u8), true, true, vec![]),
        Some(0xAA) => (Some(inst::new("TAX", vec![])), u24::from(1 as u8), true, true, vec![]),
        Some(0xBA) => (Some(inst::new("TSX", vec![])), u24::from(1 as u8), true, true, vec![]),
        Some(0xBB) => (Some(inst::new("TYX", vec![])), u24::from(1 as u8), true, true, vec![]),
        _ => (None, u24::from(0 as u8), false, true, vec![])
    }
}
