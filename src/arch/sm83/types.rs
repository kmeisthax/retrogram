//! Types used in modeling the SM83

use std::str;
use crate::{memory, ast, reg, analysis};

/// Enumeration of all architectural GBZ80 registers.
/// 
/// Couple things to note:
/// 
///  * We don't consider register pairs (e.g. BC, DE, HL)
///  * F isn't considered special here
///  * SP has been treated as a register pair and split into S and P.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Register {
    A, B, C, D, E, H, L, F, S, P
}

impl Register {
    pub fn reglist_from_sym(s: &str) -> Vec<Register> {
        match s.to_ascii_lowercase().as_str() {
            "a" => vec![Register::A],
            "b" => vec![Register::B],
            "c" => vec![Register::C],
            "d" => vec![Register::D],
            "e" => vec![Register::E],
            "h" => vec![Register::H],
            "l" => vec![Register::L],
            "af" => vec![Register::A, Register::F],
            "bc" => vec![Register::B, Register::C],
            "de" => vec![Register::D, Register::E],
            "hl" => vec![Register::H, Register::L],
            "hld" => vec![Register::H, Register::L],
            "hli" => vec![Register::H, Register::L],
            "sp" => vec![Register::S, Register::P],
            _ => vec![]
        }
    }
}

/// The type which represents a value contained in an SM83 register.
pub type Value = u8;

/// The type which represents an SM83 memory address.
pub type Pointer = u16;

/// The type which represents a positive memory offset.
pub type Offset = u16;

/// The type which represents a signed value contained in an SM83 register or register pair.
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
pub type Section = ast::Section<Offset, SignedValue, f32, Pointer, Data, Offset>;

/// The register state type which represents the execution state of a given
/// SM83 program.
pub type State = reg::State<Register, Value, Pointer, Value>;

/// The disasm type which represents a successful disassembly of a single
/// instruction.
pub type Disasm = analysis::Disasm<Offset, SignedValue, f32, Pointer, Offset>;

pub type Result<T> = analysis::Result<T, Pointer, Offset>;