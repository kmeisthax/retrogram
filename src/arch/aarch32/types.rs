//! Types used by aarch32

use crate::{memory, ast, analysis};

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Aarch32Register {
    R0, R1, R2, R3,
    R4, R5, R6, R7,
    R8, R9, R10, R11,
    R12, R13, R14, R15,
}

impl Aarch32Register {
    pub fn from_instr(reg: u32) -> Option<Aarch32Register> {
        use Aarch32Register::*;

        match reg {
            0 => Some(R0),
            1 => Some(R1),
            2 => Some(R2),
            3 => Some(R3),
            4 => Some(R4),
            5 => Some(R5),
            6 => Some(R6),
            7 => Some(R7),
            8 => Some(R8),
            9 => Some(R9),
            10 => Some(R10),
            11 => Some(R11),
            12 => Some(R12),
            13 => Some(R13),
            14 => Some(R14),
            15 => Some(R15),
            _ => None
        }
    }

    pub fn into_instr(self) -> u32 {
        use Aarch32Register::*;

        match self {
            R0 => 0,
            R1 => 1,
            R2 => 2,
            R3 => 3,
            R4 => 4,
            R5 => 5,
            R6 => 6,
            R7 => 7,
            R8 => 8,
            R9 => 9,
            R10 => 10,
            R11 => 11,
            R12 => 12,
            R13 => 13,
            R14 => 14,
            R15 => 15
        }
    }
}

impl ToString for Aarch32Register {
    fn to_string(&self) -> String {
        use Aarch32Register::*;
        
        match self {
            R0 => "R0".to_string(),
            R1 => "R1".to_string(),
            R2 => "R2".to_string(),
            R3 => "R3".to_string(),
            R4 => "R4".to_string(),
            R5 => "R5".to_string(),
            R6 => "R6".to_string(),
            R7 => "R7".to_string(),
            R8 => "R8".to_string(),
            R9 => "R9".to_string(),
            R10 => "R10".to_string(),
            R11 => "R11".to_string(),
            R12 => "R12".to_string(),
            R13 => "R13".to_string(),
            R14 => "R14".to_string(),
            R15 => "PC".to_string()
        }
    }
}

/// The type which represents a value contained in an ARM AArch32 register.
pub type Value = i32;

/// The type which represents an ARM AArch32 memory address.
pub type Pointer = u32;

/// The type which represents a positive memory offset.
pub type Offset = u32;

/// The type which represents data stored in memory as seen by the processor.
pub type Data = u8;

/// The compatible memory model type necessary to analyze AArch32 programs.
pub type Bus = memory::Memory<Pointer, Data, Offset>;

/// The AST type which represents a disassembled operand.
pub type Operand = ast::Operand<Offset, Value, f32, Pointer>;

/// The AST type which represents a disassembled instruction.
pub type Instruction = ast::Instruction<Offset, Value, f32, Pointer>;

/// The AST type which represents disassembled code.
pub type Section = ast::Section<Offset, Value, f32, Pointer, Data, Offset>;

/// The type which represents a single disassembled instruction.
pub type Disasm = analysis::Disasm<Offset, Value, f32, Pointer, Offset>;