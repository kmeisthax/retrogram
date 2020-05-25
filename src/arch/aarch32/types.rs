//! Types used by aarch32

use crate::{analysis, ast, memory, reg};
use std::{fmt, result, str};

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Aarch32Register {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
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
            _ => None,
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
            R15 => 15,
        }
    }
}

impl fmt::Display for Aarch32Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Aarch32Register::*;

        match self {
            R0 => write!(f, "R0"),
            R1 => write!(f, "R1"),
            R2 => write!(f, "R2"),
            R3 => write!(f, "R3"),
            R4 => write!(f, "R4"),
            R5 => write!(f, "R5"),
            R6 => write!(f, "R6"),
            R7 => write!(f, "R7"),
            R8 => write!(f, "R8"),
            R9 => write!(f, "R9"),
            R10 => write!(f, "R10"),
            R11 => write!(f, "R11"),
            R12 => write!(f, "R12"),
            R13 => write!(f, "R13"),
            R14 => write!(f, "R14"),
            R15 => write!(f, "PC"),
        }
    }
}

impl str::FromStr for Aarch32Register {
    type Err = ();

    fn from_str(s: &str) -> result::Result<Self, Self::Err> {
        use Aarch32Register::*;

        match s {
            "R0" => Ok(R0),
            "R1" => Ok(R1),
            "R2" => Ok(R2),
            "R3" => Ok(R3),
            "R4" => Ok(R4),
            "R5" => Ok(R5),
            "R6" => Ok(R6),
            "R7" => Ok(R7),
            "R8" => Ok(R8),
            "R9" => Ok(R9),
            "R10" => Ok(R10),
            "R11" => Ok(R11),
            "R12" => Ok(R12),
            "R13" => Ok(R13),
            "R14" => Ok(R14),
            "R15" => Ok(R15),
            "PC" => Ok(R15),
            _ => Err(()),
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

/// The compatible contextual pointer type for AArch32
pub type BusAddress = memory::Pointer<Pointer>;

/// The compatible memory model type necessary to analyze AArch32 programs.
pub type Bus = memory::Memory<Pointer, Data, Offset>;

/// The AST type which represents a disassembled operand.
pub type Operand = ast::Operand<Offset, Value, f32, Pointer>;

/// The AST type which represents a disassembled instruction.
pub type Instruction = ast::Instruction<Offset, Value, f32, Pointer>;

/// The AST type which represents disassembled code.
pub type Section = ast::Section<Offset, Value, f32, Pointer, Data, Offset>;

/// The register state type which represents the execution state of a given
/// AArch32 program.
pub type State = reg::State<Aarch32Register, Value, Pointer, Data>;

/// The trace log type which represents the past execution of a given AArch32
/// program.
pub type Trace = analysis::Trace<Aarch32Register, Value, Pointer, Data>;

/// The type which represents a single disassembled instruction.
pub type Disasm = analysis::Disasm<Offset, Value, f32, Pointer, Offset>;

/// The type which represents any analysis result for this architecture
pub type Result<T> = analysis::Result<T, Pointer, Offset>;
