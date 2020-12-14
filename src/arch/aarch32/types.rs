//! Types used by aarch32

use crate::arch::aarch32::{architectural_ctxt_parse, disassemble, prereq, trace};
use crate::arch::{ArchName, Architecture};
use crate::memory::Pointer;
use crate::{analysis, ast, memory, reg};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::{fmt, result, str};

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
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
pub type PtrVal = u32;

/// The type which represents a positive memory offset.
pub type Offset = u32;

/// The type which represents data stored in memory as seen by the processor.
pub type Data = u8;

/// The compatible contextual pointer type for AArch32
pub type BusAddress = memory::Pointer<PtrVal>;

/// The compatible memory model type necessary to analyze AArch32 programs.
pub type Bus = memory::Memory<AArch32>;

/// A trait which defines what assembler literals we need support for.
pub trait Literal: ast::Literal + From<Value> + From<Offset> + From<BusAddress> {}

impl<L> Literal for L where L: ast::Literal + From<Value> + From<Offset> + From<BusAddress> {}

/// The AST type which represents disassembled code.
///
/// Generic parameter `L` *must* match the Literal trait defined above, which
/// is an extension of the generic AST literal trait.
pub type Section<L> = ast::Section<L, PtrVal, Data, Offset>;

/// The register state type which represents the execution state of a given
/// AArch32 program.
pub type State = reg::State<AArch32>;

/// The type which represents an execution prerequisite of a given AArch32
/// program.
pub type Prerequisite = analysis::Prerequisite<AArch32>;

/// The trace log type which represents the past execution of a given AArch32
/// program.
pub type Trace = analysis::Trace<AArch32>;

/// The type which represents a single disassembled instruction.
///
/// Generic parameter `L` *must* match the Literal trait defined above, which
/// is an extension of the generic AST literal trait.
pub type Disasm<L> = analysis::Disasm<L, PtrVal, Offset>;

/// The type which represents any analysis result for this architecture.
pub type Result<T> = analysis::Result<T, AArch32>;

/// Architectural type for AArch32
#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct AArch32();

impl Architecture for AArch32 {
    type Register = Aarch32Register;
    type Word = Value;
    type Byte = Data;
    type PtrVal = PtrVal;
    type Offset = Offset;

    fn name(&self) -> ArchName {
        ArchName::AARCH32
    }

    fn parse_architectural_contexts(
        contexts: &mut &[&str],
        ptr: &mut Pointer<Self::PtrVal>,
    ) -> Option<()> {
        architectural_ctxt_parse(contexts, ptr)
    }

    fn disassemble<L>(&self, at: &Pointer<Self::PtrVal>, bus: &Bus) -> Result<Disasm<L>>
    where
        L: Literal
            + From<Self::Word>
            + From<Self::Byte>
            + From<Self::Offset>
            + From<Pointer<Self::PtrVal>>,
    {
        disassemble(at, bus)
    }

    fn prerequisites(
        &self,
        at: Self::PtrVal,
        bus: &Bus,
        state: &State,
    ) -> Result<(HashSet<Prerequisite>, bool)> {
        prereq(at, bus, state)
    }

    fn trace(
        &self,
        at: Self::PtrVal,
        bus: &Bus,
        state: State,
        this_trace: &mut Trace,
    ) -> Result<(State, Self::PtrVal)> {
        trace(at, bus, state, this_trace)
    }
}
