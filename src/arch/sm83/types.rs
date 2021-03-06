//! Types used in modeling the SM83

use crate::arch::sm83::{dataflow, disassemble, prereq, trace};
use crate::arch::{ArchName, Architecture};
use crate::memory::{Memory, Pointer};
use crate::{analysis, ast, memory, reg};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::{fmt, result, str};

/// Enumeration of all architectural GBZ80 registers.
///
/// Couple things to note:
///
///  * We don't consider register pairs (e.g. BC, DE, HL)
///  * F isn't considered special here
///  * SP has been treated as a register pair and split into S and P.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Register {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    F,
    S,
    P,
}

impl Register {
    pub fn prereqs_from_sym(s: &str) -> Vec<Requisite> {
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
            _ => vec![],
        }
        .iter()
        .map(|r| Requisite::register(*r, 0xFF))
        .collect()
    }

    pub fn into_operand<L>(self) -> ast::Operand<L>
    where
        L: ast::Literal,
    {
        match self {
            Self::A => ast::Operand::sym("a"),
            Self::B => ast::Operand::sym("b"),
            Self::C => ast::Operand::sym("c"),
            Self::D => ast::Operand::sym("d"),
            Self::E => ast::Operand::sym("e"),
            Self::H => ast::Operand::sym("h"),
            Self::L => ast::Operand::sym("l"),
            Self::F => ast::Operand::sym("f"),
            Self::S => ast::Operand::sym("s"),
            Self::P => ast::Operand::sym("p"),
        }
    }

    pub fn into_requisite(self) -> Requisite {
        Requisite::register(self, 0xFF)
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Register::*;

        match self {
            A => write!(f, "A"),
            B => write!(f, "B"),
            C => write!(f, "C"),
            D => write!(f, "D"),
            E => write!(f, "E"),
            H => write!(f, "H"),
            L => write!(f, "L"),
            F => write!(f, "F"),
            S => write!(f, "S"),
            P => write!(f, "P"),
        }
    }
}

impl str::FromStr for Register {
    type Err = ();

    fn from_str(s: &str) -> result::Result<Self, Self::Err> {
        use Register::*;

        match s {
            "A" => Ok(A),
            "B" => Ok(B),
            "C" => Ok(C),
            "D" => Ok(D),
            "E" => Ok(E),
            "H" => Ok(H),
            "L" => Ok(L),
            "F" => Ok(F),
            "S" => Ok(S),
            "P" => Ok(P),
            _ => Err(()),
        }
    }
}

/// The type which represents a value contained in an SM83 register.
pub type Value = u8;

/// The type which represents an SM83 memory address.
pub type PtrVal = u16;

/// The type which represents a positive memory offset.
pub type Offset = u16;

/// The type which represents a signed value contained in an SM83 register.
pub type SignedValue = i8;

/// The type which represents data stored in memory as seen by the processor.
pub type Data = u8;

/// The compatible memory model type necessary to analyze GBz80 programs.
pub type Bus = memory::Memory<Sm83>;

/// The pointer type necessary to model GBz80 pointers.
pub type BusAddress = memory::Pointer<PtrVal>;

/// A trait which defines what assembler literals we need support for.
pub trait Literal:
    ast::Literal + From<Value> + From<Offset> + From<memory::Pointer<PtrVal>> + From<i8>
{
}

impl<L> Literal for L where
    L: ast::Literal + From<Value> + From<Offset> + From<memory::Pointer<PtrVal>> + From<i8>
{
}

/// The AST type which represents disassembled code.
///
/// Generic parameter `L` *must* match the Literal trait defined above, which
/// is an extension of the generic AST literal trait.
pub type Section<L> = ast::Section<L, PtrVal, Data, Offset>;

/// The register state type which represents the execution state of a given
/// SM83 program.
pub type State = reg::State<Sm83>;

/// The prerequisites necessary to execute a given SM83 program.
pub type Requisite = analysis::Requisite<Sm83>;

/// The trace log type which represents the past execution of a given SM83
/// program.
pub type Trace = analysis::Trace<Sm83>;

/// The disasm type which represents a successful disassembly of a single
/// instruction.
///
/// Generic parameter `L` *must* match the Literal trait defined above, which
/// is an extension of the generic AST literal trait.
pub type Disasm<L> = analysis::Disasm<L, PtrVal, Offset>;

pub type Result<T> = analysis::Result<T, Sm83>;

/// Architectural type for SM83
#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct Sm83();

impl Architecture for Sm83 {
    type Register = Register;
    type Word = Value;
    type SignedWord = SignedValue;
    type Byte = Data;
    type PtrVal = PtrVal;
    type Offset = Offset;

    fn name(&self) -> ArchName {
        ArchName::Sm83
    }

    fn parse_architectural_contexts(
        _contexts: &mut &[&str],
        _ptr: &mut Pointer<Self::PtrVal>,
    ) -> Option<()> {
        Some(())
    }

    fn disassemble<L>(&self, at: &Pointer<Self::PtrVal>, bus: &Bus) -> Result<Disasm<L>>
    where
        L: Literal,
    {
        disassemble(at, bus)
    }

    fn dataflow(
        &self,
        at: &BusAddress,
        bus: &Bus,
    ) -> Result<(HashSet<Requisite>, HashSet<Requisite>)> {
        dataflow(at, bus)
    }

    fn prerequisites(
        &self,
        at: Self::PtrVal,
        bus: &Memory<Self>,
        state: &State,
    ) -> Result<(HashSet<Requisite>, bool)> {
        prereq(at, bus, state)
    }

    fn trace(
        &self,
        at: Self::PtrVal,
        bus: &Memory<Self>,
        state: State,
        this_trace: &mut Trace,
    ) -> Result<(State, Self::PtrVal)> {
        trace(at, bus, state, this_trace)
    }
}
