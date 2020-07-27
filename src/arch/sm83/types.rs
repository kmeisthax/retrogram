//! Types used in modeling the SM83

use crate::arch::sm83::{disassemble, prereq, trace};
use crate::arch::Architecture;
use crate::memory::{Memory, Pointer};
use crate::{analysis, ast, memory, reg};
use num::One;
use std::{fmt, result, str};

/// Enumeration of all architectural GBZ80 registers.
///
/// Couple things to note:
///
///  * We don't consider register pairs (e.g. BC, DE, HL)
///  * F isn't considered special here
///  * SP has been treated as a register pair and split into S and P.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
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
    pub fn prereqs_from_sym(s: &str) -> Vec<Prerequisite> {
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
        .map(|r| Prerequisite::from_register(*r))
        .collect()
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

/// The type which represents a signed value contained in an SM83 register or register pair.
pub type SignedValue = i16;

/// The type which represents data stored in memory as seen by the processor.
pub type Data = u8;

/// The compatible memory model type necessary to analyze GBz80 programs.
pub type Bus<IO> = memory::Memory<PtrVal, Data, Offset, IO>;

/// A trait which defines what assembler literals we need support for.
pub trait Literal:
    ast::Literal + From<Value> + From<Offset> + From<memory::Pointer<PtrVal>>
{
}

impl<L> Literal for L where
    L: ast::Literal + From<Value> + From<Offset> + From<memory::Pointer<PtrVal>>
{
}

/// The AST type which represents disassembled code.
///
/// Generic parameter `L` *must* match the Literal trait defined above, which
/// is an extension of the generic AST literal trait.
pub type Section<L> = ast::Section<L, PtrVal, Data, Offset>;

/// The register state type which represents the execution state of a given
/// SM83 program.
pub type State = reg::State<Register, Value, PtrVal, Data>;

/// The prerequisites necessary to execute a given SM83 program.
pub type Prerequisite = analysis::Prerequisite<SM83>;

/// The trace log type which represents the past execution of a given SM83
/// program.
pub type Trace = analysis::Trace<Register, Value, PtrVal, Data>;

/// The disasm type which represents a successful disassembly of a single
/// instruction.
///
/// Generic parameter `L` *must* match the Literal trait defined above, which
/// is an extension of the generic AST literal trait.
pub type Disasm<L> = analysis::Disasm<L, PtrVal, Offset>;

pub type Result<T> = analysis::Result<T, SM83>;

/// Architectural type for SM83
#[derive(Copy, Clone)]
pub struct SM83();

impl Architecture for SM83 {
    type Register = Register;
    type Word = Value;
    type Byte = Data;
    type PtrVal = PtrVal;
    type Offset = Offset;

    fn parse_architectural_contexts(
        _contexts: &mut &[&str],
        _ptr: &mut Pointer<Self::PtrVal>,
    ) -> Option<()> {
        Some(())
    }

    fn disassemble<L, IO>(&self, at: &Pointer<Self::PtrVal>, bus: &Bus<IO>) -> Result<Disasm<L>>
    where
        L: Literal,
        IO: One,
    {
        disassemble(at, bus)
    }

    fn prerequisites<IO>(
        &self,
        at: &Pointer<Self::PtrVal>,
        bus: &Memory<Self::PtrVal, Self::Byte, Self::Offset, IO>,
        state: &State,
    ) -> Result<(Vec<Prerequisite>, bool)>
    where
        IO: One,
    {
        prereq(at, bus, state)
    }

    fn trace<IO>(
        &self,
        at: &Pointer<Self::PtrVal>,
        bus: &Memory<Self::PtrVal, Self::Byte, Self::Offset, IO>,
        state: State,
        this_trace: &mut Trace,
    ) -> Result<(State, Pointer<Self::PtrVal>)>
    where
        IO: One,
    {
        trace(at, bus, state, this_trace)
    }
}
