//! Error type for analysis

use crate::arch::Architecture;
use crate::memory;
use std::{error, fmt, io, result};

/// Error type for analysis.
#[derive(Debug)]
pub enum Error<AR>
where
    AR: Architecture,
{
    /// Underlying cause of error is I/O related
    IOError(io::Error),

    /// Underlying cause of error is formatting related
    FormatError(fmt::Error),

    /// Read an unconstrained value from memory.
    ///
    /// Unconstrained means that the value returned from the memory model has
    /// more than one possible value. This can happen if the memory being read
    /// from is rewritable, and no valid memory image has been loaded for that
    /// area.
    UnconstrainedMemory(memory::Pointer<AR::PtrVal>),

    /// Read an unconstrained value from a register.
    ///
    /// All tracing should be gated behind a compatible prerequisite analysis,
    /// so this indicates a problem with said analysis. Tracers should always
    /// be given a state with the correct constraints on their registers and
    /// memory locations.
    UnconstrainedRegister,

    /// Instruction decoding failed because the value read from the program
    /// image is not valid code.
    InvalidInstruction,

    /// Instruction decoding failed because the decoding of this particular
    /// instruction is not yet implemented.
    NotYetImplemented,

    /// Analysis of a given block overflows the pointer or offset type for the
    /// given architecture.
    BlockSizeOverflow,

    /// Tracing terminated as the number of forks exceeded the analysis limit
    /// on tracing.
    TraceTooDeep {
        /// The current fork's branch score.
        branches: f64,

        /// How many additional forks would have to be created in order to
        /// continue exploring the state space of the program.
        extra_branches: f64,
    },

    /// Tracing terminated as we have already traced the same state once
    /// before.
    TraceRecurrence,

    /// Instruction decoding failed due to an internal issue.
    ///
    /// In general, a `Misinterpretation` indicates a programming error in
    /// retrogram that needs to be corrected.
    ///
    /// The S parameter indicates the size of the instruction that could not be
    /// decoded, and the `bool` parameter indicates if it was a big-endian or
    /// little-endian value. This may be useful for displaying the invalid data
    /// to the user.
    Misinterpretation(AR::Offset, bool),
}

impl<AR> fmt::Display for Error<AR>
where
    AR: Architecture,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;

        match self {
            IOError(e) => write!(f, "I/O error: {}", e),
            FormatError(e) => write!(f, "Formatting error: {}", e),
            Error::UnconstrainedMemory(p) => write!(f, "Location {:X} is not valid", p),
            UnconstrainedRegister => write!(f, "Prereq analysis failed for instruction"),
            InvalidInstruction => write!(f, "Invalid instruction"),
            NotYetImplemented => write!(f, "Disassembly not yet implemented"),
            BlockSizeOverflow => write!(f, "Block size is too large"),
            TraceTooDeep {
                branches,
                extra_branches,
            } => write!(
                f,
                "Trace too deep: would add {} on top of {} existing",
                extra_branches, branches
            ),
            TraceRecurrence => write!(f, "Block already traced"),
            Error::Misinterpretation(_, _) => {
                write!(f, "Ostensibly valid instruction failed to disassemble")
            }
        }
    }
}

impl<AR> error::Error for Error<AR>
where
    AR: Architecture,
    Self: fmt::Debug,
{
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        use Error::*;

        match self {
            IOError(e) => Some(e),
            FormatError(e) => Some(e),
            _ => None,
        }
    }
}

impl<AR> Into<io::Error> for Error<AR>
where
    AR: Architecture,
{
    fn into(self) -> io::Error {
        use Error::*;

        match self {
            IOError(e) => e,
            _ => io::Error::new(io::ErrorKind::Other, format!("{}", self)),
        }
    }
}

impl<AR> From<io::Error> for Error<AR>
where
    AR: Architecture,
{
    fn from(err: io::Error) -> Self {
        Self::IOError(err)
    }
}

pub type Result<T, AR> = result::Result<T, Error<AR>>;
