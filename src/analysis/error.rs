//! Error type for analysis

use std::{io, error, fmt, result};
use crate::memory;

/// Error type for analysis.
#[derive(Debug)]
pub enum Error<P, S> {
    /// Underlying cause of error is I/O related
    IOError(io::Error),

    /// Read an unconstrained value from memory.
    /// 
    /// Unconstrained means that the value returned from the memory model has
    /// more than one possible value. This can happen if the memory being read
    /// from is rewritable, and no valid memory image has been loaded for that
    /// area.
    UnconstrainedMemory(memory::Pointer<P>),

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

    /// Instruction decoding failed due to an internal issue.
    /// 
    /// In general, a `Misinterpretation` indicates a programming error in
    /// retrogram that needs to be corrected.
    /// 
    /// The S parameter indicates the size of the instruction that could not be
    /// decoded, and the `bool` parameter indicates if it was a big-endian or
    /// little-endian value. This may be useful for displaying the invalid data
    /// to the user.
    Misinterpretation(S, bool)
}

impl<P, S> fmt::Display for Error<P, S> where P: fmt::UpperHex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;

        match self {
            IOError(e) => write!(f, "I/O error: {}", e),
            Error::UnconstrainedMemory(p) => write!(f, "Location {:X} is not valid", p),
            UnconstrainedRegister => write!(f, "Prereq analysis failed for instruction"),
            InvalidInstruction => write!(f, "Invalid instruction"),
            NotYetImplemented => write!(f, "Disassembly not yet implemented"),
            BlockSizeOverflow => write!(f, "Block size is too large"),
            Error::Misinterpretation(_, _) => write!(f, "Ostensibly valid instruction failed to disassemble")
        }
    }
}

impl<P, S> error::Error for Error<P, S> where S: fmt::Debug, P: fmt::Debug + fmt::UpperHex {
    fn source(&self) -> Option<&(error::Error + 'static)> {
        use Error::*;

        match self {
            IOError(e) => Some(e),
            _ => None
        }
    }
}

pub type Result<T, P, S> = result::Result<T, Error<P, S>>;