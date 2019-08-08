//! Error type for analysis

use std::{io, error, fmt, result};

/// Error type for analysis.
#[derive(Debug)]
pub enum Error<S> {
    /// Underlying cause of error is I/O related
    IOError(io::Error),

    /// Read an unconstrained value from memory.
    /// 
    /// Unconstrained means that the value returned from the memory model has
    /// more than one possible value. This can happen if the memory being read
    /// from is rewritable, and no valid memory image has been loaded for that
    /// area.
    UnconstrainedMemory,

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

impl<S> fmt::Display for Error<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;

        match self {
            IOError(e) => write!(f, "I/O error: {}", e),
            UnconstrainedMemory => write!(f, "Invalid location (e.g. in uninitialized memory)"),
            InvalidInstruction => write!(f, "Invalid instruction"),
            NotYetImplemented => write!(f, "Disassembly not yet implemented"),
            BlockSizeOverflow => write!(f, "Block size is too large"),
            Error::Misinterpretation(_, _) => write!(f, "Ostensibly valid instruction failed to disassemble")
        }
    }
}

impl<S> error::Error for Error<S> where S: fmt::Debug {
    fn source(&self) -> Option<&(error::Error + 'static)> {
        use Error::*;

        match self {
            IOError(e) => Some(e),
            _ => None
        }
    }
}

pub type Result<T, S> = result::Result<T, Error<S>>;