//! Error type for analysis

use std::{io, error, fmt, result};

/// Error type for analysis.
#[derive(Debug)]
pub enum Error<S> {
    IOError(io::Error),
    UnconstrainedMemory,
    InvalidInstruction,
    NotYetImplemented,
    BlockSizeOverflow,
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