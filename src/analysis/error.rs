//! Error type for analysis

use std::{io, error, fmt, result};

/// Error type for analysis.
#[derive(Debug)]
pub enum Error {
    IOError(io::Error),
    UnconstrainedMemory,
    InvalidInstruction,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;

        match self {
            IOError(e) => write!(f, "I/O error: {}", e),
            UnconstrainedMemory => write!(f, "Attempted to disassemble an invalid location (e.g. in uninitialized memory)"),
            InvalidInstruction => write!(f, "Attempted to disassemble invalid instruction")
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(error::Error + 'static)> {
        use Error::*;

        match self {
            IOError(e) => Some(e),
            _ => None
        }
    }
}

pub type Result<T> = result::Result<T, Error>;