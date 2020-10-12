//! Error type for database save/load

use std::{error, fmt, io, result};

#[derive(Debug)]
pub enum Error {
    /// Underlying cause of error is I/O related
    IOError(io::Error),

    /// Underlying cause of error is JSON related
    JSONError(serde_json::Error),

    /// Read a program of the given name that is not listed in the project
    UnknownProgramError(String),

    /// JSON file contained unknown/missing data
    SemanticError,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::IOError(e) => write!(f, "I/O error: {}", e),
            Error::JSONError(e) => write!(f, "JSON error: {}", e),
            Error::UnknownProgramError(s) => {
                write!(f, "Program {} is missing from project file", s)
            }
            Error::SemanticError => write!(f, "Project database contains unknown data"),
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Error::IOError(e) => Some(e),
            Error::JSONError(e) => Some(e),
            _ => None,
        }
    }
}

impl Into<io::Error> for Error {
    fn into(self) -> io::Error {
        match self {
            Error::IOError(e) => e,
            _ => io::Error::new(io::ErrorKind::Other, format!("{}", self)),
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::IOError(err)
    }
}

impl From<serde_json::Error> for Error {
    fn from(err: serde_json::Error) -> Self {
        Error::JSONError(err)
    }
}

pub type Result<T> = result::Result<T, Error>;
