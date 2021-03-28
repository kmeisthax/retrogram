//! Error type for database save/load

use std::{error, fmt, io, result};

#[derive(Debug)]
pub enum Error {
    /// Underlying cause of error is I/O related
    IoError(io::Error),

    /// Underlying cause of error is JSON related
    JsonError(serde_json::Error),

    /// Read a program of the given name that is not listed in the project
    UnknownProgramError(String),

    /// JSON file contained unknown/missing data
    SemanticError,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::IoError(e) => write!(f, "I/O error: {}", e),
            Error::JsonError(e) => write!(f, "JSON error: {}", e),
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
            Error::IoError(e) => Some(e),
            Error::JsonError(e) => Some(e),
            _ => None,
        }
    }
}

impl From<Error> for io::Error {
    fn from(err: Error) -> io::Error {
        match err {
            Error::IoError(e) => e,
            _ => io::Error::new(io::ErrorKind::Other, format!("{}", err)),
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::IoError(err)
    }
}

impl From<serde_json::Error> for Error {
    fn from(err: serde_json::Error) -> Self {
        Error::JsonError(err)
    }
}

pub type Result<T> = result::Result<T, Error>;
