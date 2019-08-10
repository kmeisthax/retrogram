//! Common utilities for command implementations

use std::str;
use std::str::FromStr;

/// Enumeration of all CLI commands
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Command {
    Scan,
    Disassemble,
    Import,
    Backreference,
    Rename
}

impl FromStr for Command {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            "dis" => Ok(Command::Disassemble),
            "scan" => Ok(Command::Scan),
            "chadtronic-scan" => Ok(Command::Scan),
            "import" => Ok(Command::Import),
            "backrefs" => Ok(Command::Backreference),
            "name" => Ok(Command::Rename),
            "rename" => Ok(Command::Rename),
            _ => Err(())
        }
    }
}