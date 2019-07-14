//! CLI commands

mod dis;
mod scan;
mod import;
mod traits;
mod main;

pub use dis::dis;
pub use scan::scan;
pub use import::import;
pub use traits::*;
pub use main::main;

use std::str;

/// Enumeration of all CLI commands
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Command {
    Scan,
    Disassemble,
    Import
}

impl str::FromStr for Command {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            "dis" => Ok(Command::Disassemble),
            "scan" => Ok(Command::Scan),
            "chadtronic-scan" => Ok(Command::Scan),
            "import" => Ok(Command::Import),
            _ => Err(())
        }
    }
}