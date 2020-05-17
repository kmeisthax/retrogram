//! Common utilities for command implementations

use crate::project::Program;
use crate::platform::PlatformName;
use crate::arch::ArchName;
use crate::asm::AssemblerName;
use std::str;
use std::str::FromStr;
use std::io;

/// Enumeration of all CLI commands
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Command {
    Scan,
    Disassemble,
    Import,
    Backreference,
    Rename,
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
            _ => Err(()),
        }
    }
}

/// Resolve a program's platform, architecture, and assembler.
/// 
/// This function yields an error if any of the three could not be determined.
pub fn resolve_program_config(prog: &Program) -> io::Result<(ArchName, PlatformName, AssemblerName)> {
    let platform = prog.platform().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "Unspecified platform, analysis cannot continue.",
        )
    })?;
    let arch = prog
        .arch()
        .or_else(|| platform.default_arch())
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                "Unspecified architecture, analysis cannot continue.",
            )
        })?;
    let asm = prog
        .assembler()
        .or_else(|| arch.default_asm())
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                "Unspecified assembler for architecture, analysis cannot continue.",
            )
        })?;
    
    Ok((arch, platform, asm))
}
