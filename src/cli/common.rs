//! Common utilities for command implementations

use crate::arch::ArchName;
use crate::asm::AssemblerName;
use crate::platform::PlatformName;
use crate::project::Program;
use std::io;
use std::str;
use std::str::FromStr;

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
pub fn resolve_program_config(
    prog: &Program,
) -> io::Result<(ArchName, PlatformName, AssemblerName)> {
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

/// Execute a callback with a given set of architectural, platform, and
/// assembler related functions.
/// 
/// This macro must be invoked in order to almost anything generic with a
/// particular architecture. It is responsible for instantiating your code
/// across each architecture's particular type system.
macro_rules! with_architecture {
    ($prog:ident, $image_file:ident, |$bus:ident, $dis:ident, $fmt_section:ident, $fmt_instr:ident, $aparse:ident| $callback:block) => {
        match resolve_program_config($prog)? {
            (
                crate::arch::ArchName::SM83,
                crate::platform::PlatformName::GB,
                crate::asm::AssemblerName::RGBDS,
            ) => {
                let $bus = &crate::platform::gb::construct_platform(&mut $image_file)?;
                let $dis = crate::arch::sm83::disassemble;
                let $fmt_section = crate::asm::rgbds::format_section::<
                    crate::arch::sm83::Offset,
                    crate::arch::sm83::SignedValue,
                    f32,
                    crate::arch::sm83::Pointer,
                    crate::arch::sm83::Data,
                    crate::arch::sm83::Offset,
                >;
                let $fmt_instr = crate::asm::rgbds::format_instr::<
                    crate::arch::sm83::Offset,
                    crate::arch::sm83::SignedValue,
                    f32,
                    crate::arch::sm83::Pointer,
                >;
                let $aparse = crate::arch::sm83::architectural_ctxt_parse;
                $callback
            }
            (
                crate::arch::ArchName::AARCH32,
                crate::platform::PlatformName::AGB,
                crate::asm::AssemblerName::ARMIPS,
            ) => {
                let $bus = &crate::platform::agb::construct_platform(&mut $image_file)?;
                let $dis = crate::arch::aarch32::disassemble;
                let $fmt_section = crate::asm::armips::format_section::<
                    crate::arch::aarch32::Offset,
                    crate::arch::aarch32::Value,
                    f32,
                    crate::arch::aarch32::Pointer,
                    crate::arch::aarch32::Data,
                    crate::arch::aarch32::Offset,
                >;
                let $fmt_instr = crate::asm::armips::format_instr::<
                    crate::arch::aarch32::Offset,
                    crate::arch::aarch32::Value,
                    f32,
                    crate::arch::aarch32::Pointer,
                >;
                let $aparse = crate::arch::aarch32::architectural_ctxt_parse;
                $callback
            }
            _ => Err(::std::io::Error::new(
                ::std::io::ErrorKind::Other,
                "Unsupported combination of architecture, platform, or assembler syntax.",
            )),
        }
    };
}
