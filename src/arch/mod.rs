//! Implementations of processor architectures that Retrogram can analyze.
//!
//! Each architecture is implemented as a child module to this one. At a bare
//! minimum, the architectures must implement at least one function, called
//! "disassemble", and a series of seven types that interact with various parts
//! of retrogram's machinery:
//!
//!  * `I`, or `Value` - The type of a valid integer register value. Should be
//!    wide enough to hold any single register's value.
//!  * `SI`, or `SignedValue` - The `I` type, but signed.
//!  * `F`, or `Float` - The type of a valid floating-point register value. If
//!    your architecture does not support floating point, just provide `f32`
//!    until such time as we have a suitable never type to fit in here.
//!  * `P`, or `Pointer` - The type of a valid memory address. Archiectures with
//!    multiple buses (e.g. port I/O) must provide a Pointer type which can
//!    represent any location on any bus. See the `memory::PtrNum` trait for
//!    more information.
//!  * `S`, or `Offset` - The type of a valid memory offset. It should be
//!    possible to subtract two pointers and get an offset, and add that offset
//!    to one pointer to get the other. Allowances are given for architectures
//!    where not all pointers can be converted into a meaningful offset. See the
//!    `memory::Offset` trait for more information.
//!  * `MV`, or `Data` - The type of a single atomic memory unit, usually u8.
//!    Architectures whose memory is word-addressed would have a wider data type
//!    than a byte.
//!  * `IO` - The type used to represent offsets into a single image. Defaults
//!    to usize and should almost never be anything else.
//!
//! To determine what the disassemble function should do, please consult the
//! documenation of an existing disassembler implementation.

pub mod aarch32;
pub mod sm83;
//pub mod w65c816;

mod traits;

#[macro_use]
mod macros;

#[cfg(test)]
pub mod tests;

pub use traits::AnyArch;
pub use traits::Architecture;
pub use traits::CompatibleLiteral;

use crate::asm;
use serde::Serialize;
use std::str;

/// Enumeration of all architectures that ship with Retrogram.
#[derive(Copy, Clone, Serialize, Debug, PartialEq, Eq)]
pub enum ArchName {
    Sm83,
    AArch32,

    /// Test
    #[cfg(test)]
    Test,
}

impl ArchName {
    /// Determine the default assembler syntax for a given platform.
    ///
    /// Some architectures don't have an assembler implemented yet, so this
    /// lookup may fail.
    pub fn default_asm(self) -> Option<asm::AssemblerName> {
        match self {
            ArchName::Sm83 => Some(asm::AssemblerName::Rgbds),
            ArchName::AArch32 => None,

            #[cfg(test)]
            ArchName::Test => None,
        }
    }

    /// Iterate all supported architectures.
    pub fn iter() -> impl IntoIterator<Item = Self> {
        vec![Self::Sm83, Self::AArch32]
    }

    /// Yield a human-friendly name for this architecture.
    pub fn friendly_name(self) -> &'static str {
        match self {
            Self::Sm83 => "SM83 (LR35902 / \"Game Boy\" Z80)",
            Self::AArch32 => "ARM Architecture 32-bit",

            #[cfg(test)]
            Self::Test => "Test Architecture",
        }
    }
}

impl str::FromStr for ArchName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            "sm83" => Ok(ArchName::Sm83),
            "lr35902" => Ok(ArchName::Sm83),
            "gbz80" => Ok(ArchName::Sm83),
            "arm" => Ok(ArchName::AArch32),
            "arm32" => Ok(ArchName::AArch32),
            "aarch32" => Ok(ArchName::AArch32),
            _ => Err(()),
        }
    }
}

derive_deserialize_from_str!(ArchName, "valid architecture name");
