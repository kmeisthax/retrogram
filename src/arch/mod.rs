//! Implementations of processor architectures that Retrogram can analyze.

pub mod lr35902;
pub mod aarch32;
pub mod w65c816;

use std::str;
use serde::Serialize;
use crate::asm;

/// Enumeration of all architectures that ship with Retrogram.
#[derive(Copy, Clone, Serialize, Debug)]
pub enum ArchName {
    LR35902,
    AARCH32,
    W65C816,
}

impl ArchName {
    /// Determine the default assembler syntax for a given platform.
    /// 
    /// Some architectures don't have an assembler implemented yet, so this
    /// lookup may fail.
    pub fn default_asm(&self) -> Option<asm::AssemblerName> {
        match self {
            ArchName::LR35902 => Some(asm::AssemblerName::RGBDS),
            ArchName::AARCH32 => None,
            ArchName::W65C816 => None,
        }
    }
}

impl str::FromStr for ArchName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            "lr35902" => Ok(ArchName::LR35902),
            "gbz80" => Ok(ArchName::LR35902),
            "arm" => Ok(ArchName::AARCH32),
            "arm32" => Ok(ArchName::AARCH32),
            "aarch32" => Ok(ArchName::AARCH32),
            "65c816" => Ok(ArchName::W65C816),
            "w65c816" => Ok(ArchName::W65C816),
            _ => Err(())
        }
    }
}

derive_deserialize_from_str!(ArchName, "valid architecture name");