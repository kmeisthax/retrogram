//! Implementations of full platforms which define the entire execution
//! environment that a program image will be run in.
//!
//! A properly configured platform is required in order to analyze a program.

pub mod agb;
pub mod gb;
//pub mod sfc;

use crate::arch;
use serde::Serialize;
use std::str;

/// Enumeration of all platforms that ship with Retrogram.
#[derive(Copy, Clone, Serialize, Debug)]
pub enum PlatformName {
    GB,
    AGB,
    SFC,
}

impl PlatformName {
    /// Determine the default architecture for a given platform.
    ///
    /// Some platforms can support multiple architectures, so this lookup may
    /// fail.
    pub fn default_arch(self) -> Option<arch::ArchName> {
        match self {
            PlatformName::GB => Some(arch::ArchName::SM83),
            PlatformName::AGB => Some(arch::ArchName::AARCH32),
            PlatformName::SFC => Some(arch::ArchName::W65C816),
        }
    }
}

impl str::FromStr for PlatformName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            "gb" => Ok(PlatformName::GB),
            "gba" => Ok(PlatformName::AGB),
            "agb" => Ok(PlatformName::AGB),
            "sfc" => Ok(PlatformName::SFC),
            "snes" => Ok(PlatformName::SFC),
            _ => Err(()),
        }
    }
}

derive_deserialize_from_str!(PlatformName, "valid platform name");
