//! Implementations of full platforms which define the entire execution
//! environment that a program image will be run in.
//!
//! A properly configured platform is required in order to analyze a program.

pub mod agb;
pub mod gb;
mod traits;
//pub mod sfc;

pub use traits::*;

use crate::arch;
use serde::Serialize;
use std::str;

/// Enumeration of all platforms that ship with Retrogram.
#[derive(Copy, Clone, Serialize, Debug, PartialEq, Eq)]
pub enum PlatformName {
    Gb,
    Agb,
}

impl PlatformName {
    /// Determine the default architecture for a given platform.
    ///
    /// Some platforms can support multiple architectures, so this lookup may
    /// fail.
    pub fn default_arch(self) -> Option<arch::ArchName> {
        match self {
            PlatformName::Gb => Some(arch::ArchName::Sm83),
            PlatformName::Agb => Some(arch::ArchName::AArch32),
        }
    }

    /// Determine if this platform supports binaries compiled for this
    /// architecture.
    pub fn is_compatible_with_arch(self, arch: arch::ArchName) -> bool {
        match (self, arch) {
            (PlatformName::Gb, arch::ArchName::Sm83) => true,
            (PlatformName::Gb, _) => false,
            (PlatformName::Agb, arch::ArchName::AArch32) => true,
            (PlatformName::Agb, _) => false,
        }
    }

    /// Iterate all valid platform names.
    pub fn iter() -> impl IntoIterator<Item = PlatformName> {
        vec![Self::Gb, Self::Agb]
    }

    /// Yield a name for this platform.
    pub fn friendly_name(self) -> &'static str {
        match self {
            Self::Gb => "Game Boy",
            Self::Agb => "Game Boy Advance",
        }
    }
}

impl str::FromStr for PlatformName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            "gb" => Ok(PlatformName::Gb),
            "gba" => Ok(PlatformName::Agb),
            "agb" => Ok(PlatformName::Agb),
            _ => Err(()),
        }
    }
}

derive_deserialize_from_str!(PlatformName, "valid platform name");
