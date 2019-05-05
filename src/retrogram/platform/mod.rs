//! Implementations of full platforms which define the entire execution
//! environment that a program image will be run in.
//! 
//! A properly configured platform is required in order to analyze a program.

pub mod gb;
pub mod agb;

use std::str;
use serde::Serialize;

/// Enumeration of all platforms that ship with Retrogram.
#[derive(Copy, Clone, Serialize, Debug)]
pub enum PlatformName {
    GB,
    AGB
}

impl str::FromStr for PlatformName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            "gb" => Ok(PlatformName::GB),
            "gba" => Ok(PlatformName::AGB),
            "agb" => Ok(PlatformName::AGB),
            _ => Err(())
        }
    }
}

derive_deserialize_from_str!(PlatformName, "valid platform name");