//! Implementations of full platforms which define the entire execution
//! environment that a program image will be run in.
//! 
//! A properly configured platform is required in order to analyze a program.

pub mod gb;

use std::str;

/// Enumeration of all platforms that ship with Retrogram.
enum PlatformName {
    GB
}

impl str::FromStr for PlatformName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            "GB" => Ok(PlatformName::GB),
            _ => Err(())
        }
    }
}