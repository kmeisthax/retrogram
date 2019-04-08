//! Implementations of processor architectures that Retrogram can analyze.

pub mod lr35902;

use std::str;

/// Enumeration of all architectures that ship with Retrogram.
enum ArchName {
    LR35902
}

impl str::FromStr for ArchName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            "LR35902" => Ok(ArchName::LR35902),
            "GBZ80" => Ok(ArchName::LR35902),
            _ => Err(())
        }
    }
}