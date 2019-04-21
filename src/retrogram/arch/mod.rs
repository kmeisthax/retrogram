//! Implementations of processor architectures that Retrogram can analyze.

pub mod lr35902;
pub mod aarch32;

use std::str;
use serde::{Deserialize, Serialize};

/// Enumeration of all architectures that ship with Retrogram.
#[derive(Copy, Clone, Serialize, Debug)]
pub enum ArchName {
    LR35902,
    AARCH32,
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
            _ => Err(())
        }
    }
}

derive_deserialize_from_str!(ArchName, "valid architecture name");