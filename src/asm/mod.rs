//! Interop with different assembler syntaxes

pub mod rgbds;

use std::str;
use serde::Serialize;

#[derive(Copy, Clone, Serialize, Debug)]
pub enum AssemblerName {
    RGBDS
}

impl str::FromStr for AssemblerName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            "rgbds" => Ok(AssemblerName::RGBDS),
            _ => Err(())
        }
    }
}

derive_deserialize_from_str!(AssemblerName, "valid assembler name");