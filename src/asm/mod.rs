//! Interop with different assembler syntaxes

pub mod armips;
pub mod rgbds;

use serde::Serialize;
use std::str;

#[derive(Copy, Clone, Serialize, Debug)]
pub enum AssemblerName {
    RGBDS,
    ARMIPS,
}

impl str::FromStr for AssemblerName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            "rgbds" => Ok(AssemblerName::RGBDS),
            "armips" => Ok(AssemblerName::ARMIPS),
            _ => Err(()),
        }
    }
}

derive_deserialize_from_str!(AssemblerName, "valid assembler name");
