//! Interop with different assembler syntaxes

mod annotator;
pub mod armips;
pub mod rgbds;
mod traits;

pub use annotator::{AnnotatedText, AnnotationKind};
pub use traits::Assembler;

use serde::Serialize;
use std::str;

#[derive(Copy, Clone, Serialize, Debug, PartialEq, Eq)]
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

impl AssemblerName {
    pub fn iter() -> impl IntoIterator<Item = Self> {
        vec![Self::RGBDS, Self::ARMIPS]
    }

    pub fn friendly_name(self) -> &'static str {
        match self {
            Self::RGBDS => "RGBDS",
            Self::ARMIPS => "ARMIPS",
        }
    }
}

derive_deserialize_from_str!(AssemblerName, "valid assembler name");
