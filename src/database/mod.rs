//! Analysis database - Allows accumulation of program facts as disassembly
//! passes run on the program.

#[macro_use]
mod type_erasure;

pub mod db;
mod error;
mod projectdb;
pub mod rgbds;

#[cfg(test)]
mod tests;

pub use db::Database;
pub use error::{Error, Result};
pub use projectdb::ProjectDatabase;
pub use type_erasure::{downcast_database, AnyDatabase};

use serde::Serialize;
use std::{result, str};

/// Enum listing all of the external data formats we can add to a retrogram
/// database.
#[derive(Copy, Clone, Serialize, Debug)]
pub enum ExternalFormat {
    RGBDSSymbolFile,
}

impl str::FromStr for ExternalFormat {
    type Err = ();

    fn from_str(s: &str) -> result::Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            "rgbds_symbols" => Ok(ExternalFormat::RGBDSSymbolFile),
            _ => Err(()),
        }
    }
}

derive_deserialize_from_str!(ExternalFormat, "valid external data source type");
