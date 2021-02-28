//! Symbolic fork analysis for SM83

use crate::analysis::Error;
use crate::arch::sm83;
use crate::arch::sm83::{Bus, BusAddress, Requisite};
use std::collections::HashSet;

/// Statically determine the input and output requisites of a given SM83
/// instruction.
pub fn dataflow(
    _p: &BusAddress,
    _mem: &Bus,
) -> sm83::Result<(HashSet<Requisite>, HashSet<Requisite>)> {
    Err(Error::NotYetImplemented)
}
