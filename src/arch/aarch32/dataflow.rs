//! Dataflow analysis for AArch32

use crate::analysis::Error;
use crate::arch::aarch32::types::{Bus, BusAddress, Requisite, Result};
use std::collections::HashSet;

pub fn dataflow(_at: &BusAddress, _bus: &Bus) -> Result<(HashSet<Requisite>, HashSet<Requisite>)> {
    Err(Error::NotYetImplemented)
}
