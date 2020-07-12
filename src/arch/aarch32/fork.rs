//! Forking analysis for AArch32

use crate::analysis::Error;
use crate::arch::aarch32::types::{Bus, BusAddress, Prerequisite, Result, State};

pub fn prereq<IO>(
    _p: &BusAddress,
    _mem: &Bus<IO>,
    _state: &State,
) -> Result<(Vec<Prerequisite>, bool)> {
    Err(Error::NotYetImplemented)
}
