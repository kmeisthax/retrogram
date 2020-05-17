//! Forking analysis for AArch32

use crate::analysis::Error;
use crate::arch::aarch32::types::{Aarch32Register, Bus, BusAddress, Result, State};

pub fn prereq(
    _p: &BusAddress,
    _mem: &Bus,
    _state: &State,
) -> Result<(Vec<Aarch32Register>, Vec<BusAddress>, bool)> {
    Err(Error::NotYetImplemented)
}
