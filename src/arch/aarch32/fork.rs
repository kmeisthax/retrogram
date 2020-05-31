//! Forking analysis for AArch32

use crate::analysis::Error;
use crate::arch::aarch32::types::{Bus, BusAddress, Prerequisite, Result, State};

pub fn prereq(_p: &BusAddress, _mem: &Bus, _state: &State) -> Result<(Vec<Prerequisite>, bool)> {
    Err(Error::NotYetImplemented)
}
