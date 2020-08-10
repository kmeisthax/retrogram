//! Forking analysis for AArch32

use crate::analysis::Error;
use crate::arch::aarch32::types::{Bus, Prerequisite, PtrVal, Result, State};

pub fn prereq(_p: PtrVal, _mem: &Bus, _state: &State) -> Result<(Vec<Prerequisite>, bool)> {
    Err(Error::NotYetImplemented)
}
