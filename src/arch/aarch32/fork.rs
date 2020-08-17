//! Forking analysis for AArch32

use crate::analysis::Error;
use crate::arch::aarch32::types::{Bus, Prerequisite, PtrVal, Result, State};
use std::collections::HashSet;

pub fn prereq(_p: PtrVal, _mem: &Bus, _state: &State) -> Result<(HashSet<Prerequisite>, bool)> {
    Err(Error::NotYetImplemented)
}
