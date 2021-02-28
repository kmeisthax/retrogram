//! Forking analysis for AArch32

use crate::analysis::Error;
use crate::arch::aarch32::types::{Bus, PtrVal, Requisite, Result, State};
use std::collections::HashSet;

pub fn prereq(_p: PtrVal, _mem: &Bus, _state: &State) -> Result<(HashSet<Requisite>, bool)> {
    Err(Error::NotYetImplemented)
}
