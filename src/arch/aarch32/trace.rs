//! Tracing analysis for AArch32

use crate::analysis::Error;
use crate::arch::aarch32::types::{Bus, BusAddress, Result, State};

pub fn trace(_p: &BusAddress, _mem: &Bus, _state: State) -> Result<(State, BusAddress)> {
    Err(Error::NotYetImplemented)
}
