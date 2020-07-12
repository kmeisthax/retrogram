//! Tracing analysis for AArch32

use crate::analysis::Error;
use crate::arch::aarch32::types::{Bus, BusAddress, Result, State, Trace};

pub fn trace<IO>(
    _p: &BusAddress,
    _mem: &Bus<IO>,
    _state: State,
    _trace: &mut Trace,
) -> Result<(State, BusAddress)> {
    Err(Error::NotYetImplemented)
}
