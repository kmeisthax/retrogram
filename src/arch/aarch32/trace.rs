//! Tracing analysis for AArch32

use crate::analysis::Error;
use crate::arch::aarch32::types::{Bus, PtrVal, Result, State, Trace};

pub fn trace(_p: PtrVal, _mem: &Bus, _state: State, _trace: &mut Trace) -> Result<(State, PtrVal)> {
    Err(Error::NotYetImplemented)
}
