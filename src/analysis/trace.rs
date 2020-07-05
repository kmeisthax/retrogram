//! Analysis trace

use crate::memory::Pointer;
use crate::reg::{State, Symbolic};
use num_traits::One;
use std::hash::Hash;
use std::ops::Add;

/// An individual event in the trace log.
#[derive(Clone, PartialEq, Eq)]
pub enum TraceEvent<RK, RV, P, MV> {
    /// The execution of an instruction at a particular address.
    Execute(Pointer<P>),

    /// The setting of a register to a new value.
    RegisterSet(RK, Symbolic<RV>),

    /// The writing of one or more bytes of memory to a particular location.
    ///
    /// This allows wider writes to be listed as a single operation. Writes
    /// should always be notated from the lowest address up, regardless of
    /// endianness.
    MemoryWrite(Pointer<P>, Vec<Symbolic<MV>>),
}

/// A Trace is a structure which stores the path that a dynamic analysis pass,
/// such as symbolic execution, takes to reach a particular execution state.
///
/// This structure does not store execution state information; for that, you
/// will need to consult `reg::State`. The Trace only contains information on
/// how we got to that state.
#[derive(Clone, PartialEq, Eq)]
pub struct Trace<RK, RV, P, MV> {
    /// Every instruction execution, register set, and memory write along the
    /// trace path.
    log: Vec<TraceEvent<RK, RV, P, MV>>,
}

impl<RK, RV, P, MV> Trace<RK, RV, P, MV> {
    /// Create a new trace beginning at the given pointer.
    pub fn begin_at(start_pc: Pointer<P>) -> Self {
        Trace {
            log: vec![TraceEvent::Execute(start_pc)],
        }
    }

    /// Add a new instruction execution to the trace.
    ///
    /// This should be called *before* any register or memory writes are logged
    /// to the trace.
    pub fn traced_to(&mut self, next_pc: Pointer<P>) -> &mut Self {
        self.log.push(TraceEvent::Execute(next_pc));
        self
    }

    /// Read out the contents of the trace.
    pub fn iter(&self) -> impl Iterator<Item = &TraceEvent<RK, RV, P, MV>> {
        self.log.iter()
    }
}

impl<RK, RV, P, MV> Trace<RK, RV, P, MV>
where
    RK: Eq + Hash + Clone,
    RV: Clone,
    P: Eq + Hash,
{
    /// Add a new register set to the trace.
    ///
    /// This should be called *after* an execution trace (see `traced_to`).
    /// Events before the first trace may not be displayed to users.
    ///
    /// This function will also actually set the register value on the state.
    pub fn register_set(
        &mut self,
        reg: RK,
        val: Symbolic<RV>,
        state: &mut State<RK, RV, P, MV>,
    ) -> &mut Self {
        self.log
            .push(TraceEvent::RegisterSet(reg.clone(), val.clone()));

        state.set_register(reg, val);

        self
    }
}

impl<RK, RV, P, MV> Trace<RK, RV, P, MV>
where
    RK: Eq + Hash,
    P: Eq + Hash + Clone + One,
    Pointer<P>: Clone,
    MV: Clone,
    Pointer<P>: Add<P> + From<<Pointer<P> as Add<P>>::Output>,
{
    /// Add a new memory write to the trace.
    ///
    /// This should be called *after* an execution trace (see `traced_to`).
    /// Events before the first trace may not be displayed to users.
    ///
    /// This function will also actually write the memory value to the state.
    pub fn memory_write(
        &mut self,
        mut ptr: Pointer<P>,
        val: &[Symbolic<MV>],
        state: &mut State<RK, RV, P, MV>,
    ) -> &mut Self {
        self.log
            .push(TraceEvent::MemoryWrite(ptr.clone(), val.to_vec()));

        for mv in val {
            state.set_memory(ptr.clone(), mv.clone());

            ptr = Pointer::from(ptr + P::one());
        }

        self
    }
}
