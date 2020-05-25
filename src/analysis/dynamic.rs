//! Dynamic analysis passes

use crate::analysis::{Mappable, PrerequisiteAnalysis, Tracer};
use crate::memory::{Offset, Pointer, PtrNum};
use crate::reg::{Bitwise, Symbolic};
use crate::{analysis, memory, reg};
use num_traits::One;

type TraceResult<RK, I, P, MV, S> = analysis::Result<
    (
        memory::Pointer<P>,
        analysis::Trace<RK, I, P, MV>,
        reg::State<RK, I, P, MV>,
        Vec<RK>,
        Vec<Pointer<P>>,
    ),
    P,
    S,
>;

/// Trace a given precondition state until it is necessary to fork the analysis
/// into multiple states.
///
/// This function returns the precondition state and a pointer to the first
/// instruction which requires parallel tracing and cannot be executed
/// symbolically.
pub fn trace_until_fork<RK, I, P, MV, S, IO, PREREQ, TRACER>(
    pc: &memory::Pointer<P>,
    mut trace: analysis::Trace<RK, I, P, MV>,
    bus: &memory::Memory<P, MV, S, IO>,
    pre_state: &reg::State<RK, I, P, MV>,
    prereq: PREREQ,
    tracer: TRACER,
) -> TraceResult<RK, I, P, MV, S>
where
    RK: Mappable,
    P: Mappable + PtrNum<S>,
    S: Offset<P>,
    I: Bitwise,
    MV: Bitwise,
    IO: One,
    memory::Pointer<P>: Clone,
    reg::State<RK, I, P, MV>: Clone,
    Symbolic<I>: Clone + Default,
    Symbolic<MV>: Clone + Default,
    PREREQ: PrerequisiteAnalysis<RK, I, P, MV, S, IO>,
    TRACER: Tracer<RK, I, P, MV, S, IO>,
{
    let mut new_pc = pc.clone();
    let mut new_state = pre_state.clone();

    loop {
        let (required_regs, required_mem, _is_complete) = prereq(&new_pc, bus, &new_state)?;
        let mut missing_regs = vec![];
        let mut missing_mem = vec![];

        for reg in required_regs {
            if !new_state.get_register(reg.clone()).is_concrete() {
                missing_regs.push(reg);
            }
        }

        for ptr in required_mem {
            if !new_state.get_memory(ptr.clone(), bus).is_concrete() {
                missing_mem.push(ptr);
            }
        }

        if !missing_regs.is_empty() || !missing_mem.is_empty() {
            return Ok((new_pc, trace, new_state, missing_regs, missing_mem));
        }

        let (next_state, next_pc) = tracer(&new_pc, bus, new_state, &mut trace)?;

        new_state = next_state;
        trace.traced_to(next_pc.clone());
        new_pc = next_pc;
    }
}
