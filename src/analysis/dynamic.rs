//! Dynamic analysis passes

use crate::{reg, analysis, memory};

/// Trace a given precondition state until it is necessary to fork the analysis
/// into multiple states.
/// 
/// This function returns the precondition state and a pointer to the first
/// instruction which requires parallel tracing and cannot be executed
/// symbolically.
fn trace_until_fork<RK, I, SI, F, P, MV, S, IO>(pc: &memory::Pointer<P>,
    mut trace: analysis::Trace<P>,
    bus: &memory::Memory<P, MV, S, IO>, pre_state: &reg::State<RK, I, P, MV>,
    prereq: &dyn analysis::PrerequisiteAnalysis<RK, I, P, MV, S, IO>,
    tracer: &dyn analysis::Tracer<RK, I, P, MV, S, IO>)
        -> analysis::Result<(memory::Pointer<P>, analysis::Trace<P>, reg::State<RK, I, P, MV>), P, S>
    where RK: analysis::Mappable,
        P: analysis::Mappable,
        memory::Pointer<P>: Clone,
        reg::State<RK, I, P, MV>: Clone {
    
    let mut new_pc = pc.clone();
    let mut new_state = pre_state.clone();

    loop {
        let (missing_regs, missing_mem, _is_complete) = prereq(&new_pc, bus, &new_state);
        let is_forking = missing_regs.len() > 0 || missing_mem.len() > 0;

        if is_forking {
            break;
        }
        
        let (next_state, next_pc) = tracer(&new_pc, bus, new_state)?;

        new_state = next_state;
        trace.traced_to(next_pc.clone());
        new_pc = next_pc;
    }

    Ok((new_pc, trace, new_state))
}