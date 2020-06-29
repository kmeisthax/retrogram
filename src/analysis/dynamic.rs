//! Dynamic analysis passes

use crate::analysis::{
    Disasm, Disassembler, Mappable, PrerequisiteAnalysis, Reference, ReferenceKind, Trace,
    TraceEvent, Tracer,
};
use crate::database::Database;
use crate::memory::{Memory, Offset, Pointer, PtrNum};
use crate::reg::{Bitwise, Symbolic};
use crate::{analysis, memory, reg};
use num_traits::{One, Zero};
use std::collections::HashSet;
use std::convert::TryInto;

type TraceResult<RK, I, P, MV, S> = analysis::Result<
    (
        memory::Pointer<P>,
        analysis::Trace<RK, I, P, MV>,
        reg::State<RK, I, P, MV>,
        Vec<Prerequisite<RK, I, P, MV, S>>,
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
    S: Offset<P> + TryInto<usize> + AddAssign,
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
        let (prerequisites, _is_complete) = prereq(&new_pc, bus, &new_state)?;
        let mut missing = vec![];

        for pr in prerequisites {
            match &pr {
                Prerequisite::Register { register, mask } => {
                    if !new_state
                        .get_register(register.clone())
                        .bits_are_concrete(mask.clone())
                    {
                        missing.push(pr);
                    }
                }
                Prerequisite::Memory { ptr, length, mask } => {
                    let mut i = S::zero();

                    while i < length.clone() {
                        let mask = mask
                            .get(
                                i.clone()
                                    .try_into()
                                    .map_err(|_| analysis::Error::BlockSizeOverflow)?,
                            )
                            .cloned()
                            .unwrap_or_else(MV::zero);

                        if !new_state
                            .get_memory(ptr.clone() + length.clone(), bus)
                            .bits_are_concrete(mask)
                        {
                            missing.push(pr);
                            break;
                        }

                        i += S::one();
                    }
                }
            }
        }

        if !missing.is_empty() {
            return Ok((new_pc, trace, new_state, missing));
        }

        let (next_state, next_pc) = tracer(&new_pc, bus, new_state, &mut trace)?;

        new_state = next_state;
        trace.traced_to(next_pc.clone());
        new_pc = next_pc;
    }
}

/// Analyze a completed trace log, updating the database with any new cross
/// references which could be gleaned from the log.
///
/// This function returns a set of blocks that the trace has passed through. At
/// the end of trace execution, all of these sets must be merged, and any block
/// within them must have their trace couts incremented by one.
pub fn analyze_trace_log<LI, LSI, LF, RK, I, P, MV, S, IO, DISASM>(
    trace: &Trace<RK, I, P, MV>,
    bus: &Memory<P, MV, S, IO>,
    database: &mut Database<P, S>,
    disasm: DISASM,
) -> analysis::Result<HashSet<usize>, P, S>
where
    P: Mappable + PtrNum<S>,
    S: Offset<P>,
    DISASM: Disassembler<LI, LSI, LF, P, MV, S, IO>,
{
    let mut last_pc: Option<Pointer<P>> = None;
    let mut last_disasm: Option<Disasm<LI, LSI, LF, P, S>> = None;
    let mut traced_blocks = HashSet::new();

    for event in trace.iter() {
        match event {
            TraceEvent::Execute(pc) => {
                if let (Some(ref last_pc), Some(ref last_disasm)) = (last_pc, last_disasm) {
                    if let Some(kind) = last_disasm.flow().as_reference_kind() {
                        database.insert_crossreference(Reference::new_static_ref(
                            last_pc.clone(),
                            pc.clone(),
                            kind,
                        ));
                    }
                }

                if let Some(block_id) = database.find_block_membership(pc) {
                    traced_blocks.insert(block_id);
                }

                last_pc = Some(pc.clone());
                last_disasm = Some(disasm(pc, bus)?);
            }
            TraceEvent::RegisterSet(_, _) => {}
            TraceEvent::MemoryWrite(p, _) => {
                if let Some(ref last_pc) = last_pc {
                    database.insert_crossreference(Reference::new_static_ref(
                        last_pc.clone(),
                        p.clone(),
                        ReferenceKind::Data,
                    ));
                }
            }
        }
    }

    Ok(traced_blocks)
}
