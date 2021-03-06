//! Dynamic analysis passes

use crate::analysis::{Disasm, Reference, ReferenceKind, Requisite, Trace, TraceEvent};
use crate::arch::{Architecture, CompatibleLiteral};
use crate::database::Database;
use crate::memory::{Memory, Pointer};
use crate::reg::State;
use crate::{analysis, reg};
use num::Zero;
use std::collections::HashSet;
use std::convert::TryInto;

#[allow(type_alias_bounds)]
type TraceResult<AR>
where
    AR: Architecture,
= analysis::Result<
    (
        AR::PtrVal,
        analysis::Trace<AR>,
        reg::State<AR>,
        HashSet<Requisite<AR>>,
    ),
    AR,
>;

/// Trace a given precondition state until it is necessary to fork the analysis
/// into multiple states.
///
/// This function returns the precondition state and a pointer to the first
/// instruction which requires parallel tracing and cannot be executed
/// symbolically.
pub fn trace_until_fork<AR>(
    pc: &AR::PtrVal,
    mut trace: Trace<AR>,
    bus: &Memory<AR>,
    pre_state: &State<AR>,
    arch: AR,
) -> TraceResult<AR>
where
    AR: Architecture,
    AR::Offset: TryInto<usize>, //This shouldn't be necessary.
{
    let mut new_pc = pc.clone();
    let mut new_state = pre_state.clone();

    loop {
        let (prerequisites, _is_complete) = arch.prerequisites(new_pc.clone(), bus, &new_state)?;
        let mut missing = HashSet::new();

        for pr in prerequisites {
            if !pr.necessary_forks(&new_state, bus).is_zero() {
                let context_prs = pr.check_for_missing_contexts(&new_state, &bus);

                if !context_prs.is_empty() {
                    for cpr in context_prs {
                        missing.insert(cpr);
                    }
                } else {
                    missing.insert(pr);
                }
            }
        }

        if !missing.is_empty() {
            return Ok((new_pc, trace, new_state, missing));
        }

        let (next_state, next_pc) = arch.trace(new_pc, bus, new_state, &mut trace)?;

        new_state = next_state;
        trace.traced_to(new_state.contextualize_pointer(next_pc.clone()));
        new_pc = next_pc;
    }
}

/// Analyze a completed trace log, updating the database with any new cross
/// references which could be gleaned from the log.
///
/// This function returns a set of blocks that the trace has passed through. At
/// the end of trace execution, all of these sets must be merged, and any block
/// within them must have their trace couts incremented by one.
pub fn analyze_trace_log<L, AR>(
    trace: &Trace<AR>,
    bus: &Memory<AR>,
    database: &mut Database<AR>,
    arch: AR,
) -> analysis::Result<HashSet<usize>, AR>
where
    L: CompatibleLiteral<AR>,
    AR: Architecture,
{
    let mut last_pc: Option<Pointer<AR::PtrVal>> = None;
    let mut last_disasm: Option<Disasm<L, AR::PtrVal, AR::Offset>> = None;
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
                last_disasm = Some(arch.disassemble(pc, bus)?);
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
            TraceEvent::ArchitecturalContextSet(_ctxt, _value) => {}
            TraceEvent::PlatformContextSet(_ctxt, _value) => {}
        }
    }

    Ok(traced_blocks)
}
