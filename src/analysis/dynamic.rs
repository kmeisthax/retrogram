//! Dynamic analysis passes

use crate::analysis::{Disasm, Prerequisite, Reference, ReferenceKind, Trace, TraceEvent};
use crate::arch::{Architecture, CompatibleLiteral};
use crate::database::Database;
use crate::memory::{Memory, Pointer};
use crate::reg::State;
use crate::{analysis, memory, reg};
use num::{One, Zero};
use std::collections::HashSet;
use std::convert::TryInto;

#[allow(type_alias_bounds)]
type TraceResult<AR>
where
    AR: Architecture,
= analysis::Result<
    (
        memory::Pointer<AR::PtrVal>,
        analysis::Trace<AR>,
        reg::State<AR>,
        Vec<Prerequisite<AR>>,
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
    pc: &Pointer<AR::PtrVal>,
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
        let (prerequisites, _is_complete) = arch.prerequisites(&new_pc, bus, &new_state)?;
        let mut missing = vec![];

        for pr in prerequisites {
            match &pr {
                Prerequisite::Register { register, mask } => {
                    if !new_state
                        .get_register(&register.clone())
                        .bits_are_concrete(mask.clone())
                    {
                        missing.push(pr);
                    }
                }
                Prerequisite::Memory { ptr, length, mask } => {
                    let mut i = AR::Offset::zero();

                    while i < length.clone() {
                        let index: usize = i
                            .clone()
                            .try_into()
                            .map_err(|_| analysis::Error::BlockSizeOverflow)?;
                        let mask = mask.get(index).cloned().unwrap_or_else(AR::Byte::zero);

                        if !new_state
                            .get_memory(&(ptr.clone() + length.clone()), bus)
                            .bits_are_concrete(mask)
                        {
                            missing.push(pr);
                            break;
                        }

                        i = i + AR::Offset::one();
                    }
                }
            }
        }

        if !missing.is_empty() {
            return Ok((new_pc, trace, new_state, missing));
        }

        let (next_state, next_pc) = arch.trace(&new_pc, bus, new_state, &mut trace)?;

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
pub fn analyze_trace_log<L, AR>(
    trace: &Trace<AR>,
    bus: &Memory<AR>,
    database: &mut Database<AR::PtrVal, AR::Offset>,
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
        }
    }

    Ok(traced_blocks)
}
