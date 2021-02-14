//! Queue command executors

use crate::analysis::{
    analyze_trace_log, disassemble_block, trace_until_fork, Error, Fork, Reference, ReferenceKind,
    Trace,
};
use crate::arch::{Architecture, CompatibleLiteral};
use crate::database::Database;
use crate::memory::{Memory, Offset, Pointer};
use crate::queue::command::Command;
use crate::queue::context::QueueContext;
use crate::queue::process::process_command;
use crate::queue::response::Response;
use num::Zero;
use std::convert::TryFrom;
use std::sync::mpsc::Sender;

pub fn declare_entry_point<AR>(
    context: &QueueContext<AR>,
    start: Pointer<AR::PtrVal>,
) -> Response<AR>
where
    AR: Architecture,
{
    let mut db_write = context.project_db.write().unwrap();
    let db = db_write
        .get_database_mut::<AR>(&context.program_name)
        .unwrap();

    db.insert_crossreference(Reference::new_entrypoint(start.clone()));
    db.insert_placeholder_label(start.clone(), ReferenceKind::Entrypoint);

    Response::DeclaredEntryPoint(start)
}

/// Run a static disassembly on a particular location in the program.
///
/// All of the facts gleaned from the static disassembly will be inserted into
/// the context's associated database.
///
/// This function returns the length of the analyzed block and any error which
/// may have resulted during disassembly. The length will be `None` in the rare
/// case that the length of the block exceeds the maximum value of `AR::Offset`.
pub fn process_static_scan<L, AR>(
    context: &QueueContext<AR>,
    start: Pointer<AR::PtrVal>,
) -> Response<AR>
where
    AR: Architecture,
    L: CompatibleLiteral<AR>,
    AR::Offset: Offset<AR::PtrVal>, //I shouldn't have to do this.
{
    let (orig_asm, xrefs, pc_offset, blocks, terminating_error) =
        disassemble_block::<L, AR>(start.clone(), &context.bus, context.arch);

    let mut db_write = context.project_db.write().unwrap();
    let db = db_write.get_database_mut(&context.program_name).unwrap();

    for block in blocks {
        if let Some(_block_num) = db.find_block_membership(block.as_start()) {
            continue;
        }

        db.insert_block(block);
    }

    for xref in xrefs {
        if let Some(target) = xref.as_target() {
            if db.pointer_symbol(&target).is_none() {
                db.insert_placeholder_label(target.clone(), xref.kind());
            }

            if let Some(id) = db.find_block_membership(target) {
                let mut xref_offset = AR::Offset::zero();

                if let Some(block) = db.block(id) {
                    xref_offset = AR::Offset::try_from(
                        target.as_pointer().clone() - block.as_start().as_pointer().clone(),
                    )
                    .unwrap_or_else(|_| AR::Offset::zero());
                }

                if xref_offset > AR::Offset::zero() {
                    db.split_block(id, xref_offset);
                }
            }
        }

        db.insert_crossreference(xref);
    }

    //TODO: This seems to be polluting the symbol table for no reason.
    if let Some((_dir, loc)) = orig_asm.iter_directives().next() {
        db.insert_placeholder_label(loc.clone(), ReferenceKind::Unknown);
    }

    Response::StaticScanCode {
        scan_start: start,
        scan_end_offset: pc_offset,
        error: terminating_error,
    }
}

/// Run a static disassembly on a particular location in the program.
///
/// All of the facts gleaned from the static disassembly will be inserted into
/// the context's associated database.
///
/// This function returns the length of the analyzed block and any error which
/// may have resulted during disassembly. The length will be `None` in the rare
/// case that the length of the block exceeds the maximum value of `AR::Offset`.
pub fn process_dynamic_scan<L, AR>(context: &QueueContext<AR>, fork: Fork<AR>) -> Response<AR>
where
    AR: Architecture,
    L: CompatibleLiteral<AR>,
    AR::Offset: Offset<AR::PtrVal>, //I shouldn't have to do this.
    Memory<AR>: Send + Sync,
{
    let (branches, start, pre_state, pre_trace) = fork.into_parts();
    let start_ptr = pre_state.contextualize_pointer(start.clone());

    match trace_until_fork(&start, pre_trace, &context.bus, &pre_state, context.arch) {
        Ok((end, post_trace, post_state, prerequisites)) => {
            let end_ptr = post_state.contextualize_pointer(end.clone());

            let mut db_lock = context.project_db.write().unwrap();
            let db_mut: &mut Database<AR> =
                db_lock.get_database_mut(&context.program_name).unwrap();

            let traced_blocks =
                analyze_trace_log::<L, AR>(&post_trace, &context.bus, db_mut, context.arch);
            if let Err(e) = traced_blocks {
                return Response::DynamicScanCode {
                    scan_start: start_ptr,
                    scan_end: end_ptr,
                    error: Some(e),
                };
            }

            db_mut.insert_trace_counts(traced_blocks.unwrap(), 1);

            drop(db_lock);

            let mut extra_branch_bits = 0.0;
            for pr in prerequisites.iter() {
                extra_branch_bits += pr.necessary_forks(&post_state, &context.bus) as f64;
            }

            let extra_branches = (2.0_f64).powf(extra_branch_bits);

            if (branches + extra_branches) > (2.0_f64).powf(5.0) {
                //TODO: better heuristic please
                //TODO: this should retrieve the block's total fork score
                //TODO: what happens if we overtrace a block (say a utility fn)
                //      while it's still in the undertraced list?
                return Response::DynamicScanCode {
                    scan_start: start_ptr,
                    scan_end: end_ptr,
                    error: Some(Error::TraceTooDeep {
                        branches,
                        extra_branches,
                    }),
                };
            }

            for result_fork in Fork::make_forks(
                branches,
                end,
                post_state,
                &context.bus,
                Trace::begin_at(end_ptr.clone()),
                prerequisites.into_iter(),
            ) {
                context.spawn(|context| {
                    process_dynamic_scan::<L, AR>(context, result_fork);
                });
            }

            Response::DynamicScanCode {
                scan_start: start_ptr,
                scan_end: end_ptr,
                error: None,
            }
        }
        Err(e) => Response::DynamicScanCode {
            scan_start: start_ptr.clone(),
            scan_end: start_ptr,
            error: Some(e),
        },
    }
}

/// Search the given database for more things to scan, and scan them.
///
/// This function yields the number of commands it added to the database; if it
/// returns 0, then there was no additional work to extract from the database.
/// However, still-executing commands may create additional work. It is
/// recommended that this particular command be fenced off from previously
/// issued work.
pub fn extract_scans_from_database<L, AR>(
    context: &QueueContext<AR>,
    resp_sender: Sender<Response<AR>>,
    is_tracing_allowed: bool,
) -> Response<AR>
where
    AR: Architecture,
    L: CompatibleLiteral<AR>,
    Memory<AR>: Send + Sync,
{
    let db_lock = context.project_db.read().unwrap();
    let db: &Database<AR> = db_lock.get_database(&context.program_name).unwrap();
    let unanalyzed = db.unanalyzed_static_xrefs();
    let mut vcommands = Vec::new();

    for xref_id in unanalyzed {
        let mut target_pc = None;
        let xref = db.xref(xref_id);

        if let Some(xref) = xref {
            target_pc = xref.as_target().cloned();
        }

        if let Some(target_pc) = target_pc {
            if !db.has_target_failed_analysis(&target_pc) {
                vcommands.push(Command::StaticScanCode(target_pc));
            }
        }
    }

    if is_tracing_allowed {
        for block_id in db.undertraced_blocks().iter() {
            let block = db.block(*block_id).unwrap();

            let initial_fork = Fork::initial_fork(
                block.as_start().as_pointer().clone(),
                context.poweron_state.clone(),
            );
            vcommands.push(Command::DynamicScanCode(initial_fork));
        }
    }

    let sent_commands = vcommands.len();

    for command in vcommands {
        let my_resp_sender = resp_sender.clone();
        context.spawn(|context| {
            process_command::<L, AR>(context, command, my_resp_sender);
        });
    }

    Response::ExtractScanCount(is_tracing_allowed, sent_commands)
}
