//! Thread loops for processing queue

use crate::arch::{Architecture, CompatibleLiteral};
use crate::database::ProjectDatabase;
use crate::memory::Memory;
use crate::queue::actions::{
    extract_scans_from_database, process_dynamic_scan, process_static_scan,
};
use crate::queue::command::Command;
use crate::queue::context::QueueContext;
use crate::queue::response::Response;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::{Arc, RwLock};
use std::thread;

/// Spawn the given command into the context's Rayon scope.
///
/// This function returns `false` if it encounters a command fence, without
/// spawning any tasks. You should end the current scope and send the fence
/// response outside the scope.
pub fn process_command<L, AR>(
    context: &mut QueueContext<AR>,
    cmd: Command<AR>,
    resp_sender: Sender<Response<AR>>,
) -> bool
where
    AR: Architecture,
    L: CompatibleLiteral<AR>,
    Memory<AR>: Send + Sync,
{
    match cmd {
        Command::StaticScanCode(start) => {
            let my_sender = resp_sender;
            context.spawn(move |my_context| {
                my_sender
                    .send(process_static_scan::<L, AR>(&my_context, start))
                    .unwrap();
            });
            true
        }
        Command::SetPowerOnState(new_state) => {
            context.poweron_state = new_state;
            resp_sender.send(Response::PowerOnStateSet).unwrap();
            true
        }
        Command::DynamicScanCode(fork) => {
            let my_sender = resp_sender;
            context.spawn(move |my_context| {
                my_sender
                    .send(process_dynamic_scan::<L, AR>(&my_context, fork))
                    .unwrap();
            });
            true
        }
        Command::ExtractAllScans(is_tracing_allowed) => {
            let my_sender = resp_sender;
            context.spawn(move |my_context| {
                my_sender
                    .send(extract_scans_from_database::<L, AR>(
                        &my_context,
                        my_sender.clone(),
                        is_tracing_allowed,
                    ))
                    .unwrap();
            });
            true
        }
        Command::Fence => false,
    }
}

/// Start a separate analysis thread that performs analysis and returns
/// results of those analyses.
///
/// This function returns an MPSC channel sender that can be used for queueing
/// tasks for analysis, and a response reciever that responses will come back
/// from.
pub fn start_analysis_queue<L, AR>(
    arch: AR,
    project_db: Arc<RwLock<ProjectDatabase>>,
    program_name: String,
    bus: Arc<Memory<AR>>,
) -> (Sender<Command<AR>>, Receiver<Response<AR>>)
where
    AR: Architecture,
    L: CompatibleLiteral<AR>,
    Memory<AR>: Send + Sync,
{
    let (cmd_sender, mut cmd_recv) = channel();
    let (resp_sender, resp_recv) = channel();

    thread::spawn(move || {
        rayon::scope(move |s| {
            let mut emit_fence = false;
            let mut context = QueueContext {
                arch,
                project_db,
                program_name,
                bus,
                poweron_state: Default::default(),
                scope: s,
            };

            loop {
                let mut my_resp_sender = resp_sender.clone();

                // Process as many commands as possible before the next fence.
                // We use Rayon scopes as our fencing mechanism, which requires
                // some juggling between the inner scope and outer loop.
                let unmove = context.scope(|context| {
                    while let Ok(cmd) = cmd_recv.recv() {
                        if !process_command::<L, AR>(context, cmd, my_resp_sender.clone()) {
                            emit_fence = true;
                            break;
                        }
                    }

                    (cmd_recv, my_resp_sender)
                });

                cmd_recv = unmove.0;
                my_resp_sender = unmove.1;

                if emit_fence {
                    my_resp_sender.send(Response::Fence).unwrap();
                    emit_fence = false;
                } else {
                    // Can happen only if the scope terminated due to a
                    // `PoisonError`.
                    break;
                }
            }
        });
    });

    (cmd_sender, resp_recv)
}
