//! Queue context type

use crate::arch::Architecture;
use crate::database::ProjectDatabase;
use crate::memory::Memory;
use crate::reg::State;
use rayon::Scope;
use std::sync::{Arc, RwLock};

/// The context for a given queue.
///
/// A queue cannot be started without a queue context, and each queue context
/// must refer to a single program. Each program has a separate queue.
#[derive(Clone)]
pub struct QueueContext<'a, 'scope, AR>
where
    AR: Architecture,
{
    /// The architecture to disassemble with.
    pub arch: AR,

    /// A mutable shared reference to the current analysis database.
    pub project_db: Arc<RwLock<ProjectDatabase>>,

    /// The name of the program this queue context services.
    pub program_name: String,

    /// An immutable reference to the current memory bus.
    ///
    /// Changes to program contents are not considered during the execution of
    /// Retrogram. Instead, any information derived from the program or changes
    /// intended to be made by the user must live inside the analysis database.
    pub bus: Arc<Memory<AR>>,

    /// The power-on state for any dynamic analysis.
    pub poweron_state: State<AR>,

    /// The Rayon scope for this queue context.
    pub scope: &'a Scope<'scope>,
}

impl<'a, 'scope, AR> QueueContext<'a, 'scope, AR>
where
    AR: Architecture,
{
    /// Spawn a new task into the Rayon scope held by this queue context.
    ///
    /// The given body closure will be provided a copy of this queue context
    /// with a fresh Rayon scope for further task spawning. The context may be
    /// mutated; however, no mutations will propagate up to this context, or to
    /// parent contexts.
    pub fn spawn<BODY>(&self, body: BODY)
    where
        BODY: for<'b> FnOnce(&mut QueueContext<'b, 'scope, AR>) + Send + 'scope,
        Memory<AR>: Send + Sync,
    {
        let arch = self.arch;
        let project_db = self.project_db.clone();
        let program_name = self.program_name.clone();
        let bus = self.bus.clone();
        let poweron_state = self.poweron_state.clone();

        self.scope.spawn(move |s| {
            let mut child_context = QueueContext {
                arch,
                project_db,
                program_name,
                bus,
                poweron_state,
                scope: s,
            };

            body(&mut child_context)
        });
    }

    /// Create a new Rayon scope with the same queue context and run a task
    /// inside it.
    ///
    /// This function blocks until the scope completes, and returns whatever
    /// result the inner function computes.
    ///
    /// The given body closure will be provided a copy of this queue context
    /// with a fresh Rayon scope for further task spawning. The context may be
    /// mutated; changes to the power-on state will propagate back to this
    /// context.
    pub fn scope<'scope2, BODY, R>(&mut self, body: BODY) -> R
    where
        BODY: for<'b> FnOnce(&mut QueueContext<'b, 'scope2, AR>) -> R + Send,
        R: Send,
        Memory<AR>: Send + Sync,
    {
        let arch = self.arch;
        let project_db = self.project_db.clone();
        let program_name = self.program_name.clone();
        let bus = self.bus.clone();
        let poweron_state = self.poweron_state.clone();

        let (result, mutated_poweron_state) = rayon::scope(|s| {
            let mut child_context = QueueContext {
                arch,
                project_db,
                program_name,
                bus,
                poweron_state,
                scope: s,
            };

            let result = body(&mut child_context);

            (result, child_context.poweron_state)
        });

        self.poweron_state = mutated_poweron_state;

        result
    }
}
