//! Individual program context

use crate::analysis::{start_analysis_queue, Command, Response};
use crate::arch::{AnyArch, ArchName, Architecture, CompatibleLiteral};
use crate::asm::Assembler;
use crate::database::ProjectDatabase;
use crate::input::parse_ptr;
use crate::memory::{Memory, Pointer};
use crate::project::{Program, Project};
use owning_ref::{RwLockWriteGuardRef, RwLockWriteGuardRefMut};
use std::any::Any;
use std::sync::mpsc::{Receiver, Sender};
use std::sync::{Arc, RwLock};

/// A bundle of all of the state that is tied to a single particular program.
#[derive(Clone)]
pub struct ProgramContext<AR>
where
    AR: Architecture,
{
    /// The architecture being analyzed
    arch: AR,

    /// The project this program belongs to.
    project: Arc<RwLock<Project>>,

    /// The ID of the program.
    program: String,

    /// The project database file that contains the mutable analysis state for
    /// this program.
    database: Arc<RwLock<ProjectDatabase>>,

    /// The memory model for analyzing this program.
    bus: Arc<Memory<AR>>,

    /// An active channel to send analysis commands into.
    command_sender: Sender<Command<AR>>,
}

impl<AR> ProgramContext<AR>
where
    AR: Architecture,
{
    /// Construct a program context from a program configuration and it's
    /// associated database file.
    pub fn from_program_and_database<ASM>(
        arch: AR,
        _asm: ASM,
        project: Arc<RwLock<Project>>,
        program: String,
        database: Arc<RwLock<ProjectDatabase>>,
        bus: Arc<Memory<AR>>,
    ) -> (Self, Receiver<Response<AR>>)
    where
        ASM: Assembler,
        ASM::Literal: CompatibleLiteral<AR>,
    {
        let (command_sender, response_reciever) = start_analysis_queue::<ASM::Literal, AR>(
            arch,
            database.clone(),
            program.clone(),
            bus.clone(),
        );

        (
            Self {
                arch,
                project,
                program,
                database,
                bus,
                command_sender,
            },
            response_reciever,
        )
    }

    pub fn program(&mut self) -> RwLockWriteGuardRef<'_, Project, Program> {
        RwLockWriteGuardRefMut::new(self.project.write().unwrap())
            .map(|p| p.program(&self.program).unwrap())
    }

    pub fn program_name(&self) -> &str {
        &self.program
    }

    /// Get the memory model for this program context.
    pub fn bus(&self) -> &Memory<AR> {
        &self.bus
    }

    /// Get the project database for this program context.
    pub fn project_database(&self) -> Arc<RwLock<ProjectDatabase>> {
        self.database.clone()
    }

    /// Get the command sender.
    pub fn command_sender(&mut self) -> &mut Sender<Command<AR>> {
        &mut self.command_sender
    }

    /// Convert a text string into a pointer.
    ///
    /// This is a convenience method for `input::parse_ptr`.
    pub fn parse_ptr(&self, text_str: &str) -> Option<Pointer<AR::PtrVal>> {
        let db_lock = self.database.read().unwrap();
        let db = db_lock.get_database(self.program_name()).unwrap();

        parse_ptr(text_str, db, &self.bus, self.arch)
    }
}

impl<AR> AnyArch for ProgramContext<AR>
where
    AR: Architecture,
{
    fn arch(&self) -> ArchName {
        self.arch.name()
    }
}

/// This trait represents a type-erased program context.
///
/// The intended use of this type is to use the included `Any` trait to check
/// if a context is of a given type, and then convert it to a
/// `ProgramContext<AR>` to work with architecturally-specific functions.
///
/// It is not intended for this trait to be implemented by anything but
/// `ProgramContext<AR>`.
pub trait AnyProgramContext: Any + AnyArch {
    /// Upcast the database to `Any` so it can be downcasted into a concrete
    /// type.
    fn as_any(&self) -> &dyn Any;

    /// Upcast the database to `Any` so it can by downcasted into a mutable
    /// reference to a concrete type.
    fn as_mut_any(&mut self) -> &mut dyn Any;

    /// Grab the program config for this context.
    fn as_program(&mut self) -> RwLockWriteGuardRef<'_, Project, Program>;

    /// Grab the program name for this context.
    fn as_program_name(&self) -> &str;

    /// Copy this context.
    fn duplicate(&self) -> Box<dyn AnyProgramContext>;
}

impl<AR> AnyProgramContext for ProgramContext<AR>
where
    AR: Architecture,
{
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_mut_any(&mut self) -> &mut dyn Any {
        self
    }

    fn as_program(&mut self) -> RwLockWriteGuardRef<'_, Project, Program> {
        self.program()
    }

    fn as_program_name(&self) -> &str {
        self.program_name()
    }

    fn duplicate(&self) -> Box<dyn AnyProgramContext> {
        Box::new(self.clone())
    }
}

/// Convert an `AnyProgramContext` into a given `ProgramContext<AR>`.
pub fn downcast_context<AR>(_arch: AR, db: &dyn AnyProgramContext) -> Option<&ProgramContext<AR>>
where
    AR: Architecture,
{
    db.as_any().downcast_ref::<ProgramContext<AR>>()
}

/// Execute a callback with the architecture for a given program context.
///
/// It is expected that `$ctxt` is a boxed `AnyProgramContext`. You must also
/// expand this macro in a context that is currently using the `Any` trait.
///
/// Your callback will be given a mutable reference to a "concrete",
/// type-bearing version of the context.
///
/// This yields an IO error if the context type could not be determined.
macro_rules! with_context_architecture {
    ($ctxt:ident, |$concrete_ctxt: ident, $arch:ident, $asm:ident| $callback:block) => {{
        let program = $ctxt.as_program();
        let borrow = &program;
        let result = with_prog_architecture!(borrow, |_plat, arch, asm| {
            drop(program);
            if let Some($concrete_ctxt) = crate::tui::context::downcast_context(arch, $ctxt) {
                let $arch = arch;
                let $asm = asm;
                $callback
            } else {
                Err(::std::io::Error::new(
                    ::std::io::ErrorKind::Other,
                    "Unsupported architecture for program context.",
                ))
            }
        });

        result
    }};
}
