//! Text UI for interactive use

use crate::analysis::{start_analysis_queue, Command, Response};
use crate::arch::{Architecture, CompatibleLiteral};
use crate::asm::Assembler;
use crate::database::ProjectDatabase;
use crate::memory::Memory;
use crate::project::Program;
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

    /// The program configuration (as read in by retrogram.json).
    program: Program,

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
        program: Program,
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
            program.as_name().unwrap_or("").to_string(),
            bus.clone(),
        );

        (
            Self {
                arch,
                program,
                database,
                bus,
                command_sender,
            },
            response_reciever,
        )
    }

    /// Get the program configuration in this context.
    pub fn program(&self) -> &Program {
        &self.program
    }

    pub fn program_name(&self) -> &str {
        self.program.as_name().unwrap_or("")
    }

    /// Get the memory model for this program context.
    pub fn bus(&self) -> &Memory<AR> {
        &self.bus
    }

    /// Get the project database for this program context.
    pub fn project_database(&self) -> Arc<RwLock<ProjectDatabase>> {
        self.database.clone()
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
pub trait AnyProgramContext: Any {
    /// Upcast the database to `Any` so it can be downcasted into a concrete
    /// type.
    fn as_any(&self) -> &dyn Any;

    /// Upcast the database to `Any` so it can by downcasted into a mutable
    /// reference to a concrete type.
    fn as_mut_any(&mut self) -> &mut dyn Any;
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
}
