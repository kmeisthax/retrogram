//! Text UI for interactive use

use crate::analysis::{start_analysis_queue, Command, Response};
use crate::arch::{AnyArch, ArchName, Architecture, CompatibleLiteral};
use crate::asm::Assembler;
use crate::database::ProjectDatabase;
use crate::input::parse_ptr;
use crate::memory::{Memory, Pointer};
use crate::platform::Platform;
use crate::project::{Program, Project};
use cursive::CbSink;
use std::any::Any;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::mpsc::{Receiver, Sender};
use std::sync::{Arc, RwLock};
use std::thread::spawn;
use std::{fs, io};

/// An entire Retrogram project/session's saveable data.
///
/// Intended to be stored on a Cursive instance as user data.
pub struct SessionContext {
    /// The current project definition.
    project: Project,

    /// All open databases connected to this project, keyed by database path.
    databases: HashMap<PathBuf, Arc<RwLock<ProjectDatabase>>>,

    /// All currently-open program contexts, keyed by program.
    contexts: HashMap<String, Box<dyn AnyProgramContext>>,

    /// Number of opened tabs.
    tab_nonce: u64,
}

impl SessionContext {
    /// Build an empty project.
    pub fn empty_session() -> Self {
        Self {
            project: Project::new(),
            databases: HashMap::new(),
            contexts: HashMap::new(),
            tab_nonce: 0,
        }
    }

    /// Construct a session context from a project filename.
    pub fn from_filename<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let project = Project::read(path)?;

        Self::from_project(project)
    }

    /// Construct a session context from a project definition.
    pub fn from_project(mut project: Project) -> io::Result<Self> {
        let programs = project
            .iter_programs()
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect::<Vec<(String, Program)>>();
        let mut databases = HashMap::new();

        for (_name, program) in programs.iter() {
            if !databases.contains_key(program.as_database_path()) {
                let pjdb: io::Result<ProjectDatabase> = ProjectDatabase::read(
                    &mut project,
                    &mut fs::File::open(program.as_database_path())?,
                )
                .map_err(|e| e.into());

                let pjdb = Arc::new(RwLock::new(match pjdb {
                    Ok(pjdb) => pjdb,
                    Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
                        eprintln!("Creating new database for project");
                        ProjectDatabase::new()
                    }
                    Err(e) => return Err(e),
                }));

                databases.insert(program.as_database_path().to_path_buf(), pjdb);
            }
        }

        Ok(Self {
            project,
            databases,
            contexts: HashMap::new(),
            tab_nonce: 0,
        })
    }

    /// Get the location that this session's project file was last written to.
    ///
    /// The returned path is guaranteed to be canonical and absolute.
    ///
    /// `None` indicates that the project has not yet been written to disk.
    pub fn read_from(&self) -> Option<&Path> {
        self.project.read_from()
    }

    /// Open a program context to access the mutable state of a program with.
    ///
    /// This spawns a thread for the program context to run analysis on. The
    /// Cursive instance passed to this program will be automatically refreshed
    /// whenever the context's associated analysis queue does something.
    pub fn program_context(
        &mut self,
        cb_sink: CbSink,
        program_name: &str,
    ) -> io::Result<Box<dyn AnyProgramContext>> {
        if self.contexts.contains_key(program_name) {
            return Ok(self.contexts.get(program_name).unwrap().duplicate());
        }

        let program = self.project.program(program_name).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("Program {} does not exist", program_name),
            )
        })?;
        let program_db = self
            .databases
            .get(program.as_database_path())
            .cloned()
            .ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::NotFound,
                    format!(
                        "Unexpected database file {} encountered",
                        program.as_database_path().display()
                    ),
                )
            })?;
        let image = program
            .iter_images()
            .next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
        let mut file = fs::File::open(image)?;
        let prog_borrow = &program;

        with_prog_architecture!(prog_borrow, |plat, arch, asm| {
            let bus = plat.construct_platform(&mut file)?;
            let bus = Arc::new(bus);

            let (ctxt, recv) = ProgramContext::from_program_and_database(
                arch,
                asm,
                program.clone(),
                program_db,
                bus,
            );

            spawn(move || loop {
                match recv.recv() {
                    Ok(Response::Fence) => cb_sink
                        .send(Box::new(|siv| {
                            siv.refresh();
                        }))
                        .unwrap(),

                    // Silently ignore all other responses.
                    Ok(_) => continue,

                    // If the analysis loop has exited or paniced for whatever
                    // reason, just silently close off our callback handler
                    // thread.
                    Err(_) => return,
                }
            });

            self.contexts
                .insert(program_name.to_string(), Box::new(ctxt));

            Ok(())
        })?;

        Ok(self.contexts.get(program_name).unwrap().duplicate())
    }

    pub fn iter_programs(&self) -> impl Iterator<Item = (&str, &Program)> {
        self.project.iter_programs()
    }

    /// Get the next nonce in sequence.
    pub fn nonce(&mut self) -> u64 {
        let ret = self.tab_nonce;

        self.tab_nonce += 1;

        ret
    }
}

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
    fn as_program(&self) -> &Program;

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

    fn as_program(&self) -> &Program {
        &self.program
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
        with_prog_architecture!(program, |_plat, arch, asm| {
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
        })
    }};
}
