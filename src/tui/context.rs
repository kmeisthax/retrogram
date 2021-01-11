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
use owning_ref::{RwLockReadGuardRef, RwLockWriteGuardRef, RwLockWriteGuardRefMut};
use relative_path::{RelativePath, RelativePathBuf};
use std::any::Any;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::path::Path;
use std::sync::mpsc::{Receiver, Sender};
use std::sync::{Arc, RwLock};
use std::thread::spawn;
use std::{fs, io};

/// An entire Retrogram project/session's saveable data.
///
/// Intended to be stored on a Cursive instance as user data.
pub struct SessionContext {
    /// The current project definition.
    project: Arc<RwLock<Project>>,

    /// All open databases connected to this project, keyed by database path.
    ///
    /// The database path is relative to the intended save location of the
    /// project.
    databases: HashMap<RelativePathBuf, Arc<RwLock<ProjectDatabase>>>,

    /// All currently-open program contexts, keyed by program.
    contexts: HashMap<String, Box<dyn AnyProgramContext>>,

    /// Number of opened tabs.
    tab_nonce: u64,
}

impl SessionContext {
    /// Build an empty project.
    pub fn empty_session() -> Self {
        Self {
            project: Arc::new(RwLock::new(Project::new())),
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
                let base_path = project.implicit_path()?;
                let database_path = program.as_database_path().to_path(base_path);
                let pjdb: io::Result<ProjectDatabase> =
                    ProjectDatabase::read(&mut project, &mut fs::File::open(database_path)?)
                        .map_err(|e| e.into());

                let pjdb = Arc::new(RwLock::new(match pjdb {
                    Ok(pjdb) => pjdb,
                    Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
                        eprintln!("Creating new database for project");
                        ProjectDatabase::new()
                    }
                    Err(e) => return Err(e),
                }));

                databases.insert(program.as_database_path().to_relative_path_buf(), pjdb);
            }
        }

        Ok(Self {
            project: Arc::new(RwLock::new(project)),
            databases,
            contexts: HashMap::new(),
            tab_nonce: 0,
        })
    }

    /// Access the session's underlying project.
    pub fn project(&self) -> impl '_ + Deref<Target = Project> {
        self.project.read().unwrap()
    }

    /// Access the session's underlying project for mutation.
    pub fn project_mut(&mut self) -> impl '_ + DerefMut<Target = Project> {
        self.project.write().unwrap()
    }

    /// Get the location that this session saves and loads relative paths to
    /// and from.
    ///
    /// `None` indicates that the session has not yet been written to disk.
    pub fn path(&self) -> Option<impl '_ + Borrow<Path>> {
        RwLockReadGuardRef::new(self.project.read().unwrap())
            .try_map(|p| p.path().ok_or(()))
            .ok()
    }

    pub fn program(&mut self, name: &str) -> Option<impl '_ + Borrow<Program>> {
        RwLockWriteGuardRefMut::new(self.project.write().unwrap())
            .try_map(|p| p.program(name).ok_or(()))
            .ok()
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

        let program = self.program(program_name).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("Program {} does not exist", program_name),
            )
        })?;
        let db_path = program.borrow().as_database_path().to_relative_path_buf();
        drop(program);

        let program_db = self.databases.get(&db_path).cloned().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("Unexpected database file {} encountered", db_path),
            )
        })?;

        let child_project_ref = self.project.clone();
        let program = self.program(program_name).unwrap();
        let image = program
            .borrow()
            .iter_images()
            .next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
        let mut file = fs::File::open(image)?;
        let prog_borrow = &program.borrow();

        let new_context = with_prog_architecture!(prog_borrow, |plat, arch, asm| {
            let bus = plat.construct_platform(&mut file)?;
            let bus = Arc::new(bus);

            let (ctxt, recv) = ProgramContext::from_program_and_database(
                arch,
                asm,
                child_project_ref,
                prog_borrow.as_name().unwrap_or("").to_string(),
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

            let anyctxt: Box<dyn AnyProgramContext> = Box::new(ctxt);

            Ok(anyctxt)
        })?;

        drop(program);

        self.contexts.insert(program_name.to_string(), new_context);

        Ok(self.contexts.get(program_name).unwrap().duplicate())
    }

    pub fn iter_databases(
        &self,
    ) -> impl Iterator<Item = (&RelativePath, &Arc<RwLock<ProjectDatabase>>)> {
        self.databases.iter().map(|(p, db)| (p.as_ref(), db))
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
