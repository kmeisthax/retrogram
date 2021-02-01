//! Entire-session context

use crate::analysis::Response;
use crate::database::ProjectDatabase;
use crate::platform::Platform;
use crate::project::{Program, Project};
use crate::tui::context::program::{AnyProgramContext, ProgramContext};
use cursive::event::Event;
use cursive::CbSink;
use owning_ref::{RwLockReadGuardRef, RwLockWriteGuardRefMut};
use relative_path::{RelativePath, RelativePathBuf};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::path::Path;
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
    pub fn from_project(project: Project) -> io::Result<Self> {
        let programs = project
            .iter_programs()
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect::<Vec<(String, Program)>>();

        let mut session = Self {
            project: Arc::new(RwLock::new(project)),
            databases: HashMap::new(),
            contexts: HashMap::new(),
            tab_nonce: 0,
        };

        for (_name, program) in programs.iter() {
            session.add_database_to_project(program.as_database_path())?;
        }

        Ok(session)
    }

    /// Access the session's underlying project.
    pub fn project(&self) -> impl '_ + Deref<Target = Project> {
        self.project.read().unwrap()
    }

    /// Access the session's underlying project for mutation.
    pub fn project_mut(&mut self) -> impl '_ + DerefMut<Target = Project> {
        self.project.write().unwrap()
    }

    /// Add a database to the project (if it is not already opened).
    pub fn add_database_to_project(&mut self, db_path: &RelativePath) -> io::Result<()> {
        if !self.databases.contains_key(db_path) {
            let mut project = self.project.write().unwrap();

            let pjdb: io::Result<ProjectDatabase> = if let Some(base_path) = project.path() {
                //On-disk project, read database relative to the project
                let database_path = db_path.to_path(base_path);
                ProjectDatabase::read(&mut project, &mut fs::File::open(database_path)?)
                    .map_err(|e| e.into())
            } else {
                //In-memory project, store database in memory
                Ok(ProjectDatabase::new())
            };

            let pjdb = Arc::new(RwLock::new(match pjdb {
                Ok(pjdb) => pjdb,
                Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
                    eprintln!("Creating new database for project");
                    ProjectDatabase::new()
                }
                Err(e) => return Err(e),
            }));

            self.databases.insert(db_path.to_relative_path_buf(), pjdb);
        }

        Ok(())
    }

    /// Install a program into the current project.
    pub fn add_program_to_project(&mut self, program: Program) -> io::Result<()> {
        self.add_database_to_project(program.as_database_path())?;
        self.project.write().unwrap().add_program(program);

        Ok(())
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
                            siv.on_event(Event::Refresh);
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
