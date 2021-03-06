//! Project configuration file representation

use crate::project::datasource::DataSource;
use crate::project::program::Program;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::{env, fs, io};

/// In-memory representation of the current project configuration.
///
/// This file is typically read from a file named `retrogram.json`, and it
/// contains information on all of the programs, data sources, and other config
/// in the project.
#[derive(Serialize, Deserialize, Debug)]
pub struct Project {
    programs: HashMap<String, Program>,

    #[serde(default)]
    data_sources: HashMap<String, DataSource>,

    #[serde(skip)]
    read_from: Option<PathBuf>,
}

impl Default for Project {
    fn default() -> Self {
        Self::new()
    }
}

impl Project {
    /// Create a new, empty project.
    pub fn new() -> Self {
        Self {
            programs: HashMap::new(),
            data_sources: HashMap::new(),
            read_from: None,
        }
    }

    /// Read the project from disk.
    ///
    /// The filename the project was read from will be retained in this copy of
    /// the project.
    pub fn read<P: AsRef<Path>>(filename: P) -> io::Result<Self> {
        let filename = filename.as_ref().to_path_buf();
        let project_file = fs::File::open(&filename)?;
        let mut project: Self = serde_json::from_reader(project_file)?;

        for (name, prog) in project.programs.iter_mut() {
            if prog.as_name().is_none() {
                prog.set_name(name);
            }
        }

        project.read_from = Some(fs::canonicalize(filename)?);

        Ok(project)
    }

    /// Write the project to disk.
    pub fn write<P: AsRef<Path>>(&mut self, filename: P) -> io::Result<()> {
        let filename = filename.as_ref().to_path_buf();
        let project_file = fs::File::create(&filename)?;
        serde_json::to_writer_pretty(project_file, self)?;

        self.read_from = Some(fs::canonicalize(filename)?);

        Ok(())
    }

    /// Get the program with the given name within the project.
    pub fn program(&mut self, name: &str) -> Option<&Program> {
        let prog = self.programs.get_mut(name);

        if let Some(prog) = prog {
            prog.set_name(name);

            return Some(prog);
        }

        None
    }

    /// Add a program to the project.
    pub fn add_program(&mut self, program: Program) {
        let name = program
            .as_name()
            .expect("Cannot add unnamed program to project")
            .to_string();
        self.programs.insert(name, program);
    }

    /// Get the project's default program.
    pub fn default_program(&self) -> Option<(&String, &Program)> {
        self.programs.iter().next()
    }

    /// Get the data source with the given name within the project.
    pub fn data_source(&mut self, name: &str) -> Option<&DataSource> {
        let source = self.data_sources.get_mut(name);

        if let Some(source) = source {
            return Some(source);
        }

        None
    }

    pub fn iter_programs(&self) -> impl Iterator<Item = (&str, &Program)> {
        self.programs.iter().map(|(k, v)| (k.as_str(), v))
    }

    /// Get the location that this project file was last written to.
    ///
    /// The returned path is guaranteed to be canonical and absolute.
    ///
    /// `None` indicates that the project has not yet been written to disk.
    pub fn read_from(&self) -> Option<&Path> {
        self.read_from.as_deref()
    }

    /// Get the directory this project was stored into.
    ///
    /// The returned path is guaranteed to be canonical and absolute.
    ///
    /// `None` indicates that the project has not yet been written to disk.
    pub fn path(&self) -> Option<&Path> {
        self.read_from().and_then(|rf| rf.parent())
    }

    /// Get the directory this project was stored into, or the current
    /// directory otherwise.
    ///
    /// TODO: The returned path is almost-guaranteed to be canonicalized; but
    /// I'm not sure if that covers `env::current_dir`.
    ///
    /// Intended for use in contexts where the current working directory would
    /// be a valid path to use when working with an in-memory project.
    pub fn implicit_path(&self) -> io::Result<Cow<Path>> {
        if let Some(p) = self.path().map(|p| p.into()) {
            Ok(p)
        } else {
            Ok(env::current_dir()?.into())
        }
    }
}
