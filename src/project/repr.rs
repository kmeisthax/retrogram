//! Project configuration file representation

use crate::project::datasource::DataSource;
use crate::project::program::Program;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::{fs, io};

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
}

impl Project {
    pub fn read(filename: &str) -> io::Result<Self> {
        let project_file = fs::File::open(filename)?;
        let mut project : Self = serde_json::from_reader(project_file)?;

        for (name, prog) in project.programs.iter_mut() {
            if prog.as_name().is_none() {
                prog.set_name(name);
            }
        }

        Ok(project)
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
}
