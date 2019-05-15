//! Project file structures

use std::{io, fs};
use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use argparse;
use serde_json;
use crate::retrogram::analysis;
use crate::retrogram::platform::PlatformName;
use crate::retrogram::arch::ArchName;
use crate::retrogram::asm::AssemblerName;

#[derive(Serialize, Deserialize, Debug)]
pub struct Program {
    platform: Option<PlatformName>,
    arch: Option<ArchName>,
    assembler: Option<AssemblerName>,
    images: Vec<String>,

    #[serde(skip)]
    name: Option<String>,

    #[serde(default)]
    symbol_files: Vec<String>,

    #[serde(default="default_db_filename")]
    database_path: String
}

fn default_db_filename() -> String {
    "retrogram.db".to_string()
}

impl Default for Program {
    fn default() -> Self {
        Program {
            platform: None,
            arch: None,
            assembler: None,
            images: Vec::new(),
            name: None,
            symbol_files: Vec::new(),
            database_path: default_db_filename()
        }
    }
}

impl Program {
    pub fn refer_args<'a, 'b>(&'a mut self, ap: &'b mut argparse::ArgumentParser<'a>) {
        ap.refer(&mut self.images).add_option(&["--image"], argparse::Collect, "The program image file(s) to analyze.");
        ap.refer(&mut self.symbol_files).add_option(&["--symbol_file"], argparse::Collect, "Symbol files, if any, to look up symbols from.");
        ap.refer(&mut self.platform).add_option(&["--platform"], argparse::StoreOption, "What platform to expect");
        ap.refer(&mut self.arch).add_option(&["--arch"], argparse::StoreOption, "What architecture to expect");
        ap.refer(&mut self.assembler).add_option(&["--asm"], argparse::StoreOption, "What assembler to output");
    }

    pub fn platform(&self) -> Option<PlatformName> {
        self.platform
    }

    pub fn arch(&self) -> Option<ArchName> {
        self.arch
    }

    pub fn assembler(&self) -> Option<AssemblerName> {
        self.assembler
    }

    /// List all the image files related to a given program.
    /// 
    /// TODO: This should return &str, not &String, no?
    pub fn iter_images<'a>(&'a self) -> impl Iterator<Item = &String> + 'a {
        self.images.iter()
    }

    /// List all the symbol files related to a given program.
    /// 
    /// TODO: This should return &str, not &String, no?
    pub fn iter_symbol_files<'a>(&'a self) -> impl Iterator<Item = &String> + 'a {
        self.symbol_files.iter()
    }

    pub fn as_database_path(&self) -> &str {
        &self.database_path
    }

    pub fn as_name(&self) -> Option<&str> {
        match &self.name {
            Some(name) => Some(&name),
            None => None
        }
    }

    pub fn set_name(&mut self, name: &str) {
        self.name = Some(name.to_string());
    }

    pub fn apply_override(&self, other: &Program) -> Program {
        Program {
            platform: other.platform.or(self.platform),
            arch: other.arch.or(self.arch),
            assembler: other.assembler.or(self.assembler),
            name: other.name.clone().or(self.name.clone()),
            images: match other.images.len() {
                0 => self.images.clone(),
                _ => other.images.clone()
            },
            symbol_files: match other.symbol_files.len() {
                0 => self.symbol_files.clone(),
                _ => other.symbol_files.clone()
            },
            database_path: default_db_filename()
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Project {
    programs: HashMap<String, Program>
}

impl Project {
    pub fn read(filename: &str) -> io::Result<Self> {
        let project_file = fs::File::open(filename)?;
        let project = serde_json::from_reader(project_file)?;

        Ok(project)
    }

    /// Get the program with the given name within the project.
    pub fn program(&mut self, name: &str) -> Option<&Program> {
        let mut prog = self.programs.get_mut(name);

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
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ProjectDatabase<P, S> where P: analysis::Mappable {
    databases: HashMap<String, analysis::Database<P, S>>
}

impl<P, S> ProjectDatabase<P, S> where for <'dw> P: analysis::Mappable + Deserialize<'dw>, for <'dw> S: Deserialize<'dw> {
    pub fn read(filename: &str) -> io::Result<Self> {
        let db_file = fs::File::open(filename)?;
        let dbs = serde_json::from_reader(db_file)?;

        Ok(dbs)
    }
}

impl<P, S> ProjectDatabase<P, S> where P: analysis::Mappable + Serialize, S: Serialize {
    pub fn write(&self, filename: &str) -> io::Result<()> {
        let db_file = fs::File::create(filename)?;
        serde_json::to_writer_pretty(db_file, self).map_err(|e| io::Error::new(io::ErrorKind::Other, format!("Encoding database failed with error: {}", e)))
    }
}

impl<P, S> ProjectDatabase<P, S> where P: analysis::Mappable {
    pub fn new() -> Self {
        ProjectDatabase {
            databases: HashMap::new()
        }
    }

    pub fn get_database(&self, db_name: &str) -> Option<&analysis::Database<P, S>> {
        self.databases.get(db_name)
    }

    pub fn get_database_mut(&mut self, db_name: &str) -> &mut analysis::Database<P, S> {
        if !self.databases.contains_key(db_name) {
            self.databases.insert(db_name.to_string(), analysis::Database::new());
        }

        self.databases.get_mut(db_name).expect("I just inserted it, it should be there.")
    }
}