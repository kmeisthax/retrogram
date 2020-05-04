//! Project file structures

use crate::arch::ArchName;
use crate::asm::AssemblerName;
use crate::database::ExternalFormat;
use crate::platform::PlatformName;
use crate::{analysis, database};
use clap::{App, Arg, ArgMatches, ArgSettings};
use serde::{Deserialize, Serialize};
use serde_json;
use std::collections::HashMap;
use std::str::FromStr;
use std::{fs, io};

#[derive(Serialize, Deserialize, Debug)]
pub struct Program {
    platform: Option<PlatformName>,
    arch: Option<ArchName>,
    assembler: Option<AssemblerName>,
    images: Vec<String>,

    #[serde(skip)]
    name: Option<String>,

    #[serde(default)]
    data_sources: Vec<String>,

    #[serde(default = "default_db_filename")]
    database_path: String,
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
            data_sources: Vec::new(),
            database_path: default_db_filename(),
        }
    }
}

impl Program {
    pub fn configure_app<'a, 'b>(app: App<'a, 'b>) -> App<'a, 'b> {
        app.arg(
            Arg::with_name("image")
                .long("image")
                .value_name("image.bin")
                .help("The program image file(s) to analyze.")
                .takes_value(true)
                .set(ArgSettings::Global),
        )
        .arg(
            Arg::with_name("platform")
                .long("platform")
                .value_name("PLATFORM")
                .help("What platform to expect.")
                .takes_value(true)
                .set(ArgSettings::Global),
        )
        .arg(
            Arg::with_name("arch")
                .long("arch")
                .value_name("ARCH")
                .help("What architecture to expect.")
                .takes_value(true)
                .set(ArgSettings::Global),
        )
        .arg(
            Arg::with_name("asm")
                .long("asm")
                .value_name("ASM")
                .help("What assembler syntax to output.")
                .takes_value(true)
                .set(ArgSettings::Global),
        )
    }

    /// Construct a Program from clap ArgMatches
    pub fn from_arg_matches(args: &ArgMatches) -> Program {
        Program {
            platform: args
                .value_of("platform")
                .and_then(|s| PlatformName::from_str(s).ok()),
            arch: args
                .value_of("arch")
                .and_then(|s| ArchName::from_str(s).ok()),
            assembler: args
                .value_of("asm")
                .and_then(|s| AssemblerName::from_str(s).ok()),
            images: args
                .values_of("image")
                .map_or(Vec::new(), |v| v.map(|s| s.to_string()).collect()),
            name: None,
            data_sources: Vec::new(),
            database_path: default_db_filename(),
        }
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

    /// List all the external data sources this program pulls data from.
    ///
    /// TODO: This should return &str, not &String, no?
    pub fn iter_sources<'a>(&'a self) -> impl Iterator<Item = &String> + 'a {
        self.data_sources.iter()
    }

    pub fn as_database_path(&self) -> &str {
        &self.database_path
    }

    pub fn as_name(&self) -> Option<&str> {
        match &self.name {
            Some(name) => Some(&name),
            None => None,
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
            name: other.name.clone().or_else(|| self.name.clone()),
            images: match other.images.len() {
                0 => self.images.clone(),
                _ => other.images.clone(),
            },
            data_sources: match other.data_sources.len() {
                0 => self.data_sources.clone(),
                _ => other.data_sources.clone(),
            },
            database_path: default_db_filename(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct DataSource {
    /// The data format to use when importing data from this source.
    format: Option<ExternalFormat>,

    /// The list of files necessary to assemble this data source.
    files: Vec<String>,

    /// This list of programs that this data source can provide data for.
    ///
    /// For anonymous data sources, this field cannot be supplied and the data
    /// source is presumed to be valid for all possible programs.
    programs: Vec<String>,
}

impl Default for DataSource {
    fn default() -> Self {
        DataSource {
            format: None,
            files: Vec::new(),
            programs: Vec::new(),
        }
    }
}

impl DataSource {
    /// Add arguments to fill out an anonymous data source from user input.
    ///
    /// NOTE: This function does not collect the `programs` parameter since it
    /// is only used for error checking when the user requests an import from a
    /// named data source rather than an anonymous one. Anonymous data sources
    /// have no program limitations by design.
    pub fn configure_app<'a, 'b>(app: App<'a, 'b>) -> App<'a, 'b> {
        app.arg(
            Arg::with_name("external_db_format")
                .long("external_db_format")
                .value_name("FORMAT")
                .help("The format of external data to import data from")
                .takes_value(true)
                .set(ArgSettings::Global),
        )
        .arg(
            Arg::with_name("external_db_file")
                .long("external_db_file")
                .value_name("data.db")
                .help("The external data files to import data from")
                .takes_value(true)
                .set(ArgSettings::Global),
        )
    }

    /// Construct a Program from clap ArgMatches
    pub fn from_arg_matches(args: &ArgMatches) -> DataSource {
        DataSource {
            format: args
                .value_of("external_db_format")
                .and_then(|s| ExternalFormat::from_str(s).ok()),
            files: args
                .values_of("external_db_file")
                .map_or(Vec::new(), |v| v.map(|s| s.to_string()).collect()),
            programs: Vec::new(),
        }
    }

    pub fn format(&self) -> Option<ExternalFormat> {
        self.format
    }

    pub fn iter_files<'a>(&'a self) -> impl Iterator<Item = &String> + 'a {
        self.files.iter()
    }

    /// Determine if a given data source can provide data to the database of a
    /// given program.
    ///
    /// Anonymous data sources do not have a list of valid programs and thus are
    /// valid for all possible programs.
    pub fn is_prog_valid(&self, program_name: &str) -> bool {
        if self.programs.is_empty() {
            return true;
        }

        for program in self.programs.iter() {
            if program == program_name {
                return true;
            }
        }

        false
    }

    pub fn apply_override(&self, other: &DataSource) -> DataSource {
        DataSource {
            format: other.format.or(self.format),
            files: match other.files.len() {
                0 => self.files.clone(),
                _ => other.files.clone(),
            },
            programs: match other.programs.len() {
                0 => self.programs.clone(),
                _ => other.programs.clone(),
            },
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Project {
    programs: HashMap<String, Program>,

    #[serde(default)]
    data_sources: HashMap<String, DataSource>,
}

impl Project {
    pub fn read(filename: &str) -> io::Result<Self> {
        let project_file = fs::File::open(filename)?;
        let project = serde_json::from_reader(project_file)?;

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
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ProjectDatabase<P, S>
where
    P: analysis::Mappable,
{
    databases: HashMap<String, database::Database<P, S>>,
}

impl<P, S> ProjectDatabase<P, S>
where
    for<'dw> P: analysis::Mappable + Deserialize<'dw>,
    for<'dw> S: Deserialize<'dw>,
{
    pub fn read(filename: &str) -> io::Result<Self> {
        let db_file = fs::File::open(filename)?;
        let dbs = serde_json::from_reader(db_file)?;

        Ok(dbs)
    }
}

impl<P, S> ProjectDatabase<P, S>
where
    P: analysis::Mappable + Serialize,
    S: Serialize,
{
    pub fn write(&self, filename: &str) -> io::Result<()> {
        let db_file = fs::File::create(filename)?;
        serde_json::to_writer_pretty(db_file, self).map_err(|e| {
            io::Error::new(
                io::ErrorKind::Other,
                format!("Encoding database failed with error: {}", e),
            )
        })
    }
}

impl<P, S> Default for ProjectDatabase<P, S>
where
    P: analysis::Mappable,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<P, S> ProjectDatabase<P, S>
where
    P: analysis::Mappable,
{
    pub fn new() -> Self {
        ProjectDatabase {
            databases: HashMap::new(),
        }
    }

    pub fn get_database(&self, db_name: &str) -> Option<&database::Database<P, S>> {
        self.databases.get(db_name)
    }

    pub fn get_database_mut(&mut self, db_name: &str) -> &mut database::Database<P, S> {
        if !self.databases.contains_key(db_name) {
            self.databases
                .insert(db_name.to_string(), database::Database::new());
        }

        self.databases
            .get_mut(db_name)
            .expect("I just inserted it, it should be there.")
    }
}
