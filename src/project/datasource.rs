//! External data source identifiers

use crate::database::ExternalFormat;
use clap::{App, Arg, ArgMatches, ArgSettings};
use serde::{Deserialize, Serialize};
use std::str::FromStr;

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
