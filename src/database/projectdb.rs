//! Project-wide database

use crate::arch::Architecture;
use crate::database::{AnyDatabase, Database, Error, Result};
use crate::project::Project;
use serde_json::{json, Value};
use std::collections::HashMap;
use std::io;

/// A project's `retrogram.db` file.
#[derive(Debug)]
pub struct ProjectDatabase {
    databases: HashMap<String, Box<dyn AnyDatabase>>,
}

impl Default for ProjectDatabase {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert a JSON representation of a program database into the actual
/// `Database` structure.
///
/// At this point, the database's associated architecture must be known.
///
/// This function returns a boxed `AnyDatabase` to allow storage alongside
/// databases of different architectures. You must use the included `Any` trait
/// and an associated project file to recover access to the concrete database
/// type and do architecture-specific things with it.
fn finish_parsing_db<AR>(_arch: AR, value: Value) -> Result<Box<dyn AnyDatabase>>
where
    AR: Architecture + 'static,
{
    Ok(Box::new(serde_json::from_value::<Database<AR>>(value)?))
}

impl ProjectDatabase {
    /// Deserialize a project database from a file.
    ///
    /// The given `Project` is necessary as it informs the database which type
    /// of architecture each program runs in. This allows mixed-architecture
    /// databases where multiple programs for different architectures can be
    /// stored in the same project.
    pub fn read<F>(project: &mut Project, db_file: &mut F) -> Result<Self>
    where
        F: io::Read,
    {
        let pjdb_json: Value = serde_json::from_reader(db_file)?;
        let mut dbs = Self::new();

        match pjdb_json {
            Value::Object(map) => {
                for (k, v) in map.iter() {
                    match (k.as_str(), v) {
                        ("databases", Value::Object(dbs_json)) => {
                            for (prog_name, data) in dbs_json.iter() {
                                let program = project
                                    .program(prog_name)
                                    .ok_or_else(|| Error::UnknownProgramError(prog_name.clone()))?;
                                with_prog_architecture!(program, |_plat, arch, _asm| {
                                    dbs.databases.insert(
                                        prog_name.clone(),
                                        finish_parsing_db(arch, data.clone())?,
                                    );

                                    Ok(())
                                })?;
                            }
                        }
                        _ => return Err(Error::SemanticError),
                    }
                }

                if !map.contains_key("databases") {
                    return Err(Error::SemanticError);
                }
            }
            _ => return Err(Error::SemanticError),
        }

        Ok(dbs)
    }

    /// Serialize a project database to a file.
    pub fn write<W>(&mut self, file: &mut W) -> Result<()>
    where
        W: io::Write,
    {
        let mut db_json = json!({
            "databases": {}
        });

        for (name, anydb) in self.databases.iter() {
            with_db_architecture!(anydb, |db, _arch| {
                db_json["databases"]
                    .as_object_mut()
                    .unwrap()
                    .insert(name.to_string(), serde_json::to_value(db)?);

                Ok(())
            })?;
        }

        serde_json::to_writer_pretty(file, &db_json)?;

        Ok(())
    }

    pub fn new() -> Self {
        ProjectDatabase {
            databases: HashMap::new(),
        }
    }

    /// Attempt to retrieve a concrete database with a given architecture.
    ///
    /// This function returns `None` if the database does not exist or it is
    /// not of the given architecture.
    pub fn get_database<AR>(&self, db_name: &str) -> Option<&Database<AR>>
    where
        AR: Architecture + 'static,
    {
        self.databases
            .get(db_name)?
            .as_any()
            .downcast_ref::<Database<AR>>()
    }

    /// Attempt to retrieve a mutable reference to a concrete database with a
    /// given architecture. If it does not exist, an empty database of the
    /// given type will be created.
    ///
    /// This function returns `None` if the database already exists and is not
    /// of the given architecture.
    pub fn get_database_mut<AR>(&mut self, db_name: &str) -> Option<&mut Database<AR>>
    where
        AR: Architecture + 'static,
    {
        if !self.databases.contains_key(db_name) {
            self.databases
                .insert(db_name.to_string(), Box::new(Database::<AR>::new()));
        }

        self.databases
            .get_mut(db_name)?
            .as_mut_any()
            .downcast_mut::<Database<AR>>()
    }
}
