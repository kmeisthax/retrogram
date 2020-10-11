//! Project-wide database

use crate::arch::Architecture;
use crate::database::Database;
use serde::de::{MapAccess, Visitor};
use serde::ser::SerializeMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::{fmt, fs, io};

#[derive(Debug)]
pub struct ProjectDatabase<AR>
where
    AR: Architecture,
{
    databases: HashMap<String, Database<AR>>,
}

impl<AR> Default for ProjectDatabase<AR>
where
    AR: Architecture,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<AR> ProjectDatabase<AR>
where
    AR: Architecture,
{
    pub fn read(filename: &str) -> io::Result<Self> {
        let db_file = fs::File::open(filename)?;
        let dbs = serde_json::from_reader(db_file)?;

        Ok(dbs)
    }

    pub fn write(&self, filename: &str) -> io::Result<()> {
        let db_file = fs::File::create(filename)?;
        serde_json::to_writer_pretty(db_file, self).map_err(|e| {
            io::Error::new(
                io::ErrorKind::Other,
                format!("Encoding database failed with error: {}", e),
            )
        })
    }

    pub fn new() -> Self {
        ProjectDatabase {
            databases: HashMap::new(),
        }
    }

    pub fn get_database(&self, db_name: &str) -> Option<&Database<AR>> {
        self.databases.get(db_name)
    }

    pub fn get_database_mut(&mut self, db_name: &str) -> &mut Database<AR> {
        if !self.databases.contains_key(db_name) {
            self.databases.insert(db_name.to_string(), Database::new());
        }

        self.databases
            .get_mut(db_name)
            .expect("I just inserted it, it should be there.")
    }
}

impl<AR> Serialize for ProjectDatabase<AR>
where
    AR: Architecture,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;

        map.serialize_entry("databases", &self.databases)?;

        map.end()
    }
}

impl<'dw, AR> Deserialize<'dw> for ProjectDatabase<AR>
where
    AR: Architecture,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'dw>,
    {
        pub struct PDBVisitor<AR>(PhantomData<AR>);

        impl<'dw, AR> Visitor<'dw> for PDBVisitor<AR>
        where
            AR: Architecture,
        {
            type Value = ProjectDatabase<AR>;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(formatter, "a valid project database")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'dw>,
            {
                let mut pjdb = ProjectDatabase::new();

                while let Some((key, value)) =
                    map.next_entry::<String, HashMap<String, Database<AR>>>()?
                {
                    if key == "databases" {
                        pjdb.databases = value;
                    }
                }

                Ok(pjdb)
            }
        }

        Ok(deserializer.deserialize_map(PDBVisitor::<AR>(PhantomData))?)
    }
}
