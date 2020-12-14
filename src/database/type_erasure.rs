//! Database trait

use crate::arch::{AnyArch, Architecture};
use crate::database::Database;
use std::any::Any;
use std::fmt::Debug;

/// Convert an `AnyDatabase` into a given `Database<AR>`.
pub fn downcast_database<AR>(_arch: AR, db: &dyn AnyDatabase) -> Option<&Database<AR>>
where
    AR: Architecture,
{
    db.as_any().downcast_ref::<Database<AR>>()
}

/// This trait represents a type-erased program database.
///
/// The intended use of this type is to use the included `Any` trait to check
/// if a database is of a given type, and then convert it to a `Database<AR>`
/// to work with architecturally-specific functions.
///
/// It is not intended for this trait to be implemented by anything but
/// `Database<AR>`.
pub trait AnyDatabase: Any + Debug + Send + Sync + AnyArch {
    /// Upcast the database to `Any` so it can be downcasted into a concrete
    /// type.
    fn as_any(&self) -> &dyn Any;

    /// Upcast the database to `Any` so it can by downcasted into a mutable
    /// reference to a concrete type.
    fn as_mut_any(&mut self) -> &mut dyn Any;
}

/// Execute a callback with the architecture for a given database.
///
/// It is expected that `$db` is a boxed `AnyDatabase`. You must also expand
/// this macro in a context that is currently using the `Any` trait.
///
/// Your callback will be given a mutable reference to a "concrete",
/// type-bearing version of the database.
///
/// This yields an IO error if the database type could not be determined.
macro_rules! with_db_architecture {
    ($db:ident, |$concrete_db: ident, $arch:ident| $callback:block) => {
        with_architecture!($db, |$arch| {
            if let Some($concrete_db) = crate::database::downcast_database($arch, $db.as_ref()) {
                $callback
            } else {
                Err(::std::io::Error::new(
                    ::std::io::ErrorKind::Other,
                    "Unsupported architecture for database.",
                ))
            }
        })
    };
}
