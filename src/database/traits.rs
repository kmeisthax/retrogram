//! Database trait

use std::any::Any;
use std::fmt::Debug;

/// This trait represents a type-erased program database.
///
/// The intended use of this type is to use the included `Any` trait to check
/// if a database is of a given type, and then convert it to a `Database<AR>`
/// to work with architecturally-specific functions.
///
/// It is not intended for this trait to be implemented by anything but
/// `Database<AR>`.
pub trait AnyDatabase: Any + Debug {
    /// Upcast the database to `Any` so it can be downcasted into a concrete
    /// type.
    fn as_any(&self) -> &dyn Any;

    /// Upcast the database to `Any` so it can by downcasted into a mutable
    /// reference to a concrete type.
    fn as_mut_any(&mut self) -> &mut dyn Any;
}
