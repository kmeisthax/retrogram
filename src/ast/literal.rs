//! AST type for literal values

use crate::memory::Pointer;
use std::fmt::Debug;

/// A literal value that can be accepted by the assembler we're generating an
/// AST for.
///
/// Literal values are allowed to be any clone and debuggable value; however,
/// the intended purpose is for them to be some kind of enumeration that holds
/// multiple types of values. The primary way of constructing them is
/// implementing `From` on the `Literal` type you want to use for whatever
/// values it should accept.
pub trait Literal: Clone + Debug {
    /// The value of pointer literals.
    type PtrVal;

    /// Yields `true` if this literal is a pointer.
    fn is_pointer(&self) -> bool;

    /// If this literal represents a pointer value, then return it's pointer
    /// value with contexts preserved. Otherwise, destroy the literal.
    fn into_pointer(self) -> Option<Pointer<Self::PtrVal>>;
}
