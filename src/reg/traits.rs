//! Traits used by reg types

use crate::maths::BoundWidth;
use crate::reg;
use num::traits::{CheckedShl, CheckedShr, Zero};
use std::ops::{BitAnd, BitOr, BitXor, Not};

/// Rust doesn't let us expose the child type's From impl as another From impl
/// because there's no way to convey a blanket implementation that doesn't
/// conflict with the blanket self-from impl in core. So instead we define a new
/// trait which specifically means "convert the symbolic valid to another type",
/// and our existing From impl means "wrap a concrete value".
pub trait Convertable<R> {
    fn convert_from(v: reg::Symbolic<R>) -> Self;
}

/// Rust doesn't let us expose the child type's From impl as another From impl
/// because there's no way to convey a blanket implementation that doesn't
/// conflict with the blanket self-from impl in core. So instead we define a new
/// trait which specifically means "convert the symbolic valid to another type",
/// and our existing From impl means "wrap a concrete value".
pub trait TryConvertable<R>: Sized {
    type Error;

    fn try_convert_from(v: reg::Symbolic<R>) -> Result<Self, Self::Error>;
}

/// Guard trait for bitwise operations on a type.
///
/// Bitwise also requires the ability to shift by a `u32` count for programmer
/// convenience.
pub trait Bitwise:
    Clone
    + Zero
    + BoundWidth<u32>
    + CheckedShl
    + CheckedShr
    + BitAnd<Output = Self>
    + BitXor<Output = Self>
    + BitOr<Output = Self>
    + Not<Output = Self>
{
}

impl<T> Bitwise for T where
    T: Clone
        + Zero
        + BoundWidth<u32>
        + CheckedShl
        + CheckedShr
        + BitAnd<Output = Self>
        + BitXor<Output = Self>
        + BitOr<Output = Self>
        + Not<Output = Self>
{
}
