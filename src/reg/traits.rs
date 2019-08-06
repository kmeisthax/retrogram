//! Traits used by reg types

use std::ops::{Not, BitAnd, BitOr, BitXor};
use num::traits::{Zero, CheckedShl};
use crate::reg;
use crate::maths::BoundWidth;

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
/// This is similar to `Concretizable` but stipulates the output type of all
/// operations is identical to the implementing type. This is because `Symbolic`
/// cannot make use of `From` conversions (which is why we have `Convertable`).
/// In practice any good numerical type implementation that supports bitwise
/// operators will also return it's own types, so this excludes pathological
/// cases like redefining shifts to mean input and output (looking at you C++).
/// 
/// In short: concrete values can satisfy both traits; symbolic ones only
/// satisfy this one.
/// 
/// Bitwise also requires the ability to shift by a `usize` count for programmer
/// convenience.
pub trait Bitwise: Clone + Zero + BoundWidth<u32> + CheckedShl + BitAnd<Output=Self> + BitXor<Output=Self>
    + BitOr<Output=Self> + Not<Output=Self> {

}

impl<T> Bitwise for T where T: Clone + Zero + BoundWidth<u32> + CheckedShl + BitAnd<Output=Self> + BitXor<Output=Self>
    + BitOr<Output=Self> + Not<Output=Self> {

}