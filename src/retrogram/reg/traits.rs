//! Traits used by reg types

use std::ops::{Sub, Not, BitAnd, BitOr, BitXor, Shl, Shr};
use std::cmp::{min, max, PartialEq, PartialOrd};
use std::fmt::Debug;
use num::traits::{Zero, One, Bounded};
use crate::retrogram::reg;
use crate::retrogram::maths::BoundWidth;

/// Rust doesn't let us expose the child type's From impl as another From impl
/// because there's no way to convey a blanket implementation that doesn't
/// conflict with the blanket self-from impl in core. So instead we define a new
/// trait which specifically means "convert the symbolic valid to another type",
/// and our existing From impl means "wrap a concrete value".
pub trait Convertable<R> {
    fn convert_from(v: reg::Symbolic<R>) -> Self;
}

/// Guard trait for converting a symbolic value back into a concrete one.
/// 
/// In plain English: In order to convert a symbolic value back into a concrete
/// one, we need to be able to:
/// 
///  * Check for value equality. (Partial equivalence is fine.)
///  * Perform bitwise operations (bitwise AND, OR, NOT, XOR, and shifts) on the
///    type.
///  * Get the type's zero, one, minimum, maximum, and bit-width values.
///  * Convert the results of bitwise operations back into the type.
/// 
/// Furthermore, while this is not expressed in the trait bounds, it is implied
/// that it is possible to generate an "all bits set" value by way of inverting
/// Zero. If your type satisfies Concretizable but does not obey this expected
/// behavior then using it as a symbolic type may cause errors.
pub trait Concretizable: Clone + Bounded + BoundWidth + PartialEq + BitAnd + BitOr + BitXor + Not + Shl + Shr + Zero +
    One + From<<Self as BitAnd>::Output> + From<<Self as BitOr>::Output> + From<<Self as BitXor>::Output> +
    From<<Self as Not>::Output> + From<<Self as Shl>::Output> + From<<Self as Shr>::Output> + Debug {

}

impl<T> Concretizable for T where T: Clone + Bounded + BoundWidth + PartialEq + BitAnd + BitOr + BitXor + Not + Shl +
    Shr + Zero + One + From<<Self as BitAnd>::Output> + From<<Self as BitOr>::Output> + From<<Self as BitXor>::Output> +
    From<<Self as Not>::Output> + From<<Self as Shl>::Output> + From<<Self as Shr>::Output> + Debug {

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
pub trait Bitwise: Clone + Zero + One + BoundWidth<usize> + Shl<usize, Output=Self> + BitAnd<Output=Self> +
    BitXor<Output=Self> + BitOr<Output=Self> + Not<Output=Self> {

}

impl<T> Bitwise for T where T: Clone + Zero + One + BoundWidth<usize> + Shl<usize, Output=Self> + BitAnd<Output=Self> +
    BitXor<Output=Self> + BitOr<Output=Self> + Not<Output=Self> {

}