//! Traits used by reg types

use std::ops::{Sub, Not, BitAnd, BitOr, Shl, Shr};
use std::cmp::{min, max, PartialEq, PartialOrd};
use std::fmt::Debug;
use num::traits::{Zero, Bounded};
use crate::retrogram::reg;

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
///  * Perform bitwise OR and NOT on the type.
///  * Get the type's zero value.
///  * Convert the results of bitwise OR and NOT operations back into the type.
///  * Get the maximum and minimum bound values of a type.
/// 
/// Furthermore, while this is not expressed in the trait bounds, it is implied
/// that it is possible to generate an "all bits set" value by way of inverting
/// Zero. If your type implements Concretizable but does not satisfy this bound
/// then using it as a symbolic type may cause errors.
pub trait Concretizable: Clone + Bounded + PartialEq + BitOr + Not + Zero + From<<Self as BitOr>::Output> + From<<Self as Not>::Output> {

}

impl<T> Concretizable for T where T: Clone + Bounded + PartialEq + BitOr + Not + Zero + From<<Self as BitOr>::Output> + From<<Self as Not>::Output> {

}

/// Guard trait for checking if a concrete value satisfies a symbolic one.
/// 
/// In plain English: In order to validate that a concrete value satisfies a
/// symbolic one, we need to be able to:
/// 
///  * Check for value equality. (Partial equivalence is fine.)
///  * Compare value magnitude against a bound. (Partial order is fine.)
///  * Perform bitwise AND and NOT on the type.
///  * Convert the results of bitwise AND and NOT operations back into the type.
pub trait Validatable: Clone + PartialEq + PartialOrd + Not + BitAnd + From<<Self as Not>::Output> + From<<Self as BitAnd>::Output> {

}

impl<T> Validatable for T where T: Clone + PartialEq + PartialOrd + Not + BitAnd + From<<T as Not>::Output> + From<<T as BitAnd>::Output> {

}