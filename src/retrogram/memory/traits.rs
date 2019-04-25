//! Helper traits for defining where clauses without getting an RSI

use std::ops::{Add, Sub, Not, BitOr, BitAnd, Shl};
use num::traits::{Bounded, Zero, One};
use crate::retrogram::mynums::{BoundWidth, CheckedSub};

/// Trait which represents all operations expected of a pointer value.
/// 
/// In plain english: A pointer value must be orderable and have an offset type.
/// Ordering has the semantics that the greater pointer "comes after" the former
/// in a memory layout. It must be possible to subtract two pointers to obtain
/// the offset type, and whatever you get by adding a pointer and an offset must
/// be convertable into a pointer.
pub trait PtrNum<S> : Clone + PartialOrd + Add<S> + Sub + CheckedSub + From<<Self as Add<S>>::Output>
    where S: Offset<Self> {

}

impl <T, S> PtrNum<S> for T
    where T: Clone + PartialOrd + Add<S> + Sub + CheckedSub + From<<T as Add<S>>::Output>,
        S: Offset<Self> {

}

/// Trait which represents the offset type of a given pointer.
/// 
/// In plain english: An offset of a pointer must be orderable and convertable
/// from the difference of two pointers. Ordering has the semantics that the
/// greater offset represents a larger distance from the pointer that birthed
/// the offset than the right-hand-side offset.
/// 
/// TODO: Switch From to TryFrom. It should be possible to have two pointers
/// without a valid offset (e.g. there is no offset that takes you from a memory
/// pointer to an I/O port).
pub trait Offset<P> : Clone + PartialOrd + From<<P as Sub>::Output> where P: Sub {

}

impl <T, P> Offset<P> for T where T: Clone + PartialOrd + From<<P as Sub>::Output>, P: Sub {

}

/// Trait which represents a type which can be constructed from units of some
/// other numerical type.
/// 
/// In plain english: A type is desegmentable from the unit type of a memory
/// system if:
/// 
///  * The desegmentable type has bounds that can be expressed as a number of
///    left shifts of the type
///  * You can zero-extend the unit type to the desegmentable one
///  * You can shift the type left
///  * You can perform bitwise operations on the type
///  * The type has an additive and multiplicative identity
///  * Shifts and bitwise operations on the type produce some type convertable
///    back to this one
pub trait Desegmentable<U> : Clone + Bounded + From<U> + BoundWidth<usize> + Shl<usize> + BitOr + BitAnd + Zero + One + Not + From<<Self as Not>::Output> + From<<Self as Shl<usize>>::Output> + From<<Self as BitOr>::Output> + From<<Self as BitAnd>::Output> {

}

impl<T, U> Desegmentable<U> for T
    where T: Clone + Bounded + From<U> + BoundWidth<usize> + Shl<usize> + BitOr + BitAnd + Zero + One + Not + From<<T as Not>::Output> + From<<T as Shl<usize>>::Output> + From<<T as BitOr>::Output> + From<<T as BitAnd>::Output> {

}