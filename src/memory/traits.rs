//! Helper traits for defining where clauses without getting an RSI

use std::ops::Sub;
use std::convert::TryFrom;
use num::traits::{Zero, One};
use crate::{reg, memory};
use crate::maths::{BoundWidth, CheckedAdd, CheckedSub, u24};

/// Trait which represents all operations expected of a pointer value.
/// 
/// In plain english: A pointer value must be orderable and have an offset type.
/// Ordering has the semantics that the greater pointer "comes after" the former
/// in a memory layout. It must be possible to subtract two pointers to obtain
/// the offset type, and whatever you get by adding a pointer and an offset must
/// yield another pointer.
pub trait PtrNum<S> : Clone + PartialOrd + CheckedAdd<S, Output=Self> + Sub + CheckedSub
    where S: Offset<Self> {

}

impl <T, S> PtrNum<S> for T
    where T: Clone + PartialOrd + CheckedAdd<S, Output=Self> + Sub + CheckedSub,
        S: Offset<Self> {

}

/// Trait which represents the offset type of a given pointer.
/// 
/// In plain english: An offset of a pointer must be orderable. The ordering of
/// offsets is assumed to mean that greater offsets mean a further distance from
/// the base pointer that birthed them. It should also be possible to add two
/// offsets, though not all offsets necessarily can be added. Offsets should
/// also have an unconditionally provided zero type.
/// 
/// Offsets may be potentially convertable from the difference of two pointer
/// types, as well as the runtime platform's preferred offset representation.
/// The former allows us to calculate offsets from the target archiecture's
/// pointer type, and the latter allows us to apply pointer maths to the target
/// architecture's pointers.
/// 
/// The selection of traits used here is deliberate: it allows pointer values to
/// encompass multiple disjoint address spaces; such as memory and I/O space, or
/// program and data space. `TryFrom` allows the type system to signal that the
/// difference of two pointers is undefined.
pub trait Offset<P> : Clone + PartialOrd + CheckedAdd<Output=Self> + CheckedSub<Output=Self> + Zero + One + TryFrom<<P as Sub>::Output> + TryFrom<usize>
    where P: Sub {

}

impl <T, P> Offset<P> for T
    where T: Clone + PartialOrd + CheckedAdd<Output=Self> + CheckedSub<Output=Self> + Zero + One + TryFrom<<P as Sub>::Output> + TryFrom<usize>,
        P: Sub {

}

/// Trait which represents a type which can be constructed from units of some
/// other numerical type.
/// 
/// In plain english: A type is desegmentable from the unit type of a memory
/// system if:
/// 
///  * The desegmentable type has bounds that can be expressed as a number of
///    left shifts of the type
///  * The unit type also has bounds expressable in the same manner
///    (For some reason, Rust won't adopt this into your trait bounds, so you
///    may need to state it again.)
///  * You can convert concrete type values into symbolic ones
///  * You can zero-extend the unit type to the desegmentable one
///  * You can shift the type left
///  * You can perform bitwise operations on the type
///  * The type has an additive and multiplicative identity
///  * Shifts and bitwise operations on the type produce some type convertable
///    back to this one
/// 
/// Furthermore, this trait places bounds on the behavior of symbolic values of
/// the given type. However, Rust doesn't recognize these bounds, so you must
/// copy and update them in any code that uses `Desegmentable` until I figure
/// out how to fix that.
pub trait Desegmentable<U> : Clone + reg::Bitwise
    where U: Clone + BoundWidth<u32> {
    
    /// Given a slice of memory units, attempt to join them into the target
    /// type.
    /// 
    /// The conversion may fail if the data slice is too short.
    fn from_segments(data: &[U], endianness: memory::Endianness) -> Option<Self>;

    /// Indicates how many units must be passed into `from_segments`in order to
    /// successfully join them into the target type.
    fn units_reqd() -> usize {
        let self_units = <Self as BoundWidth<u32>>::bound_width();
        let from_units = <U as BoundWidth<u32>>::bound_width();
        (self_units as f32 / from_units as f32).round() as usize
    }
}

macro_rules! desegmentable_impl {
    ($from_type:ty, $into_type:ty) => {
        impl Desegmentable<$from_type> for $into_type where $into_type: From<$from_type> {
            fn from_segments(data: &[$from_type], endianness: memory::Endianness) -> Option<Self> {
                let units_reqd = <Self as Desegmentable<$from_type>>::units_reqd() as u32;
                let mut sum = Self::zero();
                let i_iter : Vec<u32> = match endianness {
                    memory::Endianness::BigEndian => (0..units_reqd).rev().collect(),
                    memory::Endianness::LittleEndian => (0..units_reqd).collect()
                };

                for i in i_iter {
                    let unit = Self::from(data.get(i as usize)?.clone());
                    sum = sum | unit << (i * <$from_type as BoundWidth<u32>>::bound_width());
                }

                Some(sum)
            }
        }
    }
}

desegmentable_impl!(u8, u16);
desegmentable_impl!(u8, u24);
desegmentable_impl!(u8, u32);
desegmentable_impl!(u8, u64);
desegmentable_impl!(u8, u128);