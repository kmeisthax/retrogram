//! My number traits

use std::num::ParseIntError;
use std::ops::{Add, Mul, Shl, Sub};

/// A trait which indicates the number of left shifts of the given type's
/// smallest value (1) are required in order to overflow that type.
///
/// It only makes sense to implement BoundWidth on types which can meaningfully
/// be assembled and disassembled from a wordstream of differently sized types
/// by shifting and adding the words together (see `read_manywords_*` on
/// `memory::Memory`).
///
/// The `RHS` of a `BoundWidth` impl must match the `RHS` of a valid `Shl` impl
/// on the same type. The resulting value of `bound_width` is thus the
/// overflowing shift count when shifted by something of that type.
///
/// #The limits of BoundWidth semantics
///
/// If you are implementing a numeral system with a different base or other
/// exotic requirements, you too can leverage BoundWidth to explain how to
/// read your type from smaller segments of the same type. For example, if you
/// wanted to represent decimal maths, then you would have a `Shl` that accepted
/// a decimal `RHS` and multiplied by powers of ten instead of two. BoundWidth
/// would thus indicate how many *digits* wide your type was (instead of bits)
/// and it would be possible to segment and join these decimal values into a
/// decimally-valued memory and processor architecture for analysis.
pub trait BoundWidth<RHS = Self>: Shl<RHS> {
    fn bound_width() -> RHS;
}

boundwidth_impl!(u8, u8, 8);
boundwidth_impl!(u8, u16, 8);
boundwidth_impl!(u8, u32, 8);
boundwidth_impl!(u8, u64, 8);
boundwidth_impl!(u8, u128, 8);
boundwidth_impl!(u8, usize, 8);
boundwidth_impl!(u8, i8, 8);
boundwidth_impl!(u8, i16, 8);
boundwidth_impl!(u8, i32, 8);
boundwidth_impl!(u8, i64, 8);
boundwidth_impl!(u8, i128, 8);
boundwidth_impl!(u8, isize, 8);
boundwidth_impl!(u16, u8, 16);
boundwidth_impl!(u16, u16, 16);
boundwidth_impl!(u16, u32, 16);
boundwidth_impl!(u16, u64, 16);
boundwidth_impl!(u16, u128, 16);
boundwidth_impl!(u16, usize, 16);
boundwidth_impl!(u16, i8, 16);
boundwidth_impl!(u16, i16, 16);
boundwidth_impl!(u16, i32, 16);
boundwidth_impl!(u16, i64, 16);
boundwidth_impl!(u16, i128, 16);
boundwidth_impl!(u16, isize, 16);
boundwidth_impl!(u32, u8, 32);
boundwidth_impl!(u32, u16, 32);
boundwidth_impl!(u32, u32, 32);
boundwidth_impl!(u32, u64, 32);
boundwidth_impl!(u32, u128, 32);
boundwidth_impl!(u32, usize, 32);
boundwidth_impl!(u32, i8, 32);
boundwidth_impl!(u32, i16, 32);
boundwidth_impl!(u32, i32, 32);
boundwidth_impl!(u32, i64, 32);
boundwidth_impl!(u32, i128, 32);
boundwidth_impl!(u32, isize, 32);
boundwidth_impl!(u64, u8, 64);
boundwidth_impl!(u64, u16, 64);
boundwidth_impl!(u64, u32, 64);
boundwidth_impl!(u64, u64, 64);
boundwidth_impl!(u64, u128, 64);
boundwidth_impl!(u64, usize, 64);
boundwidth_impl!(u64, i8, 64);
boundwidth_impl!(u64, i16, 64);
boundwidth_impl!(u64, i32, 64);
boundwidth_impl!(u64, i64, 64);
boundwidth_impl!(u64, i128, 64);
boundwidth_impl!(u64, isize, 64);
boundwidth_impl!(u128, u8, 128);
boundwidth_impl!(u128, u16, 128);
boundwidth_impl!(u128, u32, 128);
boundwidth_impl!(u128, u64, 128);
boundwidth_impl!(u128, u128, 128);
boundwidth_impl!(u128, usize, 128);
// Missing u128/i8 impl because 128 is not representable in i8
boundwidth_impl!(u128, i16, 128);
boundwidth_impl!(u128, i32, 128);
boundwidth_impl!(u128, i64, 128);
boundwidth_impl!(u128, i128, 128);
boundwidth_impl!(u128, isize, 128);
boundwidth_impl!(i8, u8, 8);
boundwidth_impl!(i8, u16, 8);
boundwidth_impl!(i8, u32, 8);
boundwidth_impl!(i8, u64, 8);
boundwidth_impl!(i8, u128, 8);
boundwidth_impl!(i8, usize, 8);
boundwidth_impl!(i8, i8, 8);
boundwidth_impl!(i8, i16, 8);
boundwidth_impl!(i8, i32, 8);
boundwidth_impl!(i8, i64, 8);
boundwidth_impl!(i8, i128, 8);
boundwidth_impl!(i8, isize, 8);
boundwidth_impl!(i16, u8, 16);
boundwidth_impl!(i16, u16, 16);
boundwidth_impl!(i16, u32, 16);
boundwidth_impl!(i16, u64, 16);
boundwidth_impl!(i16, u128, 16);
boundwidth_impl!(i16, usize, 16);
boundwidth_impl!(i16, i8, 16);
boundwidth_impl!(i16, i16, 16);
boundwidth_impl!(i16, i32, 16);
boundwidth_impl!(i16, i64, 16);
boundwidth_impl!(i16, i128, 16);
boundwidth_impl!(i16, isize, 16);
boundwidth_impl!(i32, u8, 32);
boundwidth_impl!(i32, u16, 32);
boundwidth_impl!(i32, u32, 32);
boundwidth_impl!(i32, u64, 32);
boundwidth_impl!(i32, u128, 32);
boundwidth_impl!(i32, usize, 32);
boundwidth_impl!(i32, i8, 32);
boundwidth_impl!(i32, i16, 32);
boundwidth_impl!(i32, i32, 32);
boundwidth_impl!(i32, i64, 32);
boundwidth_impl!(i32, i128, 32);
boundwidth_impl!(i32, isize, 32);
boundwidth_impl!(i64, u8, 64);
boundwidth_impl!(i64, u16, 64);
boundwidth_impl!(i64, u32, 64);
boundwidth_impl!(i64, u64, 64);
boundwidth_impl!(i64, u128, 64);
boundwidth_impl!(i64, usize, 64);
boundwidth_impl!(i64, i8, 64);
boundwidth_impl!(i64, i16, 64);
boundwidth_impl!(i64, i32, 64);
boundwidth_impl!(i64, i64, 64);
boundwidth_impl!(i64, i128, 64);
boundwidth_impl!(i64, isize, 64);
boundwidth_impl!(i128, u8, 128);
boundwidth_impl!(i128, u16, 128);
boundwidth_impl!(i128, u32, 128);
boundwidth_impl!(i128, u64, 128);
boundwidth_impl!(i128, u128, 128);
boundwidth_impl!(i128, usize, 128);
// Missing i128/i8 impl because 128 is not representable in i8
boundwidth_impl!(i128, i16, 128);
boundwidth_impl!(i128, i32, 128);
boundwidth_impl!(i128, i64, 128);
boundwidth_impl!(i128, i128, 128);
boundwidth_impl!(i128, isize, 128);

/// Reimplementation of the num_traits `CheckedSub` trait, except with the
/// ability to specify a different RHS and Output type.
///
/// num_traits doesn't support this behavior because Rust itself doesn't define
/// checked maths with different types of operands and output parameters.
pub trait CheckedSub<RHS = Self>: Sized + Sub<RHS> {
    fn checked_sub(self, v: RHS) -> Option<<Self as Sub<RHS>>::Output>;
}

wrap_existing_impl!(CheckedSub, checked_sub, u8, u8, Option<u8>);
wrap_existing_impl!(CheckedSub, checked_sub, u16, u16, Option<u16>);
wrap_existing_impl!(CheckedSub, checked_sub, u32, u32, Option<u32>);
wrap_existing_impl!(CheckedSub, checked_sub, u64, u64, Option<u64>);
wrap_existing_impl!(CheckedSub, checked_sub, u128, u128, Option<u128>);
wrap_existing_impl!(CheckedSub, checked_sub, usize, usize, Option<usize>);

wrap_existing_impl!(CheckedSub, checked_sub, i8, i8, Option<i8>);
wrap_existing_impl!(CheckedSub, checked_sub, i16, i16, Option<i16>);
wrap_existing_impl!(CheckedSub, checked_sub, i32, i32, Option<i32>);
wrap_existing_impl!(CheckedSub, checked_sub, i64, i64, Option<i64>);
wrap_existing_impl!(CheckedSub, checked_sub, i128, i128, Option<i128>);
wrap_existing_impl!(CheckedSub, checked_sub, isize, isize, Option<isize>);

/// Reimplementation of the num_traits `CheckedAdd` trait, except with the
/// ability to specify a different RHS and Output type.
///
/// num_traits doesn't support this behavior because Rust itself doesn't define
/// checked maths with different types of operands and output parameters.
pub trait CheckedAdd<RHS = Self>: Sized + Add<RHS> {
    fn checked_add(self, v: RHS) -> Option<<Self as Add<RHS>>::Output>;
}

wrap_existing_impl!(CheckedAdd, checked_add, u8, u8, Option<u8>);
wrap_existing_impl!(CheckedAdd, checked_add, u16, u16, Option<u16>);
wrap_existing_impl!(CheckedAdd, checked_add, u32, u32, Option<u32>);
wrap_existing_impl!(CheckedAdd, checked_add, u64, u64, Option<u64>);
wrap_existing_impl!(CheckedAdd, checked_add, u128, u128, Option<u128>);
wrap_existing_impl!(CheckedAdd, checked_add, usize, usize, Option<usize>);

wrap_existing_impl!(CheckedAdd, checked_add, i8, i8, Option<i8>);
wrap_existing_impl!(CheckedAdd, checked_add, i16, i16, Option<i16>);
wrap_existing_impl!(CheckedAdd, checked_add, i32, i32, Option<i32>);
wrap_existing_impl!(CheckedAdd, checked_add, i64, i64, Option<i64>);
wrap_existing_impl!(CheckedAdd, checked_add, i128, i128, Option<i128>);
wrap_existing_impl!(CheckedAdd, checked_add, isize, isize, Option<isize>);

pub trait WrappingMul<RHS = Self>: Mul<RHS> {
    fn wrapping_mul(self, v: RHS) -> <Self as Mul<RHS>>::Output;
}

wrap_existing_impl!(WrappingMul, wrapping_mul, u8, u8, u8);
wrap_existing_impl!(WrappingMul, wrapping_mul, u16, u16, u16);
wrap_existing_impl!(WrappingMul, wrapping_mul, u32, u32, u32);
wrap_existing_impl!(WrappingMul, wrapping_mul, u64, u64, u64);
wrap_existing_impl!(WrappingMul, wrapping_mul, u128, u128, u128);
wrap_existing_impl!(WrappingMul, wrapping_mul, usize, usize, usize);

wrap_existing_impl!(WrappingMul, wrapping_mul, i8, i8, i8);
wrap_existing_impl!(WrappingMul, wrapping_mul, i16, i16, i16);
wrap_existing_impl!(WrappingMul, wrapping_mul, i32, i32, i32);
wrap_existing_impl!(WrappingMul, wrapping_mul, i64, i64, i64);
wrap_existing_impl!(WrappingMul, wrapping_mul, i128, i128, i128);
wrap_existing_impl!(WrappingMul, wrapping_mul, isize, isize, isize);

pub trait FromStrRadix
where
    Self: Sized,
{
    fn from_str_radix(src: &str, radix: u32) -> Result<Self, ParseIntError>;
}

wrap_from_str_radix_impl!(FromStrRadix, from_str_radix, u8);
wrap_from_str_radix_impl!(FromStrRadix, from_str_radix, u16);
wrap_from_str_radix_impl!(FromStrRadix, from_str_radix, u32);
wrap_from_str_radix_impl!(FromStrRadix, from_str_radix, u64);
wrap_from_str_radix_impl!(FromStrRadix, from_str_radix, u128);
wrap_from_str_radix_impl!(FromStrRadix, from_str_radix, usize);
wrap_from_str_radix_impl!(FromStrRadix, from_str_radix, i8);
wrap_from_str_radix_impl!(FromStrRadix, from_str_radix, i16);
wrap_from_str_radix_impl!(FromStrRadix, from_str_radix, i32);
wrap_from_str_radix_impl!(FromStrRadix, from_str_radix, i64);
wrap_from_str_radix_impl!(FromStrRadix, from_str_radix, i128);
wrap_from_str_radix_impl!(FromStrRadix, from_str_radix, isize);
