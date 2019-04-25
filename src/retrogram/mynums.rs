//! My number traits

use std::ops::Sub;

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
pub trait BoundWidth<RHS = Self> {
    fn bound_width() -> RHS;
}

macro_rules! boundwidth_impl {
    ($t:ty, $rhs:ty, $shifts:expr) => {
        impl BoundWidth<$rhs> for $t {
            #[inline]
            fn bound_width() -> $rhs {
                $shifts
            }
        }
    }
}

boundwidth_impl!(u8, u8, 8);
boundwidth_impl!(u8, u16, 8);
boundwidth_impl!(u8, u32, 8);
boundwidth_impl!(u8, u64, 8);
boundwidth_impl!(u8, u128, 8);
boundwidth_impl!(u8, usize, 8);
boundwidth_impl!(u16, u8, 16);
boundwidth_impl!(u16, u16, 16);
boundwidth_impl!(u16, u32, 16);
boundwidth_impl!(u16, u64, 16);
boundwidth_impl!(u16, u128, 16);
boundwidth_impl!(u16, usize, 16);
boundwidth_impl!(u32, u8, 32);
boundwidth_impl!(u32, u16, 32);
boundwidth_impl!(u32, u32, 32);
boundwidth_impl!(u32, u64, 32);
boundwidth_impl!(u32, u128, 32);
boundwidth_impl!(u32, usize, 32);
boundwidth_impl!(u64, u8, 64);
boundwidth_impl!(u64, u16, 64);
boundwidth_impl!(u64, u32, 64);
boundwidth_impl!(u64, u64, 64);
boundwidth_impl!(u64, u128, 64);
boundwidth_impl!(u64, usize, 64);
boundwidth_impl!(u128, u8, 128);
boundwidth_impl!(u128, u16, 128);
boundwidth_impl!(u128, u32, 128);
boundwidth_impl!(u128, u64, 128);
boundwidth_impl!(u128, u128, 128);
boundwidth_impl!(u128, usize, 128);

/// Reimplementation of the num_traits `CheckedSub` trait, except with the
/// ability to specify a different RHS and Output type.
/// 
/// num_traits doesn't support this behavior because Rust itself doesn't define
/// checked maths with different types of operands and output parameters.
pub trait CheckedSub<RHS = Self>: Sized + Sub<RHS> {
    fn checked_sub(&self, v: &RHS) -> Option<<Self as Sub<RHS>>::Output>;
}

macro_rules! checked_impl {
    ($trait_name:ident, $method:ident, $t:ty, $rhs:ty, $out:ty) => {
        impl $trait_name for $t {
            #[inline]
            fn $method(&self, v: &$rhs) -> Option<$out> {
                <$t>::$method(*self, *v)
            }
        }
    }
}

checked_impl!(CheckedSub, checked_sub, u8, u8, u8);
checked_impl!(CheckedSub, checked_sub, u16, u16, u16);
checked_impl!(CheckedSub, checked_sub, u32, u32, u32);
checked_impl!(CheckedSub, checked_sub, u64, u64, u64);
checked_impl!(CheckedSub, checked_sub, u128, u128, u128);
checked_impl!(CheckedSub, checked_sub, usize, usize, usize);

checked_impl!(CheckedSub, checked_sub, i8, i8, i8);
checked_impl!(CheckedSub, checked_sub, i16, i16, i16);
checked_impl!(CheckedSub, checked_sub, i32, i32, i32);
checked_impl!(CheckedSub, checked_sub, i64, i64, i64);
checked_impl!(CheckedSub, checked_sub, i128, i128, i128);
checked_impl!(CheckedSub, checked_sub, isize, isize, isize);