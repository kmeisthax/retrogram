//! 24-bit arithmetic

use std::ops::{Add, Sub, Mul, Div, BitAnd, BitOr, BitXor, Shl, Shr, Not, AddAssign, SubAssign, MulAssign, DivAssign,
    BitAndAssign, BitOrAssign, BitXorAssign, ShlAssign, ShrAssign};
use std::fmt;
use std::fmt::{Formatter, Display, Debug};
use std::str::FromStr;
use std::convert::{TryFrom, TryInto};
use num::{Zero, One, Bounded};
use crate::retrogram::maths::{CheckedSub, WrappingMul, BoundWidth};

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct u24 {
    v: u32
}

impl Display for u24 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.v)
    }
}

impl FromStr for u24 {
    type Err = <u32 as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(u24 {
            v: u32::from_str(s)?
        })
    }
}

impl Zero for u24 {
    fn zero() -> Self {
        u24 {
            v: 0
        }
    }

    fn is_zero(&self) -> bool {
        self.v.is_zero()
    }
}

impl One for u24 {
    fn one() -> Self {
        u24 {
            v: 1
        }
    }

    fn is_one(&self) -> bool {
        self.v.is_one()
    }
}

impl Bounded for u24 {
    fn min_value() -> Self {
        u24 {
            v: 0
        }
    }

    fn max_value() -> Self {
        u24 {
            v: 0xFFFFFF
        }
    }
}

impl CheckedSub for u24 {
    fn checked_sub(self, rhs: Self) -> Option<<Self as Sub>::Output> {
        Some(u24 {
            v: self.v.checked_sub(rhs.v)? & 0xFFFFFF
        })
    }
}

impl Not for u24 {
    type Output = Self;

    fn not(self) -> Self {
        u24 {
            v: !self.v & 0xFFFFFF
        }
    }
}

masked_conv_impl!(u24, u32, u8, 0xFFFFFF);
masked_conv_impl!(u24, u32, u16, 0xFFFFFF);
masked_tryconv_impl!(u24, u32, u32, 0xFFFFFF);
masked_tryconv_impl!(u24, u32, u64, 0xFFFFFF);
masked_tryconv_impl!(u24, u32, u128, 0xFFFFFF);
masked_tryconv_impl!(u24, u32, usize, 0xFFFFFF);

try_unwrap_impl!(u24, u32, u8);
try_unwrap_impl!(u24, u32, u16);
unwrap_impl!(u24, u32, u32);
unwrap_impl!(u24, u32, u64);
unwrap_impl!(u24, u32, u128);
try_unwrap_impl!(u24, u32, usize);

binary_op_masked_impl!(u24, Add, add, 0xFFFFFF);
binary_op_masked_impl!(u24, Sub, sub, 0xFFFFFF);
binary_op_masked_impl!(u24, Div, div, 0xFFFFFF);
binary_op_masked_impl!(u24, Mul, mul, 0xFFFFFF);
binary_op_masked_impl!(u24, BitAnd, bitand, 0xFFFFFF);
binary_op_masked_impl!(u24, BitOr, bitor, 0xFFFFFF);
binary_op_masked_impl!(u24, BitXor, bitxor, 0xFFFFFF);
binary_op_masked_impl!(u24, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, Shr, shr, 0xFFFFFF);

assign_binary_op_masked_impl!(u24, AddAssign, add_assign, 0xFFFFFF);
assign_binary_op_masked_impl!(u24, SubAssign, sub_assign, 0xFFFFFF);
assign_binary_op_masked_impl!(u24, DivAssign, div_assign, 0xFFFFFF);
assign_binary_op_masked_impl!(u24, MulAssign, mul_assign, 0xFFFFFF);
assign_binary_op_masked_impl!(u24, BitAndAssign, bitand_assign, 0xFFFFFF);
assign_binary_op_masked_impl!(u24, BitOrAssign, bitor_assign, 0xFFFFFF);
assign_binary_op_masked_impl!(u24, BitXorAssign, bitxor_assign, 0xFFFFFF);
assign_binary_op_masked_impl!(u24, ShlAssign, shl_assign, 0xFFFFFF);
assign_binary_op_masked_impl!(u24, ShrAssign, shr_assign, 0xFFFFFF);

binary_op_masked_impl_notype!(u24, WrappingMul, wrapping_mul, 0xFFFFFF);

binary_op_masked_impl!(u24, u8, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, u16, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, u32, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, u64, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, u128, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, usize, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, i8, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, i16, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, i32, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, i64, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, i128, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, isize, Shl, shl, 0xFFFFFF);

binary_op_masked_impl!(u24, u8, Shr, shr, 0xFFFFFF);
binary_op_masked_impl!(u24, u16, Shr, shr, 0xFFFFFF);
binary_op_masked_impl!(u24, u32, Shr, shr, 0xFFFFFF);
binary_op_masked_impl!(u24, u64, Shr, shr, 0xFFFFFF);
binary_op_masked_impl!(u24, u128, Shr, shr, 0xFFFFFF);
binary_op_masked_impl!(u24, usize, Shr, shr, 0xFFFFFF);
binary_op_masked_impl!(u24, i8, Shr, shr, 0xFFFFFF);
binary_op_masked_impl!(u24, i16, Shr, shr, 0xFFFFFF);
binary_op_masked_impl!(u24, i32, Shr, shr, 0xFFFFFF);
binary_op_masked_impl!(u24, i64, Shr, shr, 0xFFFFFF);
binary_op_masked_impl!(u24, i128, Shr, shr, 0xFFFFFF);
binary_op_masked_impl!(u24, isize, Shr, shr, 0xFFFFFF);

boundwidth_impl!(u24, u8, 24);
boundwidth_impl!(u24, u16, 24);
boundwidth_impl!(u24, u24, u24::from(24 as u16));
boundwidth_impl!(u24, u32, 24);
boundwidth_impl!(u24, u64, 24);
boundwidth_impl!(u24, u128, 24);
boundwidth_impl!(u24, usize, 24);