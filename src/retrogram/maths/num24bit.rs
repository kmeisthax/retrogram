//! 24-bit arithmetic

use std::ops::{Add, Sub, Mul, Div, BitAnd, BitOr, BitXor, Shl};
use crate::retrogram::maths::CheckedSub;

#[allow(non_camel_case_types)]
pub struct u24 {
    v: u32
}

masked_conv_impl!(u24, u32, 0xFFFFFF);

binary_op_masked_impl!(u24, Add, add, 0xFFFFFF);
binary_op_masked_impl!(u24, Sub, sub, 0xFFFFFF);
binary_op_masked_impl!(u24, Div, div, 0xFFFFFF);
binary_op_masked_impl!(u24, Mul, mul, 0xFFFFFF);
binary_op_masked_impl!(u24, BitAnd, bitand, 0xFFFFFF);
binary_op_masked_impl!(u24, BitOr, bitor, 0xFFFFFF);
binary_op_masked_impl!(u24, BitXor, bitxor, 0xFFFFFF);
binary_op_masked_impl!(u24, Shl, shl, 0xFFFFFF);

binary_op_masked_impl!(u24, u8, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, u16, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, u32, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, u64, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, u128, Shl, shl, 0xFFFFFF);
binary_op_masked_impl!(u24, usize, Shl, shl, 0xFFFFFF);

boundwidth_impl!(u24, u8, 24);
boundwidth_impl!(u24, u16, 24);
boundwidth_impl!(u24, u32, 24);
boundwidth_impl!(u24, u64, 24);
boundwidth_impl!(u24, u128, 24);
boundwidth_impl!(u24, usize, 24);