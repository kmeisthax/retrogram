//! Population count / Hamming Weight / number of 1s and 0s

use std::convert::TryFrom;
use crate::maths::{WrappingMul, u24};

/// Count the number of symbols in a bitwise value.
pub trait Popcount {
    /// The resulting output of a successful population count.
    /// 
    /// Usually, this is an integer, though population counts on 
    type Output;

    /// Count the number of one symbols in the binary representation of an integer.
    fn pop_count(self) -> Self::Output;

    /// Count the number of zero symbols in the binary representation of an integer.
    fn depop_count(self) -> Self::Output;
}

impl Popcount for u8 {
    type Output = u8;

    fn pop_count(mut self) -> Self::Output {
        self -= (self >> 1) & 0x55;
        self = (self & 0x33) + ((self >> 2) & 0x33);
        (self + (self >> 4)) & 0x0F
    }

    fn depop_count(self) -> Self::Output {
        (!self).pop_count()
    }
}

impl Popcount for u16 {
    type Output = u16;

    fn pop_count(mut self) -> Self::Output {
        self -= (self >> 1) & 0x5555;
        self = (self & 0x3333) + ((self >> 2) & 0x3333);
        self = (self + (self >> 4)) & 0x0F0F;
        self.wrapping_mul(0x0101) >> 8
    }

    fn depop_count(self) -> Self::Output {
        (!self).pop_count()
    }
}

impl Popcount for u24 {
    type Output = u24;

    fn pop_count(mut self) -> Self::Output {
        self -= (self >> 1) & u24::try_from(0x555555 as u32).unwrap();
        self = (self & u24::try_from(0x333333 as u32).unwrap()) + ((self >> 2) & u24::try_from(0x333333 as u32).unwrap());
        self = (self + (self >> 4)) & u24::try_from(0x0F0F0F as u32).unwrap();
        self.wrapping_mul(u24::try_from(0x010101 as u32).unwrap()) >> 16
    }

    fn depop_count(self) -> Self::Output {
        (!self).pop_count()
    }
}

impl Popcount for u32 {
    type Output = u32;

    fn pop_count(mut self) -> Self::Output {
        self -= (self >> 1) & 0x55555555;
        self = (self & 0x33333333) + ((self >> 2) & 0x33333333);
        self = (self + (self >> 4)) & 0x0F0F0F0F;
        self.wrapping_mul(0x01010101) >> 24
    }

    fn depop_count(self) -> Self::Output {
        (!self).pop_count()
    }
}

impl Popcount for u64 {
    type Output = u64;

    fn pop_count(mut self) -> Self::Output {
        self -= (self >> 1) & 0x5555555555555555;
        self = (self & 0x3333333333333333) + ((self >> 2) & 0x3333333333333333);
        self = (self + (self >> 4)) & 0x0F0F0F0F0F0F0F0F;
        self.wrapping_mul(0x0101010101010101) >> 56
    }

    fn depop_count(self) -> Self::Output {
        (!self).pop_count()
    }
}

impl Popcount for u128 {
    type Output = u128;

    fn pop_count(mut self) -> Self::Output {
        self -= (self >> 1) & 0x55555555555555555555555555555555;
        self = (self & 0x33333333333333333333333333333333) + ((self >> 2) & 0x33333333333333333333333333333333);
        self = (self + (self >> 4)) & 0x0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F;
        self.wrapping_mul(0x01010101010101010101010101010101) >> 120
    }

    fn depop_count(self) -> Self::Output {
        (!self).pop_count()
    }
}