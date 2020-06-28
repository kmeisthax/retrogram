//! Population count / Hamming Weight / number of 1s and 0s

use crate::maths::{u24, WrappingMul};
use std::convert::TryFrom;

/// Count the number of symbols in a bitwise value.
pub trait Popcount {
    /// The resulting output of a successful population count.
    ///
    /// Usually, this is an integer, though population counts on
    type Output;

    /// Count the number of one symbols in the binary representation of an
    /// integer.
    ///
    /// NOTE: For bounded-width types, this is not necessarily required to
    /// mirror `depop_count`. It is perfectly valid for a population count to
    /// not be equal to the bound width minus the depopulation count.
    fn pop_count(self) -> Self::Output;

    /// Count the number of zero symbols in the binary representation of an
    /// integer.
    ///
    /// NOTE: For bounded-width types, this is not necessarily required to
    /// mirror `pop_count`. It is perfectly valid for a depopulation count to
    /// not be equal to the bound width minus the population count.
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
        self -= (self >> 1) & u24::try_from(0x55_5555 as u32).unwrap();
        self = (self & u24::try_from(0x33_3333 as u32).unwrap())
            + ((self >> 2) & u24::try_from(0x33_3333 as u32).unwrap());
        self = (self + (self >> 4)) & u24::try_from(0x0F_0F0F as u32).unwrap();
        self.wrapping_mul(u24::try_from(0x01_0101 as u32).unwrap()) >> 16
    }

    fn depop_count(self) -> Self::Output {
        (!self).pop_count()
    }
}

impl Popcount for u32 {
    type Output = u32;

    fn pop_count(mut self) -> Self::Output {
        self -= (self >> 1) & 0x5555_5555;
        self = (self & 0x3333_3333) + ((self >> 2) & 0x3333_3333);
        self = (self + (self >> 4)) & 0x0F0F_0F0F;
        self.wrapping_mul(0x0101_0101) >> 24
    }

    fn depop_count(self) -> Self::Output {
        (!self).pop_count()
    }
}

impl Popcount for u64 {
    type Output = u64;

    fn pop_count(mut self) -> Self::Output {
        self -= (self >> 1) & 0x5555_5555_5555_5555;
        self = (self & 0x3333_3333_3333_3333) + ((self >> 2) & 0x3333_3333_3333_3333);
        self = (self + (self >> 4)) & 0x0F0F_0F0F_0F0F_0F0F;
        self.wrapping_mul(0x0101_0101_0101_0101) >> 56
    }

    fn depop_count(self) -> Self::Output {
        (!self).pop_count()
    }
}

impl Popcount for u128 {
    type Output = u128;

    fn pop_count(mut self) -> Self::Output {
        self -= (self >> 1) & 0x5555_5555_5555_5555_5555_5555_5555_5555;
        self = (self & 0x3333_3333_3333_3333_3333_3333_3333_3333)
            + ((self >> 2) & 0x3333_3333_3333_3333_3333_3333_3333_3333);
        self = (self + (self >> 4)) & 0x0F0F_0F0F_0F0F_0F0F_0F0F_0F0F_0F0F_0F0F;
        self.wrapping_mul(0x0101_0101_0101_0101_0101_0101_0101_0101) >> 120
    }

    fn depop_count(self) -> Self::Output {
        (!self).pop_count()
    }
}
