//! Math trait testing

use crate::maths::{u24, Popcount};
use std::convert::TryFrom;

#[test]
fn popcount_u128() {
    assert_eq!(
        (0xFFFF_0000_FFFF_0000_9F0F_1121_6124_FFFF_u128).pop_count(),
        67
    );
}

#[test]
fn popcount_u64() {
    assert_eq!((0x9F0F_1121_6124_FFFF_u64).pop_count(), 35);
}

#[test]
fn popcount_u32() {
    assert_eq!((0x9F0F_1121_u32).pop_count(), 14);
}

#[test]
fn popcount_u24() {
    assert_eq!(
        u24::try_from(0x9F_0F21_u32).unwrap().pop_count(),
        u24::try_from(12_u32).unwrap()
    );
}

#[test]
fn popcount_u16() {
    assert_eq!((0x9F0F_u128).pop_count(), 10);
}

#[test]
fn popcount_u8() {
    assert_eq!((0x9F_u128).pop_count(), 6);
}
