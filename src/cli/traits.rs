//! Utility traits for CLI module

use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Display, LowerHex, UpperHex};
use std::str::FromStr;

/// Guard trait for pointer values users can input to us and we can output.
///
/// Effectively, this covers all of the operations we need to do in order for
/// both the program and it's users to be able to name locations within a
/// program.
///
/// The trait bounds, in plain english, require that we can:
///
///  * Parse the value from a string
///  * Display the value, with upper or lowercase hexdecimal notation if needed
///  * Attempt to convert the value from a u64 (for user input contexts)
///  * Attempt to convert the value to and from a u8, so that `UpperHex` and
///    `LowerHex` implementations on `Symbolic` can print the value four bits
///    at a time. (We don't have a u4 impl, so u8 is the best option.)
pub trait Nameable:
    Clone + Debug + FromStr + Display + LowerHex + UpperHex + TryFrom<u64> + TryFrom<u8> + TryInto<u8>
{
}

impl<T> Nameable for T where
    T: Clone
        + Debug
        + FromStr
        + Display
        + LowerHex
        + UpperHex
        + TryFrom<u64>
        + TryFrom<u8>
        + TryInto<u8>
{
}
