//! Utility traits for CLI module

use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Display, LowerHex, UpperHex};
use std::str::FromStr;

/// Guard trait for values users can input to us and we can output.
///
/// Effectively, this covers all of the operations we need to do in order for
/// both the program and it's users to be able to name locations within a
/// program. Hence, "nameable".
///
/// The trait bounds, in plain english, require that we can:
///
///  * Parse the value from a string
///  * Display the value, in a handful of different Rust-supported formatting
///    modes.
///  * Attempt to convert the value to and from a u8. This is to facilitate
///    displaying symbolic versions of this value, as some of the traits
///    mentioned here can only be symbolically accessed with a valid u8
///    representation. Note that this will only be expected to be able to
///    convert values up to 16, as it will always be first masked off with 0xF.
pub trait Nameable:
    Clone + Debug + FromStr + Display + LowerHex + UpperHex + TryFrom<u8> + TryInto<u8>
{
}

impl<T> Nameable for T where
    T: Clone + Debug + FromStr + Display + LowerHex + UpperHex + TryFrom<u8> + TryInto<u8>
{
}
