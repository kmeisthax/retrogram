//! Classes and traits that define maths in various number systems

#[macro_use]
mod macros;
mod traits;
mod num24bit;

pub use num24bit::u24;
pub use traits::*;