//! Classes and traits that define maths in various number systems

#[macro_use]
mod macros;
mod num24bit;
mod popcount;
mod traits;

pub use num24bit::u24;
pub use popcount::Popcount;
pub use traits::*;

#[cfg(test)]
mod tests;
