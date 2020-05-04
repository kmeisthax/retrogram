//! A set of types which allow analysis to model memory correctly.

mod behavior;
mod bss;
mod cptr;
mod endianness;
mod image;
mod region;
mod rombin;
mod traits;

pub use behavior::Action;
pub use behavior::Behavior;
pub use cptr::Pointer;
pub use endianness::Endianness;
pub use image::Image;
pub use region::Memory;
pub use traits::*;

#[cfg(test)]
mod tests;
