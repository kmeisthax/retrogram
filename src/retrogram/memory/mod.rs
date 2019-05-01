//! A set of types which allow analysis to model memory correctly.

mod behavior;
mod region;
mod image;
mod cptr;
mod bss;
mod rombin;
mod traits;

pub use behavior::Behavior;
pub use region::Memory;
pub use image::Image;
pub use cptr::Pointer;
pub use traits::*;