//! A set of types which allow analysis to model memory correctly.

mod behavior;
mod region;
mod image;
mod cptr;

pub use behavior::Behavior;
pub use region::Memory;
pub use image::Image;
pub use cptr::Pointer;