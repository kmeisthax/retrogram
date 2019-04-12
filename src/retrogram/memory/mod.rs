//! A set of types which allow analysis to model memory correctly.

mod behavior;
mod region;
mod image;

pub use behavior::Behavior;
pub use region::Memory;
pub use image::Image;