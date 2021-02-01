//! View Builder pattern

mod bind;
mod build;
mod merge;

pub use bind::Binder;
pub use build::Builder;
pub use merge::BoxedMergeable;
