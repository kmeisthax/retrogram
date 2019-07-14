//! Types which model architectural and platform registers, or combinations of
//! the two.

mod symbolic;
mod state;
mod traits;

pub use symbolic::Symbolic;
pub use state::State;
pub use traits::*;

#[cfg(test)]
mod tests;