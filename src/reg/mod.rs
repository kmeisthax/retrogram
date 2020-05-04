//! Types which model architectural and platform registers, or combinations of
//! the two.

mod state;
mod symbolic;
mod traits;

pub use state::State;
pub use symbolic::Symbolic;
pub use traits::*;

#[cfg(test)]
mod tests;
