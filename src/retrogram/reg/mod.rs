//! Types which model architectural and platform registers, or combinations of
//! the two.

mod symbolic;
mod state;

pub use symbolic::Symbolic;
pub use symbolic::Convertable;
pub use state::State;