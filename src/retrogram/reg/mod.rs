//! Types which model architectural and platform registers, or combinations of
//! the two.

mod symbolic;
mod state;
mod context;
mod cptr;

pub use symbolic::Symbolic;
pub use state::State;
pub use context::Context;
pub use cptr::ContextualPointer;