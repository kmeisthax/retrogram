//! Functions relating to session storage & context.
//!
//! Code in the `context` module exists primarily to store the current user
//! session state and provide access to it for other parts of the TUI.

#[macro_use]
mod program;
mod session;

pub use program::{downcast_context, AnyProgramContext, ProgramContext};
pub use session::SessionContext;
