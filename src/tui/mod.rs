//! Text User Interface

#[macro_use]
mod context;
mod disasm_view;
mod jump;
mod label;
mod main;

pub use crate::tui::main::main;
pub use context::ProgramContext;
