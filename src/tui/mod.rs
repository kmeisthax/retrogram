//! Text User Interface

#[macro_use]
mod context;
mod builder;
mod disasm_view;
mod pickers;
mod jump;
mod label;
mod main;
mod menu;
mod tabs;

pub use crate::tui::main::main;
pub use context::ProgramContext;
