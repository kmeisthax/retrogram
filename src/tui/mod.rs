//! Text User Interface

#[macro_use]
mod context;
mod builder;
mod dialog;
mod disasm_view;
mod main;
mod menu;
mod tabs;

pub use crate::tui::main::main;
pub use context::ProgramContext;
