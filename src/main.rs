#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate serde_plain;

#[macro_use]
extern crate clap;

#[macro_use]
pub mod memory;
pub mod analysis;
pub mod arch;
pub mod asm;
pub mod ast;
pub mod cli;
pub mod database;
pub mod input;
mod maths;
pub mod platform;
pub mod project;
pub mod reg;
mod tui;

use std::io;

fn main() -> io::Result<()> {
    cli::main()
}
