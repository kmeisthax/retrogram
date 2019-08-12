#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate serde_plain;

#[macro_use]
extern crate clap;

#[macro_use]
pub mod memory;
pub mod reg;
pub mod ast;
pub mod arch;
pub mod platform;
pub mod analysis;
pub mod database;
pub mod asm;
pub mod project;
pub mod input;
pub mod cli;
mod maths;

use std::io;

fn main() -> io::Result<()> {
    cli::main()
}