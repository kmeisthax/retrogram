//! ARM, formerly an acronym of Acorn RISC Machine, and a quite popular ISA.
//! 
//! This only covers 32-bit ARM, now known as AArch32.

mod types;
mod dis;
mod arm;
mod thumb;

pub use types::*;
pub use dis::disassemble;
pub use thumb::THUMB_STATE;