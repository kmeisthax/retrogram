//! ARM, formerly an acronym of Acorn RISC Machine, and a quite popular ISA.
//!
//! This only covers 32-bit ARM, now known as AArch32.

mod arm;
mod context;
mod dis;
mod thumb;
mod types;

pub use context::architectural_ctxt_parse;
pub use dis::disassemble;
pub use thumb::THUMB_STATE;
pub use types::*;
