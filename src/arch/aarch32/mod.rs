//! ARM, formerly an acronym of Acorn RISC Machine, and a quite popular ISA.
//!
//! This only covers 32-bit ARM, now known as AArch32.

mod arm;
mod context;
mod dataflow;
mod dis;
mod fork;
mod thumb;
mod trace;
mod types;

pub use context::architectural_ctxt_parse;
pub use dataflow::dataflow;
pub use dis::disassemble;
pub use fork::prereq;
pub use thumb::THUMB_STATE;
pub use trace::trace;
pub use types::*;
