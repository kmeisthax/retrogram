//! Tools for analysis of disassembled program code

mod block;
mod disasm;
mod dynamic;
mod error;
mod flow;
mod passes;
mod reference;
mod trace;
mod traits;

pub use block::Block;
pub use disasm::Disasm;
pub use dynamic::{trace_until_fork, Prerequisite};
pub use error::Error;
pub use error::Result;
pub use flow::Flow;
pub use passes::disassemble_block;
pub use passes::inject_labels;
pub use passes::inject_orgs;
pub use passes::replace_labels;
pub use reference::Reference;
pub use reference::ReferenceKind;
pub use trace::{Trace, TraceEvent};
pub use traits::*;
