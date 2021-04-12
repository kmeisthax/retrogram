//! THUMB instruction set

mod dis;
mod instr;

pub use dis::disassemble;
pub use instr::ThumbInstruction;

pub const THUMB_STATE: &str = "T";
