//! Disassembly result type

use crate::{ast, analysis};

pub struct Disasm {
    instr: Option<ast::Instruction<I, SI, F, P>>,
    next_offset: S,
    flow: analysis::Flow,
    targets: Vec<analysis::Reference<P>>
}