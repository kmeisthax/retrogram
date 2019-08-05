//! Disassembly result type

use crate::{ast, analysis};

/// Representation of a static disassembly of a valid instruction.
/// 
/// A `Disasm` includes the following information:
/// 
///  * AST representation of the disassembled instruction, primarily intended
///    for user display.
///  * Offset to the next instruction. For most architectures, this is
///    equivalent to the length of the current one. For architectures with non-
///    arithmetic PCs, this is exclusively an offset to be added to obtain the
///    next instruction.
///  * Indication of the control flow of the given instruction.
///  * A list of references generated by this instruction.
/// 
/// It is only appropriate to return a `Disasm` if an actual disassembly was
/// made of a given instruction. An error type is provided for indicating that
/// disassembly failed for various possible reasons.
pub struct Disasm<I, SI, F, P, S> where P: analysis::Mappable {
    instr: ast::Instruction<I, SI, F, P>,
    next_offset: S,
    flow: analysis::Flow,
    targets: Vec<analysis::Reference<P>>
}

impl<I, SI, F, P, S> Disasm<I, SI, F, P, S> where P: analysis::Mappable {
    pub fn new(instr: ast::Instruction<I, SI, F, P>, next_offset: S, flow: analysis::Flow, targets: Vec<analysis::Reference<P>>) -> Self {
        Disasm {
            instr: instr,
            next_offset: next_offset,
            flow: flow,
            targets: targets
        }
    }
}