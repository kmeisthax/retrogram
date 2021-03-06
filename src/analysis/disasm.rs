//! Disassembly result type

use crate::analysis::{Flow, Mappable, Reference};
use crate::ast::{Directive, Instruction, Literal};
use crate::cli::Nameable;

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
pub struct Disasm<L, P, S>
where
    L: Literal,
    P: Mappable + Nameable,
{
    instr: Instruction<L>,
    next_offset: S,
    flow: Flow,
    targets: Vec<Reference<P>>,
}

impl<L, P, S> Disasm<L, P, S>
where
    L: Literal,
    P: Mappable + Nameable,
{
    pub fn new(
        instr: Instruction<L>,
        next_offset: S,
        flow: Flow,
        targets: Vec<Reference<P>>,
    ) -> Self {
        Disasm {
            instr,
            next_offset,
            flow,
            targets,
        }
    }

    pub fn as_instr(&self) -> &Instruction<L> {
        &self.instr
    }

    pub fn iter_targets(&self) -> impl Iterator<Item = &Reference<P>> {
        self.targets.iter()
    }

    pub fn flow(&self) -> Flow {
        self.flow
    }
}

impl<L, P, S> Disasm<L, P, S>
where
    L: Literal,
    P: Mappable + Nameable,
    S: Clone,
{
    pub fn next_offset(&self) -> S {
        self.next_offset.clone()
    }
}

impl<L, P, S> Disasm<L, P, S>
where
    L: Literal,
    P: Mappable + Nameable,
    Instruction<L>: Clone,
    S: Clone,
{
    pub fn directive<MV>(&self) -> Directive<L, P, MV, S> {
        Directive::EmitInstr(self.instr.clone(), self.next_offset.clone())
    }
}
