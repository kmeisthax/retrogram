//! Analysis of instruction control flow

use crate::analysis::ReferenceKind;

/// Indicates what kind of control flow a particular instruction involves.
///
/// Control flow determines in what order instructions are executed within a
/// subroutine. This implies the notion of a "next" instruction, present at the
/// pointer constructed from the given instruction offset, and one or more
/// possible alternative instructions. Execution within subroutines, as well as
/// interrupts, are not considered for control flow analysis.
#[derive(Copy, Clone, Debug)]
pub enum Flow {
    /// Normal control flow.
    ///
    /// Instruction control predictably flows from the current instruction to
    /// the following instruction.
    Normal,

    /// Branching control flow.
    ///
    /// Instruction control diverges at this point with a number of possible
    /// continuing branches. The boolean parameter indicates whether or not the
    /// branch is *conditional*: if true, then the branch may not be taken, and
    /// the offset to the next instruction provided by the disassembly result
    /// is implicitly included as one of the possible branches.
    Branching(bool),

    /// Calling control flow.
    ///
    /// Instruction control flow diverges to another subroutine, with the
    /// expectation that it will converge back to the next instruction after
    /// this one.
    Calling,

    /// Returning control flow.
    ///
    /// Instruction control flow for this subroutine ends. The boolean
    /// parameter indicates whether or not the return is *conditional*: if
    /// true, then the return may not be taken, and the offset to the next
    /// instruction is implicitly included as one of the possible branches.
    Returning(bool),
}

impl Flow {
    /// Indicate if this instruction prohibits execution of the following
    /// instruction in the stream.
    pub fn is_final(self) -> bool {
        use Flow::*;

        match self {
            Normal => false,
            Branching(conditional) => !conditional,
            Calling => false,
            Returning(conditional) => !conditional,
        }
    }

    /// Indicate if this instruction marks the end of the current block.
    ///
    /// A block consists of a run of instructions that always execute
    /// unconditionally. Instructions that end the run are said to be branching.
    pub fn is_branching(self) -> bool {
        use Flow::*;

        match self {
            Normal => false,
            Branching(_) => true,
            Calling => false,
            Returning(_) => true,
        }
    }

    /// Convert this flow type into a reference kind.
    ///
    /// Not all flow implies a cross reference; this function will yield `None`
    /// if so.
    pub fn as_reference_kind(self) -> Option<ReferenceKind> {
        use Flow::*;

        match self {
            Normal | Returning(_) => None,
            Calling => Some(ReferenceKind::Subroutine),
            Branching(_) => Some(ReferenceKind::Code),
        }
    }
}
