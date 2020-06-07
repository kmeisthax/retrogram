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
    ///
    /// Equivalent to (true, true)
    Normal,

    /// Branching control flow.
    ///
    /// Instruction control diverges at this point with a number of possible
    /// continuing branches. The boolean parameter indicates whether or not the
    /// offset to the next instruction provided by the disassembly result is
    /// included as one of the possible branches.
    ///
    /// Equivalent to (true, false) or (false, false)
    Branching(bool),

    /// Returning control flow.
    ///
    /// Instruction control flow for this subroutine ends.
    ///
    /// Equivalent to (false, true)
    Returning,
}

impl Flow {
    pub fn is_final(self) -> bool {
        use Flow::*;

        match self {
            Normal => false,
            Branching(with_next) => !with_next,
            Returning => true,
        }
    }

    pub fn is_branching(self) -> bool {
        use Flow::*;

        match self {
            Normal => false,
            Branching(_) => true,
            Returning => false,
        }
    }

    /// Convert this flow type into a reference kind.
    ///
    /// Not all flow implies a cross reference; this function will yield `None`
    /// if so.
    pub fn as_reference_kind(self) -> Option<ReferenceKind> {
        use Flow::*;

        match self {
            Normal | Returning => None,
            Branching(true) => Some(ReferenceKind::Subroutine),
            Branching(false) => Some(ReferenceKind::Code),
        }
    }
}
