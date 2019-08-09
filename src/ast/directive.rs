//! Generic assembler directives for the AST

use crate::ast::{Instruction, Label};
use crate::memory::Pointer;

/// Represents a particular assembler directive.
/// 
/// An assembler directive consists of a particular command to the assembler
/// that either:
/// 
/// 1. Generates data, such as an instruction mnemonic or raw data stream
/// 2. Moves the assembled code's location around, or adds spaces to the stream
/// 3. Creates new labels in the generated assembly
#[derive(Clone, Debug)]
pub enum Directive<I, SI, F, P, MV, S> {
    /// Generate an instruction in the resulting instruction stream.
    /// 
    /// The offset parameter is the expected offset to the next instruction. If
    /// the expected offset does not match the PC of the next directive in the
    /// stream, then a `DeclareOrg` directive must be inserted in disassemblies.
    EmitInstr(Instruction<I, SI, F, P>, S),

    /// Generate raw data in the resulting instruction stream.
    EmitData(Vec<MV>),

    /// Declare an empty space of some size
    EmitSpace(S),

    /// Declare a new label.
    DeclareLabel(Label),

    /// Declare the location of future instructions.
    DeclareOrg(Pointer<P>),
    
    /// Declare a comment.
    DeclareComment(String)
}