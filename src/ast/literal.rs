//! AST type for literal values

use crate::memory;

///A literal value, such as an integer, pointer, or other kind of reference.
#[derive(Clone, Debug)]
pub enum Literal<I, S, F, P = I> {
    /// Unsigned integer constant
    Integer(I),

    /// Signed integer constant
    SignedInteger(S),

    /// Some kind of floating-point constant
    Float(F),

    /// Pointer constant to data (such as a global variable etc)
    Pointer(memory::Pointer<P>),

    /// Some kind of string constant
    String(String),

    /// A literal that could not be disassembled from a given image
    Missing,
}
