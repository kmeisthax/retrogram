///A literal value, such as an integer, pointer, or other kind of reference.
/// 
///Ordinarily, one would keep this enum as-is. The integer and float
///representations may be changed out for other representations, but doing so
///will define a different, potentially incompatible AST. (Besides, what
///architecture are you using that needs `u128` pointers? AS/400 TIMI doesn't
///count.)
pub enum Literal<I = u64, F = f64, P = I> {
    /// Some kind of integer constant
    Integer(I),

    /// Some kind of floating-point constant
    Float(F),

    /// Some kind of pointer constant
    Pointer(P),

    /// Some kind of string constant
    String(String)
}

pub enum Operand<L = Literal> {
    /// The name of an architecturally defined register, or some derivative of
    /// that register, or another non-register operand defined by the
    /// architecture.
    Symbol(String),

    /// A literal constant value.
    Literal(L),

    //TODO: Symbolized memory references
}

pub struct Instruction<L = Literal> {
    /// The instruction being executed
    opcode: String,
    /// Operands for the instruction, if any
    operands: Vec<Operand<L>>
}

pub struct Line<L = Literal> {
    label: Option<String>,
    instruction: Instruction<L>,
    comment: Option<String>
}
