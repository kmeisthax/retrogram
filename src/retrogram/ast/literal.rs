///
pub enum Literal<I, F, P> {
    /// Some kind of integer constant
    Integer(I),

    /// Some kind of floating-point constant
    Float(F),

    /// Some kind of pointer constant
    Pointer(P),

    /// Some kind of string constant
    String(String)
}

pub enum Operand<L, S, R> {
    /// The name of an architecturally defined register
    Register(R),

    /// A non-register symbol otherwise defined by the instruction format
    /// (e.g. a condition code)
    Symbol(S),

    /// A literal value
    Literal(L),
}

pub struct Instruction<O, V> {
    opcode: O,
    values: Vec<V>
}

pub enum Statement<I> {
    Instruction(I),
    Label(String)
}

pub struct Line<S, P> {
    statement: S,
    prog_loc: P
}
