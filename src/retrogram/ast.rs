//!An abstract syntax tree representation of disassembled code

use std::fmt;

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
    String(String),

    /// A literal that could not be disassembled from a given image
    Missing
}

pub enum Operand<L = Literal> {
    /// The name of an architecturally defined register, or some derivative of
    /// that register, or another non-register operand defined by the
    /// architecture.
    Symbol(String),

    /// A literal constant value.
    Literal(L),

    /// The addition of two operands
    Add(Box<Operand<L>>, Box<Operand<L>>),

    //TODO: Symbolized memory references
}

impl<I, F, P> Operand<Literal<I, F, P>> {
    pub fn sym(sym: &str) -> Self {
        Operand::Symbol(sym.to_string())
    }

    pub fn int<MI>(int: MI) -> Self where I: From<MI> {
        Operand::Literal(Literal::Integer(I::from(int)))
    }

    pub fn float<MF>(flot: MF) -> Self where F: From<MF> {
        Operand::Literal(Literal::Float(F::from(flot)))
    }

    pub fn ptr<MP>(ptr: MP) -> Self where P: From<MP> {
        Operand::Literal(Literal::Pointer(P::from(ptr)))
    }

    pub fn str(s: &str) -> Self {
        Operand::Literal(Literal::String(s.to_string()))
    }

    pub fn miss() -> Self {
        Operand::Literal(Literal::Missing)
    }

    pub fn add(op1: Self, op2: Self) -> Self {
        Operand::Add(Box::new(op1), Box::new(op2))
    }
}

impl<I, F, P> fmt::Display for Operand<Literal<I, F, P>> where I: fmt::Display + fmt::LowerHex, F: fmt::Display, P: fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Symbol(s) => write!(f, "{}", s),
            Operand::Literal(Literal::Integer(i)) => write!(f, "${:x}", i),
            Operand::Literal(Literal::Float(fl)) => write!(f, "{}", fl),
            Operand::Literal(Literal::Pointer(p)) => write!(f, "{}", p),
            Operand::Literal(Literal::String(s)) => write!(f, "{}", s),
            Operand::Literal(Literal::Missing) => write!(f, "?"),
            Operand::Add(op1, op2) => write!(f, "{} + {}", op1, op2),
        }
    }
}

pub struct Instruction<L = Literal> {
    /// The instruction being executed
    opcode: String,
    /// Operands for the instruction, if any
    operands: Vec<Operand<L>>
}

impl<L> Instruction<L> {
    pub fn new(opcode: &str, operands: Vec<Operand<L>>) -> Self {
        Instruction {
            opcode: opcode.to_string(),
            operands: operands
        }
    }
}

impl<L> fmt::Display for Instruction<L> where Operand<L>: fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.operands.len() == 0 {
            write!(f, "{}", self.opcode)
        } else {
            let mut first = true;
            write!(f, "{} ", self.opcode)?;

            for operand in &self.operands {
                if first {
                    first = false;
                    write!(f, "{}", operand)?;
                    continue;
                }

                write!(f, ", {}", operand)?;
            }

            Ok(())
        }
    }
}

pub struct Line<L = Literal> {
    label: Option<String>,
    instruction: Instruction<L>,
    comment: Option<String>
}