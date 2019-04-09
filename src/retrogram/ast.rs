//!An abstract syntax tree representation of disassembled code

use std::fmt;

///A literal value, such as an integer, pointer, or other kind of reference.
/// 
///Ordinarily, one would keep this enum as-is. The integer and float
///representations may be changed out for other representations, but doing so
///will define a different, potentially incompatible AST. (Besides, what
///architecture are you using that needs `u128` pointers? AS/400 TIMI doesn't
///count.)
#[derive(Clone)]
pub enum Literal<I = u64, F = f64, P = I> {
    /// Some kind of integer constant
    Integer(I),

    /// Some kind of floating-point constant
    Float(F),

    /// Pointer constant to data (such as a global variable etc)
    DataPtr(P),

    /// Pointer to code
    CodePtr(P),

    /// Some kind of string constant
    String(String),

    /// A literal that could not be disassembled from a given image
    Missing
}

#[derive(Clone)]
pub enum Operand<L = Literal> {
    /// The name of an architecturally defined register, or some derivative of
    /// that register, or another non-register operand defined by the
    /// architecture.
    Symbol(String),

    /// A literal constant value.
    Literal(L),

    /// The indirection of the given operand. (e.g. HL to [HL])
    Indirect(Box<Operand<L>>),

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

    pub fn dptr<MP>(ptr: MP) -> Self where P: From<MP> {
        Operand::Literal(Literal::DataPtr(P::from(ptr)))
    }

    pub fn cptr<MP>(ptr: MP) -> Self where P: From<MP> {
        Operand::Literal(Literal::CodePtr(P::from(ptr)))
    }

    pub fn str(s: &str) -> Self {
        Operand::Literal(Literal::String(s.to_string()))
    }

    pub fn miss() -> Self {
        Operand::Literal(Literal::Missing)
    }

    pub fn indir(op: Self) -> Self {
        Operand::Indirect(Box::new(op))
    }

    pub fn add(op1: Self, op2: Self) -> Self {
        Operand::Add(Box::new(op1), Box::new(op2))
    }
}

impl<I, F, P> fmt::Display for Operand<Literal<I, F, P>> where I: fmt::Display, F: fmt::Display, P: fmt::Display + fmt::LowerHex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Symbol(s) => write!(f, "{}", s),
            Operand::Literal(Literal::Integer(i)) => write!(f, "{}", i),
            Operand::Literal(Literal::Float(fl)) => write!(f, "{}", fl),
            Operand::Literal(Literal::DataPtr(p)) => write!(f, "${:x}", p),
            Operand::Literal(Literal::CodePtr(p)) => write!(f, "${:x}", p),
            Operand::Literal(Literal::String(s)) => write!(f, "{}", s),
            Operand::Literal(Literal::Missing) => write!(f, "?"),
            Operand::Indirect(op) => write!(f, "[{}]", op),
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

pub struct Label {
    /// Name of the label.
    name: String,

    /// Whether or not the label can be referred to outside of it's own local
    /// block. All global labels must have a unique name.
    global: bool,
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.global {
            write!(f, "{}:", self.name)
        } else {
            write!(f, ".{}", self.name)
        }
    }
}

pub struct Line<L = Literal> {
    label: Option<Label>,
    instruction: Option<Instruction<L>>,
    comment: Option<String>
}

impl<L> fmt::Display for Line<L> where Instruction<L>: fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref label) = self.label {
            write!(f, "{}", label)?;
        }

        if let Some(ref instruction) = self.instruction {
            write!(f, "{}", instruction)?;
        }

        if let Some(ref comment) = self.comment {
            write!(f, ";{}", comment)?;
        }

        write!(f, "\n")
    }
}