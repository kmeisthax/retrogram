//!An abstract syntax tree representation of disassembled code

use std::{fmt, slice, str};
use crate::retrogram::memory;

///A literal value, such as an integer, pointer, or other kind of reference.
/// 
///Ordinarily, one would keep this enum as-is. The integer and float
///representations may be changed out for other representations, but doing so
///will define a different, potentially incompatible AST. (Besides, what
///architecture are you using that needs `u128` pointers? AS/400 TIMI doesn't
///count.)
#[derive(Clone, Debug)]
pub enum Literal<I, F, P = I> {
    /// Some kind of integer constant
    Integer(I),

    /// Some kind of floating-point constant
    Float(F),

    /// Pointer constant to data (such as a global variable etc)
    Pointer(P),

    /// Some kind of string constant
    String(String),

    /// A literal that could not be disassembled from a given image
    Missing
}

#[derive(Clone, Debug)]
pub enum Operand<I, F, P> {
    /// The name of an architecturally defined register, or some derivative of
    /// that register, or another non-register operand defined by the
    /// architecture.
    Symbol(String),

    /// A literal constant value.
    Literal(Literal<I, F, P>),

    /// A reference to a user-defined label.
    Label(Label),

    /// An operand which constitutes a data reference.
    DataReference(Box<Operand<I, F, P>>),

    /// An operand which constitutes a code reference.
    CodeReference(Box<Operand<I, F, P>>),

    /// The indirection of the given operand. (e.g. HL to [HL])
    Indirect(Box<Operand<I, F, P>>),

    /// The addition of two operands
    Add(Box<Operand<I, F, P>>, Box<Operand<I, F, P>>),

    //TODO: Symbolized memory references
}

impl<I, F, P> Operand<I, F, P> {
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
        Operand::DataReference(Box::new(Operand::Literal(Literal::Pointer(P::from(ptr)))))
    }

    pub fn cptr<MP>(ptr: MP) -> Self where P: From<MP> {
        Operand::CodeReference(Box::new(Operand::Literal(Literal::Pointer(P::from(ptr)))))
    }

    pub fn dlbl(label: Label) -> Self {
        Operand::DataReference(Box::new(Operand::Label(label)))
    }

    pub fn clbl(label: Label) -> Self {
        Operand::CodeReference(Box::new(Operand::Label(label)))
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

impl<I, F, P> fmt::Display for Operand<I, F, P> where I: fmt::Display, F: fmt::Display, P: fmt::Display + fmt::LowerHex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Symbol(s) => write!(f, "{}", s),
            Operand::Literal(Literal::Integer(i)) => write!(f, "{}", i),
            Operand::Literal(Literal::Float(fl)) => write!(f, "{}", fl),
            Operand::Literal(Literal::Pointer(p)) => write!(f, "${:x}", p),
            Operand::Literal(Literal::String(s)) => write!(f, "{}", s),
            Operand::Literal(Literal::Missing) => write!(f, "?"),
            Operand::Label(lbl) if lbl.parent_name == None => write!(f, "{}", lbl.name),
            Operand::Label(lbl) => write!(f, ".{}", lbl.name),
            Operand::DataReference(op) => write!(f, "{}", op),
            Operand::CodeReference(op) => write!(f, "{}", op),
            Operand::Indirect(op) => write!(f, "[{}]", op),
            Operand::Add(op1, op2) => write!(f, "{} + {}", op1, op2),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Instruction<I, F, P> {
    /// The instruction being executed
    opcode: String,
    /// Operands for the instruction, if any
    operands: Vec<Operand<I, F, P>>
}

impl<I, F, P> Instruction<I, F, P> {
    pub fn new(opcode: &str, operands: Vec<Operand<I, F, P>>) -> Self {
        Instruction {
            opcode: opcode.to_string(),
            operands: operands
        }
    }

    pub fn opcode(&self) -> &String {
        &self.opcode
    }

    pub fn iter_operands(&self) -> slice::Iter<Operand<I, F, P>> {
        self.operands.iter()
    }
}

impl<I, F, P> fmt::Display for Instruction<I, F, P> where Operand<I, F, P>: fmt::Display {
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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Label {
    /// Name of the label.
    name: String,

    /// Name of the parent label (if any).
    /// If None, then the label is global.
    parent_name: Option<String>
}

impl Label {
    pub fn new(name: &str, parent_name: Option<&str>) -> Label {
        Label {
            name: name.to_string(),
            parent_name: parent_name.map(|s| s.to_string())
        }
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    pub fn parent_name(&self) -> Option<&String> {
        if let Some(ref parent_label) = self.parent_name {
            Some(&parent_label)
        } else {
            None
        }
    }
}

impl str::FromStr for Label {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split(".");
        let maybe_parent = split.next();
        let maybe_child = split.next();

        match (maybe_parent, maybe_child) {
            (Some(parent), Some(child)) => Ok(Label {
                name: child.to_string(),
                parent_name: Some(parent.to_string())
            }),
            (Some(parent), None) => Ok(Label {
                name: parent.to_string(),
                parent_name: None
            }),
            _ => Err(())
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(_parent_label) = &self.parent_name {
            write!(f, ".{}:", self.name)
        } else {
            write!(f, "{}:", self.name)
        }
    }
}

#[derive(Clone, Debug)]
pub struct Line<I = u64, F = f64, P = I> {
    label: Option<Label>,
    instruction: Option<Instruction<I, F, P>>,
    comment: Option<String>,
    source_address: memory::Pointer<P>,
}

impl<I, F, P> fmt::Display for Line<I, F, P> where Instruction<I, F, P>: fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref label) = self.label {
            write!(f, "{} ", label)?;
        }

        if let Some(ref instruction) = self.instruction {
            write!(f, "{} ", instruction)?;
        }

        if let Some(ref comment) = self.comment {
            write!(f, ";{}", comment)?;
        }

        write!(f, "\n")
    }
}

impl<I, F, P> Line<I, F, P> {
    pub fn new(label: Option<Label>, instruction: Option<Instruction<I, F, P>>, comment: Option<String>, source_address: memory::Pointer<P>) -> Self {
        Line {
            label: label,
            instruction: instruction,
            comment: comment,
            source_address: source_address
        }
    }
    
    pub fn label(&self) -> Option<&Label> {
        if let Some(ref label) = self.label {
            Some(label)
        } else {
            None
        }
    }

    pub fn instr(&self) -> Option<&Instruction<I, F, P>> {
        if let Some(ref instruction) = self.instruction {
            Some(instruction)
        } else {
            None
        }
    }

    pub fn comment(&self) -> Option<&String> {
        if let Some(ref comment) = self.comment {
            Some(comment)
        } else {
            None
        }
    }

    pub fn source_address(&self) -> &memory::Pointer<P> {
        &self.source_address
    }

    pub fn into_parts(self) -> (Option<Label>, Option<Instruction<I, F, P>>, Option<String>, memory::Pointer<P>) {
        (self.label, self.instruction, self.comment, self.source_address)
    }
}

#[derive(Clone, Debug)]
pub struct Assembly<I = u64, F = f64, P = I> {
    lines: Vec<Line<I, F, P>>
}

impl<I, F, P> Assembly<I, F, P> {
    pub fn new() -> Self {
        Assembly {
            lines: Vec::new()
        }
    }

    pub fn iter_lines(&self) -> slice::Iter<Line<I, F, P>> {
        self.lines.iter()
    }

    pub fn append_line(&mut self, line: Line<I, F, P>) {
        self.lines.push(line);
    }
}

impl<I, F, P> fmt::Display for Assembly<I, F, P> where Line<I, F, P>: fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for line in self.iter_lines() {
            write!(f, "{}", line)?;
        }

        Ok(())
    }
}