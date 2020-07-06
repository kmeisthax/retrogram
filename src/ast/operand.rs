//! Operand AST type

use crate::ast::{Label, Literal};
use std::str;

#[derive(Clone, Debug)]
pub enum Operand<L>
where
    L: Literal,
{
    /// The name of an architecturally defined register, or some derivative of
    /// that register, or another non-register operand defined by the
    /// architecture.
    Symbol(String),

    /// A literal constant value.
    Literal(L),

    /// An unparsable literal value.
    Missing,

    /// A reference to a user-defined label.
    Label(Label),

    /// An operand which constitutes a data reference.
    DataReference(Box<Operand<L>>),

    /// An operand which constitutes a code reference.
    CodeReference(Box<Operand<L>>),

    /// The indirection of the given operand. (e.g. HL to [HL])
    Indirect(Box<Operand<L>>),

    /// Some infix operand, e.g. +, * etc
    Infix(Box<Operand<L>>, String, Box<Operand<L>>),

    ///A symbol prefixed to an operand
    PrefixSymbol(String, Box<Operand<L>>),

    ///A symbol suffixed to an operand
    SuffixSymbol(Box<Operand<L>>, String),

    ///A symbol that wraps an operand
    WrapperSymbol(String, Vec<Operand<L>>, String),
}

impl<L> Operand<L>
where
    L: Literal,
{
    pub fn sym(sym: &str) -> Self {
        Operand::Symbol(sym.to_string())
    }

    pub fn lit<LV>(v: LV) -> Self
    where
        LV: Into<L>,
    {
        Operand::Literal(v.into())
    }

    pub fn dptr<LV>(v: LV) -> Self
    where
        LV: Into<L>,
    {
        Operand::DataReference(Box::new(Operand::Literal(v.into())))
    }

    pub fn cptr<LV>(v: LV) -> Self
    where
        LV: Into<L>,
    {
        Operand::CodeReference(Box::new(Operand::Literal(v.into())))
    }

    pub fn dlbl(label: Label) -> Self {
        Operand::DataReference(Box::new(Operand::Label(label)))
    }

    pub fn clbl(label: Label) -> Self {
        Operand::CodeReference(Box::new(Operand::Label(label)))
    }

    pub fn miss() -> Self {
        Operand::Missing
    }

    pub fn indir(op: Self) -> Self {
        Operand::Indirect(Box::new(op))
    }

    pub fn addop(op1: Self, op2: Self) -> Self {
        Operand::Infix(Box::new(op1), "+".to_string(), Box::new(op2))
    }

    pub fn infix(op1: Self, infix_sym: &str, op2: Self) -> Self {
        Operand::Infix(Box::new(op1), infix_sym.to_string(), Box::new(op2))
    }

    pub fn pref(sym: &str, op: Self) -> Self {
        Operand::PrefixSymbol(sym.to_string(), Box::new(op))
    }

    pub fn suff(op: Self, sym: &str) -> Self {
        Operand::SuffixSymbol(Box::new(op), sym.to_string())
    }

    pub fn wrap(sym1: &str, ops: Vec<Self>, sym2: &str) -> Self {
        Operand::WrapperSymbol(sym1.to_string(), ops, sym2.to_string())
    }
}
