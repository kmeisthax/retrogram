//! Operand AST type

use std::str;
use crate::ast::{Literal, Label};

#[derive(Clone, Debug)]
pub enum Operand<I, S, F, P> {
    /// The name of an architecturally defined register, or some derivative of
    /// that register, or another non-register operand defined by the
    /// architecture.
    Symbol(String),

    /// A literal constant value.
    Literal(Literal<I, S, F, P>),

    /// A reference to a user-defined label.
    Label(Label),

    /// An operand which constitutes a data reference.
    DataReference(Box<Operand<I, S, F, P>>),

    /// An operand which constitutes a code reference.
    CodeReference(Box<Operand<I, S, F, P>>),

    /// The indirection of the given operand. (e.g. HL to [HL])
    Indirect(Box<Operand<I, S, F, P>>),

    /// Some infix operand, e.g. +, * etc
    Infix(Box<Operand<I, S, F, P>>, String, Box<Operand<I, S, F, P>>),

    ///A symbol prefixed to an operand
    PrefixSymbol(String, Box<Operand<I, S, F, P>>),

    ///A symbol suffixed to an operand
    SuffixSymbol(Box<Operand<I, S, F, P>>, String),
    
    ///A symbol that wraps an operand
    WrapperSymbol(String, Vec<Operand<I, S, F, P>>, String),
}

impl<I, S, F, P> Operand<I, S, F, P> {
    pub fn sym(sym: &str) -> Self {
        Operand::Symbol(sym.to_string())
    }

    pub fn int<MI>(int: MI) -> Self where I: From<MI> {
        Operand::Literal(Literal::Integer(I::from(int)))
    }

    pub fn sint<MI>(int: MI) -> Self where S: From<MI> {
        Operand::Literal(Literal::SignedInteger(S::from(int)))
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
