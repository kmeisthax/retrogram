//! Assembler support and integration for ARMIPS

use crate::arch::aarch32::THUMB_STATE;
use crate::ast;
use crate::memory::Pointer;
use std::{cmp, fmt};

/// Valid literal values for the ARMIPS assembler.
#[derive(Clone, Debug)]
pub enum Literal {
    Integer(u32),
    SignedInteger(i32),
    Float(f64),
    Pointer(Pointer<u32>),
    String(String),
}

impl ast::Literal for Literal {
    type PtrVal = u32;

    fn is_pointer(&self) -> bool {
        match self {
            Literal::Pointer(_) => true,
            _ => false,
        }
    }

    fn into_pointer(self) -> Option<Pointer<Self::PtrVal>> {
        match self {
            Literal::Pointer(p) => Some(p),
            _ => None,
        }
    }
}

impl From<u8> for Literal {
    fn from(v: u8) -> Self {
        Self::Integer(v.into())
    }
}

impl From<u16> for Literal {
    fn from(v: u16) -> Self {
        Self::Integer(v.into())
    }
}

impl From<u32> for Literal {
    fn from(v: u32) -> Self {
        Self::Integer(v)
    }
}

impl From<i32> for Literal {
    fn from(v: i32) -> Self {
        Self::SignedInteger(v)
    }
}

impl From<f64> for Literal {
    fn from(v: f64) -> Self {
        Self::Float(v)
    }
}

impl From<Pointer<u32>> for Literal {
    fn from(v: Pointer<u32>) -> Self {
        Self::Pointer(v)
    }
}

impl From<&str> for Literal {
    fn from(v: &str) -> Self {
        Self::String(v.to_string())
    }
}

pub struct OperandFmtWrap<'a> {
    tree: &'a ast::Operand<Literal>,
}

impl<'a> OperandFmtWrap<'a> {
    pub fn wrap(tree: &'a ast::Operand<Literal>) -> Self {
        OperandFmtWrap { tree }
    }
}

impl<'a> OperandFmtWrap<'a> {
    fn write_operand(
        &self,
        operand: &ast::Operand<Literal>,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        match operand {
            ast::Operand::Symbol(s) => write!(f, "{}", s)?,
            ast::Operand::Literal(Literal::Integer(i)) => write!(f, "{}", i)?,
            ast::Operand::Literal(Literal::SignedInteger(i)) => write!(f, "{}", i)?,
            ast::Operand::Literal(Literal::Float(fl)) => write!(f, "{}", fl)?,
            ast::Operand::Literal(Literal::Pointer(p)) => write!(f, "0x{:x}", p.as_pointer())?,
            ast::Operand::Literal(Literal::String(s)) => write!(f, "{}", s)?,
            ast::Operand::Missing => write!(f, "?")?,
            ast::Operand::Label(lbl) if lbl.parent_name() == None => write!(f, "{}", lbl.name())?,
            ast::Operand::Label(lbl) => write!(f, "@@{}", lbl.name())?,
            ast::Operand::DataReference(op) => self.write_operand(&op, f)?,
            ast::Operand::CodeReference(op) => self.write_operand(&op, f)?,
            ast::Operand::Indirect(op) => {
                write!(f, "[")?;
                self.write_operand(&op, f)?;
                write!(f, "]")?;
            }
            ast::Operand::Infix(op1, infix_sym, op2) => {
                self.write_operand(&op1, f)?;
                write!(f, " {} ", infix_sym)?;
                self.write_operand(&op2, f)?;
            }
            ast::Operand::PrefixSymbol(s, op) => {
                write!(f, "{} ", s)?;
                self.write_operand(&op, f)?;
            }
            ast::Operand::SuffixSymbol(op, s) => {
                self.write_operand(&op, f)?;
                write!(f, " {}", s)?;
            }
            ast::Operand::WrapperSymbol(s1, ops, s2) => {
                write!(f, "{}", s1)?;

                let mut first = true;
                for op in ops {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }

                    self.write_operand(&op, f)?;
                }

                write!(f, "{}", s2)?;
            }
        };

        Ok(())
    }
}

impl<'a> fmt::Display for OperandFmtWrap<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.write_operand(self.tree, f)
    }
}

pub struct InstrFmtWrap<'a> {
    tree: &'a ast::Instruction<Literal>,
}

impl<'a> InstrFmtWrap<'a> {
    pub fn wrap(tree: &'a ast::Instruction<Literal>) -> Self {
        InstrFmtWrap { tree }
    }
}

pub fn format_instr(tree: &ast::Instruction<Literal>) -> String {
    format!("{}", InstrFmtWrap::wrap(tree))
}

impl<'a> fmt::Display for InstrFmtWrap<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.tree.opcode())?;

        let mut has_written_operand = false;
        for operand in self.tree.iter_operands() {
            if !has_written_operand {
                write!(f, " ")?;
                has_written_operand = true;
            } else {
                write!(f, ", ")?;
            }

            write!(f, "{}", OperandFmtWrap::wrap(&operand))?;
        }

        Ok(())
    }
}

pub struct SectionFmtWrap<'a, P, MV, S> {
    tree: &'a ast::Section<Literal, P, MV, S>,
}

impl<'a, P, MV, S> SectionFmtWrap<'a, P, MV, S> {
    pub fn wrap(tree: &'a ast::Section<Literal, P, MV, S>) -> Self {
        SectionFmtWrap { tree }
    }
}

impl<'a, P, MV, S> fmt::Display for SectionFmtWrap<'a, P, MV, S>
where
    P: Clone + From<u16> + fmt::Display + cmp::PartialOrd + fmt::LowerHex + fmt::UpperHex,
    MV: fmt::UpperHex,
    S: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (directive, _loc) in self.tree.iter_directives() {
            match directive {
                ast::Directive::DeclareComment(comment) => write!(f, ";{}", comment)?,
                ast::Directive::DeclareLabel(label) => {
                    if let Some(_parent_label) = label.parent_name() {
                        writeln!(f, "@@{}:", label.name())?;
                    } else {
                        writeln!(f, "{}:", label.name())?;
                    }
                }
                ast::Directive::DeclareOrg(loc) => {
                    let thumb_state = loc.get_arch_context(THUMB_STATE).into_concrete();

                    writeln!(f, ".org 0x{:X}", loc.as_pointer())?;
                    match thumb_state {
                        Some(0) => writeln!(f, ".arm")?,
                        Some(1) => writeln!(f, ".thumb")?,
                        _ => {}
                    }
                }
                ast::Directive::EmitData(data) => {
                    if data.is_empty() {
                        write!(f, ".db ")?;

                        for byte in data {
                            write!(f, "0x{:X}", byte)?;
                        }

                        writeln!(f)?;
                    }
                }
                ast::Directive::EmitInstr(instr, _) => {
                    writeln!(f, "    {}", InstrFmtWrap::wrap(instr))?
                }
                ast::Directive::EmitSpace(offset) => writeln!(f, ".skip {}", offset)?,
            }
        }

        Ok(())
    }
}

pub fn format_section<'a, P, MV, S>(tree: &'a ast::Section<Literal, P, MV, S>) -> String
where
    P: Clone + From<u16> + fmt::Display + cmp::PartialOrd + fmt::LowerHex + fmt::UpperHex,
    MV: fmt::UpperHex,
    S: fmt::Display,
{
    format!("{}", SectionFmtWrap::wrap(tree))
}
