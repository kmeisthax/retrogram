//! Assembler support and integration for ARMIPS

use crate::arch::aarch32::THUMB_STATE;
use crate::asm::traits::Assembler;
use crate::ast;
use crate::ast::{Instruction, Label, Operand};
use crate::memory::Pointer;
use std::io::{Result, Write};
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
                    if !data.is_empty() {
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

#[derive(Copy, Clone)]
pub struct ARMIPS();

impl Assembler for ARMIPS {
    type Literal = Literal;

    fn emit_comment(&self, stream: &mut dyn Write, comment_data: &str) -> Result<()> {
        write!(stream, ";{}", comment_data)
    }

    fn emit_org(
        &self,
        stream: &mut dyn Write,
        _section_name: &str,
        where_to: &Pointer<<Self::Literal as ast::Literal>::PtrVal>,
    ) -> Result<()> {
        let thumb_state = where_to.get_arch_context(THUMB_STATE).into_concrete();

        writeln!(stream, ".org 0x{:X}", where_to.as_pointer())?;
        match thumb_state {
            Some(0) => writeln!(stream, ".arm")?,
            Some(1) => writeln!(stream, ".thumb")?,
            _ => {}
        };

        Ok(())
    }

    fn emit_label_decl(&self, stream: &mut dyn Write, label: &Label) -> Result<()> {
        if let Some(_parent_label) = label.parent_name() {
            writeln!(stream, "@@{}:", label.name())?;
        } else {
            writeln!(stream, "{}:", label.name())?;
        }

        Ok(())
    }

    fn emit_space<S>(&self, stream: &mut dyn Write, space: S) -> Result<()>
    where
        S: fmt::Display,
    {
        writeln!(stream, ".skip {}", space)
    }

    fn emit_data<MV>(&self, stream: &mut dyn Write, data: &[MV]) -> Result<()>
    where
        MV: fmt::Display + fmt::UpperHex + fmt::LowerHex,
    {
        if !data.is_empty() {
            write!(stream, ".db ")?;

            for byte in data {
                write!(stream, "0x{:X}", byte)?;
            }

            writeln!(stream)?;
        }

        Ok(())
    }

    fn emit_instr_start(
        &self,
        stream: &mut dyn Write,
        _instr: &Instruction<Self::Literal>,
    ) -> Result<()> {
        write!(stream, "    ")
    }

    fn emit_instr_opcode(&self, stream: &mut dyn Write, opcode: &str) -> Result<()> {
        write!(stream, "{}", opcode)
    }

    fn emit_operand_start(
        &self,
        stream: &mut dyn Write,
        _instr: &Instruction<Self::Literal>,
        operand_index: usize,
    ) -> Result<()> {
        if operand_index == 0 {
            write!(stream, " ")?;
        } else {
            write!(stream, ", ")?;
        }

        Ok(())
    }

    fn emit_symbol(&self, stream: &mut dyn Write, symbol: &str) -> Result<()> {
        write!(stream, "{}", symbol)
    }

    fn emit_literal(&self, stream: &mut dyn Write, literal: &Self::Literal) -> Result<()> {
        match literal {
            Literal::Integer(i) => write!(stream, "{}", i)?,
            Literal::SignedInteger(i) => write!(stream, "{}", i)?,
            Literal::Float(fl) => write!(stream, "{}", fl)?,
            Literal::Pointer(p) => write!(stream, "0x{:x}", p.as_pointer())?,
            Literal::String(s) => write!(stream, "{}", s)?,
        };

        Ok(())
    }

    fn emit_missing_operand(&self, stream: &mut dyn Write) -> Result<()> {
        write!(stream, "?")
    }

    fn emit_label_operand(&self, stream: &mut dyn Write, label: &Label) -> Result<()> {
        if label.parent_name().is_none() {
            write!(stream, "{}", label.name())
        } else {
            write!(stream, "@@{}", label.name())
        }
    }

    fn emit_operand_wrapper_start(
        &self,
        stream: &mut dyn Write,
        operand: &Operand<Self::Literal>,
    ) -> Result<()> {
        match operand {
            Operand::DataReference(_)
            | Operand::CodeReference(_)
            | Operand::Symbol(_)
            | Operand::Literal(_)
            | Operand::Missing
            | Operand::Label(_)
            | Operand::Infix(_, _, _)
            | Operand::SuffixSymbol(_, _) => Ok(()),
            Operand::Indirect(_op) => write!(stream, "["),
            Operand::PrefixSymbol(s, _op) => write!(stream, "{}", s),
            Operand::WrapperSymbol(s1, _ops, _s2) => write!(stream, "{}", s1),
        }
    }

    fn emit_operand_wrapper_infix(
        &self,
        stream: &mut dyn Write,
        operand: &Operand<Self::Literal>,
        _index: usize,
    ) -> Result<()> {
        match operand {
            Operand::DataReference(_)
            | Operand::CodeReference(_)
            | Operand::Symbol(_)
            | Operand::Literal(_)
            | Operand::Missing
            | Operand::Indirect(_)
            | Operand::Label(_)
            | Operand::PrefixSymbol(_, _)
            | Operand::SuffixSymbol(_, _) => Ok(()),
            Operand::Infix(_, infix_sym, _) => write!(stream, "{}", infix_sym),
            Operand::WrapperSymbol(_s1, _ops, _s2) => write!(stream, ", "),
        }
    }

    fn emit_operand_wrapper_end(
        &self,
        stream: &mut dyn Write,
        operand: &Operand<Self::Literal>,
    ) -> Result<()> {
        match operand {
            Operand::DataReference(_)
            | Operand::CodeReference(_)
            | Operand::Symbol(_)
            | Operand::Literal(_)
            | Operand::Missing
            | Operand::Label(_)
            | Operand::Infix(_, _, _)
            | Operand::PrefixSymbol(_, _) => Ok(()),
            Operand::Indirect(_op) => write!(stream, "]"),
            Operand::SuffixSymbol(_op, s) => write!(stream, "{}", s),
            Operand::WrapperSymbol(s1, _ops, _s2) => write!(stream, "{}", s1),
        }
    }

    fn emit_operand_end(
        &self,
        _stream: &mut dyn Write,
        _instr: &Instruction<Self::Literal>,
        _operand_index: usize,
    ) -> Result<()> {
        Ok(())
    }

    fn emit_instr_end(
        &self,
        stream: &mut dyn Write,
        _instr: &Instruction<Self::Literal>,
    ) -> Result<()> {
        writeln!(stream)
    }
}
