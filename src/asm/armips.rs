//! Assembler support and integration for ARMIPS

use crate::arch::aarch32::THUMB_STATE;
use crate::asm::traits::Assembler;
use crate::ast;
use crate::ast::{Instruction, Label, Operand};
use crate::memory::Pointer;
use std::fmt;
use std::io::{Result, Write};

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
