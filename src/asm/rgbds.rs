//! Assembler support and integration for rgbds.

use crate::asm::traits::Assembler;
use crate::ast;
use crate::ast::{Instruction, Label, Operand};
use crate::memory::Pointer;
use std::fmt;
use std::io::{Result, Write};

/// Valid literal values for the RGBDS assembler.
#[derive(Clone, Debug)]
pub enum Literal {
    Integer(u16),
    SignedInteger(i16),
    Pointer(Pointer<u16>),
    String(String),
}

impl ast::Literal for Literal {
    type PtrVal = u16;

    fn is_pointer(&self) -> bool {
        matches!(self, Literal::Pointer(_))
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
        Self::Integer(v)
    }
}

impl From<i8> for Literal {
    fn from(v: i8) -> Self {
        Self::SignedInteger(v.into())
    }
}

impl From<i16> for Literal {
    fn from(v: i16) -> Self {
        Self::SignedInteger(v)
    }
}

impl From<Pointer<u16>> for Literal {
    fn from(v: Pointer<u16>) -> Self {
        Self::Pointer(v)
    }
}

impl From<&str> for Literal {
    fn from(v: &str) -> Self {
        Self::String(v.to_string())
    }
}

#[derive(Copy, Clone)]
pub struct RGBDS();

impl Assembler for RGBDS {
    type Literal = Literal;

    fn emit_comment(&self, stream: &mut dyn Write, comment_data: &str) -> Result<()> {
        write!(stream, ";{}", comment_data)
    }

    fn emit_org(
        &self,
        stream: &mut dyn Write,
        section_name: &str,
        where_to: &Pointer<<Self::Literal as ast::Literal>::PtrVal>,
    ) -> Result<()> {
        if *where_to.as_pointer() < 0x4000_u16 {
            writeln!(
                stream,
                "SECTION \"{}\", ROM0[${:X}]",
                section_name,
                where_to.as_pointer()
            )
        } else if *where_to.as_pointer() < 0x8000_u16 {
            if let Some(bank) = where_to.get_platform_context("R").into_concrete() {
                writeln!(
                    stream,
                    "SECTION \"{}\", ROMX[${:X}], BANK[${:X}]",
                    section_name,
                    where_to.as_pointer(),
                    bank
                )
            } else {
                writeln!(
                    stream,
                    "SECTION \"{}\", ROMX[${:X}]",
                    section_name,
                    where_to.as_pointer()
                )
            }
        } else if *where_to.as_pointer() < 0xA000_u16 {
            if let Some(bank) = where_to.get_platform_context("V").into_concrete() {
                writeln!(
                    stream,
                    "SECTION \"{}\", VRAM[${:X}], BANK[${:X}]",
                    section_name,
                    where_to.as_pointer(),
                    bank
                )
            } else {
                writeln!(
                    stream,
                    "SECTION \"{}\", VRAM[${:X}]",
                    section_name,
                    where_to.as_pointer()
                )
            }
        } else if *where_to.as_pointer() < 0xC000_u16 {
            if let Some(bank) = where_to.get_platform_context("S").into_concrete() {
                writeln!(
                    stream,
                    "SECTION \"{}\", SRAM[${:X}], BANK[${:X}]",
                    section_name,
                    where_to.as_pointer(),
                    bank
                )
            } else {
                writeln!(
                    stream,
                    "SECTION \"{}\", SRAM[${:X}]",
                    section_name,
                    where_to.as_pointer()
                )
            }
        } else if *where_to.as_pointer() < 0xD000_u16 {
            writeln!(
                stream,
                "SECTION \"{}\", WRAM0[${:X}]",
                section_name,
                where_to.as_pointer()
            )
        } else if *where_to.as_pointer() < 0xE000_u16 {
            if let Some(bank) = where_to.get_platform_context("W").into_concrete() {
                writeln!(
                    stream,
                    "SECTION \"{}\", WRAMX[${:X}], BANK[${:X}]",
                    section_name,
                    where_to.as_pointer(),
                    bank
                )
            } else {
                writeln!(
                    stream,
                    "SECTION \"{}\", WRAMX[${:X}]",
                    section_name,
                    where_to.as_pointer()
                )
            }
        } else {
            writeln!(
                stream,
                "SECTION \"{}\", HRAM[${:X}]",
                section_name,
                where_to.as_pointer()
            )
        }
    }

    fn emit_label_decl(&self, stream: &mut dyn Write, label: &Label) -> Result<()> {
        if let Some(_parent_label) = label.parent_name() {
            writeln!(stream, ".{}:", label.name())
        } else {
            writeln!(stream, "{}:", label.name())
        }
    }

    fn emit_space<S>(&self, stream: &mut dyn Write, space: S) -> Result<()>
    where
        S: fmt::Display,
    {
        write!(stream, "    ds {}", space)
    }

    fn emit_data<MV>(&self, stream: &mut dyn Write, data: &[MV]) -> Result<()>
    where
        MV: fmt::Display + fmt::UpperHex + fmt::LowerHex,
    {
        if !data.is_empty() {
            write!(stream, "    db ")?;

            for byte in data {
                write!(stream, "${:X}", byte)?;
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
            write!(stream, " ")
        } else {
            write!(stream, ", ")
        }
    }

    fn emit_symbol(&self, stream: &mut dyn Write, symbol: &str) -> Result<()> {
        write!(stream, "{}", symbol)
    }

    fn emit_literal(&self, stream: &mut dyn Write, literal: &Self::Literal) -> Result<()> {
        match literal {
            Literal::Integer(i) => write!(stream, "{}", i),
            Literal::SignedInteger(i) => write!(stream, "{}", i),
            Literal::Pointer(p) => write!(stream, "${:x}", p),
            Literal::String(s) => write!(stream, "{}", s),
        }
    }

    fn emit_missing_operand(&self, stream: &mut dyn Write) -> Result<()> {
        write!(stream, "?")
    }

    fn emit_label_operand(&self, stream: &mut dyn Write, label: &Label) -> Result<()> {
        if label.parent_name().is_none() {
            write!(stream, "{}", label.name())
        } else {
            write!(stream, ".{}", label.name())
        }
    }

    fn emit_operand_wrapper_start(
        &self,
        stream: &mut dyn Write,
        operand: &Operand<Self::Literal>,
    ) -> Result<()> {
        match operand {
            Operand::Symbol(_)
            | Operand::Literal(_)
            | Operand::Missing
            | Operand::Label(_)
            | Operand::DataReference(_)
            | Operand::CodeReference(_)
            | Operand::Infix(_, _, _)
            | Operand::SuffixSymbol(_, _) => Ok(()),
            Operand::Indirect(_op) => write!(stream, "["),
            Operand::PrefixSymbol(s, _op) => write!(stream, "{}", s),
            Operand::WrapperSymbol(s1, _op, _s2) => write!(stream, "{}", s1),
        }
    }

    fn emit_operand_wrapper_infix(
        &self,
        stream: &mut dyn Write,
        operand: &Operand<Self::Literal>,
        _index: usize,
    ) -> Result<()> {
        match operand {
            Operand::Symbol(_)
            | Operand::Literal(_)
            | Operand::Missing
            | Operand::Label(_)
            | Operand::DataReference(_)
            | Operand::CodeReference(_)
            | Operand::PrefixSymbol(_, _)
            | Operand::SuffixSymbol(_, _)
            | Operand::Indirect(_) => Ok(()),
            Operand::Infix(_, infix_sym, _) => write!(stream, " {} ", infix_sym),
            Operand::WrapperSymbol(_s1, _op, _s2) => write!(stream, ", "),
        }
    }

    fn emit_operand_wrapper_end(
        &self,
        stream: &mut dyn Write,
        operand: &Operand<Self::Literal>,
    ) -> Result<()> {
        match operand {
            Operand::Symbol(_)
            | Operand::Literal(_)
            | Operand::Missing
            | Operand::Label(_)
            | Operand::DataReference(_)
            | Operand::CodeReference(_)
            | Operand::Infix(_, _, _)
            | Operand::PrefixSymbol(_, _) => Ok(()),
            Operand::Indirect(_op) => write!(stream, "]"),
            Operand::SuffixSymbol(_op, s) => write!(stream, "{}", s),
            Operand::WrapperSymbol(_s1, _op, s2) => write!(stream, "{}", s2),
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
