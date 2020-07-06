//! Assembler support and integration for rgbds.

use crate::ast;
use crate::memory::Pointer;
use std::cmp::PartialOrd;
use std::fmt;

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
        Self::Integer(v)
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
            ast::Operand::Literal(Literal::Pointer(p)) => write!(f, "${:x}", p)?,
            ast::Operand::Literal(Literal::String(s)) => write!(f, "{}", s)?,
            ast::Operand::Missing => write!(f, "?")?,
            ast::Operand::Label(lbl) if lbl.parent_name() == None => write!(f, "{}", lbl.name())?,
            ast::Operand::Label(lbl) => write!(f, ".{}", lbl.name())?,
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

            write!(f, "{}", OperandFmtWrap::wrap(operand))?;
        }

        Ok(())
    }
}

pub fn format_instr(tree: &ast::Instruction<Literal>) -> String {
    format!("{}", InstrFmtWrap::wrap(tree))
}

pub struct SectionFmtWrap<'a, P, MV, S> {
    tree: &'a ast::Section<Literal, P, MV, S>,
}

impl<'a, P, MV, S> SectionFmtWrap<'a, P, MV, S>
where
    P: Clone + From<u16> + fmt::Display + PartialOrd + fmt::LowerHex + fmt::UpperHex,
    MV: fmt::UpperHex,
    S: fmt::Display,
{
    pub fn wrap(tree: &'a ast::Section<Literal, P, MV, S>) -> Self {
        SectionFmtWrap { tree }
    }
}

impl<'a, P, MV, S> fmt::Display for SectionFmtWrap<'a, P, MV, S>
where
    P: Clone + From<u16> + fmt::Display + PartialOrd + fmt::LowerHex + fmt::UpperHex,
    MV: fmt::UpperHex,
    S: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (directive, _loc) in self.tree.iter_directives() {
            match directive {
                ast::Directive::DeclareComment(comment) => write!(f, ";{}", comment)?,
                ast::Directive::DeclareLabel(label) => {
                    if let Some(_parent_label) = label.parent_name() {
                        writeln!(f, ".{}:", label.name())?;
                    } else {
                        writeln!(f, "{}:", label.name())?;
                    }
                }
                ast::Directive::DeclareOrg(loc) => {
                    if *loc.as_pointer() < P::from(0x4000 as u16) {
                        writeln!(
                            f,
                            "SECTION \"{}\", ROM0[${:X}]",
                            self.tree.section_name(),
                            loc.as_pointer()
                        )?;
                    } else if *loc.as_pointer() < P::from(0x8000 as u16) {
                        if let Some(bank) = loc.get_platform_context("R").into_concrete() {
                            writeln!(
                                f,
                                "SECTION \"{}\", ROMX[${:X}], BANK[${:X}]",
                                self.tree.section_name(),
                                loc.as_pointer(),
                                bank
                            )?;
                        } else {
                            writeln!(
                                f,
                                "SECTION \"{}\", ROMX[${:X}]",
                                self.tree.section_name(),
                                loc.as_pointer()
                            )?;
                        }
                    } else if *loc.as_pointer() < P::from(0xA000 as u16) {
                        if let Some(bank) = loc.get_platform_context("V").into_concrete() {
                            writeln!(
                                f,
                                "SECTION \"{}\", VRAM[${:X}], BANK[${:X}]",
                                self.tree.section_name(),
                                loc.as_pointer(),
                                bank
                            )?;
                        } else {
                            writeln!(
                                f,
                                "SECTION \"{}\", VRAM[${:X}]",
                                self.tree.section_name(),
                                loc.as_pointer()
                            )?;
                        }
                    } else if *loc.as_pointer() < P::from(0xC000 as u16) {
                        if let Some(bank) = loc.get_platform_context("S").into_concrete() {
                            writeln!(
                                f,
                                "SECTION \"{}\", SRAM[${:X}], BANK[${:X}]",
                                self.tree.section_name(),
                                loc.as_pointer(),
                                bank
                            )?;
                        } else {
                            writeln!(
                                f,
                                "SECTION \"{}\", SRAM[${:X}]",
                                self.tree.section_name(),
                                loc.as_pointer()
                            )?;
                        }
                    } else if *loc.as_pointer() < P::from(0xD000 as u16) {
                        writeln!(
                            f,
                            "SECTION \"{}\", WRAM0[${:X}]",
                            self.tree.section_name(),
                            loc.as_pointer()
                        )?;
                    } else if *loc.as_pointer() < P::from(0xE000 as u16) {
                        if let Some(bank) = loc.get_platform_context("W").into_concrete() {
                            writeln!(
                                f,
                                "SECTION \"{}\", WRAMX[${:X}], BANK[${:X}]",
                                self.tree.section_name(),
                                loc.as_pointer(),
                                bank
                            )?;
                        } else {
                            writeln!(
                                f,
                                "SECTION \"{}\", WRAMX[${:X}]",
                                self.tree.section_name(),
                                loc.as_pointer()
                            )?;
                        }
                    } else {
                        writeln!(
                            f,
                            "SECTION \"{}\", HRAM[${:X}]",
                            self.tree.section_name(),
                            loc.as_pointer()
                        )?;
                    }
                }
                ast::Directive::EmitData(data) => {
                    if data.is_empty() {
                        write!(f, "    db ")?;

                        for byte in data {
                            write!(f, "${:X}", byte)?;
                        }

                        writeln!(f)?;
                    }
                }
                ast::Directive::EmitInstr(instr, _) => {
                    writeln!(f, "    {}", InstrFmtWrap::wrap(instr))?
                }
                ast::Directive::EmitSpace(offset) => write!(f, "    ds {}", offset)?,
            }
        }

        Ok(())
    }
}

pub fn format_section<'a, P, MV, S>(tree: &'a ast::Section<Literal, P, MV, S>) -> String
where
    P: Clone + From<u16> + fmt::Display + PartialOrd + fmt::LowerHex + fmt::UpperHex,
    MV: fmt::UpperHex,
    S: fmt::Display,
{
    format!("{}", SectionFmtWrap::wrap(tree))
}
