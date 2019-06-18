//! Assembler support and integration for rgbds.

use std::fmt;
use std::cmp::PartialOrd;
use crate::retrogram::ast;

pub struct RGBDSAstFormatee<'a, I, S, F, P> {
    tree: &'a ast::Section<I, S, F, P>
}

impl<'a, I, S, F, P> RGBDSAstFormatee<'a, I, S, F, P> {
    pub fn wrap(tree: &'a ast::Section<I, S, F, P>) -> Self {
        RGBDSAstFormatee {
            tree: tree
        }
    }
}

impl<'a, I, S, F, P> RGBDSAstFormatee<'a, I, S, F, P> where I: fmt::Display, S: fmt::Display, F: fmt::Display, P: fmt::Display + fmt::LowerHex {
    fn write_operand(&self, operand: &ast::Operand<I, S, F, P>, f: &mut fmt::Formatter) -> fmt::Result {
        match operand {
            ast::Operand::Symbol(s) => write!(f, "{}", s)?,
            ast::Operand::Literal(ast::Literal::Integer(i)) => write!(f, "{}", i)?,
            ast::Operand::Literal(ast::Literal::SignedInteger(i)) => write!(f, "{}", i)?,
            ast::Operand::Literal(ast::Literal::Float(fl)) => write!(f, "{}", fl)?,
            ast::Operand::Literal(ast::Literal::Pointer(p)) => write!(f, "${:x}", p)?,
            ast::Operand::Literal(ast::Literal::String(s)) => write!(f, "{}", s)?,
            ast::Operand::Literal(ast::Literal::Missing) => write!(f, "?")?,
            ast::Operand::Label(lbl) if lbl.parent_name() == None => write!(f, "{}", lbl.name())?,
            ast::Operand::Label(lbl) => write!(f, ".{}", lbl.name())?,
            ast::Operand::DataReference(op) => self.write_operand(&op, f)?,
            ast::Operand::CodeReference(op) => self.write_operand(&op, f)?,
            ast::Operand::Indirect(op) => {
                write!(f, "[")?;
                self.write_operand(&op, f)?;
                write!(f, "]")?;
            },
            ast::Operand::Add(op1, op2) => {
                self.write_operand(&op1, f)?;
                write!(f, " + ")?;
                self.write_operand(&op2, f)?;
            },
            ast::Operand::PrefixSymbol(s, op) => {
                write!(f, "{} ", s)?;
                self.write_operand(&op, f)?;
            }
            ast::Operand::SuffixSymbol(op, s) => {
                self.write_operand(&op, f)?;
                write!(f, " {}", s)?;
            },
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

impl<'a, I, S, F, P> fmt::Display for RGBDSAstFormatee<'a, I, S, F, P>
    where I: fmt::Display, S: fmt::Display, F: fmt::Display,
        P: Clone + From<u16> + fmt::Display + PartialOrd + fmt::LowerHex + fmt::UpperHex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let loc = self.tree.section_loc();

        if *loc.as_pointer() < P::from(0x4000 as u16) {
            write!(f, "SECTION \"{}\", ROM0[${:X}]\n", self.tree.section_name(), loc.as_pointer())?;
        } else if *loc.as_pointer() < P::from(0x8000 as u16) {
            if let Some(bank) = loc.get_platform_context("R").into_concrete() {
                write!(f, "SECTION \"{}\", ROMX[${:X}], BANK[${:X}]\n", self.tree.section_name(), loc.as_pointer(), bank)?;
            } else {
                write!(f, "SECTION \"{}\", ROMX[${:X}]\n", self.tree.section_name(), loc.as_pointer())?;
            }
        } else if *loc.as_pointer() < P::from(0xA000 as u16) {
            if let Some(bank) = loc.get_platform_context("V").into_concrete() {
                write!(f, "SECTION \"{}\", VRAM[${:X}], BANK[${:X}]\n", self.tree.section_name(), loc.as_pointer(), bank)?;
            } else {
                write!(f, "SECTION \"{}\", VRAM[${:X}]\n", self.tree.section_name(), loc.as_pointer())?;
            }
        } else if *loc.as_pointer() < P::from(0xC000 as u16) {
            if let Some(bank) = loc.get_platform_context("S").into_concrete() {
                write!(f, "SECTION \"{}\", SRAM[${:X}], BANK[${:X}]\n", self.tree.section_name(), loc.as_pointer(), bank)?;
            } else {
                write!(f, "SECTION \"{}\", SRAM[${:X}]\n", self.tree.section_name(), loc.as_pointer())?;
            }
        } else if *loc.as_pointer() < P::from(0xD000 as u16) {
            write!(f, "SECTION \"{}\", WRAM0[${:X}]\n", self.tree.section_name(), loc.as_pointer())?;
        } else if *loc.as_pointer() < P::from(0xE000 as u16) {
            if let Some(bank) = loc.get_platform_context("W").into_concrete() {
                write!(f, "SECTION \"{}\", WRAMX[${:X}], BANK[${:X}]\n", self.tree.section_name(), loc.as_pointer(), bank)?;
            } else {
                write!(f, "SECTION \"{}\", WRAMX[${:X}]\n", self.tree.section_name(), loc.as_pointer())?;
            }
        } else {
            write!(f, "SECTION \"{}\", HRAM[${:X}]\n", self.tree.section_name(), loc.as_pointer())?;
        }

        for line in self.tree.iter_lines() {
            if let Some(ref label) = line.label() {
                if let Some(_parent_label) = label.parent_name() {
                    write!(f, ".{}:\n", label.name())?;
                } else {
                    write!(f, "{}:\n", label.name())?;
                }
            }

            if let Some(ref instr) = line.instr() {
                write!(f, "    {}", instr.opcode())?;

                let mut has_written_operand = false;
                for operand in instr.iter_operands() {
                    if !has_written_operand {
                        write!(f, " ")?;
                        has_written_operand = true;
                    } else {
                        write!(f, ", ")?;
                    }

                    self.write_operand(operand, f)?;
                }
            }

            if let Some(ref comment) = line.comment() {
                write!(f, ";{}", comment)?;
            }

            write!(f, "\n")?;
        }

        Ok(())
    }
}