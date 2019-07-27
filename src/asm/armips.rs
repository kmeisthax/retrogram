//! Assembler support and integration for ARMIPS

use std::{fmt, cmp};
use crate::ast;
use crate::arch::aarch32::THUMB_STATE;

pub struct ArmipsAstFormattee<'a, I, S, F, P> {
    tree: &'a ast::Section<I, S, F, P>
}

impl<'a, I, S, F, P> ArmipsAstFormattee<'a, I, S, F, P> {
    pub fn wrap(tree: &'a ast::Section<I, S, F, P>) -> Self {
        ArmipsAstFormattee {
            tree: tree
        }
    }
}

impl<'a, I, S, F, P> ArmipsAstFormattee<'a, I, S, F, P> where I: fmt::Display, S: fmt::Display, F: fmt::Display, P: fmt::Display + fmt::LowerHex {
    fn write_operand(&self, operand: &ast::Operand<I, S, F, P>, f: &mut fmt::Formatter) -> fmt::Result {
        match operand {
            ast::Operand::Symbol(s) => write!(f, "{}", s)?,
            ast::Operand::Literal(ast::Literal::Integer(i)) => write!(f, "{}", i)?,
            ast::Operand::Literal(ast::Literal::SignedInteger(i)) => write!(f, "{}", i)?,
            ast::Operand::Literal(ast::Literal::Float(fl)) => write!(f, "{}", fl)?,
            ast::Operand::Literal(ast::Literal::Pointer(p)) => write!(f, "0x{:x}", p)?,
            ast::Operand::Literal(ast::Literal::String(s)) => write!(f, "{}", s)?,
            ast::Operand::Literal(ast::Literal::Missing) => write!(f, "?")?,
            ast::Operand::Label(lbl) if lbl.parent_name() == None => write!(f, "{}", lbl.name())?,
            ast::Operand::Label(lbl) => write!(f, "@@{}", lbl.name())?,
            ast::Operand::DataReference(op) => self.write_operand(&op, f)?,
            ast::Operand::CodeReference(op) => self.write_operand(&op, f)?,
            ast::Operand::Indirect(op) => {
                write!(f, "[")?;
                self.write_operand(&op, f)?;
                write!(f, "]")?;
            },
            ast::Operand::Infix(op1, infix_sym, op2) => {
                self.write_operand(&op1, f)?;
                write!(f, " {} ", infix_sym)?;
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

impl<'a, I, S, F, P> fmt::Display for ArmipsAstFormattee<'a, I, S, F, P>
    where I: fmt::Display, S: fmt::Display, F: fmt::Display,
        P: Clone + From<u16> + fmt::Display + cmp::PartialOrd + fmt::LowerHex + fmt::UpperHex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let loc = self.tree.section_loc();
        let thumb_state = loc.get_platform_context(THUMB_STATE).into_concrete();

        write!(f, ".org 0x{:X}", loc.as_pointer())?;
        match thumb_state {
            Some(0) => write!(f, ".arm")?,
            Some(1) => write!(f, ".thumb")?,
            _ => return Err(fmt::Error::default())
        }

        for line in self.tree.iter_lines() {
            if let Some(ref label) = line.label() {
                if let Some(_parent_label) = label.parent_name() {
                    write!(f, "@@{}:\n", label.name())?;
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