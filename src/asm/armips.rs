//! Assembler support and integration for ARMIPS

use crate::arch::aarch32::THUMB_STATE;
use crate::ast;
use std::{cmp, fmt};

pub struct OperandFmtWrap<'a, I, S, F, P> {
    tree: &'a ast::Operand<I, S, F, P>,
}

impl<'a, I, S, F, P> OperandFmtWrap<'a, I, S, F, P> {
    pub fn wrap(tree: &'a ast::Operand<I, S, F, P>) -> Self {
        OperandFmtWrap { tree }
    }
}

impl<'a, I, S, F, P> OperandFmtWrap<'a, I, S, F, P>
where
    I: fmt::Display,
    S: fmt::Display,
    F: fmt::Display,
    P: fmt::Display + fmt::LowerHex,
{
    fn write_operand(
        &self,
        operand: &ast::Operand<I, S, F, P>,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        match operand {
            ast::Operand::Symbol(s) => write!(f, "{}", s)?,
            ast::Operand::Literal(ast::Literal::Integer(i)) => write!(f, "{}", i)?,
            ast::Operand::Literal(ast::Literal::SignedInteger(i)) => write!(f, "{}", i)?,
            ast::Operand::Literal(ast::Literal::Float(fl)) => write!(f, "{}", fl)?,
            ast::Operand::Literal(ast::Literal::Pointer(p)) => write!(f, "0x{:x}", p.as_pointer())?,
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

impl<'a, I, S, F, P> fmt::Display for OperandFmtWrap<'a, I, S, F, P>
where
    I: fmt::Display,
    S: fmt::Display,
    F: fmt::Display,
    P: fmt::Display + fmt::LowerHex,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.write_operand(self.tree, f)
    }
}

pub struct InstrFmtWrap<'a, I, S, F, P> {
    tree: &'a ast::Instruction<I, S, F, P>,
}

impl<'a, I, S, F, P> InstrFmtWrap<'a, I, S, F, P> {
    pub fn wrap(tree: &'a ast::Instruction<I, S, F, P>) -> Self {
        InstrFmtWrap { tree }
    }
}

pub fn format_instr<'a, I, S, F, P>(tree: &'a ast::Instruction<I, S, F, P>) -> String
where
    I: fmt::Display,
    S: fmt::Display,
    F: fmt::Display,
    P: Clone + From<u16> + fmt::Display + PartialOrd + fmt::LowerHex + fmt::UpperHex,
{
    format!("{}", InstrFmtWrap::wrap(tree))
}

impl<'a, I, S, F, P> fmt::Display for InstrFmtWrap<'a, I, S, F, P>
where
    I: fmt::Display,
    S: fmt::Display,
    F: fmt::Display,
    P: Clone + From<u16> + fmt::Display + cmp::PartialOrd + fmt::LowerHex + fmt::UpperHex,
{
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

pub struct SectionFmtWrap<'a, I, SI, F, P, MV, S> {
    tree: &'a ast::Section<I, SI, F, P, MV, S>,
}

impl<'a, I, SI, F, P, MV, S> SectionFmtWrap<'a, I, SI, F, P, MV, S> {
    pub fn wrap(tree: &'a ast::Section<I, SI, F, P, MV, S>) -> Self {
        SectionFmtWrap { tree }
    }
}

impl<'a, I, SI, F, P, MV, S> fmt::Display for SectionFmtWrap<'a, I, SI, F, P, MV, S>
where
    I: fmt::Display,
    SI: fmt::Display,
    F: fmt::Display,
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

pub fn format_section<'a, I, SI, F, P, MV, S>(tree: &'a ast::Section<I, SI, F, P, MV, S>) -> String
where
    I: fmt::Display,
    SI: fmt::Display,
    F: fmt::Display,
    P: Clone + From<u16> + fmt::Display + cmp::PartialOrd + fmt::LowerHex + fmt::UpperHex,
    MV: fmt::UpperHex,
    S: fmt::Display,
{
    format!("{}", SectionFmtWrap::wrap(tree))
}
