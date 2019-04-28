//! Assembler support and integration for rgbds.

use std::{io, fmt};
use std::ops::{Shl, BitOr};
use crate::retrogram::ast;
use crate::retrogram::arch::lr35902;
use crate::retrogram::platform::gb;
use crate::retrogram::analysis::Database;

fn str2hex<I>(thestr: &str) -> Option<I> where I: From<u8> + Shl + From<<I as Shl>::Output> + BitOr + From<<I as BitOr>::Output> {
    let mut out = I::from(0);

    for char in thestr.chars() {
        match char {
            '0' => { out = I::from(I::from(out << I::from(4)) | I::from(0)) },
            '1' => { out = I::from(I::from(out << I::from(4)) | I::from(1)) },
            '2' => { out = I::from(I::from(out << I::from(4)) | I::from(2)) },
            '3' => { out = I::from(I::from(out << I::from(4)) | I::from(3)) },
            '4' => { out = I::from(I::from(out << I::from(4)) | I::from(4)) },
            '5' => { out = I::from(I::from(out << I::from(4)) | I::from(5)) },
            '6' => { out = I::from(I::from(out << I::from(4)) | I::from(6)) },
            '7' => { out = I::from(I::from(out << I::from(4)) | I::from(7)) },
            '8' => { out = I::from(I::from(out << I::from(4)) | I::from(8)) },
            '9' => { out = I::from(I::from(out << I::from(4)) | I::from(9)) },
            'A' => { out = I::from(I::from(out << I::from(4)) | I::from(10)) },
            'B' => { out = I::from(I::from(out << I::from(4)) | I::from(11)) },
            'C' => { out = I::from(I::from(out << I::from(4)) | I::from(12)) },
            'D' => { out = I::from(I::from(out << I::from(4)) | I::from(13)) },
            'E' => { out = I::from(I::from(out << I::from(4)) | I::from(14)) },
            'F' => { out = I::from(I::from(out << I::from(4)) | I::from(15)) },
            _ => return None
        }
    }

    Some(out)
}

/// Read the symbols from an RGBDS symbol file.
pub fn parse_symbol_file<F>(file: F, db: &mut Database<lr35902::Pointer>) -> io::Result<()> where F: io::BufRead {
    for line in file.lines() {
        let line = line?;
        let mut split = line.split(" ");

        if let Some(ptr_str) = split.next() {
            if ptr_str.chars().next() == Some(';') {
                continue;
            }

            let mut ptr_split = ptr_str.split(":");
            let mut bank_addr : u16 = 0;
            let mut ptr_addr : u16 = 0;
            if let Some(bank_part) = ptr_split.next() {
                bank_addr = str2hex(bank_part).unwrap_or(0);
            }

            if let Some(ptr_part) = ptr_split.next() {
                ptr_addr = str2hex(ptr_part).unwrap_or(0);
            }

            if let Some(ctxt_ptr) = gb::create_context(&vec![bank_addr, ptr_addr]) {
                if let Some(label_str) = split.next() {
                    let mut name_split = label_str.split(".");

                    if let Some(global_part) = name_split.next() {
                        if let Some(local_part) = name_split.next() {
                            db.insert_label(ast::Label::new(local_part, Some(global_part)), ctxt_ptr);
                        } else {
                            db.insert_label(ast::Label::new(global_part, None), ctxt_ptr);
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

pub struct RGBDSAstFormatee<'a, I, S, F, P> {
    tree: &'a ast::Assembly<I, S, F, P>
}

impl<'a, I, S, F, P> RGBDSAstFormatee<'a, I, S, F, P> {
    pub fn wrap(tree: &'a ast::Assembly<I, S, F, P>) -> Self {
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
            }
        };

        Ok(())
    }
}

impl<'a, I, S, F, P> fmt::Display for RGBDSAstFormatee<'a, I, S, F, P> where I: fmt::Display, S: fmt::Display, F: fmt::Display, P: fmt::Display + fmt::LowerHex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for line in self.tree.iter_lines() {
            if let Some(ref label) = line.label() {
                if let Some(_parent_label) = label.parent_name() {
                    write!(f, ".{}:", label.name())?;
                } else {
                    write!(f, "{}:", label.name())?;
                }
            }

            if let Some(ref instr) = line.instr() {
                write!(f, "{}", instr.opcode())?;

                let has_written_operand = false;
                for operand in instr.iter_operands() {
                    if !has_written_operand {
                        write!(f, " ")?;
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