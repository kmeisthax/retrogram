//! Assembler trait

use crate::ast::{Directive, Instruction, Label, Literal, Operand, Section};
use crate::memory::Pointer;
use std::borrow::Cow;
use std::fmt;
use std::io::{Result, Write};

/// Trait which represents a particular assembler syntax.
pub trait Assembler: Clone
where
    Self::Literal: Literal,
{
    /// The representation of literals for this particular assembler syntax.
    ///
    /// In order for an assembler to be useful, it's literal type must have
    /// representations for all of the standard types that an `Architecture`
    /// provides. This is expressed as the `CompatibleLiteral<AR>` trait.
    type Literal;

    /// Emit a comment into the disassembly stream.
    fn emit_comment(&self, stream: &mut dyn Write, comment_data: &str) -> Result<()>;

    /// Emit an org statement or other section declaration into the disassembly
    /// stream.
    fn emit_org(
        &self,
        stream: &mut dyn Write,
        section_name: &str,
        where_to: &Pointer<<Self::Literal as Literal>::PtrVal>,
    ) -> Result<()>;

    /// Emit a label declaration into the disassembly stream.
    fn emit_label_decl(&self, stream: &mut dyn Write, label: &Label) -> Result<()>;

    /// Emit empty space declarations into the disassembly stream.
    fn emit_space<S>(&self, stream: &mut dyn Write, space: S) -> Result<()>
    where
        S: fmt::Display;

    /// Emit constant data declarations into the disassembly stream.
    fn emit_data<MV>(&self, stream: &mut dyn Write, data: &[MV]) -> Result<()>
    where
        MV: fmt::Display + fmt::UpperHex + fmt::LowerHex;

    /// Alter the architecture's disassembled instruction representation, if
    /// necessary.
    ///
    /// This is primarily intended to support instruction syntaxes with things
    /// such as different operand orders or different ways of noting offsets.
    /// Notably, things like x86's AT&T syntax requires a complete rewrite of
    /// the instruction stream in order to support.
    ///
    /// The default implementation does nothing; it is assumed that each
    /// architecture will emit instruction formats suitable as-is for at least
    /// one assembler syntax.
    fn preprocess_instr<'a>(
        &self,
        instr: &'a Instruction<Self::Literal>,
    ) -> Cow<'a, Instruction<Self::Literal>> {
        Cow::Borrowed(instr)
    }

    /// Emit any instruction whitespace necessary for the start of an
    /// instruction.
    ///
    /// This method is given the entire instruction as a reference; but it must
    /// not render any part of it, as that is covered by other trait methods.
    fn emit_instr_start(
        &self,
        stream: &mut dyn Write,
        instr: &Instruction<Self::Literal>,
    ) -> Result<()>;

    /// Emit an instruction's opcode.
    fn emit_instr_opcode(&self, stream: &mut dyn Write, opcode: &str) -> Result<()>;

    /// Emit any operand whitespace necessary for the start of an operand.
    ///
    /// This method is given the entire instruction and operand index as a
    /// reference; but it must not render any part of it, as that is covered by
    /// other trait methods.
    fn emit_operand_start(
        &self,
        stream: &mut dyn Write,
        instr: &Instruction<Self::Literal>,
        operand_index: usize,
    ) -> Result<()>;

    /// Emit an architecturally-defined symbol.
    fn emit_symbol(&self, stream: &mut dyn Write, symbol: &str) -> Result<()>;

    /// Emit an assembler-defined literal.
    fn emit_literal(&self, stream: &mut dyn Write, literal: &Self::Literal) -> Result<()>;

    /// Emit a missing operand (one that could not be disassembled for whatever
    /// reason).
    fn emit_missing_operand(&self, stream: &mut dyn Write) -> Result<()>;

    /// Emit a label operand (not a label declaration).
    fn emit_label_operand(&self, stream: &mut dyn Write, label: &Label) -> Result<()>;

    /// Emit the start of an operand that wraps other operands.
    ///
    /// This should not render the wrapped operands, merely any whitespace or
    /// operand-defined symbols that appear at the start of the operand.
    fn emit_operand_wrapper_start(
        &self,
        stream: &mut dyn Write,
        operand: &Operand<Self::Literal>,
    ) -> Result<()>;

    /// Emit dividers that appear between symbols that wrap other operands.
    ///
    /// This should not render the wrapped operands, merely any whitespace or
    /// operand-defined infix symbols that appear between two operands.
    ///
    /// The index given is the index of the last operand rendered.
    fn emit_operand_wrapper_infix(
        &self,
        stream: &mut dyn Write,
        operand: &Operand<Self::Literal>,
        index: usize,
    ) -> Result<()>;

    /// Emit the end of an operand that wraps other operands.
    ///
    /// This should not render the wrapped operands, merely any whitespace or
    /// operand-defined symbols that appear at the end of the operand.
    fn emit_operand_wrapper_end(
        &self,
        stream: &mut dyn Write,
        operand: &Operand<Self::Literal>,
    ) -> Result<()>;

    /// Emit any operand whitespace necessary for the end of an operand.
    ///
    /// This method is given the entire instruction and operand index as a
    /// reference; but it must not render any part of it, as that is covered by
    /// other trait methods.
    fn emit_operand_end(
        &self,
        stream: &mut dyn Write,
        instr: &Instruction<Self::Literal>,
        operand_index: usize,
    ) -> Result<()>;

    /// Emit an entire operand.
    fn emit_operand(&self, stream: &mut dyn Write, operand: &Operand<Self::Literal>) -> Result<()> {
        match operand {
            Operand::Symbol(symbol) => self.emit_symbol(stream, symbol)?,
            Operand::Literal(literal) => self.emit_literal(stream, literal)?,
            Operand::Missing => self.emit_missing_operand(stream)?,
            Operand::Label(label) => self.emit_label_operand(stream, label)?,
            Operand::DataReference(opr)
            | Operand::CodeReference(opr)
            | Operand::Indirect(opr)
            | Operand::PrefixSymbol(_, opr)
            | Operand::SuffixSymbol(opr, _) => {
                self.emit_operand_wrapper_start(stream, operand)?;
                self.emit_operand(stream, &opr)?;
                self.emit_operand_wrapper_end(stream, operand)?;
            }
            Operand::Infix(opr1, _infix, opr2) => {
                self.emit_operand_wrapper_start(stream, operand)?;
                self.emit_operand(stream, &opr1)?;
                self.emit_operand_wrapper_infix(stream, operand, 0)?;
                self.emit_operand(stream, &opr2)?;
                self.emit_operand_wrapper_end(stream, operand)?;
            }
            Operand::WrapperSymbol(_prefix, operands, _suffix) => {
                self.emit_operand_wrapper_start(stream, operand)?;

                for (i, inner_opr) in operands.iter().enumerate() {
                    self.emit_operand(stream, inner_opr)?;

                    if i < (operands.len() - 1) {
                        self.emit_operand_wrapper_infix(stream, operand, i)?;
                    }
                }

                self.emit_operand_wrapper_end(stream, operand)?;
            }
        }

        Ok(())
    }

    /// Emit any instruction whitespace necessary for the end of an
    /// instruction.
    ///
    /// This method is given the entire instruction as a reference; but it must
    /// not render any part of it, as that is covered by other trait methods.
    fn emit_instr_end(
        &self,
        stream: &mut dyn Write,
        instr: &Instruction<Self::Literal>,
    ) -> Result<()>;

    /// Emit an entire instruction.
    fn emit_instr(&self, stream: &mut dyn Write, instr: &Instruction<Self::Literal>) -> Result<()> {
        self.emit_instr_start(stream, instr)?;
        self.emit_instr_opcode(stream, instr.opcode())?;

        for (i, operand) in instr.iter_operands().enumerate() {
            self.emit_operand_start(stream, instr, i)?;
            self.emit_operand(stream, operand)?;
            self.emit_operand_end(stream, instr, i)?;
        }

        self.emit_instr_end(stream, instr)?;

        Ok(())
    }

    /// Emit an entire section.
    fn emit_section<MV, S>(
        &self,
        stream: &mut dyn Write,
        section: &Section<Self::Literal, <Self::Literal as Literal>::PtrVal, MV, S>,
    ) -> Result<()>
    where
        <Self::Literal as Literal>::PtrVal: Clone,
        S: fmt::Display,
        MV: fmt::Display + fmt::UpperHex + fmt::LowerHex,
    {
        for (directive, _mloc) in section.iter_directives() {
            match directive {
                Directive::DeclareComment(comment) => self.emit_comment(stream, &comment)?,
                Directive::DeclareLabel(label) => self.emit_label_decl(stream, &label)?,
                Directive::DeclareOrg(org) => self.emit_org(stream, section.section_name(), org)?,
                Directive::EmitData(data) => self.emit_data(stream, &data)?,
                Directive::EmitInstr(instr, _size) => self.emit_instr(stream, instr)?,
                Directive::EmitSpace(space) => self.emit_space(stream, space)?,
            }
        }

        Ok(())
    }
}
