//! Annotated syntax generator

use crate::asm::Assembler;
use crate::ast::{Directive, Instruction, Label, Literal, Operand, Section};
use crate::memory::Pointer;
use std::fmt;
use std::io::{Result, Write};
use std::result::Result as AnyResult;
use std::str::{from_utf8, Utf8Error};

/// Enumeration of all possible annotations that can be applied to text.
#[derive(Clone, PartialEq, Eq)]
pub enum AnnotationKind {
    /// Indicates text that forms the assembler syntax.
    Syntactic,

    /// Indicates text that contains a comment.
    Comment,

    /// Indicates text that corresponds to a labeled memory location.
    Label,

    /// Indicates text that is unknown data.
    Data,

    /// Indicates text that is the opcode of a disassembled instruction.
    Opcode,

    /// Indicates text that is the result of an error condition.
    Error,
}

/// An annotation that can be applied to text.
///
/// This structure does not contain span starts or ends, only a width.
pub struct Annotation {
    size: usize,
    kind: AnnotationKind,
}

/// Text annotations stored alongside the text it's annotating.
pub struct AnnotatedText {
    text: Vec<u8>,
    annotations: Vec<Annotation>,

    /// The point in `text` at which the last annotation in the list starts (or
    /// 0, if there are no annotations).
    last_annotation_start: usize,
}

impl Default for AnnotatedText {
    fn default() -> Self {
        Self::new()
    }
}

impl AnnotatedText {
    pub fn new() -> Self {
        Self {
            text: Vec::new(),
            annotations: Vec::new(),
            last_annotation_start: 0,
        }
    }

    /// Change the annotation type.
    pub fn change_annotation(&mut self, kind: AnnotationKind) {
        if let Some(a) = self.annotations.last_mut() {
            a.size = self.text.len() - self.last_annotation_start;
        }

        if !self
            .annotations
            .last()
            .map(|a| a.kind == kind)
            .unwrap_or(false)
        {
            self.annotations.push(Annotation { size: 0, kind });

            self.last_annotation_start = self.text.len()
        }
    }

    /// Get an annotated text writer.
    pub fn as_writer<'a>(&'a mut self) -> impl 'a + Write {
        &mut self.text
    }

    /// Yield the unannotated text written to this annotation's writer.
    pub fn unannotated_text(&self) -> AnyResult<&str, Utf8Error> {
        from_utf8(&self.text)
    }

    /// Iterate annotations
    pub fn iter_annotations<'a>(
        &'a self,
    ) -> impl 'a + Iterator<Item = AnyResult<(&'a str, AnnotationKind), Utf8Error>> {
        let mut last_annotation_start = 0;

        self.annotations.iter().enumerate().map(move |(i, a)| {
            let last_annotation_end = if i == (self.annotations.len() - 1) {
                last_annotation_start + a.size
            } else {
                self.text.len()
            };

            let strdat = self
                .text
                .get(last_annotation_start..last_annotation_end)
                .unwrap_or(&[]);

            last_annotation_start = last_annotation_end;

            Ok((from_utf8(strdat)?, a.kind.clone()))
        })
    }

    /// Emit an entire section into annotated text.
    fn emit_section<ASM, MV, S>(
        &mut self,
        asm: ASM,
        section: &Section<ASM::Literal, <ASM::Literal as Literal>::PtrVal, MV, S>,
    ) -> Result<()>
    where
        ASM: Assembler,
        <ASM::Literal as Literal>::PtrVal: Clone,
        MV: fmt::Display + fmt::UpperHex + fmt::LowerHex,
        S: fmt::Display,
    {
        for (directive, _mloc) in section.iter_directives() {
            match directive {
                Directive::DeclareComment(comment) => self.emit_comment(asm.clone(), &comment)?,
                Directive::DeclareLabel(label) => self.emit_label_decl(asm.clone(), &label)?,
                Directive::DeclareOrg(org) => {
                    self.emit_org(asm.clone(), section.section_name(), org)?
                }
                Directive::EmitData(data) => self.emit_data(asm.clone(), &data)?,
                Directive::EmitInstr(instr, _size) => self.emit_instr(asm.clone(), instr)?,
                Directive::EmitSpace(space) => self.emit_space(asm.clone(), space)?,
            }
        }

        Ok(())
    }

    /// Emit an entire instruction into annotated text.
    pub fn emit_instr<ASM>(&mut self, asm: ASM, instr: &Instruction<ASM::Literal>) -> Result<()>
    where
        ASM: Assembler,
    {
        self.emit_instr_start(asm.clone(), instr)?;
        self.emit_instr_opcode(asm.clone(), instr.opcode())?;

        for (i, operand) in instr.iter_operands().enumerate() {
            self.emit_operand_start(asm.clone(), instr, i)?;
            self.emit_operand(asm.clone(), operand)?;
            self.emit_operand_end(asm.clone(), instr, i)?;
        }

        self.emit_instr_end(asm, instr)?;

        Ok(())
    }

    /// Emit a single operand into annotated text.
    fn emit_operand<ASM>(&mut self, asm: ASM, operand: &Operand<ASM::Literal>) -> Result<()>
    where
        ASM: Assembler,
    {
        match operand {
            Operand::Symbol(symbol) => self.emit_symbol(asm, symbol)?,
            Operand::Literal(literal) => self.emit_literal(asm, literal)?,
            Operand::Missing => self.emit_missing_operand(asm)?,
            Operand::Label(label) => self.emit_label_operand(asm, label)?,
            Operand::DataReference(opr)
            | Operand::CodeReference(opr)
            | Operand::Indirect(opr)
            | Operand::PrefixSymbol(_, opr)
            | Operand::SuffixSymbol(opr, _) => {
                self.emit_operand_wrapper_start(asm.clone(), operand)?;
                self.emit_operand(asm.clone(), &opr)?;
                self.emit_operand_wrapper_end(asm, operand)?;
            }
            Operand::Infix(opr1, _infix, opr2) => {
                self.emit_operand_wrapper_start(asm.clone(), operand)?;
                self.emit_operand(asm.clone(), &opr1)?;
                self.emit_operand_wrapper_infix(asm.clone(), operand, 0)?;
                self.emit_operand(asm.clone(), &opr2)?;
                self.emit_operand_wrapper_end(asm, operand)?;
            }
            Operand::WrapperSymbol(_prefix, operands, _suffix) => {
                self.emit_operand_wrapper_start(asm.clone(), operand)?;

                for (i, inner_opr) in operands.iter().enumerate() {
                    self.emit_operand(asm.clone(), inner_opr)?;

                    if i < (operands.len() - 1) {
                        self.emit_operand_wrapper_infix(asm.clone(), operand, i)?;
                    }
                }

                self.emit_operand_wrapper_end(asm, operand)?;
            }
        }

        Ok(())
    }

    /// Emit a comment into annotated text.
    pub fn emit_comment<ASM>(&mut self, asm: ASM, comment_data: &str) -> Result<()>
    where
        ASM: Assembler,
    {
        self.change_annotation(AnnotationKind::Comment);
        asm.emit_comment(&mut self.text, comment_data)
    }

    /// Emit an org statement or other section declaration into annotated text.
    pub fn emit_org<ASM>(
        &mut self,
        asm: ASM,
        section_name: &str,
        where_to: &Pointer<<ASM::Literal as Literal>::PtrVal>,
    ) -> Result<()>
    where
        ASM: Assembler,
    {
        self.change_annotation(AnnotationKind::Syntactic);
        asm.emit_org(&mut self.text, section_name, where_to)
    }

    /// Emit a label declaration into annotated text.
    pub fn emit_label_decl<ASM>(&mut self, asm: ASM, label: &Label) -> Result<()>
    where
        ASM: Assembler,
    {
        self.change_annotation(AnnotationKind::Label);
        asm.emit_label_decl(&mut self.text, label)
    }

    /// Emit empty space declarations into annotated text.
    pub fn emit_space<ASM, S>(&mut self, asm: ASM, space: S) -> Result<()>
    where
        ASM: Assembler,
        S: fmt::Display,
    {
        self.change_annotation(AnnotationKind::Syntactic);
        asm.emit_space(&mut self.text, space)
    }

    /// Emit constant data declarations into annotated text.
    pub fn emit_data<ASM, MV>(&mut self, asm: ASM, data: &[MV]) -> Result<()>
    where
        ASM: Assembler,
        MV: fmt::Display + fmt::UpperHex + fmt::LowerHex,
    {
        self.change_annotation(AnnotationKind::Data);
        asm.emit_data(&mut self.text, data)
    }

    /// Emit any instruction whitespace necessary for the start of an
    /// instruction into annotated text.
    fn emit_instr_start<ASM>(&mut self, asm: ASM, instr: &Instruction<ASM::Literal>) -> Result<()>
    where
        ASM: Assembler,
    {
        self.change_annotation(AnnotationKind::Syntactic);
        asm.emit_instr_start(&mut self.text, instr)
    }

    /// Emit an instruction's opcode into annotated text.
    fn emit_instr_opcode<ASM>(&mut self, asm: ASM, opcode: &str) -> Result<()>
    where
        ASM: Assembler,
    {
        self.change_annotation(AnnotationKind::Opcode);
        asm.emit_instr_opcode(&mut self.text, opcode)
    }

    /// Emit any operand whitespace necessary for the start of an operand into
    /// annotated text.
    fn emit_operand_start<ASM>(
        &mut self,
        asm: ASM,
        instr: &Instruction<ASM::Literal>,
        operand_index: usize,
    ) -> Result<()>
    where
        ASM: Assembler,
    {
        self.change_annotation(AnnotationKind::Opcode);
        asm.emit_operand_start(&mut self.text, instr, operand_index)
    }

    /// Emit an architecturally-defined symbol into annotated text.
    fn emit_symbol<ASM>(&mut self, asm: ASM, symbol: &str) -> Result<()>
    where
        ASM: Assembler,
    {
        self.change_annotation(AnnotationKind::Label);
        asm.emit_symbol(&mut self.text, symbol)
    }

    /// Emit an assembler-defined literal into annotated text.
    fn emit_literal<ASM>(&mut self, asm: ASM, literal: &ASM::Literal) -> Result<()>
    where
        ASM: Assembler,
    {
        self.change_annotation(AnnotationKind::Data);
        asm.emit_literal(&mut self.text, literal)
    }

    /// Emit a missing operand (one that could not be disassembled for whatever
    /// reason) into annotated text.
    fn emit_missing_operand<ASM>(&mut self, asm: ASM) -> Result<()>
    where
        ASM: Assembler,
    {
        self.change_annotation(AnnotationKind::Error);
        asm.emit_missing_operand(&mut self.text)
    }

    /// Emit a label operand (not a label declaration) into annotated text.
    fn emit_label_operand<ASM>(&mut self, asm: ASM, label: &Label) -> Result<()>
    where
        ASM: Assembler,
    {
        self.change_annotation(AnnotationKind::Label);
        asm.emit_label_operand(&mut self.text, label)
    }

    /// Emit the start of an operand that wraps other operands into annotated
    /// text.
    fn emit_operand_wrapper_start<ASM>(
        &mut self,
        asm: ASM,
        operand: &Operand<ASM::Literal>,
    ) -> Result<()>
    where
        ASM: Assembler,
    {
        self.change_annotation(AnnotationKind::Syntactic);
        asm.emit_operand_wrapper_start(&mut self.text, operand)
    }

    /// Emit dividers that appear between symbols that wrap other operands into
    /// annotated text.
    fn emit_operand_wrapper_infix<ASM>(
        &mut self,
        asm: ASM,
        operand: &Operand<ASM::Literal>,
        index: usize,
    ) -> Result<()>
    where
        ASM: Assembler,
    {
        self.change_annotation(AnnotationKind::Syntactic);
        asm.emit_operand_wrapper_infix(&mut self.text, operand, index)
    }

    /// Emit the end of an operand that wraps other operands into annotated
    /// text.
    fn emit_operand_wrapper_end<ASM>(
        &mut self,
        asm: ASM,
        operand: &Operand<ASM::Literal>,
    ) -> Result<()>
    where
        ASM: Assembler,
    {
        self.change_annotation(AnnotationKind::Syntactic);
        asm.emit_operand_wrapper_end(&mut self.text, operand)
    }

    /// Emit any operand whitespace necessary for the end of an operand into
    /// annotated text.
    fn emit_operand_end<ASM>(
        &mut self,
        asm: ASM,
        instr: &Instruction<ASM::Literal>,
        operand_index: usize,
    ) -> Result<()>
    where
        ASM: Assembler,
    {
        self.change_annotation(AnnotationKind::Syntactic);
        asm.emit_operand_end(&mut self.text, instr, operand_index)
    }

    /// Emit any instruction whitespace necessary for the end of an
    /// instruction into annotated text.
    fn emit_instr_end<ASM>(&mut self, asm: ASM, instr: &Instruction<ASM::Literal>) -> Result<()>
    where
        ASM: Assembler,
    {
        self.change_annotation(AnnotationKind::Syntactic);
        asm.emit_instr_end(&mut self.text, instr)
    }
}
