//! AST type for a single instruction line

use crate::memory;
use crate::ast::{Label, Instruction};

#[derive(Clone, Debug)]
pub struct Line<I, S, F, P> {
    label: Option<Label>,
    instruction: Option<Instruction<I, S, F, P>>,
    comment: Option<String>,
    source_address: memory::Pointer<P>,
}

impl<I, S, F, P> Line<I, S, F, P> {
    pub fn new(label: Option<Label>, instruction: Option<Instruction<I, S, F, P>>, comment: Option<String>, source_address: memory::Pointer<P>) -> Self {
        Line {
            label: label,
            instruction: instruction,
            comment: comment,
            source_address: source_address
        }
    }
    
    pub fn label(&self) -> Option<&Label> {
        if let Some(ref label) = self.label {
            Some(label)
        } else {
            None
        }
    }

    pub fn instr(&self) -> Option<&Instruction<I, S, F, P>> {
        if let Some(ref instruction) = self.instruction {
            Some(instruction)
        } else {
            None
        }
    }

    pub fn comment(&self) -> Option<&String> {
        if let Some(ref comment) = self.comment {
            Some(comment)
        } else {
            None
        }
    }

    pub fn source_address(&self) -> &memory::Pointer<P> {
        &self.source_address
    }

    pub fn into_parts(self) -> (Option<Label>, Option<Instruction<I, S, F, P>>, Option<String>, memory::Pointer<P>) {
        (self.label, self.instruction, self.comment, self.source_address)
    }
}