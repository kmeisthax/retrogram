//! Instruction AST type

use crate::ast::{Literal, Operand};
use std::{slice, str};

#[derive(Clone, Debug)]
pub struct Instruction<L>
where
    L: Literal,
{
    /// The instruction being executed
    opcode: String,

    /// Operands for the instruction, if any
    operands: Vec<Operand<L>>,
}

impl<L> Instruction<L>
where
    L: Literal,
{
    pub fn new(opcode: &str, operands: Vec<Operand<L>>) -> Self {
        Instruction {
            opcode: opcode.to_string(),
            operands,
        }
    }

    pub fn opcode(&self) -> &String {
        &self.opcode
    }

    pub fn operand_count(&self) -> usize {
        self.operands.len()
    }

    pub fn iter_operands(&self) -> slice::Iter<Operand<L>> {
        self.operands.iter()
    }
}
