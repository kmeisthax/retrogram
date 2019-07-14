//! Instruction AST type

use std::{slice, str};
use crate::ast::Operand;

#[derive(Clone, Debug)]
pub struct Instruction<I, S, F, P> {
    /// The instruction being executed
    opcode: String,
    /// Operands for the instruction, if any
    operands: Vec<Operand<I, S, F, P>>
}

impl<I, S, F, P> Instruction<I, S, F, P> {
    pub fn new(opcode: &str, operands: Vec<Operand<I, S, F, P>>) -> Self {
        Instruction {
            opcode: opcode.to_string(),
            operands: operands
        }
    }

    pub fn opcode(&self) -> &String {
        &self.opcode
    }

    pub fn iter_operands(&self) -> slice::Iter<Operand<I, S, F, P>> {
        self.operands.iter()
    }
}