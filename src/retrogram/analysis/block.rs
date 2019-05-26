//! Analysis sections (e.g. blocks, subroutines, etc)

use std::collections::HashSet;
use serde::{Serialize, Deserialize};
use crate::retrogram::{analysis, memory};

/// Represents a sequence of instructions with the following properties:
/// 
/// 1. The sequence of instructions are executed in sequence.
/// 2. Control flow does not diverge within the block.
/// 3. At the end of a block, execution diverges to zero or more other
///    locations, one of which may be the next instruction in sequence.
/// 
/// Effectively, it is a run of instructions with no jumps.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Block<P, S> where P: analysis::Mappable {
    start: memory::Pointer<P>,
    length: S
}

impl<P, S> Block<P, S> where P: analysis::Mappable {
    pub fn from_parts(start: memory::Pointer<P>, length: S) -> Self {
        Block {
            start: start,
            length: length
        }
    }

    pub fn as_start(&self) -> &memory::Pointer<P> {
        &self.start
    }

    pub fn as_length(&self) -> &S {
        &self.length
    }
}

impl<P, S> Block<P, S> where P: analysis::Mappable + memory::PtrNum<S>, S: memory::Offset<P> {
    pub fn is_ptr_within_block(&self, ptr: &memory::Pointer<P>) -> bool {
        let diff = ptr.as_pointer().clone() - self.start.as_pointer().clone();
        
        if let Ok(diff) = S::try_from(diff) {
            if diff < self.length.clone() {
                if self.start.is_context_eq(ptr) {
                    return true;
                }
            }
        }

        false
    }
}