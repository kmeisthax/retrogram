//! Analysis sections (e.g. blocks, subroutines, etc)

use std::collections::HashSet;
use serde::{Serialize, Deserialize};
use crate::retrogram::{analysis, memory};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Block<P, S> where P: analysis::Mappable {
    start: memory::Pointer<P>,
    length: S,
    exits: HashSet<Option<memory::Pointer<P>>>
}

impl<P, S> Block<P, S> where P: analysis::Mappable {
    pub fn from_parts(start: memory::Pointer<P>, length: S, exits: HashSet<Option<memory::Pointer<P>>>) -> Self {
        Block {
            start: start,
            length: length,
            exits: exits
        }
    }

    pub fn as_start(&self) -> &memory::Pointer<P> {
        &self.start
    }

    pub fn as_length(&self) -> &S {
        &self.length
    }

    pub fn as_exits(&self) -> &HashSet<Option<memory::Pointer<P>>> {
        &self.exits
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