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
}