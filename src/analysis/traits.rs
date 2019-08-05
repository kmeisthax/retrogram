//! Traits for analysis

use std::hash::Hash;
use std::fmt::Display;
use std::cmp::Ord;
use std::str::FromStr;
use crate::{memory, analysis};

pub trait Mappable : Clone + Eq + Hash + Ord + Display + FromStr {

}

impl<T> Mappable for T where T: Clone + Eq + Hash + Ord + Display + FromStr {

}

pub type Disassembler<I, SI, F, P, MV, S, IO> = Fn(&memory::Pointer<P>, &memory::Memory<P, MV, S, IO>) ->
            analysis::Result<analysis::Disasm<I, SI, F, P, S>>;