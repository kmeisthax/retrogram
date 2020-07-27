//! Traits for analysis

use std::cmp::Ord;
use std::hash::Hash;

pub trait Mappable: Clone + Eq + Hash + Ord {}

impl<T> Mappable for T where T: Clone + Eq + Hash + Ord {}
