//! Traits for analysis

use std::hash::Hash;

pub trait Mappable : Clone + Eq + Hash {

}

impl<T> Mappable for T where T: Clone + Eq + Hash {

}