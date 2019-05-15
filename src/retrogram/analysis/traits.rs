//! Traits for analysis

use std::hash::Hash;
use std::fmt::Display;
use std::str::FromStr;

pub trait Mappable : Clone + Eq + Hash + Display + FromStr {

}

impl<T> Mappable for T where T: Clone + Eq + Hash + Display + FromStr {

}