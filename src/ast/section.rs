//! Top-level AST type which represents a single section.

use std::slice;
use crate::memory;
use crate::ast::Directive;

#[derive(Clone, Debug)]
pub struct Section<I, SI, F, P, MV, S> {
    name: String,
    directives: Vec<(Directive<I, SI, F, P, MV, S>, memory::Pointer<P>)>
}

impl<I, SI, F, P, MV, S> Section<I, SI, F, P, MV, S> where P: Clone {
    pub fn new(name: &str) -> Self {
        Section {
            name: name.to_string(),
            directives: Vec::new()
        }
    }

    pub fn iter_directives(&self) -> slice::Iter<(Directive<I, SI, F, P, MV, S>, memory::Pointer<P>)> {
        self.directives.iter()
    }

    pub fn append_directive(&mut self, dir: Directive<I, SI, F, P, MV, S>, loc: memory::Pointer<P>) {
        self.directives.push((dir, loc));
    }

    pub fn section_name(&self) -> &str {
        &self.name
    }
}