//! Top-level AST type which represents a single section.

use crate::ast::{Directive, Literal};
use crate::memory;

type FixedDirective<L, P, MV, S> = (Directive<L, P, MV, S>, memory::Pointer<P>);

#[derive(Clone, Debug)]
pub struct Section<L, P, MV, S>
where
    L: Literal,
{
    name: String,
    directives: Vec<FixedDirective<L, P, MV, S>>,
}

impl<L, P, MV, S> Section<L, P, MV, S>
where
    L: Literal,
    P: Clone,
{
    pub fn new(name: &str) -> Self {
        Section {
            name: name.to_string(),
            directives: Vec::new(),
        }
    }

    pub fn iter_directives(&self) -> impl Iterator<Item = &FixedDirective<L, P, MV, S>> {
        self.directives.iter()
    }

    pub fn append_directive(&mut self, dir: Directive<L, P, MV, S>, loc: memory::Pointer<P>) {
        self.directives.push((dir, loc));
    }

    pub fn section_name(&self) -> &str {
        &self.name
    }
}
