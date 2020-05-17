//! Top-level AST type which represents a single section.

use crate::ast::Directive;
use crate::memory;

type FixedDirective<I, SI, F, P, MV, S> = (Directive<I, SI, F, P, MV, S>, memory::Pointer<P>);

#[derive(Clone, Debug)]
pub struct Section<I, SI, F, P, MV, S> {
    name: String,
    directives: Vec<FixedDirective<I, SI, F, P, MV, S>>,
}

impl<I, SI, F, P, MV, S> Section<I, SI, F, P, MV, S>
where
    P: Clone,
{
    pub fn new(name: &str) -> Self {
        Section {
            name: name.to_string(),
            directives: Vec::new(),
        }
    }

    pub fn iter_directives(&self) -> impl Iterator<Item = &FixedDirective<I, SI, F, P, MV, S>> {
        self.directives.iter()
    }

    pub fn append_directive(
        &mut self,
        dir: Directive<I, SI, F, P, MV, S>,
        loc: memory::Pointer<P>,
    ) {
        self.directives.push((dir, loc));
    }

    pub fn section_name(&self) -> &str {
        &self.name
    }
}
