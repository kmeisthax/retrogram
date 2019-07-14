//! Top-level AST type which represents a single section.

use std::slice;
use crate::memory;
use crate::ast::Line;

#[derive(Clone, Debug)]
pub struct Section<I, S, F, P> {
    name: String,
    loc: memory::Pointer<P>,
    lines: Vec<Line<I, S, F, P>>
}

impl<I, S, F, P> Section<I, S, F, P> where P: Clone {
    pub fn new(name: &str, loc: &memory::Pointer<P>) -> Self {
        Section {
            name: name.to_string(),
            loc: loc.clone(),
            lines: Vec::new()
        }
    }

    pub fn iter_lines(&self) -> slice::Iter<Line<I, S, F, P>> {
        self.lines.iter()
    }

    pub fn append_line(&mut self, line: Line<I, S, F, P>) {
        self.lines.push(line);
    }

    pub fn section_name(&self) -> &str {
        &self.name
    }

    pub fn section_loc(&self) -> &memory::Pointer<P> {
        &self.loc
    }
}