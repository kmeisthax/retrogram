//! Analysis database - Allows accumulation of program facts as disassembly
//! passes run on the program.

use std::collections::HashMap;
use crate::retrogram::{ast, memory};

/// A repository of information obtained from the program under analysis.
struct Database<P> {
    /// A list of all labels in the program.
    labels: HashMap<ast::Label, memory::Pointer<P>>
}

impl<P> Database<P> {
    pub fn new() -> Self {
        Database {
            labels: HashMap::new()
        }
    }

    pub fn insert_label(&mut self, label: ast::Label, ptr: memory::Pointer<P>) {
        self.labels.insert(label, ptr);
    }
}