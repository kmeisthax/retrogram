//! Register definitions for contextual information, such as bank switchers,
//! multi-mode CPUs, and so on.

use std::collections::HashMap;
use crate::retrogram::reg::Symbolic;

/// The bag of state which represents which contexts are currently active.
/// 
/// A context can represent any useful state necessary to disassemble a program.
/// It can be used to store both architectural and platform state, such as a
/// selected bank value, or the operating mode of a processor.
#[derive(Clone)]
pub struct Context {
    contexts: HashMap<String, Symbolic<u64>>
}

impl Context {
    pub fn new() -> Self {
        Context {
            contexts: HashMap::new()
        }
    }
    
    /// Get an architecturally-defined context.
    /// 
    /// Architectural contexts are prefixed with an `A` to avoid conflicts with
    /// platform-specific contexts.
    pub fn get_arch_context(&self, context_name: &str) -> Symbolic<u64> {
        let inner_name = format!("A{}", context_name);
        if let Some(val) = self.contexts.get(&inner_name) {
            return val.clone();
        }

        Symbolic::default()
    }

    /// Set an architecturally-defined context.
    /// 
    /// Architectural contexts are prefixed with an `A` to avoid conflicts with
    /// platform-specific contexts.
    pub fn set_arch_context(&mut self, context_name: &str, value: Symbolic<u64>) {
        let inner_name = format!("A{}", context_name);
        self.contexts.insert(inner_name, value);
    }

    /// Get a context specific to a given platform.
    /// 
    /// Platform contexts are prefixed with a `P` to avoid conflicts with
    /// architecturally defined contexts.
    pub fn get_platform_context(&self, context_name: &str) -> Symbolic<u64> {
        let inner_name = format!("P{}", context_name);
        if let Some(val) = self.contexts.get(&inner_name) {
            return val.clone();
        }

        Symbolic::default()
    }

    /// Set a context specific to a given platform.
    /// 
    /// Platform contexts are prefixed with a `P` to avoid conflicts with
    /// architecturally defined contexts.
    pub fn set_platform_context(&mut self, context_name: &str, value: Symbolic<u64>) {
        let inner_name = format!("P{}", context_name);
        self.contexts.insert(inner_name, value);
    }
}