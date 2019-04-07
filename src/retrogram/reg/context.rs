//! Register definitions for contextual information, such as bank switchers,
//! multi-mode CPUs, and so on.

use std::collections::HashMap;
use crate::retrogram::reg::Symbolic;

/// The bag of state which represents which contexts are currently active.
/// 
/// A context can represent any useful state necessary to disassemble a program.
/// It can be used to store both architectural and platform state, such as a
/// selected bank value, or the operating mode of a processor.
pub type Context = HashMap<char, Symbolic<u64>>;