//! Analysis trace

use crate::memory::Pointer;

/// A Trace is a structure which stores the path that a dynamic analysis pass,
/// such as symbolic execution, takes to reach a particular execution state.
/// 
/// This structure does not store execution state information; for that, you
/// will need to consult `reg::State`. The Trace only contains information on
/// how we got to that state.
#[derive(PartialEq, Eq, Hash)]
pub struct Trace<P> {
    /// Every program counter value along the execution path.
    /// 
    /// Each pointer along the path represents the address of an instruction
    /// that was executed. The final item on the path represents the next
    /// instruction to execute.
    pc_path: Vec<Pointer<P>>
}

impl<P> Trace<P> {
    /// Create a new trace beginning at the given pointer.
    pub fn begin_at(start_pc: Pointer<P>) -> Self {
        Trace {
            pc_path: vec![start_pc]
        }
    }
}

impl<P> Trace<P> where P: PartialEq {
    /// Determine if this trace is a subtrace of the RHS trace.
    /// 
    /// A trace that is a subtrace of a given supertrace is one where the trace
    /// begins at some point in it's supertrace and continues on the same path
    /// until the end of the supertrace. A supertrace represents more preceding
    /// analysis that has been performed compared to the subtrace.
    pub fn is_subtrace(&self, rhs: &Self) -> bool {
        rhs.pc_path.ends_with(&self.pc_path[..])
    }
}