//! A model of program state.

use std::collections::HashMap;
use std::hash::Hash;
use num_traits::bounds::Bounded;
use crate::retrogram::reg::Symbolic;

/// Represents a bundle of known program state.
/// 
/// Program state is separated into two categories: architectural and memory.
/// An architectural register is one whose type is specified by the CPU
/// architecture of the program under analysis. It is referred to by a given
/// value of type `RK`, and always has a value of type `RV`.
/// 
/// We may also want to analyze memory state, in which case we retain a separate
/// list of memory locations of type `P` and known values for them of type `MV`.
/// 
/// # Architectural representation
/// 
/// The type `RK` must be capable of naming all CPU registers for the given
/// architecture, and the type `RV` must be capable of representing any possible
/// value for any CPU register. The simplest way to implement this would be to
/// have an enumeration for CPU registers, and provide the widest available
/// integer type as `RV`.
/// 
/// `P` must be a valid integer type wide enough to represent any valid memory
/// address for the program. `MV` must be an integer type whose width matches
/// the smallest addressible unit on the given architecture. Usually, `P` will
/// be `u32` or `u64` and `MV` will be `u8`, though systems with word-addressed
/// memory do exist.
/// 
/// # Interaction with 
pub struct State<RK, RV, P, MV> where RK: Eq + Hash, P: Eq + Hash {
    /// Architectural program state, such as CPU registers.
    cpu_state: HashMap<RK, Symbolic<RV>>,

    /// Non-architectural, or memory-related program state.
    mem_state: HashMap<P, Symbolic<MV>>
}

impl<RK, RV, P, MV> State<RK, RV, P, MV> where RK: Eq + Hash, P: Eq + Hash {

}

impl<RK, RV, P, MV> Default for State<RK, RV, P, MV> where RK: Eq + Hash, P: Eq + Hash {
    fn default() -> Self {
        State {
            cpu_state: HashMap::new(),
            mem_state: HashMap::new()
        }
    }
}

impl<RK, RV, P, MV> State<RK, RV, P, MV> where RK: Eq + Hash, P: Eq + Hash, RV: Bounded + From<u8> + Clone {
    pub fn get_register(&self, k: RK) -> Symbolic<RV> {
        if let Some(val) = self.cpu_state.get(&k) {
            return val.clone();
        }

        Symbolic::default()
    }
}

impl<RK, RV, P, MV> State<RK, RV, P, MV> where RK: Eq + Hash, P: Eq + Hash, MV: Bounded + From<u8> + Clone {
    pub fn get_memory(&self, k: P) -> Symbolic<MV> {
        if let Some(val) = self.mem_state.get(&k) {
            return val.clone();
        }

        Symbolic::default()
    }
}