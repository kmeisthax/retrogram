//! A model of program state.

use crate::arch::Architecture;
use crate::memory::{Contexts, Pointer};
use crate::reg::Symbolic;
use std::cmp::Ordering;
use std::collections::{BTreeSet, HashMap};
use std::hash::{Hash, Hasher};

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
#[derive(Clone)]
pub struct State<AR>
where
    AR: Architecture,
{
    /// Architectural program state, such as CPU registers.
    cpu_state: HashMap<AR::Register, Symbolic<AR::Word>>,

    /// Non-architectural, or memory-related program state.
    mem_state: HashMap<Pointer<AR::PtrVal>, Symbolic<AR::Byte>>,

    /// All currently applicable contexts for memory reads and writes without
    /// known contexts.
    context_state: Contexts<String, u64>,
}

impl<AR> PartialEq for State<AR>
where
    AR: Architecture,
    Symbolic<AR::Word>: PartialEq + Default,
    Symbolic<AR::Byte>: PartialEq + Default,
{
    fn eq(&self, other: &Self) -> bool {
        for (rk, rv) in self.cpu_state.iter() {
            if rv != other.cpu_state.get(rk).unwrap_or(&Default::default()) {
                return false;
            }
        }

        for (rk, rv) in other.cpu_state.iter() {
            if rv != self.cpu_state.get(rk).unwrap_or(&Default::default()) {
                return false;
            }
        }

        for (p, mv) in self.mem_state.iter() {
            if mv != other.mem_state.get(p).unwrap_or(&Default::default()) {
                return false;
            }
        }

        for (p, mv) in other.mem_state.iter() {
            if mv != self.mem_state.get(p).unwrap_or(&Default::default()) {
                return false;
            }
        }

        for (s, cv) in self.context_state.iter() {
            if cv != other.context_state.get(s).unwrap_or(&Default::default()) {
                return false;
            }
        }

        for (s, cv) in other.context_state.iter() {
            if cv != self.context_state.get(s).unwrap_or(&Default::default()) {
                return false;
            }
        }

        true
    }
}

impl<AR> Eq for State<AR>
where
    AR: Architecture,
    Symbolic<AR::Word>: Eq + Default,
    Symbolic<AR::Byte>: Eq + Default,
{
}

impl<AR> Hash for State<AR>
where
    AR: Architecture,
    Symbolic<AR::Word>: Eq + Hash + Default,
    Symbolic<AR::Byte>: Eq + Hash + Default,
{
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        // Implementation note: `Hash` requires that hash equality mirror
        // equality of the underlying type. This poses problems for hashing an
        // entire `State`, as undefined keys are treated as set to the default.
        // We cannot enumerate all possible registers or pointers in a `State`,
        // so we instead we have to treat default values as if they were
        // undefined and refuse to include them in our hash.
        for (rk, rv) in self.cpu_state.iter() {
            if *rv == Default::default() {
                continue;
            }

            rk.hash(state);
            rv.hash(state);
        }

        for (p, mv) in self.mem_state.iter() {
            if *mv == Default::default() {
                continue;
            }

            p.hash(state);
            mv.hash(state);
        }

        for (s, cv) in self.context_state.iter() {
            if *cv == Default::default() {
                continue;
            }

            s.hash(state);
            cv.hash(state);
        }
    }
}

impl<AR> PartialOrd for State<AR>
where
    AR: Architecture,
    Symbolic<AR::Word>: Ord + Clone + Default,
    Symbolic<AR::Byte>: Ord + Clone + Default,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let rk_list = self
            .cpu_state
            .keys()
            .chain(other.cpu_state.keys())
            .cloned()
            .collect::<BTreeSet<AR::Register>>();

        for rk in rk_list.iter() {
            let cmp = self
                .cpu_state
                .get(rk)
                .cloned()
                .unwrap_or_else(Default::default)
                .partial_cmp(
                    &other
                        .cpu_state
                        .get(rk)
                        .cloned()
                        .unwrap_or_else(Default::default),
                );

            if cmp != Some(Ordering::Equal) {
                return cmp;
            }
        }

        let p_list = self
            .mem_state
            .keys()
            .chain(other.mem_state.keys())
            .cloned()
            .collect::<BTreeSet<Pointer<AR::PtrVal>>>();

        for p in p_list.iter() {
            let cmp = self
                .mem_state
                .get(p)
                .cloned()
                .unwrap_or_else(Default::default)
                .partial_cmp(
                    &other
                        .mem_state
                        .get(p)
                        .cloned()
                        .unwrap_or_else(Default::default),
                );

            if cmp != Some(Ordering::Equal) {
                return cmp;
            }
        }

        let ctxt_list = self
            .context_state
            .keys()
            .chain(other.context_state.keys())
            .cloned()
            .collect::<BTreeSet<String>>();

        for s in ctxt_list.iter() {
            let cmp = self
                .context_state
                .get(s)
                .cloned()
                .unwrap_or_else(Default::default)
                .partial_cmp(
                    &other
                        .context_state
                        .get(s)
                        .cloned()
                        .unwrap_or_else(Default::default),
                );

            if cmp != Some(Ordering::Equal) {
                return cmp;
            }
        }

        Some(Ordering::Equal)
    }
}

impl<AR> Ord for State<AR>
where
    AR: Architecture,
    Symbolic<AR::Word>: Ord + Clone + Default,
    Symbolic<AR::Byte>: Ord + Clone + Default,
{
    fn cmp(&self, other: &Self) -> Ordering {
        let rk_list = self
            .cpu_state
            .keys()
            .chain(other.cpu_state.keys())
            .cloned()
            .collect::<BTreeSet<AR::Register>>();

        for rk in rk_list.iter() {
            let cmp = self
                .cpu_state
                .get(rk)
                .cloned()
                .unwrap_or_else(Default::default)
                .cmp(
                    &other
                        .cpu_state
                        .get(rk)
                        .cloned()
                        .unwrap_or_else(Default::default),
                );

            if cmp != Ordering::Equal {
                return cmp;
            }
        }

        let p_list = self
            .mem_state
            .keys()
            .chain(other.mem_state.keys())
            .cloned()
            .collect::<BTreeSet<Pointer<AR::PtrVal>>>();

        for p in p_list.iter() {
            let cmp = self
                .mem_state
                .get(p)
                .cloned()
                .unwrap_or_else(Default::default)
                .cmp(
                    &other
                        .mem_state
                        .get(p)
                        .cloned()
                        .unwrap_or_else(Default::default),
                );

            if cmp != Ordering::Equal {
                return cmp;
            }
        }

        let ctxt_list = self
            .context_state
            .keys()
            .chain(other.context_state.keys())
            .cloned()
            .collect::<BTreeSet<String>>();

        for s in ctxt_list.iter() {
            let cmp = self
                .context_state
                .get(s)
                .cloned()
                .unwrap_or_else(Default::default)
                .cmp(
                    &other
                        .context_state
                        .get(s)
                        .cloned()
                        .unwrap_or_else(Default::default),
                );

            if cmp != Ordering::Equal {
                return cmp;
            }
        }

        Ordering::Equal
    }
}

impl<AR> State<AR>
where
    AR: Architecture,
{
    /// Determine if the trace that generated the current architectural state
    /// did or did not define the value of a register at some point in time.
    ///
    /// If the value was later undefined by other actions, this function will
    /// still return true. To determine if the register is currently undefined,
    /// get the value and check if it's symbolic or not.
    pub fn register_was_written(&self, k: &AR::Register) -> bool {
        self.cpu_state.get(k).is_some()
    }

    /// Determine if the trace that generated the current architectural state
    /// did or did not define the value of a memory location at some point in
    /// time.
    ///
    /// If the value was later undefined by other actions, this function will
    /// still return true. To determine if the memory location is currently
    /// undefined, get the value and check if it's symbolic or not.
    pub fn memory_was_written(&self, k: &Pointer<AR::PtrVal>) -> bool {
        self.mem_state.get(k).is_some()
    }

    pub fn set_register(&mut self, k: AR::Register, v: Symbolic<AR::Word>) {
        self.cpu_state.insert(k, v);
    }

    pub fn set_memory(&mut self, k: Pointer<AR::PtrVal>, v: Symbolic<AR::Byte>) {
        self.mem_state.insert(k, v);
    }

    /// Set an architectural context on this state.
    pub fn set_arch_context(&mut self, s: &str, v: Symbolic<u64>) {
        let inner_name = format!("A{}", s);
        self.context_state.insert(inner_name, v);
    }

    /// Get an architectural context from this state.
    pub fn get_arch_context(&self, s: &str) -> Symbolic<u64> {
        let inner_name = format!("A{}", s);
        if let Some(v) = self.context_state.get(&inner_name) {
            return *v;
        }

        Symbolic::default()
    }

    /// Set a platform context on this state.
    pub fn set_platform_context(&mut self, s: &str, v: Symbolic<u64>) {
        let inner_name = format!("P{}", s);
        self.context_state.insert(inner_name, v);
    }

    /// Get a platform context from this state.
    pub fn get_platform_context(&self, s: &str) -> Symbolic<u64> {
        let inner_name = format!("P{}", s);
        if let Some(v) = self.context_state.get(&inner_name) {
            return *v;
        }

        Symbolic::default()
    }

    /// Construct a new pointer with all of the contexts known to the current
    /// state.
    pub fn contextualize_pointer(&self, ptrval: AR::PtrVal) -> Pointer<AR::PtrVal> {
        Pointer::from_ptrval_and_contexts(ptrval, self.context_state.clone())
    }

    /// Contextualize the state with the contexts in a given contextual
    /// pointer.
    pub fn contextualize_self(&mut self, pointer: &Pointer<AR::PtrVal>) {
        for (is_arch, context, value) in pointer.iter_contexts() {
            if is_arch {
                self.set_arch_context(context, *value);
            } else {
                self.set_platform_context(context, *value);
            }
        }
    }
}

impl<AR> Default for State<AR>
where
    AR: Architecture,
{
    fn default() -> Self {
        State {
            cpu_state: HashMap::new(),
            mem_state: HashMap::new(),
            context_state: HashMap::new(),
        }
    }
}

impl<AR> State<AR>
where
    AR: Architecture,
{
    pub fn get_register(&self, k: &AR::Register) -> Symbolic<AR::Word> {
        if let Some(val) = self.cpu_state.get(k) {
            return val.clone();
        }

        Symbolic::default()
    }

    pub fn get_memory(&self, k: &Pointer<AR::PtrVal>) -> Symbolic<AR::Byte> {
        if let Some(val) = self.mem_state.get(k) {
            return val.clone();
        }

        Symbolic::default()
    }
}
