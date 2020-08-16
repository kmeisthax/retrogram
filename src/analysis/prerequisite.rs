//! Prerequisite list type

use crate::arch::Architecture;
use crate::maths::Popcount;
use crate::memory::Memory;
use crate::reg::{State, Symbolic};
use num::{One, Zero};
use std::collections::HashSet;
use std::convert::TryInto;

/// Indicates a memory or register value that needs to be a concrete value
/// before execution can continue.
#[derive(Debug)]
pub enum Prerequisite<AR>
where
    AR: Architecture,
{
    /// A register that must be resolved before execution can continue.
    Register {
        /// The register to resolve.
        register: AR::Register,

        /// Which bits are considered necessary to be resolved.
        ///
        /// A value of all-ones (e.g. 0xFF) would indicate a register which
        /// needs total resolution, while a value of all-zeroes would indicate
        /// a register that does not need to be resolved.
        mask: AR::Word,
    },

    /// A memory location (or set of locations) that must be resolved before
    /// execution can continue.
    Memory {
        /// The memory location to resolve.
        ///
        /// Contexts on this pointer will be resolved with the current state.
        ptr: AR::PtrVal,

        /// How wide the memory location is.
        length: AR::Offset,

        /// Which bits are considered necessary to be resolved.
        ///
        /// Memory locations not listed in the mask shall be considered equal
        /// to all-ones. Ergo, to indicate memory that needs total resolution,
        /// you may use an empty `Vec`.
        mask: Vec<AR::Byte>,
    },

    /// An architectural context value to resolve.
    ArchitecturalContext {
        /// The context to be resolved.
        context: String,

        /// The bits we care about.
        mask: u64,
    },

    /// A platform context value to resolve.
    PlatformContext {
        /// The context to be resolved.
        context: String,

        /// The bits we care about.
        mask: u64,
    },
}

impl<AR> Prerequisite<AR>
where
    AR: Architecture,
{
    /// Construct a new memory prerequisite.
    pub fn memory(ptr: AR::PtrVal, length: AR::Offset) -> Self {
        Prerequisite::Memory {
            ptr,
            length,
            mask: vec![],
        }
    }

    /// Construct a new register prerequisite.
    pub fn register(register: AR::Register, mask: AR::Word) -> Self {
        Prerequisite::Register { register, mask }
    }

    /// Construct a new architectural context prerequisite.
    pub fn arch_context(context: String, mask: u64) -> Self {
        Prerequisite::ArchitecturalContext { context, mask }
    }

    /// Construct a new platform context prerequisite.
    pub fn platform_context(context: String, mask: u64) -> Self {
        Prerequisite::PlatformContext { context, mask }
    }

    /// Compute the number of forks needed to explore every branch implied by a
    /// given set of prerequisites, with the current execution state and memory
    /// bus.
    ///
    /// The returned state count will be in bits (log-2). If considering
    /// multiple prerequisites or otherwise accumulating a branch count, you
    /// cannot add fork counts directly. Adding actually multiplies the number
    /// of forks. To add two unrelated numbers of forks, you must first raise
    /// 2 to the power of the fork count, then add.
    ///
    /// Memory prerequisites on improperly contextualized states will generate
    /// forks as if the underlying memory was undefined. This is the worst-case
    /// scenario. If the contexts needed are forked on beforehand, then you may
    /// get a more accurate result.
    ///
    /// A fork count of zero indicates that the prerequisite is already
    /// satisfied by the current state.
    pub fn necessary_forks(&self, state: &State<AR>, bus: &Memory<AR>) -> u64
    where
        AR::Word: TryInto<u64>,
        AR::Byte: TryInto<u64>,
        AR::Offset: TryInto<usize>,
    {
        match self {
            Prerequisite::Register { register, mask } => {
                let rv = state.get_register(register);
                let needs = rv.not_cares() & mask.clone();

                needs.pop_count().try_into().unwrap_or_else(|_| 0)
            }
            Prerequisite::Memory { ptr, length, mask } => {
                let mut needs = 0;
                let data = bus.read_memory_stateful(ptr.clone(), length.clone(), state);

                for (count, mv) in data.iter().enumerate() {
                    let this_needs = mv.not_cares()
                        & mask
                            .get(count)
                            .cloned()
                            .unwrap_or_else(|| !AR::Byte::zero());

                    needs += this_needs.pop_count().try_into().unwrap_or_else(|_| 0);
                }

                needs
            }
            Prerequisite::ArchitecturalContext { context, mask } => {
                let cval = state.get_arch_context(context);
                let needs = cval.not_cares() & mask;

                needs.pop_count()
            }
            Prerequisite::PlatformContext { context, mask } => {
                let cval = state.get_platform_context(context);
                let needs = cval.not_cares() & mask;

                needs.pop_count()
            }
        }
    }

    /// Fork all of the states in the list such that this prerequisite is
    /// satisfied.
    pub fn fork_state(
        &self,
        state_list: &HashSet<State<AR>>,
        bus: &Memory<AR>,
    ) -> HashSet<State<AR>> {
        match self {
            Prerequisite::Register { register, mask } => {
                let mut new_state_list = HashSet::new();

                for state in state_list {
                    let rv = state.get_register(register);
                    let needed_rv = rv.clone() & Symbolic::from(mask.clone());
                    let unneeded_rv = rv & Symbolic::from(!(mask.clone()));

                    for possible_rv in needed_rv.valid() {
                        let mut new_state = state.clone();
                        let new_rv = Symbolic::from(possible_rv) | unneeded_rv.clone();
                        new_state.set_register(register.clone(), new_rv);

                        new_state_list.insert(new_state);
                    }
                }

                new_state_list
            }
            Prerequisite::Memory { ptr, length, mask } => {
                let mut count = AR::Offset::zero();
                let mut state_list = state_list.clone();

                while count < length.clone() {
                    let ucount = count.clone().try_into().unwrap_or_else(|_| 0);
                    let mask_part = mask
                        .get(ucount as usize)
                        .cloned()
                        .unwrap_or_else(|| !AR::Byte::zero());
                    if mask_part == AR::Byte::zero() {
                        count = count + AR::Offset::one();
                        continue;
                    }

                    let mut new_state_list = HashSet::new();

                    for state in state_list {
                        let mptr = ptr.clone() + count.clone();
                        let context_prerequisites = bus.prerequisites(mptr.clone());
                        let mut child_state_list = HashSet::new();
                        child_state_list.insert(state);

                        // Whoops, looks like we need to resolve this recursively!
                        for pr in context_prerequisites {
                            if matches!(pr, Prerequisite::Memory { .. }) {
                                continue;
                            }

                            child_state_list = pr.fork_state(&child_state_list, bus);
                        }

                        for child_state in child_state_list {
                            let mv = bus.read_unit_stateful(mptr.clone(), &child_state);
                            let needed_mv = mv.clone() & Symbolic::from(mask_part.clone());
                            let unneeded_mv = mv & Symbolic::from(!(mask_part.clone()));

                            for possible_mv in needed_mv.valid() {
                                let mut new_state = child_state.clone();
                                let new_mv = Symbolic::from(possible_mv) | unneeded_mv.clone();
                                new_state.set_memory(
                                    child_state.contextualize_pointer(mptr.clone()),
                                    new_mv,
                                );

                                new_state_list.insert(new_state);
                            }
                        }
                    }

                    state_list = new_state_list;
                }

                state_list
            }
            Prerequisite::ArchitecturalContext { context, mask } => {
                let mut new_state_list = HashSet::new();

                for state in state_list {
                    let cv = state.get_arch_context(context);
                    let needed_cv = cv & Symbolic::from(*mask);
                    let unneeded_cv = cv & Symbolic::from(!mask);

                    for possible_cv in needed_cv.valid() {
                        let mut new_state = state.clone();
                        let new_cv = Symbolic::from(possible_cv) | unneeded_cv;
                        new_state.set_arch_context(context, new_cv);

                        new_state_list.insert(new_state);
                    }
                }

                new_state_list
            }
            Prerequisite::PlatformContext { context, mask } => {
                let mut new_state_list = HashSet::new();

                for state in state_list {
                    let cv = state.get_platform_context(context);
                    let needed_cv = cv & Symbolic::from(*mask);
                    let unneeded_cv = cv & Symbolic::from(!mask);

                    for possible_cv in needed_cv.valid() {
                        let mut new_state = state.clone();
                        let new_cv = Symbolic::from(possible_cv) | unneeded_cv;
                        new_state.set_platform_context(context, new_cv);

                        new_state_list.insert(new_state);
                    }
                }

                new_state_list
            }
        }
    }
}
