//! Dynamic analysis passes

use crate::analysis::{Prerequisite, Trace};
use crate::arch::Architecture;
use crate::memory::{Memory, Pointer};
use crate::reg::{State, Symbolic};
use num::{One, Zero};
use std::cmp::{Ord, Ordering};
use std::collections::HashSet;
use std::convert::TryInto;

/// A single fork of an ongoing multiply-forked tracing operation.
///
/// Each fork represents the start of a potential tracing operation, and is
/// ranked by a branch count: the number of undefined bits that had to be
/// filled in to reach this particular point in execution. So, for example, if
/// tracing forked once on a prerequisite with one unsatisfied bit, and another
/// with three, then the branch count is four.
pub struct Fork<AR>
where
    AR: Architecture,
{
    /// The number of alternate forks that are executing alongside this fork.
    ///
    /// A fork with a higher `num_branches` count has produced more forks into
    /// the state space of the program.
    num_branches: f64,

    /// The place to start execution of this fork from.
    pc: Pointer<AR::PtrVal>,

    /// Any register or memory values which were defined either during the
    /// normal execution of the trace,
    pre_state: State<AR>,

    /// The execution history of this fork.
    trace: Trace<AR>,
}

impl<AR> Fork<AR>
where
    AR: Architecture,
{
    /// Construct an initial fork at some location.
    pub fn initial_fork(pc: Pointer<AR::PtrVal>, pre_state: State<AR>) -> Self {
        Self {
            num_branches: 0.0,
            pc: pc.clone(),
            pre_state,
            trace: Trace::begin_at(pc),
        }
    }

    /// Given a list of prerequisites and the end of a tracing operation,
    /// construct a new list of forks to pursue.
    pub fn make_forks(
        num_branches: f64,
        pc: Pointer<AR::PtrVal>,
        post_state: State<AR>,
        bus: &Memory<AR>,
        trace: Trace<AR>,
        prerequisites: &[Prerequisite<AR>],
    ) -> Vec<Self>
    where
        AR::Offset: TryInto<usize>,
    {
        if prerequisites.is_empty() {
            return vec![Self {
                num_branches,
                pc,
                pre_state: post_state,
                trace,
            }];
        }

        let mut state_list = HashSet::new();
        state_list.insert(post_state.clone());

        let mut addl_branch_bits = num_branches as f64;

        for prerequisite in prerequisites {
            //TODO: If the prerequisite list has overlaps, then this will be wrong
            addl_branch_bits +=
                (2.0 as f64).powf(prerequisite.necessary_forks(&post_state, bus) as f64);

            match prerequisite {
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

                    state_list = new_state_list;
                }
                Prerequisite::Memory { ptr, length, mask } => {
                    let mut count = AR::Offset::zero();

                    while count < length.clone() {
                        let ucount = count.clone().try_into().unwrap_or_else(|_| 0);
                        let mask_part = mask
                            .get(ucount)
                            .cloned()
                            .unwrap_or_else(|| !AR::Byte::zero());
                        if mask_part == AR::Byte::zero() {
                            count = count + AR::Offset::one();
                            continue;
                        }

                        let mut new_state_list = HashSet::new();

                        for state in state_list {
                            let mptr = ptr.clone() + count.clone();
                            let mv = state.get_memory(&mptr, bus);
                            let needed_mv = mv.clone() & Symbolic::from(mask_part.clone());
                            let unneeded_mv = mv & Symbolic::from(!(mask_part.clone()));

                            for possible_mv in needed_mv.valid() {
                                let mut new_state = state.clone();
                                let new_mv = Symbolic::from(possible_mv) | unneeded_mv.clone();
                                new_state.set_memory(mptr.clone(), new_mv);

                                new_state_list.insert(new_state);
                            }
                        }

                        state_list = new_state_list;
                    }
                }
            }
        }

        let mut fork_list = Vec::new();

        for state in state_list {
            fork_list.push(Fork {
                num_branches: addl_branch_bits as f64,
                pc: pc.clone(),
                pre_state: state,
                trace: trace.clone(),
            })
        }

        fork_list
    }

    /// Consume a Fork, returning the branch count, PC, state, and the trace
    /// that got us this far.
    #[allow(clippy::type_complexity)]
    pub fn into_parts(self) -> (f64, Pointer<AR::PtrVal>, State<AR>, Trace<AR>) {
        (self.num_branches, self.pc, self.pre_state, self.trace)
    }
}

impl<AR> PartialEq for Fork<AR>
where
    AR: Architecture,
{
    fn eq(&self, other: &Self) -> bool {
        self.num_branches == other.num_branches
            && self.pc == other.pc
            && self.pre_state == other.pre_state
    }
}

impl<AR> Eq for Fork<AR> where AR: Architecture {}

impl<AR> PartialOrd for Fork<AR>
where
    AR: Architecture,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(
            self.num_branches
                .partial_cmp(&other.num_branches)?
                .then(self.pc.partial_cmp(&other.pc)?)
                .then(self.pre_state.partial_cmp(&other.pre_state)?),
        )
    }
}

impl<AR> Ord for Fork<AR>
where
    AR: Architecture,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.num_branches
            .partial_cmp(&other.num_branches)
            .unwrap_or(Ordering::Equal) //Will cause logic errors if we ever actually have NaN branches
            .then_with(|| self.pc.cmp(&other.pc))
            .then_with(|| self.pre_state.cmp(&other.pre_state))
    }
}
