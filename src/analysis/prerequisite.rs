//! Prerequisite list type

use crate::analysis::Mappable;
use crate::maths::{Numerical, Popcount};
use crate::memory::{Memory, Offset, Pointer, PtrNum};
use crate::reg::{Bitwise, State};
use num_traits::{One, Zero};
use std::convert::TryInto;
use std::ops::Not;

/// Indicates a memory or register value that needs to be a concrete value
/// before execution can continue.
pub enum Prerequisite<RK, I, P, MV, S> {
    /// A register that must be resolved before execution can continue.
    Register {
        /// The register to resolve.
        register: RK,

        /// Which bits are considered necessary to be resolved.
        ///
        /// A value of all-ones (e.g. 0xFF) would indicate a register which
        /// needs total resolution, while a value of all-zeroes would indicate
        /// a register that does not need to be resolved.
        mask: I,
    },

    /// A memory location (or set of locations) that must be resolved before
    /// execution can continue.
    Memory {
        /// The memory location to resolve.
        ptr: Pointer<P>,

        /// How wide the memory location is.
        length: S,

        /// Which bits are considered necessary to be resolved.
        ///
        /// Memory locations not listed in the mask shall be considered equal
        /// to all-ones. Ergo, to indicate memory that needs total resolution,
        /// you may use an empty `Vec`.
        mask: Vec<MV>,
    },
}

impl<RK, I, P, MV, S> Prerequisite<RK, I, P, MV, S> {
    pub fn memory(ptr: Pointer<P>, length: S) -> Self {
        Prerequisite::Memory {
            ptr,
            length,
            mask: vec![],
        }
    }

    pub fn register(register: RK, mask: I) -> Self {
        Prerequisite::Register { register, mask }
    }
}

impl<RK, I, P, MV, S> Prerequisite<RK, I, P, MV, S>
where
    RK: Mappable,
    I: Bitwise + TryInto<u64> + Popcount<Output = I>,
    P: Mappable + PtrNum<S>,
    S: Numerical + Offset<P> + TryInto<usize>,
    MV: Bitwise + TryInto<u64> + Popcount<Output = MV>,
{
    /// Compute the number of forks needed to explore every branch implied by a
    /// given set of prerequisites, with the current execution state and memory
    /// bus.
    ///
    /// The returned state count will be in bits (log-2). If considering
    /// multiple prerequisites or otherwise accumulating a branch count, you
    /// cannot add fork counts directly. Adding actually multiplies the number
    /// of forks. To add two unrelated numbers of forks, you must first raise
    /// 2 to the power of the fork count, then add.
    pub fn necessary_forks<IO>(
        &self,
        state: &State<RK, I, P, MV>,
        bus: &Memory<P, MV, S, IO>,
    ) -> u64
    where
        IO: One,
    {
        match self {
            Prerequisite::Register { register, mask } => {
                let rv = state.get_register(register);
                let needs = rv.not_cares() & mask.clone();

                needs.pop_count().try_into().unwrap_or_else(|_| 0)
            }
            Prerequisite::Memory { ptr, length, mask } => {
                let mut needs = 0;
                let mut count = S::zero();

                while count < length.clone() {
                    let mv = state.get_memory(&(ptr.clone() + count.clone()), bus);
                    let this_needs = mv.not_cares()
                        & mask
                            .get(count.clone().try_into().unwrap_or_else(|_| 0))
                            .cloned()
                            .unwrap_or_else(|| !MV::zero());

                    needs += this_needs.pop_count().try_into().unwrap_or_else(|_| 0);

                    count = count + S::one();
                }

                needs
            }
        }
    }
}

impl<RK, I, P, MV, S> From<RK> for Prerequisite<RK, I, P, MV, S>
where
    I: Zero + Not<Output = I>,
{
    fn from(register: RK) -> Self {
        Prerequisite::Register {
            register,
            mask: !I::zero(),
        }
    }
}
