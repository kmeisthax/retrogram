//! Prerequisite list type

use crate::arch::Architecture;
use crate::maths::Popcount;
use crate::memory::{Memory, Pointer};
use crate::reg::State;
use num_traits::{One, Zero};
use std::convert::TryInto;

/// Indicates a memory or register value that needs to be a concrete value
/// before execution can continue.
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
        ptr: Pointer<AR::PtrVal>,

        /// How wide the memory location is.
        length: AR::Offset,

        /// Which bits are considered necessary to be resolved.
        ///
        /// Memory locations not listed in the mask shall be considered equal
        /// to all-ones. Ergo, to indicate memory that needs total resolution,
        /// you may use an empty `Vec`.
        mask: Vec<AR::Byte>,
    },
}

impl<AR> Prerequisite<AR>
where
    AR: Architecture,
{
    pub fn memory(ptr: Pointer<AR::PtrVal>, length: AR::Offset) -> Self {
        Prerequisite::Memory {
            ptr,
            length,
            mask: vec![],
        }
    }

    pub fn register(register: AR::Register, mask: AR::Word) -> Self {
        Prerequisite::Register { register, mask }
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
    pub fn necessary_forks<IO>(
        &self,
        state: &State<AR::Register, AR::Word, AR::PtrVal, AR::Byte>,
        bus: &Memory<AR::PtrVal, AR::Byte, AR::Offset, IO>,
    ) -> u64
    where
        IO: One,
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
                let mut count = AR::Offset::zero();

                while count < length.clone() {
                    let mv = state.get_memory(&(ptr.clone() + count.clone()), bus);
                    let this_needs = mv.not_cares()
                        & mask
                            .get(count.clone().try_into().unwrap_or_else(|_| 0))
                            .cloned()
                            .unwrap_or_else(|| !AR::Byte::zero());

                    needs += this_needs.pop_count().try_into().unwrap_or_else(|_| 0);

                    count = count + AR::Offset::one();
                }

                needs
            }
        }
    }

    pub fn from_register(register: AR::Register) -> Self {
        Prerequisite::Register {
            register,
            mask: !AR::Word::zero(),
        }
    }
}
