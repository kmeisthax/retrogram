//! Dynamic analysis passes

use crate::analysis::{Mappable, PrerequisiteAnalysis, Tracer};
use crate::memory::{Offset, Pointer, PtrNum};
use crate::reg::{Bitwise, Symbolic};
use crate::{analysis, memory, reg};
use num_traits::{One, Zero};
use std::convert::TryInto;
use std::ops::{AddAssign, Not};

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

type TraceResult<RK, I, P, MV, S> = analysis::Result<
    (
        memory::Pointer<P>,
        analysis::Trace<RK, I, P, MV>,
        reg::State<RK, I, P, MV>,
        Vec<Prerequisite<RK, I, P, MV, S>>,
    ),
    P,
    S,
>;

/// Trace a given precondition state until it is necessary to fork the analysis
/// into multiple states.
///
/// This function returns the precondition state and a pointer to the first
/// instruction which requires parallel tracing and cannot be executed
/// symbolically.
pub fn trace_until_fork<RK, I, P, MV, S, IO, PREREQ, TRACER>(
    pc: &memory::Pointer<P>,
    mut trace: analysis::Trace<RK, I, P, MV>,
    bus: &memory::Memory<P, MV, S, IO>,
    pre_state: &reg::State<RK, I, P, MV>,
    prereq: PREREQ,
    tracer: TRACER,
) -> TraceResult<RK, I, P, MV, S>
where
    RK: Mappable,
    P: Mappable + PtrNum<S>,
    S: Offset<P> + TryInto<usize> + AddAssign,
    I: Bitwise,
    MV: Bitwise,
    IO: One,
    memory::Pointer<P>: Clone,
    reg::State<RK, I, P, MV>: Clone,
    Symbolic<I>: Clone + Default,
    Symbolic<MV>: Clone + Default,
    PREREQ: PrerequisiteAnalysis<RK, I, P, MV, S, IO>,
    TRACER: Tracer<RK, I, P, MV, S, IO>,
{
    let mut new_pc = pc.clone();
    let mut new_state = pre_state.clone();

    loop {
        let (prerequisites, _is_complete) = prereq(&new_pc, bus, &new_state)?;
        let mut missing = vec![];

        for pr in prerequisites {
            match &pr {
                Prerequisite::Register { register, mask } => {
                    if !new_state
                        .get_register(register.clone())
                        .bits_are_concrete(mask.clone())
                    {
                        missing.push(pr);
                    }
                }
                Prerequisite::Memory { ptr, length, mask } => {
                    let mut i = S::zero();

                    while i < length.clone() {
                        let mask = mask
                            .get(
                                i.clone()
                                    .try_into()
                                    .map_err(|_| analysis::Error::BlockSizeOverflow)?,
                            )
                            .cloned()
                            .unwrap_or_else(MV::zero);

                        if !new_state
                            .get_memory(ptr.clone() + length.clone(), bus)
                            .bits_are_concrete(mask)
                        {
                            missing.push(pr);
                            break;
                        }

                        i += S::one();
                    }
                }
            }
        }

        if !missing.is_empty() {
            return Ok((new_pc, trace, new_state, missing));
        }

        let (next_state, next_pc) = tracer(&new_pc, bus, new_state, &mut trace)?;

        new_state = next_state;
        trace.traced_to(next_pc.clone());
        new_pc = next_pc;
    }
}
