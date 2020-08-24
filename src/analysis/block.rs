//! Analysis sections (e.g. blocks, subroutines, etc)

use crate::analysis::Mappable;
use crate::cli::Nameable;
use crate::memory;
use serde::{Deserialize, Serialize};

/// Represents a sequence of instructions with the following properties:
///
/// 1. The sequence of instructions are executed in sequence.
/// 2. Control flow does not diverge within the block.
/// 3. At the end of a block, execution diverges to zero or more other
///    locations, one of which may be the next instruction in sequence.
///
/// Effectively, it is a run of instructions with no jumps.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct Block<P, S>
where
    P: Mappable + Nameable,
{
    /// The start pointer of the block.
    start: memory::Pointer<P>,

    /// The offset to the end of the block.
    length: S,

    /// The number of traces that have been executed within this block.
    traces: u32,
}

impl<P, S> Block<P, S>
where
    P: Mappable + Nameable,
{
    pub fn from_parts(start: memory::Pointer<P>, length: S) -> Self {
        Block {
            start,
            length,
            traces: 0,
        }
    }

    pub fn as_start(&self) -> &memory::Pointer<P> {
        &self.start
    }

    pub fn as_length(&self) -> &S {
        &self.length
    }

    /// Return how many traces have been executed within this block.
    pub fn traces(&self) -> u32 {
        self.traces
    }

    /// Increase the trace count for this block.
    pub fn add_traces(&mut self, new_traces: u32) {
        self.traces = self.traces.saturating_add(new_traces);
    }
}

impl<P, S> Block<P, S>
where
    P: memory::PtrNum<S> + Mappable + Nameable,
    S: memory::Offset<P>,
{
    pub fn is_ptr_within_block(&self, ptr: &memory::Pointer<P>) -> bool {
        if self.start.as_pointer() <= ptr.as_pointer() {
            let diff = ptr.as_pointer().clone() - self.start.as_pointer().clone();

            if let Ok(diff) = S::try_from(diff) {
                if diff < self.length.clone() && self.start.is_context_eq(ptr) {
                    return true;
                }
            }
        }

        false
    }

    /// Check if this and another block can be merged together.
    pub fn can_coalesce(&self, other: &Self) -> bool {
        let end = self.start.clone() + self.length.clone();

        end == other.start
    }

    /// Merge this and another block together.
    ///
    /// The result will likely be invalid if the blocks are not compatible, see
    /// `can_coalesce`.
    pub fn coalesce(self, other: Self) -> Self {
        Block {
            start: self.start,
            length: self.length + other.length,
            traces: self.traces + other.traces,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::Block;
    use crate::memory::Pointer;
    use crate::reg::Symbolic;

    #[test]
    fn block_within() {
        let block = Block::from_parts(Pointer::from(0x100), 0x50);

        assert!(block.is_ptr_within_block(&Pointer::from(0x105)));
    }

    #[test]
    fn block_edge() {
        let block = Block::from_parts(Pointer::from(0x100), 0x50);

        assert!(!block.is_ptr_within_block(&Pointer::from(0x150)));
    }

    #[test]
    fn block_underflow() {
        let block = Block::from_parts(Pointer::from(0x100), 0x50);

        assert!(!block.is_ptr_within_block(&Pointer::from(0x50)));
    }

    #[test]
    fn block_separate_arch_context() {
        let mut base = Pointer::from(0x100);
        base.set_arch_context("T", Symbolic::from(0));

        let mut tgt = Pointer::from(0x105);
        tgt.set_arch_context("T", Symbolic::from(1));

        let block = Block::from_parts(base, 0x50);

        assert!(!block.is_ptr_within_block(&tgt));
    }

    #[test]
    fn block_separate_plat_context() {
        let mut base = Pointer::from(0x100);
        base.set_platform_context("R", Symbolic::from(0));

        let mut tgt = Pointer::from(0x105);
        tgt.set_platform_context("R", Symbolic::from(1));

        let block = Block::from_parts(base, 0x50);

        assert!(!block.is_ptr_within_block(&tgt));
    }

    #[test]
    fn block_separate_context() {
        let mut base = Pointer::from(0x100);
        base.set_arch_context("T", Symbolic::from(0));

        let mut tgt = Pointer::from(0x105);
        tgt.set_platform_context("R", Symbolic::from(1));

        let block = Block::from_parts(base, 0x50);

        assert!(!block.is_ptr_within_block(&tgt));
    }
}
