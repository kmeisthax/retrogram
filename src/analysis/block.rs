//! Analysis sections (e.g. blocks, subroutines, etc)

use crate::{analysis, memory};
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
    P: analysis::Mappable,
{
    start: memory::Pointer<P>,
    length: S,
}

impl<P, S> Block<P, S>
where
    P: analysis::Mappable,
{
    pub fn from_parts(start: memory::Pointer<P>, length: S) -> Self {
        Block {
            start: start,
            length: length,
        }
    }

    pub fn as_start(&self) -> &memory::Pointer<P> {
        &self.start
    }

    pub fn as_length(&self) -> &S {
        &self.length
    }
}

impl<P, S> Block<P, S>
where
    P: analysis::Mappable + memory::PtrNum<S>,
    S: memory::Offset<P>,
{
    pub fn is_ptr_within_block(&self, ptr: &memory::Pointer<P>) -> bool {
        if self.start.as_pointer() <= ptr.as_pointer() {
            let diff = ptr.as_pointer().clone() - self.start.as_pointer().clone();

            if let Ok(diff) = S::try_from(diff) {
                if diff < self.length.clone() {
                    if self.start.is_context_eq(ptr) {
                        return true;
                    }
                }
            }
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::Block;
    use crate::memory::Pointer;
    use crate::reg::{New, Symbolic};

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
        base.set_arch_context("T", Symbolic::new(0));

        let mut tgt = Pointer::from(0x105);
        tgt.set_arch_context("T", Symbolic::new(1));

        let block = Block::from_parts(base, 0x50);

        assert!(!block.is_ptr_within_block(&tgt));
    }

    #[test]
    fn block_separate_plat_context() {
        let mut base = Pointer::from(0x100);
        base.set_platform_context("R", Symbolic::new(0));

        let mut tgt = Pointer::from(0x105);
        tgt.set_platform_context("R", Symbolic::new(1));

        let block = Block::from_parts(base, 0x50);

        assert!(!block.is_ptr_within_block(&tgt));
    }

    #[test]
    fn block_separate_context() {
        let mut base = Pointer::from(0x100);
        base.set_arch_context("T", Symbolic::new(0));

        let mut tgt = Pointer::from(0x105);
        tgt.set_platform_context("R", Symbolic::new(1));

        let block = Block::from_parts(base, 0x50);

        assert!(!block.is_ptr_within_block(&tgt));
    }
}
