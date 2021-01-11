//! Analysis sections (e.g. blocks, subroutines, etc)

use crate::arch::Architecture;
use crate::maths::CheckedSub;
use crate::memory;
use crate::memory::Pointer;
use serde::{Deserialize, Serialize};
use std::cmp::min;
use std::collections::BTreeSet;
use std::convert::TryFrom;
use std::ops::Sub;

/// Represents a sequence of instructions with the following properties:
///
/// 1. The sequence of instructions are executed in sequence.
/// 2. Control flow does not diverge within the block.
/// 3. At the end of a block, execution diverges to zero or more other
///    locations, one of which may be the next instruction in sequence.
///
/// Effectively, it is a run of instructions with no jumps.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct Block<AR>
where
    AR: Architecture,
{
    /// The start pointer of the block.
    start: memory::Pointer<AR::PtrVal>,

    /// The offset to the end of the block.
    length: AR::Offset,

    /// A list of offsets known to be the start of instructions within the
    /// block.
    ///
    /// Offsets beyond the length of the block should be ignored, nor should
    /// they be allowed to remain in the offsets list.
    ///
    /// This is a `BTreeSet` purely because a `HashSet` would prohibit us from
    /// deriving `Ord`. In practice, it should never come into play when
    /// ordering blocks.
    instr_offsets: BTreeSet<AR::Offset>,

    /// The number of traces that have been executed within this block.
    traces: u32,
}

impl<AR> Block<AR>
where
    AR: Architecture,
    AR::Offset: TryFrom<<AR::PtrVal as Sub>::Output>,
{
    pub fn from_parts(start: memory::Pointer<AR::PtrVal>, length: AR::Offset) -> Self {
        Block {
            start,
            length,
            instr_offsets: BTreeSet::new(),
            traces: 0,
        }
    }

    pub fn as_start(&self) -> &memory::Pointer<AR::PtrVal> {
        &self.start
    }

    pub fn as_length(&self) -> &AR::Offset {
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

    pub fn is_ptr_within_block(&self, ptr: &memory::Pointer<AR::PtrVal>) -> bool {
        if self.start.as_pointer() <= ptr.as_pointer() {
            let diff = ptr.as_pointer().clone() - self.start.as_pointer().clone();

            if let Ok(diff) = AR::Offset::try_from(diff) {
                if diff < self.length.clone() && self.start.is_context_eq(ptr) {
                    return true;
                }
            }
        }

        false
    }

    /// Given a pointer, return the instruction it may be part of.
    pub fn align_to_instruction(
        &self,
        ptr: &memory::Pointer<AR::PtrVal>,
    ) -> Option<memory::Pointer<AR::PtrVal>> {
        if self.start.is_context_eq(ptr) && self.start.as_pointer() <= ptr.as_pointer() {
            let block_offset =
                AR::Offset::try_from(ptr.as_pointer().clone() - self.start.as_pointer().clone());

            if let Ok(block_offset) = block_offset {
                let adjusted_block_offset = self
                    .instr_offsets
                    .range(..=block_offset.clone())
                    .next_back()
                    .cloned()
                    .unwrap_or(block_offset);

                return Some(self.start.clone() + adjusted_block_offset);
            }
        }

        None
    }

    /// Given a pointer into the block, yield the offset of the previous
    /// instruction in this block, `count` instructions back.
    ///
    /// Pointers which are "between instructions" are treated as if they
    /// pointed to the last valid instruction offset prior to the current
    /// pointer.
    ///
    /// The location of the last instruction is returned as an `InstrLocation`,
    /// please see it's documentation for more info.
    ///
    /// If `None`, the pointer is outside of the current block already.
    pub fn last_instruction(
        &self,
        ptr: &memory::Pointer<AR::PtrVal>,
        count: usize,
    ) -> Option<InstrLocation<AR::Offset>> {
        if self.start.is_context_eq(ptr) && self.start.as_pointer() <= ptr.as_pointer() {
            let block_offset =
                AR::Offset::try_from(ptr.as_pointer().clone() - self.start.as_pointer().clone());

            if let Ok(block_offset) = block_offset {
                let adjusted_block_offset = self
                    .instr_offsets
                    .range(..=block_offset.clone())
                    .next_back()
                    .cloned()
                    .unwrap_or(block_offset);

                let mut last_i = count;
                for (i, instr_offset) in self
                    .instr_offsets
                    .range(..=adjusted_block_offset)
                    .rev()
                    .enumerate()
                {
                    last_i = count.saturating_sub(i);

                    if i == count {
                        return Some(InstrLocation::InsideBlock(instr_offset.clone()));
                    }
                }

                return Some(InstrLocation::OutsideBlock(last_i));
            }
        }

        None
    }

    /// Given a pointer into the block, yield the offset of the next
    /// instruction in the block.
    ///
    /// Pointers which are "between instructions" are treated as if they
    /// pointed to the last valid instruction offset prior to the current
    /// pointer.
    ///
    /// The location of the next instruction is returned as an `InstrLocation`,
    /// please see it's documentation for more info.
    ///
    /// If `None`, the pointer is outside of the current block already.
    pub fn next_instruction(
        &self,
        ptr: &memory::Pointer<AR::PtrVal>,
        count: usize,
    ) -> Option<InstrLocation<AR::Offset>> {
        if self.start.is_context_eq(ptr) && self.start.as_pointer() <= ptr.as_pointer() {
            let block_offset =
                AR::Offset::try_from(ptr.as_pointer().clone() - self.start.as_pointer().clone());

            if let Ok(block_offset) = block_offset {
                let adjusted_block_offset = self
                    .instr_offsets
                    .range(..=block_offset.clone())
                    .next_back()
                    .cloned()
                    .unwrap_or(block_offset);

                let mut last_i = count;
                for (i, instr_offset) in self
                    .instr_offsets
                    .range(adjusted_block_offset..)
                    .enumerate()
                {
                    last_i = count.saturating_sub(i);

                    if i == count {
                        return Some(InstrLocation::InsideBlock(instr_offset.clone()));
                    }
                }

                return Some(InstrLocation::OutsideBlock(last_i));
            }
        }

        None
    }

    /// Check if this and another block can be merged together.
    pub fn can_coalesce(&self, other: &Self) -> bool {
        let end = self.start.clone() + self.length.clone();
        let other_end = other.start.clone() + other.length.clone();

        end == other.start || other_end == self.start
    }

    /// Merge this and another block together.
    ///
    /// The result will likely be invalid if the blocks are not compatible, see
    /// `can_coalesce`.
    pub fn coalesce(self, other: Self) -> Self {
        let is_reversed = other.start < self.start;
        let first_start = min(self.start.clone(), other.start.clone());

        let instr_offsets = if is_reversed {
            other
                .instr_offsets
                .iter()
                .cloned()
                .chain(
                    self.instr_offsets
                        .iter()
                        .cloned()
                        .map(|o| o + other.length.clone()),
                )
                .collect()
        } else {
            self.instr_offsets
                .iter()
                .cloned()
                .chain(
                    other
                        .instr_offsets
                        .iter()
                        .cloned()
                        .map(|s| s + self.length.clone()),
                )
                .collect()
        };

        Block {
            start: first_start,
            length: self.length + other.length,
            instr_offsets,
            traces: self.traces + other.traces,
        }
    }

    /// Mark an instruction at a given address.
    ///
    /// If the given pointer is outside the bounds of the block, it will not be
    /// added to this block.
    pub fn mark_instr_at(&mut self, instr_ptr: Pointer<AR::PtrVal>) {
        if self.start.is_context_eq(&instr_ptr) {
            if let Ok(instr_offset) = AR::Offset::try_from(
                instr_ptr.as_pointer().clone() - self.start.as_pointer().clone(),
            ) {
                if instr_offset < self.length {
                    self.instr_offsets.insert(instr_offset);
                }
            }
        }
    }

    /// Iterate the list of instruction offsets within this block.
    pub fn instr_offsets(&self) -> impl '_ + Iterator<Item = AR::Offset> {
        self.instr_offsets.iter().cloned()
    }

    /// Reduce the length of this block, and produce a new block with a smaller
    /// size.
    ///
    /// The lower half of the block will be this block, modified inline. The
    /// upper half will be returned. This function yields `None` if the new
    /// size requested is longer than the current block, and no change occurs.
    pub fn split_block(&mut self, new_size: AR::Offset) -> Option<Self> {
        if &new_size < self.as_length() {
            let remaining_size = self.as_length().clone() - new_size.clone();
            let new_block_start = self
                .as_start()
                .contextualize(self.as_start().as_pointer().clone() + new_size.clone());

            let mut new_block = Block::from_parts(new_block_start, remaining_size);
            new_block.traces = self.traces;
            new_block.instr_offsets = self
                .instr_offsets
                .iter()
                .filter_map(|o| o.clone().checked_sub(new_size.clone()))
                .collect();

            self.length = new_size.clone();
            self.instr_offsets = self
                .instr_offsets
                .iter()
                .cloned()
                .filter(|o| o < &new_size.clone())
                .collect();

            return Some(new_block);
        }

        None
    }
}

/// Represents the result of searching for an instruction within a block.
///
/// A previous or next instruction can be present either within or outside of
/// a given block.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum InstrLocation<S> {
    /// The requested instruction exists within this block at this offset.
    InsideBlock(S),

    /// The requested instruction exists outside the current block.
    ///
    /// The `usize` offset here should be treated as a `Tumbler` scroll offset
    /// relative to the memory bus that generated the block you queried.
    OutsideBlock(usize),
}

#[cfg(test)]
mod tests {
    use crate::analysis::{Block, InstrLocation};
    use crate::arch::tests::TestArchitecture;
    use crate::memory::Pointer;
    use crate::reg::Symbolic;

    #[test]
    fn block_within() {
        let block: Block<TestArchitecture> = Block::from_parts(Pointer::from(0x100), 0x50);

        assert!(block.is_ptr_within_block(&Pointer::from(0x105)));
    }

    #[test]
    fn block_edge() {
        let block: Block<TestArchitecture> = Block::from_parts(Pointer::from(0x100), 0x50);

        assert!(!block.is_ptr_within_block(&Pointer::from(0x150)));
    }

    #[test]
    fn block_underflow() {
        let block: Block<TestArchitecture> = Block::from_parts(Pointer::from(0x100), 0x50);

        assert!(!block.is_ptr_within_block(&Pointer::from(0x50)));
    }

    #[test]
    fn block_separate_arch_context() {
        let mut base = Pointer::from(0x100);
        base.set_arch_context("T", Symbolic::from(0));

        let mut tgt = Pointer::from(0x105);
        tgt.set_arch_context("T", Symbolic::from(1));

        let block: Block<TestArchitecture> = Block::from_parts(base, 0x50);

        assert!(!block.is_ptr_within_block(&tgt));
    }

    #[test]
    fn block_separate_plat_context() {
        let mut base = Pointer::from(0x100);
        base.set_platform_context("R", Symbolic::from(0));

        let mut tgt = Pointer::from(0x105);
        tgt.set_platform_context("R", Symbolic::from(1));

        let block: Block<TestArchitecture> = Block::from_parts(base, 0x50);

        assert!(!block.is_ptr_within_block(&tgt));
    }

    #[test]
    fn block_separate_context() {
        let mut base = Pointer::from(0x100);
        base.set_arch_context("T", Symbolic::from(0));

        let mut tgt = Pointer::from(0x105);
        tgt.set_platform_context("R", Symbolic::from(1));

        let block: Block<TestArchitecture> = Block::from_parts(base, 0x50);

        assert!(!block.is_ptr_within_block(&tgt));
    }

    #[test]
    fn block_offsets() {
        let mut block: Block<TestArchitecture> = Block::from_parts(Pointer::from(0x100), 0x50);

        block.mark_instr_at(Pointer::from(0x105));

        let mut tgt = Pointer::from(0x109);
        tgt.set_platform_context("R", Symbolic::from(1));

        block.mark_instr_at(tgt);

        block.mark_instr_at(Pointer::from(0x195));

        let offsets: Vec<u32> = block.instr_offsets().collect();

        assert_eq!(vec![0x5], offsets);
    }

    #[test]
    fn block_align_to_instr() {
        let mut block: Block<TestArchitecture> = Block::from_parts(Pointer::from(0x100), 0x50);

        block.mark_instr_at(Pointer::from(0x100));
        block.mark_instr_at(Pointer::from(0x103));
        block.mark_instr_at(Pointer::from(0x106));
        block.mark_instr_at(Pointer::from(0x107));
        block.mark_instr_at(Pointer::from(0x108));
        block.mark_instr_at(Pointer::from(0x109));
        block.mark_instr_at(Pointer::from(0x10C));
        block.mark_instr_at(Pointer::from(0x10D));
        block.mark_instr_at(Pointer::from(0x10E));
        block.mark_instr_at(Pointer::from(0x10F));

        assert_eq!(
            block.align_to_instruction(&Pointer::from(0x103)),
            Some(Pointer::from(0x103))
        );
        assert_eq!(
            block.align_to_instruction(&Pointer::from(0x102)),
            Some(Pointer::from(0x100))
        );
        assert_eq!(
            block.align_to_instruction(&Pointer::from(0x104)),
            Some(Pointer::from(0x103))
        );
    }

    #[test]
    fn block_last_instr() {
        let mut block: Block<TestArchitecture> = Block::from_parts(Pointer::from(0x100), 0x50);

        block.mark_instr_at(Pointer::from(0x100));
        block.mark_instr_at(Pointer::from(0x103));
        block.mark_instr_at(Pointer::from(0x106));
        block.mark_instr_at(Pointer::from(0x107));
        block.mark_instr_at(Pointer::from(0x108));
        block.mark_instr_at(Pointer::from(0x109));
        block.mark_instr_at(Pointer::from(0x10C));
        block.mark_instr_at(Pointer::from(0x10D));
        block.mark_instr_at(Pointer::from(0x10E));
        block.mark_instr_at(Pointer::from(0x10F));

        assert_eq!(
            block.last_instruction(&Pointer::from(0x107), 1),
            Some(InstrLocation::InsideBlock(0x6))
        );

        assert_eq!(
            block.last_instruction(&Pointer::from(0x100), 1),
            Some(InstrLocation::OutsideBlock(1))
        );

        assert_eq!(
            block.last_instruction(&Pointer::from(0x102), 1),
            Some(InstrLocation::OutsideBlock(1))
        );

        assert_eq!(
            block.last_instruction(&Pointer::from(0x105), 1),
            Some(InstrLocation::InsideBlock(0))
        );

        assert_eq!(
            block.last_instruction(&Pointer::from(0x10F), 3),
            Some(InstrLocation::InsideBlock(0xC))
        );

        assert_eq!(
            block.last_instruction(&Pointer::from(0x107), 4),
            Some(InstrLocation::OutsideBlock(1))
        );

        assert_eq!(
            block.last_instruction(&Pointer::from(0x107), 5),
            Some(InstrLocation::OutsideBlock(2))
        );

        assert_eq!(
            block.last_instruction(&Pointer::from(0x107), 6),
            Some(InstrLocation::OutsideBlock(3))
        );
    }

    #[test]
    fn block_next_instr() {
        let mut block: Block<TestArchitecture> = Block::from_parts(Pointer::from(0x100), 0x50);

        block.mark_instr_at(Pointer::from(0x100));
        block.mark_instr_at(Pointer::from(0x103));
        block.mark_instr_at(Pointer::from(0x106));
        block.mark_instr_at(Pointer::from(0x107));
        block.mark_instr_at(Pointer::from(0x108));
        block.mark_instr_at(Pointer::from(0x109));
        block.mark_instr_at(Pointer::from(0x10C));
        block.mark_instr_at(Pointer::from(0x10D));
        block.mark_instr_at(Pointer::from(0x10E));
        block.mark_instr_at(Pointer::from(0x10F));

        assert_eq!(
            block.next_instruction(&Pointer::from(0x107), 1),
            Some(InstrLocation::InsideBlock(0x8))
        );

        assert_eq!(
            block.next_instruction(&Pointer::from(0x10F), 1),
            Some(InstrLocation::OutsideBlock(1))
        );

        assert_eq!(
            block.next_instruction(&Pointer::from(0x11F), 1),
            Some(InstrLocation::OutsideBlock(1))
        );

        assert_eq!(
            block.next_instruction(&Pointer::from(0x105), 1),
            Some(InstrLocation::InsideBlock(0x6))
        );

        assert_eq!(
            block.next_instruction(&Pointer::from(0x100), 3),
            Some(InstrLocation::InsideBlock(0x7))
        );

        assert_eq!(
            block.next_instruction(&Pointer::from(0x10C), 4),
            Some(InstrLocation::OutsideBlock(1))
        );

        assert_eq!(
            block.next_instruction(&Pointer::from(0x10C), 5),
            Some(InstrLocation::OutsideBlock(2))
        );

        assert_eq!(
            block.next_instruction(&Pointer::from(0x10C), 6),
            Some(InstrLocation::OutsideBlock(3))
        );
    }
}
