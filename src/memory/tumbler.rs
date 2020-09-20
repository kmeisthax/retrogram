//! Memory space navigation

use crate::analysis::{InstrLocation, replace_labels};
use crate::arch::Architecture;
use crate::database::Database;
use crate::maths::CheckedSub;
use crate::memory::{Memory, Pointer};
use crate::asm::Assembler;
use crate::ast::{Section, Directive};
use num_traits::{One, Zero};

/// A discrete space on a given memory bus that allows indexing through the
/// contents of that bus.
///
/// The purpose of a Tumbler is to allow a UI to scan through the disassembly
/// line by line. Ergo, it exists precisely to relate lines of a disassembly to
/// regions and image offsets.
///
/// The word "Tumbler" is stolen from Ted Nelson's Project Xanadu, where it is
/// used to indicate a numerical addressing scheme that subsumes other forms of
/// numerical addressing.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tumbler {
    /// The region this tumbler references.
    region_index: usize,

    /// An offset into the region being referenced.
    image_index: usize,

    /// An offset into each line within the individual location's disassembly.
    line_index: usize,
}

impl Default for Tumbler {
    fn default() -> Tumbler {
        Tumbler {
            region_index: 0,
            image_index: 0,
            line_index: 0,
        }
    }
}

impl From<(usize, usize, usize)> for Tumbler {
    fn from(pair: (usize, usize, usize)) -> Tumbler {
        Tumbler {
            region_index: pair.0,
            image_index: pair.1,
            line_index: pair.2,
        }
    }
}

impl Tumbler {
    /// Retrieve the region-index component of this tumbler.
    pub fn region_index(&self) -> usize {
        self.region_index
    }

    /// Retrieve the image-index component of this tumbler.
    pub fn image_index(&self) -> usize {
        self.image_index
    }

    /// Get the index and length of the region after a given one.
    ///
    /// If `None`, then the bus is empty.
    fn next_region<AR>(&self, bus: &Memory<AR>, region: usize) -> Option<(usize, usize)>
    where
        AR: Architecture,
    {
        let mut next_r = region.checked_add(1).unwrap_or(0);
        let mut max_io = bus.region_image_size(next_r);
        if max_io.is_none() {
            next_r = 0;
            max_io = bus.region_image_size(next_r);
        }

        Some((next_r, max_io?))
    }

    /// Get the index and length of the current region, or if it's invalid,
    /// then the index of some other region.
    ///
    /// This returns a new region index, image offset, and max offset.
    ///
    /// If `None`, then the bus is empty.
    fn valid_region<AR>(&self, bus: &Memory<AR>) -> Option<(usize, usize, usize)>
    where
        AR: Architecture,
    {
        let mut this_r = self.region_index;
        let mut this_i = self.image_index;
        let mut max_io = bus.region_image_size(this_r);
        if max_io.is_none() {
            this_r = 0;
            this_i = 0;
            max_io = bus.region_image_size(this_r);
        }

        Some((this_r, this_i, max_io?))
    }

    /// Get the index and length of the region before a given one.
    ///
    /// If `None`, then the bus is empty.
    pub fn prev_region<AR>(&self, bus: &Memory<AR>, region: usize) -> Option<(usize, usize)>
    where
        AR: Architecture,
    {
        let max_r = bus.region_count();
        let mut prev_r = region
            .checked_sub(1)
            .unwrap_or_else(|| max_r.saturating_sub(1));
        let mut max_io = bus.region_image_size(prev_r);
        if max_io.is_none() {
            prev_r = max_r.saturating_sub(1);
            max_io = bus.region_image_size(prev_r);
        }

        Some((prev_r, max_io?))
    }

    /// Determine how many lines of disassembly exist at a particular memory
    /// location.
    pub fn disasm_lines_at_location<AR, ASM>(&self, arch: AR, asm: ASM, bus: &Memory<AR>, db: &mut Database<AR>, loc: &Pointer<AR::PtrVal>) -> usize where AR: Architecture, ASM: Assembler {
        if let Some(_) = db.find_block_membership(loc).and_then(|id| db.block(id)) {
            match arch.disassemble(loc, bus) {
                Ok(disasm) => {
                    let mut instr_directive = Section::new("");

                    instr_directive.append_directive(Directive::EmitInstr(disasm.as_instr().clone(), disasm.next_offset()), loc.clone());

                    instr_directive = replace_labels(instr_directive, db, bus);

                    let mut disasm = "";

                    for (directive, _size) in instr_directive.iter_directives() {
                        if directive.is_emit_instr() {
                            asm.emit_instr(&mut disasm, directive);
                        }
                    }

                    disasm.matches("\n").count()
                },
                Err(e) => 1
            }
        } else {
            1
        }
    }

    /// Produce a tumbler a given number of lines ahead.
    ///
    /// If `None`, then the bus is empty.
    pub fn scroll_forward_by_lines<AR>(
        self,
        bus: &Memory<AR>,
        db: &Database<AR>,
        mut amount: usize,
    ) -> Option<Self>
    where
        AR: Architecture,
    {
        let (mut next_r, mut next_io, mut max_io) = self.valid_region(bus)?;
        let mut encoded = bus.encode_tumbler(self)?;

        while amount > 0 {
            if let Some(block) = db
                .find_block_membership(&encoded)
                .and_then(|bid| db.block(bid))
            {
                match block.next_instruction(&encoded, amount)? {
                    InstrLocation::InsideBlock(s) => {
                        encoded = encoded.contextualize(block.as_start().as_pointer().clone() + s);
                        amount = 0;
                    }
                    InstrLocation::OutsideBlock(new_count) => {
                        // Try to find the address of the block right after
                        // this one. This involves switching between tumbler
                        // and pointer form multiple times.
                        encoded = encoded.contextualize(
                            block.as_start().as_pointer().clone()
                                + block
                                    .as_length()
                                    .clone()
                                    .checked_sub(AR::Offset::one())
                                    .unwrap_or_else(AR::Offset::zero),
                        );

                        let new_parts = bus.decode_tumbler(encoded.clone())?.valid_region(bus)?;

                        if new_parts.1 < (new_parts.2 - 1) {
                            next_r = new_parts.0;
                            next_io = new_parts.1 + 1;
                        } else {
                            let (r_plus, _r_plus_max_io) = self.next_region(bus, next_r).unwrap();

                            next_r = r_plus;
                            next_io = 0;
                        }

                        encoded = bus.encode_tumbler((next_r, next_io).into())?;
                        amount = new_count.saturating_sub(1);
                    }
                };

                if let Some(other_block) = db
                    .find_block_membership(&encoded)
                    .and_then(|bid| db.block(bid))
                {
                    encoded = other_block.align_to_instruction(&encoded)?;
                }

                let decoded = bus.decode_tumbler(encoded.clone())?;
                let new_parts = decoded.valid_region(bus)?;

                next_r = new_parts.0;
                next_io = new_parts.1;
            } else {
                //TODO: This used to be able to skip multiple addresses when
                //scrolling, but now we can't because of the block thing. We
                //should at least be able to skip to the next block.
                let remaining_io = max_io.saturating_sub(next_io + 1);
                if remaining_io == 0 {
                    let (r_plus, r_plus_max_io) = self.next_region(bus, next_r).unwrap();

                    max_io = r_plus_max_io;
                    next_r = r_plus;
                    next_io = 0;
                    encoded = bus.encode_tumbler((next_r, next_io).into())?;
                    amount = amount.saturating_sub(1);
                } else {
                    next_io += 1;
                    encoded = bus.encode_tumbler((next_r, next_io).into())?;
                    amount = amount.saturating_sub(1);
                }
            }
        }

        if let Some(block) = db
            .find_block_membership(&encoded)
            .and_then(|bid| db.block(bid))
        {
            encoded = block.align_to_instruction(&encoded)?;

            let new_parts = bus.decode_tumbler(encoded.clone())?.valid_region(bus)?;

            next_r = new_parts.0;
            next_io = new_parts.1;
        }

        Some((next_r, next_io).into())
    }

    /// Produce a tumbler a given number of lines behind.
    ///
    /// If `None`, then the bus is empty.
    pub fn scroll_backward_by_lines<AR>(
        self,
        bus: &Memory<AR>,
        db: &Database<AR>,
        mut amount: usize,
    ) -> Option<Self>
    where
        AR: Architecture,
    {
        let (mut next_r, mut next_io, mut _max_io) = self.valid_region(bus)?;
        let mut encoded = bus.encode_tumbler(self)?;

        while amount > 0 {
            if let Some(block) = db
                .find_block_membership(&encoded)
                .and_then(|bid| db.block(bid))
            {
                match block.last_instruction(&encoded, amount)? {
                    InstrLocation::InsideBlock(s) => {
                        encoded = encoded.contextualize(block.as_start().as_pointer().clone() + s);
                        amount = 0;
                    }
                    InstrLocation::OutsideBlock(new_count) => {
                        // Try to find the address of the block right before
                        // this one. This involves switching between tumbler
                        // and pointer form multiple times.
                        encoded = encoded.contextualize(block.as_start().as_pointer().clone());
                        let new_parts = bus.decode_tumbler(encoded.clone())?.valid_region(bus)?;

                        if new_parts.1 > 0 {
                            next_r = new_parts.0;
                            next_io = new_parts.1 - 1;
                        } else {
                            let (r_minus, r_minus_max_io) = self.prev_region(bus, next_r)?;

                            next_r = r_minus;
                            next_io = r_minus_max_io.saturating_sub(1);
                        }

                        encoded = bus.encode_tumbler((next_r, next_io).into())?;
                        amount = new_count.saturating_sub(1);
                    }
                };

                if let Some(other_block) = db
                    .find_block_membership(&encoded)
                    .and_then(|bid| db.block(bid))
                {
                    encoded = other_block.align_to_instruction(&encoded)?;
                }

                let decoded = bus.decode_tumbler(encoded.clone())?;
                let new_parts = decoded.valid_region(bus)?;

                next_r = new_parts.0;
                next_io = new_parts.1;
            } else if next_io < amount {
                //TODO: This used to be able to skip multiple addresses when
                //scrolling, but now we can't because of the block thing. We
                //should at least be able to skip to the last block.
                let (r_minus, r_minus_max_io) = self.prev_region(bus, next_r)?;

                amount = amount.saturating_sub(next_io + 1);
                next_r = r_minus;
                next_io = r_minus_max_io.saturating_sub(1);
                encoded = bus.encode_tumbler((next_r, next_io).into())?;
            } else {
                next_io -= amount;
                encoded = bus.encode_tumbler((next_r, next_io).into())?;
                amount = 0;
            }
        }

        if let Some(block) = db
            .find_block_membership(&encoded)
            .and_then(|bid| db.block(bid))
        {
            encoded = block.align_to_instruction(&encoded)?;

            let new_parts = bus.decode_tumbler(encoded.clone())?.valid_region(bus)?;

            next_r = new_parts.0;
            next_io = new_parts.1;
        }

        Some((next_r, next_io).into())
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::Block;
    use crate::arch::tests::TestArchitecture;
    use crate::database::Database;
    use crate::memory::{Memory, Pointer};

    #[test]
    fn tumbler_decode() {
        let mut bus = Memory::<TestArchitecture>::new();

        bus.install_ram(0x0000, 0x4000);
        bus.install_ram(0x4000, 0x4000);
        bus.install_ram(0x8000, 0x1000);

        assert_eq!(
            Some((0, 0x400).into()),
            bus.decode_tumbler(Pointer::from(0x400))
        );
        assert_eq!(
            Some((1, 0x400).into()),
            bus.decode_tumbler(Pointer::from(0x4400))
        );
        assert_eq!(
            Some((1, 0x3FFF).into()),
            bus.decode_tumbler(Pointer::from(0x7FFF))
        );
        assert_eq!(
            Some((2, 0x400).into()),
            bus.decode_tumbler(Pointer::from(0x8400))
        );
        assert_eq!(None, bus.decode_tumbler(Pointer::from(0xF400)));
    }

    #[test]
    fn tumbler_encode() {
        let mut bus = Memory::<TestArchitecture>::new();

        bus.install_ram(0x0000, 0x4000);
        bus.install_ram(0x4000, 0x4000);
        bus.install_ram(0x8000, 0x1000);

        assert_eq!(
            bus.encode_tumbler((0, 0x400).into()),
            Some(Pointer::from(0x400))
        );
        assert_eq!(
            bus.encode_tumbler((1, 0x400).into()),
            Some(Pointer::from(0x4400))
        );
        assert_eq!(
            bus.encode_tumbler((1, 0x3FFF).into()),
            Some(Pointer::from(0x7FFF))
        );
        assert_eq!(
            bus.encode_tumbler((2, 0x400).into()),
            Some(Pointer::from(0x8400))
        );
        assert_eq!(bus.encode_tumbler((3, 0x123).into()), None);
    }

    #[test]
    fn tumbler_scroll_back() {
        let mut bus = Memory::<TestArchitecture>::new();

        bus.install_ram(0x0000, 0x2000);
        bus.install_ram(0x2000, 0x2000);
        bus.install_ram(0x4000, 0x4000);
        bus.install_ram(0x8000, 0x1000);

        let mut db = Database::<TestArchitecture>::new();

        let mut block = Block::from_parts(Pointer::from(0x100), 0x10);

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

        db.insert_block(block);

        let mut block = Block::from_parts(Pointer::from(0x110), 0x10);

        block.mark_instr_at(Pointer::from(0x110));
        block.mark_instr_at(Pointer::from(0x113));
        block.mark_instr_at(Pointer::from(0x116));
        block.mark_instr_at(Pointer::from(0x119));
        block.mark_instr_at(Pointer::from(0x11C));

        db.insert_block(block);

        let mut block = Block::from_parts(Pointer::from(0x3FF0), 0x10);

        block.mark_instr_at(Pointer::from(0x3FF0));
        block.mark_instr_at(Pointer::from(0x3FF3));
        block.mark_instr_at(Pointer::from(0x3FF6));
        block.mark_instr_at(Pointer::from(0x3FF9));
        block.mark_instr_at(Pointer::from(0x3FFC));
        block.mark_instr_at(Pointer::from(0x3FFF));

        db.insert_block(block);

        let mut block = Block::from_parts(Pointer::from(0x4000), 0x10);

        block.mark_instr_at(Pointer::from(0x4000));
        block.mark_instr_at(Pointer::from(0x4003));
        block.mark_instr_at(Pointer::from(0x4006));
        block.mark_instr_at(Pointer::from(0x4009));
        block.mark_instr_at(Pointer::from(0x400C));
        block.mark_instr_at(Pointer::from(0x400F));

        db.insert_block(block);

        let mut block = Block::from_parts(Pointer::from(0x7FF0), 0x10);

        block.mark_instr_at(Pointer::from(0x7FF0));
        block.mark_instr_at(Pointer::from(0x7FF3));
        block.mark_instr_at(Pointer::from(0x7FF6));
        block.mark_instr_at(Pointer::from(0x7FF9));
        block.mark_instr_at(Pointer::from(0x7FFC));
        block.mark_instr_at(Pointer::from(0x7FFE));

        db.insert_block(block);

        // scroll back within same block
        let tumbler = bus.decode_tumbler(Pointer::from(0x113)).unwrap();

        assert_eq!(
            tumbler.scroll_backward_by_lines(&bus, &db, 1),
            Some((0, 0x110).into())
        );

        // scroll back between blocks in same region
        assert_eq!(
            tumbler.scroll_backward_by_lines(&bus, &db, 2),
            Some((0, 0x10F).into())
        );
        assert_eq!(
            tumbler.scroll_backward_by_lines(&bus, &db, 4),
            Some((0, 0x10D).into())
        );

        // scroll back off edge of block within same region
        let tumbler2 = bus.decode_tumbler(Pointer::from(0x100)).unwrap();

        assert_eq!(
            tumbler2.scroll_backward_by_lines(&bus, &db, 3),
            Some((0, 0xFD).into())
        );

        // scroll back wraparound
        let tumbler3 = bus.decode_tumbler(Pointer::from(0x0)).unwrap();

        assert_eq!(
            tumbler3.scroll_backward_by_lines(&bus, &db, 3),
            Some((3, 0xFFD).into())
        );

        // scroll back from no block onto block in another region
        let tumbler4 = bus.decode_tumbler(Pointer::from(0x8002)).unwrap();

        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &db, 1),
            Some((3, 0x1).into())
        );
        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &db, 2),
            Some((3, 0x0).into())
        );
        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &db, 3),
            Some((2, 0x3FFE).into())
        );

        // scroll back from block in one region to block in another region
        let tumbler5 = bus.decode_tumbler(Pointer::from(0x4006)).unwrap();

        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &db, 1),
            Some((2, 3).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &db, 2),
            Some((2, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &db, 3),
            Some((1, 0x1FFF).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &db, 4),
            Some((1, 0x1FFC).into())
        );
    }

    #[test]
    fn tumbler_scroll_forward() {
        let mut bus = Memory::<TestArchitecture>::new();

        bus.install_ram(0x0000, 0x2000);
        bus.install_ram(0x2000, 0x2000);
        bus.install_ram(0x4000, 0x4000);
        bus.install_ram(0x8000, 0x1000);

        let mut db = Database::<TestArchitecture>::new();

        let mut block = Block::from_parts(Pointer::from(0x0), 0x10);

        block.mark_instr_at(Pointer::from(0x0));
        block.mark_instr_at(Pointer::from(0x3));
        block.mark_instr_at(Pointer::from(0x6));
        block.mark_instr_at(Pointer::from(0x9));
        block.mark_instr_at(Pointer::from(0xC));
        block.mark_instr_at(Pointer::from(0xE));

        db.insert_block(block);

        let mut block = Block::from_parts(Pointer::from(0x100), 0x10);

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

        db.insert_block(block);

        let mut block = Block::from_parts(Pointer::from(0x110), 0x10);

        block.mark_instr_at(Pointer::from(0x110));
        block.mark_instr_at(Pointer::from(0x113));
        block.mark_instr_at(Pointer::from(0x116));
        block.mark_instr_at(Pointer::from(0x119));
        block.mark_instr_at(Pointer::from(0x11C));

        db.insert_block(block);

        let mut block = Block::from_parts(Pointer::from(0x2000), 0x10);

        block.mark_instr_at(Pointer::from(0x2000));
        block.mark_instr_at(Pointer::from(0x2003));
        block.mark_instr_at(Pointer::from(0x2006));
        block.mark_instr_at(Pointer::from(0x2009));
        block.mark_instr_at(Pointer::from(0x200C));

        db.insert_block(block);

        let mut block = Block::from_parts(Pointer::from(0x3FF0), 0x10);

        block.mark_instr_at(Pointer::from(0x3FF0));
        block.mark_instr_at(Pointer::from(0x3FF3));
        block.mark_instr_at(Pointer::from(0x3FF6));
        block.mark_instr_at(Pointer::from(0x3FF9));
        block.mark_instr_at(Pointer::from(0x3FFC));
        block.mark_instr_at(Pointer::from(0x3FFF));

        db.insert_block(block);

        let mut block = Block::from_parts(Pointer::from(0x4000), 0x10);

        block.mark_instr_at(Pointer::from(0x4000));
        block.mark_instr_at(Pointer::from(0x4003));
        block.mark_instr_at(Pointer::from(0x4006));
        block.mark_instr_at(Pointer::from(0x4009));
        block.mark_instr_at(Pointer::from(0x400C));
        block.mark_instr_at(Pointer::from(0x400F));

        db.insert_block(block);

        let mut block = Block::from_parts(Pointer::from(0x7FF0), 0x10);

        block.mark_instr_at(Pointer::from(0x7FF0));
        block.mark_instr_at(Pointer::from(0x7FF3));
        block.mark_instr_at(Pointer::from(0x7FF6));
        block.mark_instr_at(Pointer::from(0x7FF9));
        block.mark_instr_at(Pointer::from(0x7FFC));
        block.mark_instr_at(Pointer::from(0x7FFE));

        db.insert_block(block);

        // scroll forward within block
        let tumbler = bus.decode_tumbler(Pointer::from(0x116)).unwrap();

        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &db, 1),
            Some((0, 0x119).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &db, 2),
            Some((0, 0x11C).into())
        );

        // scroll off of edge of block within region
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &db, 3),
            Some((0, 0x120).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &db, 4),
            Some((0, 0x121).into())
        );

        // scroll wraparound onto block
        let tumbler3 = bus.decode_tumbler(Pointer::from(0x8FFF)).unwrap();

        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &db, 1),
            Some((0, 0x0).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &db, 2),
            Some((0, 0x3).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &db, 3),
            Some((0, 0x6).into())
        );

        // scroll off of edge of block onto another block across regions
        let tumbler4 = bus.decode_tumbler(Pointer::from(0x3FF9)).unwrap();

        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &db, 1),
            Some((1, 0x1FFC).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &db, 2),
            Some((1, 0x1FFF).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &db, 3),
            Some((2, 0x0000).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &db, 4),
            Some((2, 0x0003).into())
        );

        // scroll off of edge of block onto no block across regions
        let tumbler5 = bus.decode_tumbler(Pointer::from(0x7FF9)).unwrap();

        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &db, 1),
            Some((2, 0x3FFC).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &db, 2),
            Some((2, 0x3FFE).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &db, 3),
            Some((3, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &db, 4),
            Some((3, 1).into())
        );

        // scroll off of no block onto region with block
        let tumbler6 = bus.decode_tumbler(Pointer::from(0x1FFE)).unwrap();

        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &db, 1),
            Some((0, 0x1FFF).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &db, 2),
            Some((1, 0).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &db, 3),
            Some((1, 3).into())
        );
    }
}
