//! Memory space navigation

use crate::analysis::InstrLocation;
use crate::arch::Architecture;
use crate::database::Database;
use crate::maths::CheckedSub;
use crate::memory::{Memory, Pointer};
use num_traits::{One, Zero};
use std::cmp::min;

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

    /// Retrieve the line-index component of this tumbler.
    pub fn line_index(&self) -> usize {
        self.line_index
    }

    /// Get the tumbler parameters of the region after a given one.
    ///
    /// The image offset of the returned parameter is the *end* of the region,
    /// not the start - if constructing a new tumbler, use 0 instead of the
    /// middle parameter.
    ///
    /// If `None`, then the bus is empty.
    fn next_region<AR>(&self, bus: &Memory<AR>, region: usize) -> Option<(usize, usize, usize)>
    where
        AR: Architecture,
    {
        let mut next_r = region.checked_add(1).unwrap_or(0);
        let mut max_io = bus.region_image_size(next_r);
        if max_io.is_none() {
            next_r = 0;
            max_io = bus.region_image_size(next_r);
        }

        Some((next_r, max_io?, 0))
    }

    /// Get the index and length of the current region, or if it's invalid,
    /// then the index of some other region.
    ///
    /// This returns a new region index, image offset, lines offset, and max
    /// offset.
    ///
    /// If `None`, then the bus is empty.
    fn valid_region<AR>(&self, bus: &Memory<AR>) -> Option<(usize, usize, usize, usize)>
    where
        AR: Architecture,
    {
        let mut this_r = self.region_index;
        let mut this_i = self.image_index;
        let mut this_l = self.line_index;
        let mut max_io = bus.region_image_size(this_r);
        if max_io.is_none() {
            this_r = 0;
            this_i = 0;
            this_l = 0;
            max_io = bus.region_image_size(this_r);
        }

        Some((this_r, this_i, this_l, max_io?))
    }

    /// Get the tumbler parameters of the region before a given one.
    ///
    /// If `None`, then the bus is empty.
    pub fn prev_region<AR, LinesAtLocation>(
        &self,
        bus: &Memory<AR>,
        db: &mut Database<AR>,
        region: usize,
        lines: &mut LinesAtLocation,
    ) -> Option<(usize, usize, usize)>
    where
        AR: Architecture,
        LinesAtLocation: FnMut(&Memory<AR>, &mut Database<AR>, &Pointer<AR::PtrVal>) -> usize,
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
        let prev_io = max_io?;
        let tumbl = (prev_r, prev_io, 0).into();
        let enc = bus.encode_tumbler(tumbl)?;
        let prev_l = lines(bus, db, &enc).saturating_sub(1);

        Some((prev_r, prev_io, prev_l))
    }

    /// Produce a tumbler a given number of lines ahead.
    ///
    /// If `None`, then the bus is empty.
    pub fn scroll_forward_by_lines<AR, LinesAtLocation>(
        self,
        bus: &Memory<AR>,
        db: &mut Database<AR>,
        lines: &mut LinesAtLocation,
        mut amount: usize,
    ) -> Option<Self>
    where
        AR: Architecture,
        LinesAtLocation: FnMut(&Memory<AR>, &mut Database<AR>, &Pointer<AR::PtrVal>) -> usize,
    {
        let (mut next_r, mut next_io, mut next_l, mut max_io) = self.valid_region(bus)?;
        let mut encoded = bus.encode_tumbler(self)?;
        let mut lines_at_loc = lines(bus, db, &encoded);

        while amount > 0 {
            let remaining_l = lines_at_loc.saturating_sub(next_l);
            if remaining_l > 1 {
                next_l += min(amount, remaining_l.saturating_sub(1));
                amount = amount.saturating_sub(min(amount, remaining_l.saturating_sub(1)));
            } else if let Some(block) = db
                .find_block_membership(&encoded)
                .and_then(|bid| db.block(bid))
            {
                match block.next_instruction(&encoded, 1)? {
                    InstrLocation::InsideBlock(s) => {
                        encoded = encoded.contextualize(block.as_start().as_pointer().clone() + s);
                        lines_at_loc = lines(bus, db, &encoded);
                    }
                    InstrLocation::OutsideBlock(_new_count) => {
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

                        let (new_r, new_io, _new_l, new_max_io) =
                            bus.decode_tumbler(encoded.clone())?.valid_region(bus)?;

                        if new_io < (new_max_io - 1) {
                            next_r = new_r;
                            next_io = new_io + 1;
                            next_l = 0;
                        } else {
                            let (r_plus, _r_plus_max_io, _r_plus_l) =
                                self.next_region(bus, next_r).unwrap();

                            next_r = r_plus;
                            next_io = 0;
                            next_l = 0;
                        }

                        encoded = bus.encode_tumbler((next_r, next_io, 0).into())?;
                        lines_at_loc = lines(bus, db, &encoded);
                    }
                };

                if let Some(other_block) = db
                    .find_block_membership(&encoded)
                    .and_then(|bid| db.block(bid))
                {
                    encoded = other_block.align_to_instruction(&encoded)?;
                    lines_at_loc = lines(bus, db, &encoded);
                }

                let decoded = bus.decode_tumbler(encoded.clone())?;
                let (new_r, new_io, new_l, _new_max_io) = decoded.valid_region(bus)?;

                if new_r != next_r || new_io != next_io {
                    next_r = new_r;
                    next_io = new_io;
                    next_l = new_l;
                }

                amount = amount.saturating_sub(1);
            } else {
                //TODO: This used to be able to skip multiple addresses when
                //scrolling, but now we can't because of the block thing. We
                //should at least be able to skip to the next block.
                let remaining_io = max_io.saturating_sub(next_io + 1);
                if remaining_io == 0 {
                    let (r_plus, r_plus_max_io, r_plus_l) = self.next_region(bus, next_r).unwrap();

                    max_io = r_plus_max_io;
                    next_r = r_plus;
                    next_io = 0;
                    next_l = r_plus_l;
                    encoded = bus.encode_tumbler((next_r, next_io, next_l).into())?;
                    lines_at_loc = lines(bus, db, &encoded);
                    amount = amount.saturating_sub(1);
                } else {
                    next_io += 1;
                    next_l = 0;
                    encoded = bus.encode_tumbler((next_r, next_io, next_l).into())?;
                    lines_at_loc = lines(bus, db, &encoded);
                    amount = amount.saturating_sub(1);
                }
            }
        }

        if let Some(block) = db
            .find_block_membership(&encoded)
            .and_then(|bid| db.block(bid))
        {
            encoded = block.align_to_instruction(&encoded)?;

            let (new_r, new_io, new_l, _new_max_io) =
                bus.decode_tumbler(encoded.clone())?.valid_region(bus)?;

            if new_r != next_r || new_io != next_io {
                next_r = new_r;
                next_io = new_io;
                next_l = new_l;
            }
        }

        Some((next_r, next_io, next_l).into())
    }

    /// Produce a tumbler a given number of lines behind.
    ///
    /// If `None`, then the bus is empty.
    pub fn scroll_backward_by_lines<AR, LinesAtLocation>(
        self,
        bus: &Memory<AR>,
        db: &mut Database<AR>,
        lines: &mut LinesAtLocation,
        mut amount: usize,
    ) -> Option<Self>
    where
        AR: Architecture,
        LinesAtLocation: FnMut(&Memory<AR>, &mut Database<AR>, &Pointer<AR::PtrVal>) -> usize,
    {
        let (mut next_r, mut next_io, mut next_l, mut _max_io) = self.valid_region(bus)?;
        let mut encoded = bus.encode_tumbler(self)?;

        while amount > 0 {
            if next_l > 0 {
                next_l = next_l.saturating_sub(min(amount, next_l));
                amount = amount.saturating_sub(min(amount, next_l));
            } else if let Some(block) = db
                .find_block_membership(&encoded)
                .and_then(|bid| db.block(bid))
            {
                match block.last_instruction(&encoded, 1)? {
                    InstrLocation::InsideBlock(s) => {
                        encoded = encoded.contextualize(block.as_start().as_pointer().clone() + s);
                        amount = amount.saturating_sub(1);
                    }
                    InstrLocation::OutsideBlock(_new_count) => {
                        // Try to find the address of the block right before
                        // this one. This involves switching between tumbler
                        // and pointer form multiple times.
                        encoded = encoded.contextualize(block.as_start().as_pointer().clone());
                        let (new_r, new_io, _new_l, _new_max_io) =
                            bus.decode_tumbler(encoded.clone())?.valid_region(bus)?;

                        if new_io > 0 {
                            next_r = new_r;
                            next_io = new_io - 1;
                            next_l = lines(bus, db, &encoded).saturating_sub(1);
                        } else {
                            let (r_minus, r_minus_max_io, r_l) =
                                self.prev_region(bus, db, next_r, lines)?;

                            next_r = r_minus;
                            next_io = r_minus_max_io.saturating_sub(1);
                            next_l = r_l;
                        }

                        encoded = bus.encode_tumbler((next_r, next_io, next_l).into())?;
                        amount = amount.saturating_sub(1);
                    }
                };

                if let Some(other_block) = db
                    .find_block_membership(&encoded)
                    .and_then(|bid| db.block(bid))
                {
                    encoded = other_block.align_to_instruction(&encoded)?;
                }

                let decoded = bus.decode_tumbler(encoded.clone())?;
                let (new_r, new_io, _new_l, _new_max_io) = decoded.valid_region(bus)?;

                if new_r != next_r || new_io != next_io {
                    next_r = new_r;
                    next_io = new_io;
                    next_l = lines(bus, db, &encoded).saturating_sub(1);
                }
            } else if next_io < amount {
                //TODO: This used to be able to skip multiple addresses when
                //scrolling, but now we can't because of the block thing. We
                //should at least be able to skip to the last block.
                let (r_minus, r_minus_max_io, r_l) = self.prev_region(bus, db, next_r, lines)?;

                next_r = r_minus;
                next_io = r_minus_max_io.saturating_sub(1);
                next_l = r_l;
                encoded = bus.encode_tumbler((next_r, next_io, next_l).into())?;
                amount = amount.saturating_sub(1);
            } else {
                next_io -= 1;
                encoded = bus.encode_tumbler((next_r, next_io, next_l).into())?;
                amount = amount.saturating_sub(1);
            }
        }

        if let Some(block) = db
            .find_block_membership(&encoded)
            .and_then(|bid| db.block(bid))
        {
            encoded = block.align_to_instruction(&encoded)?;

            let (new_r, new_io, _new_l, _new_max_io) =
                bus.decode_tumbler(encoded.clone())?.valid_region(bus)?;

            if new_r != next_r || new_io != next_io {
                next_r = new_r;
                next_io = new_io;
                next_l = lines(bus, db, &encoded).saturating_sub(1);
            }
        }

        Some((next_r, next_io, next_l).into())
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
            Some((0, 0x4000, 0).into()),
            bus.decode_tumbler(Pointer::from(0x400))
        );
        assert_eq!(
            Some((1, 0x400, 0).into()),
            bus.decode_tumbler(Pointer::from(0x4400))
        );
        assert_eq!(
            Some((1, 0x3FFF, 0).into()),
            bus.decode_tumbler(Pointer::from(0x7FFF))
        );
        assert_eq!(
            Some((2, 0x400, 0).into()),
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
            bus.encode_tumbler((0, 0x400, 0).into()),
            Some(Pointer::from(0x400))
        );
        assert_eq!(
            bus.encode_tumbler((1, 0x400, 0).into()),
            Some(Pointer::from(0x4400))
        );
        assert_eq!(
            bus.encode_tumbler((1, 0x3FFF, 0).into()),
            Some(Pointer::from(0x7FFF))
        );
        assert_eq!(
            bus.encode_tumbler((2, 0x400, 0).into()),
            Some(Pointer::from(0x8400))
        );
        assert_eq!(bus.encode_tumbler((3, 0x123, 0).into()), None);
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
            tumbler.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1),
            Some((0, 0x110, 0).into())
        );

        // scroll back between blocks in same region
        assert_eq!(
            tumbler.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2),
            Some((0, 0x10F, 0).into())
        );
        assert_eq!(
            tumbler.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 4),
            Some((0, 0x10D, 0).into())
        );

        // scroll back off edge of block within same region
        let tumbler2 = bus.decode_tumbler(Pointer::from(0x100)).unwrap();

        assert_eq!(
            tumbler2.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3),
            Some((0, 0xFD, 0).into())
        );

        // scroll back wraparound
        let tumbler3 = bus.decode_tumbler(Pointer::from(0x0)).unwrap();

        assert_eq!(
            tumbler3.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3),
            Some((3, 0xFFD, 0).into())
        );

        // scroll back from no block onto block in another region
        let tumbler4 = bus.decode_tumbler(Pointer::from(0x8002)).unwrap();

        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1),
            Some((3, 0x1, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2),
            Some((3, 0x0, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3),
            Some((2, 0x3FFE, 0).into())
        );

        // scroll back from block in one region to block in another region
        let tumbler5 = bus.decode_tumbler(Pointer::from(0x4006)).unwrap();

        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1),
            Some((2, 3, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2),
            Some((2, 0, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3),
            Some((1, 0x1FFF, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 4),
            Some((1, 0x1FFC, 0).into())
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
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1),
            Some((0, 0x119, 0).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2),
            Some((0, 0x11C, 0).into())
        );

        // scroll off of edge of block within region
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3),
            Some((0, 0x120, 0).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 4),
            Some((0, 0x121, 0).into())
        );

        // scroll wraparound onto block
        let tumbler3 = bus.decode_tumbler(Pointer::from(0x8FFF)).unwrap();

        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1),
            Some((0, 0x0, 0).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2),
            Some((0, 0x3, 0).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3),
            Some((0, 0x6, 0).into())
        );

        // scroll off of edge of block onto another block across regions
        let tumbler4 = bus.decode_tumbler(Pointer::from(0x3FF9)).unwrap();

        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1),
            Some((1, 0x1FFC, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2),
            Some((1, 0x1FFF, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3),
            Some((2, 0x0000, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 4),
            Some((2, 0x0003, 0).into())
        );

        // scroll off of edge of block onto no block across regions
        let tumbler5 = bus.decode_tumbler(Pointer::from(0x7FF9)).unwrap();

        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1),
            Some((2, 0x3FFC, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2),
            Some((2, 0x3FFE, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3),
            Some((3, 0, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 4),
            Some((3, 1, 0).into())
        );

        // scroll off of no block onto region with block
        let tumbler6 = bus.decode_tumbler(Pointer::from(0x1FFE)).unwrap();

        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1),
            Some((0, 0x1FFF, 0).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2),
            Some((1, 0, 0).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3),
            Some((1, 3, 0).into())
        );
    }
}
