//! Memory space navigation

use crate::analysis::InstrLocation;
use crate::arch::Architecture;
use crate::database::Database;
use crate::maths::CheckedSub;
use crate::memory::{Memory, Pointer};
use num_traits::{One, Zero};
use std::cmp::min;
use std::fmt;

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

impl Into<(usize, usize, usize)> for Tumbler {
    fn into(self) -> (usize, usize, usize) {
        (self.region_index, self.image_index, self.line_index)
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
    fn next_region<AR>(
        &self,
        bus: &Memory<AR>,
        region: usize,
    ) -> Result<(usize, usize, usize), TumblerError>
    where
        AR: Architecture,
    {
        let mut next_r = region.checked_add(1).unwrap_or(0);
        let mut max_io = bus.region_image_size(next_r).ok_or(TumblerError::EmptyBus);
        if max_io.is_err() {
            next_r = 0;
            max_io = bus.region_image_size(next_r).ok_or(TumblerError::EmptyBus);
        }

        Ok((next_r, max_io?, 0))
    }

    /// Get the index and length of the current region, or if it's invalid,
    /// then the index of some other region.
    ///
    /// This returns a new region index, image offset, lines offset, and max
    /// offset.
    ///
    /// If `None`, then the bus is empty.
    fn valid_region<AR>(
        &self,
        bus: &Memory<AR>,
    ) -> Result<(usize, usize, usize, usize), TumblerError>
    where
        AR: Architecture,
    {
        let mut this_r = self.region_index;
        let mut this_i = self.image_index;
        let mut this_l = self.line_index;
        let mut max_io = bus.region_image_size(this_r).ok_or(TumblerError::EmptyBus);
        if max_io.is_err() {
            this_r = 0;
            this_i = 0;
            this_l = 0;
            max_io = bus.region_image_size(this_r).ok_or(TumblerError::EmptyBus);
        }

        Ok((this_r, this_i, this_l, max_io?))
    }

    /// Get the tumbler parameters of the region before a given one.
    pub fn prev_region<AR, LinesAtLocation>(
        &self,
        bus: &Memory<AR>,
        db: &mut Database<AR>,
        region: usize,
        lines: &mut LinesAtLocation,
    ) -> Result<(usize, usize, usize), TumblerError>
    where
        AR: Architecture,
        LinesAtLocation: FnMut(&Memory<AR>, &mut Database<AR>, &Pointer<AR::PtrVal>) -> usize,
    {
        let max_r = bus.region_count();
        let mut prev_r = region
            .checked_sub(1)
            .unwrap_or_else(|| max_r.saturating_sub(1));
        let mut max_io = bus.region_image_size(prev_r).ok_or(TumblerError::EmptyBus);
        if max_io.is_err() {
            prev_r = max_r.saturating_sub(1);
            max_io = bus.region_image_size(prev_r).ok_or(TumblerError::EmptyBus);
        }
        let prev_io = max_io?;
        let tumbl = (prev_r, prev_io, 0).into();
        let enc = bus
            .encode_tumbler(tumbl)
            .ok_or(TumblerError::CouldNotEncodeAdjAddress)?;
        let prev_l = lines(bus, db, &enc).saturating_sub(1);

        Ok((prev_r, prev_io, prev_l))
    }

    /// Produce a tumbler a given number of lines ahead.
    pub fn scroll_forward_by_lines<AR, LinesAtLocation>(
        self,
        bus: &Memory<AR>,
        db: &mut Database<AR>,
        lines: &mut LinesAtLocation,
        mut amount: usize,
    ) -> Result<Self, TumblerError>
    where
        AR: Architecture,
        LinesAtLocation: FnMut(&Memory<AR>, &mut Database<AR>, &Pointer<AR::PtrVal>) -> usize,
    {
        let (mut next_r, mut next_io, mut next_l, mut max_io) = self.valid_region(bus)?;
        let mut encoded = bus
            .encode_tumbler(self)
            .ok_or(TumblerError::CouldNotEncodeSelf)?;
        let mut lines_at_loc = lines(bus, db, &encoded);

        while amount > 0 {
            let remaining_l = lines_at_loc.saturating_sub(next_l);
            if remaining_l > 1 {
                let adjust_l = min(amount, remaining_l.saturating_sub(1));
                next_l += adjust_l;
                amount = amount.saturating_sub(adjust_l);
            } else if let Some(block) = db
                .find_block_membership(&encoded)
                .and_then(|bid| db.block(bid))
            {
                match block
                    .next_instruction(&encoded, 1)
                    .ok_or(TumblerError::TumblerOutsideBlockAlready)?
                {
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

                        let (new_r, new_io, _new_l, new_max_io) = bus
                            .decode_tumbler(encoded.clone())
                            .ok_or(TumblerError::CouldNotDecodeAdjAddress)?
                            .valid_region(bus)?;

                        if new_io < (new_max_io - 1) {
                            next_r = new_r;
                            next_io = new_io + 1;
                            next_l = 0;
                        } else {
                            let (r_plus, _r_plus_max_io, _r_plus_l) =
                                self.next_region(bus, next_r)?;

                            next_r = r_plus;
                            next_io = 0;
                            next_l = 0;
                        }

                        encoded = bus
                            .encode_tumbler((next_r, next_io, 0).into())
                            .ok_or(TumblerError::CouldNotEncodeAdjAddress)?;
                        lines_at_loc = lines(bus, db, &encoded);
                    }
                };

                if let Some(other_block) = db
                    .find_block_membership(&encoded)
                    .and_then(|bid| db.block(bid))
                {
                    encoded = other_block
                        .align_to_instruction(&encoded)
                        .ok_or(TumblerError::TumblerOutsideBlockAlready)?;
                    lines_at_loc = lines(bus, db, &encoded);
                }

                let decoded = bus
                    .decode_tumbler(encoded.clone())
                    .ok_or(TumblerError::CouldNotDecodeAdjAddress)?;
                let (new_r, new_io, new_l, _new_max_io) = decoded.valid_region(bus)?;

                if new_r != next_r || new_io != next_io {
                    next_r = new_r;
                    next_io = new_io;
                    next_l = new_l;
                }

                amount = amount.saturating_sub(1);
            } else {
                let remaining_io = max_io.saturating_sub(next_io + 1);
                if remaining_io == 0 {
                    let (r_plus, r_plus_max_io, r_plus_l) = self.next_region(bus, next_r)?;

                    max_io = r_plus_max_io;
                    next_r = r_plus;
                    next_io = 0;
                    next_l = r_plus_l;
                    encoded = bus
                        .encode_tumbler((next_r, next_io, next_l).into())
                        .ok_or(TumblerError::CouldNotEncodeAdjAddress)?;
                    lines_at_loc = lines(bus, db, &encoded);
                    amount = amount.saturating_sub(1);
                } else {
                    next_io += 1;
                    next_l = 0;
                    encoded = bus
                        .encode_tumbler((next_r, next_io, next_l).into())
                        .ok_or(TumblerError::CouldNotEncodeAdjAddress)?;
                    lines_at_loc = lines(bus, db, &encoded);
                    amount = amount.saturating_sub(1);
                }
            }
        }

        if let Some(block) = db
            .find_block_membership(&encoded)
            .and_then(|bid| db.block(bid))
        {
            encoded = block
                .align_to_instruction(&encoded)
                .ok_or(TumblerError::TumblerOutsideBlockAlready)?;

            let (new_r, new_io, new_l, _new_max_io) = bus
                .decode_tumbler(encoded.clone())
                .ok_or(TumblerError::CouldNotDecodeAdjAddress)?
                .valid_region(bus)?;

            if new_r != next_r || new_io != next_io {
                next_r = new_r;
                next_io = new_io;
                next_l = new_l;
            }
        }

        Ok((next_r, next_io, next_l).into())
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
    ) -> Result<Self, TumblerError>
    where
        AR: Architecture,
        LinesAtLocation: FnMut(&Memory<AR>, &mut Database<AR>, &Pointer<AR::PtrVal>) -> usize,
    {
        let (mut next_r, mut next_io, mut next_l, mut _max_io) = self.valid_region(bus)?;
        let mut encoded = bus
            .encode_tumbler(self)
            .ok_or(TumblerError::CouldNotEncodeSelf)?;

        while amount > 0 {
            if next_l > 0 {
                let subtractable_amount = min(amount, next_l);

                next_l = next_l.saturating_sub(subtractable_amount);
                amount = amount.saturating_sub(subtractable_amount);
            } else if let Some(block) = db
                .find_block_membership(&encoded)
                .and_then(|bid| db.block(bid))
            {
                match block
                    .last_instruction(&encoded, 1)
                    .ok_or(TumblerError::TumblerOutsideBlockAlready)?
                {
                    InstrLocation::InsideBlock(s) => {
                        encoded = encoded.contextualize(block.as_start().as_pointer().clone() + s);
                        amount = amount.saturating_sub(1);
                    }
                    InstrLocation::OutsideBlock(_new_count) => {
                        // Try to find the address of the block right before
                        // this one. This involves switching between tumbler
                        // and pointer form multiple times.
                        encoded = encoded.contextualize(block.as_start().as_pointer().clone());
                        let (new_r, new_io, _new_l, _new_max_io) = bus
                            .decode_tumbler(encoded.clone())
                            .ok_or(TumblerError::CouldNotDecodeAdjAddress)?
                            .valid_region(bus)?;

                        if new_io > 0 {
                            next_r = new_r;
                            next_io = new_io - 1;
                            encoded = bus
                                .encode_tumbler((next_r, next_io, next_l).into())
                                .ok_or(TumblerError::CouldNotEncodeAdjAddress)?;
                            next_l = lines(bus, db, &encoded).saturating_sub(1);
                        } else {
                            let (r_minus, r_minus_max_io, r_l) =
                                self.prev_region(bus, db, next_r, lines)?;

                            next_r = r_minus;
                            next_io = r_minus_max_io.saturating_sub(1);
                            next_l = r_l;
                        }

                        encoded = bus
                            .encode_tumbler((next_r, next_io, next_l).into())
                            .ok_or(TumblerError::CouldNotEncodeAdjAddress)?;
                        amount = amount.saturating_sub(1);
                    }
                };

                // This bit of code snaps the tumbler back to the nearest
                // instruction.
                if let Some(other_block) = db
                    .find_block_membership(&encoded)
                    .and_then(|bid| db.block(bid))
                {
                    encoded = other_block
                        .align_to_instruction(&encoded)
                        .ok_or(TumblerError::TumblerOutsideBlockAlready)?;
                }

                let decoded = bus
                    .decode_tumbler(encoded.clone())
                    .ok_or(TumblerError::CouldNotDecodeAdjAddress)?;
                let (new_r, new_io, _new_l, _new_max_io) = decoded.valid_region(bus)?;

                if new_r != next_r || new_io != next_io {
                    next_r = new_r;
                    next_io = new_io;
                    encoded = bus
                        .encode_tumbler((next_r, next_io, next_l).into())
                        .ok_or(TumblerError::CouldNotEncodeAdjAddress)?;
                    next_l = lines(bus, db, &encoded).saturating_sub(1);
                }
            } else if next_io > 0 {
                // This is the case where we're scrolling back from a non-block
                // area onto something that might potentially have a block.
                // This requires, again, multiple jumps between forms to both
                // properly snap the pointers and make sure we're still on the
                // last line.
                //
                // We don't have to worry about snapping tumblers scrolling
                // forward as it is expected that all blocks start with a valid
                // instruction.
                next_io -= 1;
                encoded = bus
                    .encode_tumbler((next_r, next_io, next_l).into())
                    .ok_or(TumblerError::CouldNotEncodeAdjAddress)?;

                if let Some(other_block) = db
                    .find_block_membership(&encoded)
                    .and_then(|bid| db.block(bid))
                {
                    encoded = other_block
                        .align_to_instruction(&encoded)
                        .ok_or(TumblerError::TumblerOutsideBlockAlready)?;
                }

                let decoded = bus
                    .decode_tumbler(encoded.clone())
                    .ok_or(TumblerError::CouldNotDecodeAdjAddress)?;
                let (new_r, new_io, _new_l, _new_max_io) = decoded.valid_region(bus)?;

                next_r = new_r;
                next_io = new_io;
                encoded = bus
                    .encode_tumbler((next_r, next_io, next_l).into())
                    .ok_or(TumblerError::CouldNotEncodeAdjAddress)?;
                next_l = lines(bus, db, &encoded).saturating_sub(1);
                amount = amount.saturating_sub(1);
            } else {
                let (r_minus, r_minus_max_io, r_l) = self.prev_region(bus, db, next_r, lines)?;

                next_r = r_minus;
                next_io = r_minus_max_io.saturating_sub(1);
                next_l = r_l;
                encoded = bus
                    .encode_tumbler((next_r, next_io, next_l).into())
                    .ok_or(TumblerError::CouldNotEncodeAdjAddress)?;
                amount = amount.saturating_sub(1);
            }
        }

        encoded = bus
            .encode_tumbler((next_r, next_io, next_l).into())
            .ok_or(TumblerError::CouldNotEncodeAdjAddress)?;

        if let Some(block) = db
            .find_block_membership(&encoded)
            .and_then(|bid| db.block(bid))
        {
            encoded = block
                .align_to_instruction(&encoded)
                .ok_or(TumblerError::TumblerOutsideBlockAlready)?;

            let (new_r, new_io, _new_l, _new_max_io) = bus
                .decode_tumbler(encoded.clone())
                .ok_or(TumblerError::CouldNotDecodeAdjAddress)?
                .valid_region(bus)?;

            if new_r != next_r || new_io != next_io {
                next_r = new_r;
                next_io = new_io;
                next_l = min(next_l, lines(bus, db, &encoded).saturating_sub(1));
            }
        }

        Ok((next_r, next_io, next_l).into())
    }
}

/// All the ways that a tumbler operation can fail.
#[derive(Debug)]
pub enum TumblerError {
    /// The bus given to the tumbler has no valid regions.
    EmptyBus,

    /// The tumbler could not be encoded into a valid address.
    CouldNotEncodeSelf,

    /// The adjacent tumbler could not be encoded into a valid address.
    CouldNotEncodeAdjAddress,

    /// The adjacent address could not be decoded into a valid tumbler.
    CouldNotDecodeAdjAddress,

    /// The tumbler points to a block that's somehow outside of itself.
    ///
    /// This is usually a programming error.
    TumblerOutsideBlockAlready,
}

impl fmt::Display for TumblerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TumblerError::*;

        match self {
            EmptyBus => write!(f, "The bus given to the tumbler has no valid regions"),
            CouldNotEncodeSelf => write!(
                f,
                "The tumbler could not encode itself into a valid address"
            ),
            CouldNotEncodeAdjAddress => write!(
                f,
                "The adjacent tumbler could not be encoded into a valid address."
            ),
            CouldNotDecodeAdjAddress => write!(
                f,
                "The adjacent address could not be decoded into a valid tumbler."
            ),
            TumblerOutsideBlockAlready => write!(
                f,
                "The tumbler points to a block that's somehow outside of itself"
            ),
        }
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
            Some((0, 0x400, 0).into()),
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
            tumbler.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1).ok(),
            Some((0, 0x110, 0).into())
        );

        // scroll back between blocks in same region
        assert_eq!(
            tumbler.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2).ok(),
            Some((0, 0x10F, 0).into())
        );
        assert_eq!(
            tumbler.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 4).ok(),
            Some((0, 0x10D, 0).into())
        );

        // scroll back off edge of block within same region
        let tumbler2 = bus.decode_tumbler(Pointer::from(0x100)).unwrap();

        assert_eq!(
            tumbler2.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3).ok(),
            Some((0, 0xFD, 0).into())
        );

        // scroll back wraparound
        let tumbler3 = bus.decode_tumbler(Pointer::from(0x0)).unwrap();

        assert_eq!(
            tumbler3.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3).ok(),
            Some((3, 0xFFD, 0).into())
        );

        // scroll back from no block onto block in another region
        let tumbler4 = bus.decode_tumbler(Pointer::from(0x8002)).unwrap();

        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1).ok(),
            Some((3, 0x1, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2).ok(),
            Some((3, 0x0, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3).ok(),
            Some((2, 0x3FFE, 0).into())
        );

        // scroll back from block in one region to block in another region
        let tumbler5 = bus.decode_tumbler(Pointer::from(0x4006)).unwrap();

        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1).ok(),
            Some((2, 3, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2).ok(),
            Some((2, 0, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3).ok(),
            Some((1, 0x1FFF, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 4).ok(),
            Some((1, 0x1FFC, 0).into())
        );
    }

    #[test]
    fn tumbler_scroll_back_multiline() {
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
            tumbler.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 1).ok(),
            Some((0, 0x110, 2).into())
        );
        assert_eq!(
            tumbler.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 2).ok(),
            Some((0, 0x110, 1).into())
        );
        assert_eq!(
            tumbler.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 3).ok(),
            Some((0, 0x110, 0).into())
        );

        // scroll back between blocks in same region
        assert_eq!(
            tumbler.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 4).ok(),
            Some((0, 0x10F, 2).into())
        );
        assert_eq!(
            tumbler.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 5).ok(),
            Some((0, 0x10F, 1).into())
        );
        assert_eq!(
            tumbler.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 6).ok(),
            Some((0, 0x10F, 0).into())
        );
        assert_eq!(
            tumbler.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 10).ok(),
            Some((0, 0x10D, 2).into())
        );
        assert_eq!(
            tumbler.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 11).ok(),
            Some((0, 0x10D, 1).into())
        );
        assert_eq!(
            tumbler.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 12).ok(),
            Some((0, 0x10D, 0).into())
        );

        // scroll back off edge of block within same region
        let tumbler2 = bus.decode_tumbler(Pointer::from(0x100)).unwrap();

        assert_eq!(
            tumbler2.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 1).ok(),
            Some((0, 0xFF, 2).into())
        );
        assert_eq!(
            tumbler2.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 2).ok(),
            Some((0, 0xFF, 1).into())
        );
        assert_eq!(
            tumbler2.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 3).ok(),
            Some((0, 0xFF, 0).into())
        );
        assert_eq!(
            tumbler2.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 4).ok(),
            Some((0, 0xFE, 2).into())
        );
        assert_eq!(
            tumbler2.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 5).ok(),
            Some((0, 0xFE, 1).into())
        );
        assert_eq!(
            tumbler2.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 6).ok(),
            Some((0, 0xFE, 0).into())
        );
        assert_eq!(
            tumbler2.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 7).ok(),
            Some((0, 0xFD, 2).into())
        );
        assert_eq!(
            tumbler2.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 8).ok(),
            Some((0, 0xFD, 1).into())
        );
        assert_eq!(
            tumbler2.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 9).ok(),
            Some((0, 0xFD, 0).into())
        );

        // scroll back wraparound
        let tumbler3 = bus.decode_tumbler(Pointer::from(0x0)).unwrap();

        assert_eq!(
            tumbler3.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 7).ok(),
            Some((3, 0xFFD, 2).into())
        );
        assert_eq!(
            tumbler3.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 8).ok(),
            Some((3, 0xFFD, 1).into())
        );
        assert_eq!(
            tumbler3.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 9).ok(),
            Some((3, 0xFFD, 0).into())
        );

        // scroll back from no block onto block in another region
        let tumbler4 = bus.decode_tumbler(Pointer::from(0x8002)).unwrap();

        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 1).ok(),
            Some((3, 0x1, 2).into())
        );
        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 2).ok(),
            Some((3, 0x1, 1).into())
        );
        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 3).ok(),
            Some((3, 0x1, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 4).ok(),
            Some((3, 0x0, 2).into())
        );
        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 5).ok(),
            Some((3, 0x0, 1).into())
        );
        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 6).ok(),
            Some((3, 0x0, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 7).ok(),
            Some((2, 0x3FFE, 2).into())
        );
        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 8).ok(),
            Some((2, 0x3FFE, 1).into())
        );
        assert_eq!(
            tumbler4.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 9).ok(),
            Some((2, 0x3FFE, 0).into())
        );

        // scroll back from block in one region to block in another region
        let tumbler5 = bus.decode_tumbler(Pointer::from(0x4006)).unwrap();

        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 1).ok(),
            Some((2, 3, 2).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 2).ok(),
            Some((2, 3, 1).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 3).ok(),
            Some((2, 3, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 4).ok(),
            Some((2, 0, 2).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 5).ok(),
            Some((2, 0, 1).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 6).ok(),
            Some((2, 0, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 7).ok(),
            Some((1, 0x1FFF, 2).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 8).ok(),
            Some((1, 0x1FFF, 1).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 9).ok(),
            Some((1, 0x1FFF, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 10).ok(),
            Some((1, 0x1FFC, 2).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 11).ok(),
            Some((1, 0x1FFC, 1).into())
        );
        assert_eq!(
            tumbler5.scroll_backward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 12).ok(),
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
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1).ok(),
            Some((0, 0x119, 0).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2).ok(),
            Some((0, 0x11C, 0).into())
        );

        // scroll off of edge of block within region
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3).ok(),
            Some((0, 0x120, 0).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 4).ok(),
            Some((0, 0x121, 0).into())
        );

        // scroll wraparound onto block
        let tumbler3 = bus.decode_tumbler(Pointer::from(0x8FFF)).unwrap();

        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1).ok(),
            Some((0, 0x0, 0).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2).ok(),
            Some((0, 0x3, 0).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3).ok(),
            Some((0, 0x6, 0).into())
        );

        // scroll off of edge of block onto another block across regions
        let tumbler4 = bus.decode_tumbler(Pointer::from(0x3FF9)).unwrap();

        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1).ok(),
            Some((1, 0x1FFC, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2).ok(),
            Some((1, 0x1FFF, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3).ok(),
            Some((2, 0x0000, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 4).ok(),
            Some((2, 0x0003, 0).into())
        );

        // scroll off of edge of block onto no block across regions
        let tumbler5 = bus.decode_tumbler(Pointer::from(0x7FF9)).unwrap();

        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1).ok(),
            Some((2, 0x3FFC, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2).ok(),
            Some((2, 0x3FFE, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3).ok(),
            Some((3, 0, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 4).ok(),
            Some((3, 1, 0).into())
        );

        // scroll off of no block onto region with block
        let tumbler6 = bus.decode_tumbler(Pointer::from(0x1FFE)).unwrap();

        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 1).ok(),
            Some((0, 0x1FFF, 0).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 2).ok(),
            Some((1, 0, 0).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 1, 3).ok(),
            Some((1, 3, 0).into())
        );
    }

    #[test]
    fn tumbler_scroll_forward_multiline() {
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
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 1).ok(),
            Some((0, 0x116, 1).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 2).ok(),
            Some((0, 0x116, 2).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 3).ok(),
            Some((0, 0x119, 0).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 4).ok(),
            Some((0, 0x119, 1).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 5).ok(),
            Some((0, 0x119, 2).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 6).ok(),
            Some((0, 0x11C, 0).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 7).ok(),
            Some((0, 0x11C, 1).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 8).ok(),
            Some((0, 0x11C, 2).into())
        );

        // scroll off of edge of block within region
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 9).ok(),
            Some((0, 0x120, 0).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 10).ok(),
            Some((0, 0x120, 1).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 11).ok(),
            Some((0, 0x120, 2).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 12).ok(),
            Some((0, 0x121, 0).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 13).ok(),
            Some((0, 0x121, 1).into())
        );
        assert_eq!(
            tumbler.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 14).ok(),
            Some((0, 0x121, 2).into())
        );

        // scroll wraparound onto block
        let tumbler3 = bus.decode_tumbler(Pointer::from(0x8FFF)).unwrap();

        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 1).ok(),
            Some((3, 0xFFF, 1).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 2).ok(),
            Some((3, 0xFFF, 2).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 3).ok(),
            Some((0, 0x0, 0).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 4).ok(),
            Some((0, 0x0, 1).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 5).ok(),
            Some((0, 0x0, 2).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 6).ok(),
            Some((0, 0x3, 0).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 7).ok(),
            Some((0, 0x3, 1).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 8).ok(),
            Some((0, 0x3, 2).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 9).ok(),
            Some((0, 0x6, 0).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 10).ok(),
            Some((0, 0x6, 1).into())
        );
        assert_eq!(
            tumbler3.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 11).ok(),
            Some((0, 0x6, 2).into())
        );

        // scroll off of edge of block onto another block across regions
        let tumbler4 = bus.decode_tumbler(Pointer::from(0x3FF9)).unwrap();

        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 1).ok(),
            Some((1, 0x1FF9, 1).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 2).ok(),
            Some((1, 0x1FF9, 2).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 3).ok(),
            Some((1, 0x1FFC, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 4).ok(),
            Some((1, 0x1FFC, 1).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 5).ok(),
            Some((1, 0x1FFC, 2).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 6).ok(),
            Some((1, 0x1FFF, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 7).ok(),
            Some((1, 0x1FFF, 1).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 8).ok(),
            Some((1, 0x1FFF, 2).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 9).ok(),
            Some((2, 0x0000, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 10).ok(),
            Some((2, 0x0000, 1).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 11).ok(),
            Some((2, 0x0000, 2).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 12).ok(),
            Some((2, 0x0003, 0).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 13).ok(),
            Some((2, 0x0003, 1).into())
        );
        assert_eq!(
            tumbler4.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 14).ok(),
            Some((2, 0x0003, 2).into())
        );

        // scroll off of edge of block onto no block across regions
        let tumbler5 = bus.decode_tumbler(Pointer::from(0x7FF9)).unwrap();

        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 1).ok(),
            Some((2, 0x3FF9, 1).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 2).ok(),
            Some((2, 0x3FF9, 2).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 3).ok(),
            Some((2, 0x3FFC, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 4).ok(),
            Some((2, 0x3FFC, 1).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 5).ok(),
            Some((2, 0x3FFC, 2).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 6).ok(),
            Some((2, 0x3FFE, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 7).ok(),
            Some((2, 0x3FFE, 1).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 8).ok(),
            Some((2, 0x3FFE, 2).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 9).ok(),
            Some((3, 0, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 10).ok(),
            Some((3, 0, 1).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 11).ok(),
            Some((3, 0, 2).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 12).ok(),
            Some((3, 1, 0).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 13).ok(),
            Some((3, 1, 1).into())
        );
        assert_eq!(
            tumbler5.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 14).ok(),
            Some((3, 1, 2).into())
        );

        // scroll off of no block onto region with block
        let tumbler6 = bus.decode_tumbler(Pointer::from(0x1FFE)).unwrap();

        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 1).ok(),
            Some((0, 0x1FFE, 1).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 2).ok(),
            Some((0, 0x1FFE, 2).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 3).ok(),
            Some((0, 0x1FFF, 0).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 4).ok(),
            Some((0, 0x1FFF, 1).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 5).ok(),
            Some((0, 0x1FFF, 2).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 6).ok(),
            Some((1, 0, 0).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 7).ok(),
            Some((1, 0, 1).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 8).ok(),
            Some((1, 0, 2).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 9).ok(),
            Some((1, 3, 0).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 10).ok(),
            Some((1, 3, 1).into())
        );
        assert_eq!(
            tumbler6.scroll_forward_by_lines(&bus, &mut db, &mut |_, _, _| 3, 11).ok(),
            Some((1, 3, 2).into())
        );
    }
}
