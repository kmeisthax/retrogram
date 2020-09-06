//! Memory space navigation

use crate::arch::Architecture;
use crate::memory::Memory;

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
}

impl Default for Tumbler {
    fn default() -> Tumbler {
        Tumbler {
            region_index: 0,
            image_index: 0,
        }
    }
}

impl From<(usize, usize)> for Tumbler {
    fn from(pair: (usize, usize)) -> Tumbler {
        Tumbler {
            region_index: pair.0,
            image_index: pair.1,
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

    /// Produce a tumbler a given number of lines ahead.
    ///
    /// If `None`, then the bus is empty.
    pub fn scroll_forward_by_offset<AR>(self, bus: &Memory<AR>, mut amount: usize) -> Option<Self>
    where
        AR: Architecture,
    {
        let (mut next_r, mut next_io, mut max_io) = self.valid_region(bus)?;

        while amount > 0 {
            let remaining_io = max_io.saturating_sub(next_io);
            if remaining_io < amount {
                let (r_plus, r_plus_max_io) = self.next_region(bus, next_r).unwrap();

                max_io = r_plus_max_io;
                next_r = r_plus;
                next_io = 0;
                amount = amount.saturating_sub(remaining_io);
            } else {
                next_io += amount;
                amount = 0;
            }
        }

        Some((next_r, next_io).into())
    }

    /// Produce a tumbler a given number of lines behind.
    ///
    /// If `None`, then the bus is empty.
    pub fn scroll_backward_by_offset<AR>(self, bus: &Memory<AR>, mut amount: usize) -> Option<Self>
    where
        AR: Architecture,
    {
        let (mut next_r, mut next_io, mut _max_io) = self.valid_region(bus)?;

        while amount > 0 {
            if next_io < amount {
                let (r_minus, r_minus_max_io) = self.prev_region(bus, next_r).unwrap();

                next_r = r_minus;
                next_io = r_minus_max_io.saturating_sub(1);
                amount = amount.saturating_sub(next_io);
            } else {
                next_io -= amount;
                amount = 0;
            }
        }

        Some((next_r, next_io).into())
    }
}
