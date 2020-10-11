//! Platform constructor trait

use crate::arch::Architecture;
use crate::memory::Memory;
use std::io;

/// Trait to represent a platform.
///
/// A platform is a thing that turns a ROM file into a memory bus that analysis
/// can actually happen to.
pub trait Platform<AR>
where
    AR: Architecture,
{
    /// Construct a platform's memory bus from the given file.
    fn construct_platform<F>(&self, file: &mut F) -> io::Result<Memory<AR>>
    where
        F: io::Read + io::Seek;
}
