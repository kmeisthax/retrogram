//! ROM binary dump image types

use crate::arch::Architecture;
use crate::maths::CheckedSub;
use crate::memory::{Image, Offset, Pointer};
use std::convert::{TryFrom, TryInto};
use std::io;

pub struct ROMBinaryImage<AR>
where
    AR: Architecture,
{
    data: Vec<AR::Byte>,
}

impl<AR> ROMBinaryImage<AR>
where
    AR: Architecture,
{
    /// Read a ROM image procured from a ROM whose data width is 8 bits or
    /// smaller.
    pub fn read_bytes<F>(file: &mut F) -> io::Result<Self>
    where
        F: io::Read,
    {
        let mut data = Vec::new();
        file.read_to_end(&mut data)?;

        let mut conv_data = Vec::with_capacity(data.len());
        for byte in data {
            conv_data.push(AR::Byte::try_from(byte).map_err(|_| {
                io::Error::new(
                    io::ErrorKind::Other,
                    "Could not convert value into byte".to_string(),
                )
            })?)
        }

        Ok(Self { data: conv_data })
    }
}

impl<AR> Image<AR> for ROMBinaryImage<AR>
where
    AR: Architecture,
    AR::Offset: Offset<AR::PtrVal> + TryInto<usize>,
{
    fn retrieve(&self, offset: usize, count: usize) -> Option<&[AR::Byte]> {
        self.data.get(offset..offset + count)
    }

    fn decode_addr(&self, ptr: &Pointer<AR::PtrVal>, base: AR::PtrVal) -> Option<usize> {
        match ptr.as_pointer().clone().checked_sub(base) {
            Some(p) => AR::Offset::try_from(p).ok()?.try_into().ok(),
            None => None,
        }
    }

    fn minimize_context(&self, ptr: Pointer<AR::PtrVal>) -> Pointer<AR::PtrVal> {
        Pointer::from(ptr.as_pointer().clone())
    }
}
