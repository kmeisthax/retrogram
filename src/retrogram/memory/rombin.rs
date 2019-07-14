//! ROM binary dump image types

use std::marker::PhantomData;
use std::convert::TryFrom;
use std::io;
use crate::retrogram::maths::CheckedSub;
use crate::retrogram::memory::{Pointer, Image, PtrNum, Offset};

pub struct ROMBinaryImage<P, MV> {
    data: Vec<MV>,
    pdata: PhantomData<P>
}

impl<P, MV> ROMBinaryImage<P, MV> where MV: From<u8> {
    /// Read a ROM image procured from a ROM whose data width is 8 bits or
    /// smaller.
    pub fn read_bytes<F>(file: &mut F) -> io::Result<ROMBinaryImage<P, MV>> where F: io::Read {
        let mut data = Vec::new();
        file.read_to_end(&mut data)?;

        let mut conv_data = Vec::with_capacity(data.len());
        for byte in data {
            conv_data.push(MV::from(byte))
        }

        Ok(ROMBinaryImage {
            data: conv_data,
            pdata: PhantomData
        })
    }
}

impl<P, MV> Image for ROMBinaryImage<P, MV> where P: Clone + CheckedSub, usize: Offset<P> {
    type Pointer = P;
    type Offset = usize;
    type Data = MV;

    fn retrieve(&self, offset: Self::Offset, count: Self::Offset) -> Option<&[Self::Data]> {
        self.data.get(offset..offset + count)
    }

    fn decode_addr(&self, ptr: &Pointer<Self::Pointer>, base: Self::Pointer) -> Option<Self::Offset> {
        match base.checked_sub(ptr.as_pointer().clone()) {
            Some(p) => usize::try_from(p).ok(),
            None => None
        }
    }

    fn minimize_context(&self, ptr: Pointer<Self::Pointer>) -> Pointer<Self::Pointer> {
        Pointer::from(ptr.as_pointer().clone())
    }
}