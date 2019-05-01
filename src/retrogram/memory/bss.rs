//! A generic Image type for modeling memory that is either uninitialized or
//! unknown.

use std::marker::PhantomData;
use std::convert::TryFrom;
use std::ops::Sub;
use crate::retrogram::mynums::CheckedSub;
use crate::retrogram::memory::{Image, Pointer, PtrNum, Offset};
use crate::retrogram::reg;

/// Models a range of memory whose contents are unknown.
/// 
/// "Unknown" means that the contents of memory cannot be statically analyzed.
pub struct UnknownImage<P, MV, IO = usize> {
    pdata: PhantomData<P>,
    mvdata: PhantomData<MV>,
    iodata: PhantomData<IO>,
}

impl<P, MV, IO> UnknownImage<P, MV, IO> {
    pub fn new() -> Self {
        UnknownImage {
            pdata: PhantomData,
            mvdata: PhantomData,
            iodata: PhantomData
        }
    }
}

impl<P, MV, IO> Image for UnknownImage<P, MV, IO> where P: Clone + CheckedSub, IO: Offset<P> {
    type Pointer = P;
    type Offset = IO;
    type Data = MV;

    fn retrieve(&self, _offset: Self::Offset, _count: Self::Offset) -> Option<&[Self::Data]> {
        None
    }

    fn decode_addr(&self, ptr: &Pointer<Self::Pointer>, base: Self::Pointer) -> Option<Self::Offset> {
        match base.checked_sub(ptr.as_pointer()) {
            Some(p) => IO::try_from(p).ok(),
            None => None
        }
    }

    fn minimize_context(&self, ptr: Pointer<Self::Pointer>) -> Pointer<Self::Pointer> {
        Pointer::from(ptr.as_pointer().clone())
    }
}

/// Models a range of memory whose contents are unknown but requires a context
/// in order to decode.
/// 
/// "Unknown" means that the contents of memory cannot be statically analyzed.
pub struct UnknownBankedImage<P, MV, IO = usize> {
    banking_ctxt: &'static str,
    pdata: PhantomData<*const P>,
    mvdata: PhantomData<*const MV>,
    iodata: PhantomData<*const IO>,
}

impl<P, MV, IO> UnknownBankedImage<P, MV, IO> {
    pub fn new(context: &'static str) -> Self {
        UnknownBankedImage {
            banking_ctxt: context,
            pdata: PhantomData,
            mvdata: PhantomData,
            iodata: PhantomData
        }
    }
}

impl<P, MV, IO> Image for UnknownBankedImage<P, MV, IO> where P: Clone + CheckedSub, IO: Offset<P> {
    type Pointer = P;
    type Offset = IO;
    type Data = MV;

    fn retrieve(&self, _offset: Self::Offset, _count: Self::Offset) -> Option<&[Self::Data]> {
        None
    }

    fn decode_addr(&self, ptr: &Pointer<Self::Pointer>, base: Self::Pointer) -> Option<Self::Offset> {
        match base.checked_sub(ptr.as_pointer()) {
            Some(p) => IO::try_from(p).ok(),
            None => None
        }
    }

    fn minimize_context(&self, ptr: Pointer<Self::Pointer>) -> Pointer<Self::Pointer> {
        let my_ctxt = ptr.get_platform_context(self.banking_ctxt);
        let mut stripped_ptr = Pointer::from(ptr.as_pointer().clone());
        
        stripped_ptr.set_platform_context(self.banking_ctxt, my_ctxt);
        stripped_ptr
    }

    fn insert_user_context(&self, mut ptr: Pointer<Self::Pointer>, ctxts: &[u64]) -> Pointer<Self::Pointer> {
        match ctxts.get(0) {
            Some(ctxt) => ptr.set_platform_context(self.banking_ctxt, reg::Symbolic::from(*ctxt)),
            _ => {}
        }

        ptr
    }
}