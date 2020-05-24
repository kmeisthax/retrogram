//! A generic Image type for modeling memory that is either uninitialized or
//! unknown.

use crate::maths::CheckedSub;
use crate::memory::{Image, Pointer};
use crate::reg;
use std::convert::TryInto;
use std::marker::PhantomData;

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
            iodata: PhantomData,
        }
    }
}

impl<P, MV, IO> Image for UnknownImage<P, MV, IO>
where
    P: Clone + CheckedSub,
    <P as std::ops::Sub>::Output: TryInto<IO>,
{
    type Pointer = P;
    type Offset = IO;
    type Data = MV;

    fn retrieve(&self, _offset: Self::Offset, _count: Self::Offset) -> Option<&[Self::Data]> {
        None
    }

    fn decode_addr(
        &self,
        ptr: &Pointer<Self::Pointer>,
        base: Self::Pointer,
    ) -> Option<Self::Offset> {
        match ptr.as_pointer().clone().checked_sub(base) {
            Some(p) => p.try_into().ok(),
            None => None,
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
            iodata: PhantomData,
        }
    }
}

impl<P, MV, IO> Image for UnknownBankedImage<P, MV, IO>
where
    P: Clone + CheckedSub,
    <P as std::ops::Sub>::Output: TryInto<IO>,
{
    type Pointer = P;
    type Offset = IO;
    type Data = MV;

    fn retrieve(&self, _offset: Self::Offset, _count: Self::Offset) -> Option<&[Self::Data]> {
        None
    }

    fn decode_addr(
        &self,
        ptr: &Pointer<Self::Pointer>,
        base: Self::Pointer,
    ) -> Option<Self::Offset> {
        match ptr.as_pointer().clone().checked_sub(base) {
            Some(p) => p.try_into().ok(),
            None => None,
        }
    }

    fn minimize_context(&self, ptr: Pointer<Self::Pointer>) -> Pointer<Self::Pointer> {
        let my_ctxt = ptr.get_platform_context(self.banking_ctxt);
        let mut stripped_ptr = Pointer::from(ptr.as_pointer().clone());

        stripped_ptr.set_platform_context(self.banking_ctxt, my_ctxt);
        stripped_ptr
    }

    fn insert_user_context(
        &self,
        mut ptr: Pointer<Self::Pointer>,
        ctxts: &[&str],
    ) -> Pointer<Self::Pointer> {
        if let Some(ctxt) = ctxts.get(0) {
            if let Ok(cval) = u64::from_str_radix(ctxt, 16) {
                ptr.set_platform_context(self.banking_ctxt, reg::Symbolic::from(cval));
            }
        }

        ptr
    }
}
