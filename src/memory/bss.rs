//! A generic Image type for modeling memory that is either uninitialized or
//! unknown.

use crate::analysis::Prerequisite;
use crate::arch::Architecture;
use crate::maths::CheckedSub;
use crate::memory::{Image, Offset, Pointer};
use crate::reg;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::marker::PhantomData;

/// Models a range of memory whose contents are unknown.
///
/// "Unknown" means that the contents of memory cannot be statically analyzed.
pub struct UnknownImage<AR>
where
    AR: Architecture,
{
    phantom_arch: PhantomData<AR>,
    size: usize,
}

impl<AR> UnknownImage<AR>
where
    AR: Architecture,
{
    pub fn new(size: usize) -> Self {
        Self {
            phantom_arch: PhantomData,
            size,
        }
    }
}

impl<AR> Image<AR> for UnknownImage<AR>
where
    AR: Architecture,
    AR::Offset: Offset<AR::PtrVal>,
{
    fn retrieve(&self, _offset: usize, _count: usize) -> Option<&[AR::Byte]> {
        None
    }

    fn decode_addr(&self, ptr: &Pointer<AR::PtrVal>, base: AR::PtrVal) -> Option<usize> {
        match ptr.as_pointer().clone().checked_sub(base) {
            Some(p) => AR::Offset::try_from(p).ok()?.try_into().ok(),
            None => None,
        }
    }

    fn encode_addr(&self, ioffset: usize, base: AR::PtrVal) -> Option<Pointer<AR::PtrVal>> {
        let pval = base + AR::Offset::try_from(ioffset).ok()?;
        Some(Pointer::from_ptrval_and_contexts(pval, HashMap::new()))
    }

    fn image_size(&self) -> usize {
        self.size
    }

    fn decode_prerequisites(&self, _ptr: AR::PtrVal, _base: AR::PtrVal) -> Vec<Prerequisite<AR>> {
        Vec::new()
    }

    fn minimize_context(&self, ptr: Pointer<AR::PtrVal>) -> Pointer<AR::PtrVal> {
        Pointer::from(ptr.as_pointer().clone())
    }
}

/// Models a range of memory whose contents are unknown but requires a context
/// in order to decode.
///
/// "Unknown" means that the contents of memory cannot be statically analyzed.
pub struct UnknownBankedImage {
    banking_ctxt: &'static str,
    mask: u64,
    size: usize,
}

impl UnknownBankedImage {
    pub fn new(banking_ctxt: &'static str, mask: u64, size: usize) -> Self {
        Self {
            banking_ctxt,
            mask,
            size,
        }
    }
}

impl<AR> Image<AR> for UnknownBankedImage
where
    AR: Architecture,
    AR::Offset: Offset<AR::PtrVal>,
{
    fn retrieve(&self, _offset: usize, _count: usize) -> Option<&[AR::Byte]> {
        None
    }

    fn decode_addr(&self, ptr: &Pointer<AR::PtrVal>, base: AR::PtrVal) -> Option<usize> {
        match ptr.as_pointer().clone().checked_sub(base) {
            Some(p) => AR::Offset::try_from(p).ok()?.try_into().ok(),
            None => None,
        }
    }

    fn encode_addr(&self, ioffset: usize, base: AR::PtrVal) -> Option<Pointer<AR::PtrVal>> {
        let pval = base + AR::Offset::try_from(ioffset).ok()?;
        Some(Pointer::from_ptrval_and_contexts(pval, HashMap::new()))
    }

    fn image_size(&self) -> usize {
        self.size
    }

    fn minimize_context(&self, ptr: Pointer<AR::PtrVal>) -> Pointer<AR::PtrVal> {
        let my_ctxt = ptr.get_platform_context(self.banking_ctxt);
        let mut stripped_ptr = Pointer::from(ptr.as_pointer().clone());

        stripped_ptr.set_platform_context(self.banking_ctxt, my_ctxt);
        stripped_ptr
    }

    fn decode_prerequisites(&self, _ptr: AR::PtrVal, _base: AR::PtrVal) -> Vec<Prerequisite<AR>> {
        vec![Prerequisite::platform_context(
            self.banking_ctxt.to_string(),
            self.mask,
        )]
    }

    fn insert_user_context(
        &self,
        mut ptr: Pointer<AR::PtrVal>,
        ctxts: &[&str],
    ) -> Pointer<AR::PtrVal> {
        if let Some(ctxt) = ctxts.get(0) {
            if let Ok(cval) = u64::from_str_radix(ctxt, 16) {
                ptr.set_platform_context(self.banking_ctxt, reg::Symbolic::from(cval));
            }
        }

        ptr
    }
}
