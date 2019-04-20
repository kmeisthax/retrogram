//! A special-purpose type for modeling the address decoding of a particular
//! platform.

use std::ops::{Add, Sub, Not};
use std::cmp::PartialOrd;
use std::slice::SliceIndex;
use std::num::Wrapping;
use num_traits::Bounded;
use num_traits::ops::checked::CheckedSub;
use crate::retrogram::reg;
use crate::retrogram::memory::{Image, Behavior, Pointer, UnknownImage};

/// Models a region of memory visible to the program under analysis.
/// 
/// `P` is the architectural representation of a pointer, and `S` is any
/// compatible type which represents a size or offset of an architectural
/// pointer. Compatibility is defined as the ability to add an offset `s` to a
/// pointer `p` and obtain some output value convertable to `P` which represents
/// a pointer that many bytes forwards in the image.
/// 
/// `T` is the image type in use for the given region. The offset type of an
/// image is `IO`, which is allowed to differ from architectural pointer and
/// offset types. (For example, if your platform supports bank switching, `IO`
/// must be wide enough to represent a fully decoded offset into a ROM image.)
/// 
/// #Typical type parameter values
/// 
/// For a 32-bit CPU such as AArch32, `P` and `S` would be `u32` and `MV` would
/// be `u8`. `IO` would likely be `u32`, unless you specifically wished to model
/// platforms with more complicated addressing systems. This seems so
/// straightforward as to reveal our type system as overcomplicated, so let's
/// get weirder.
/// 
/// Certain computer architectures use uncommon architectural register types of
/// odd sizes. For example, the WDC 65816 is a CPU with a 24-bit addressing
/// space, and would need a custom `u24` type as `P` and `S` in order to be
/// properly analyzed. If we go back further in computing history, we'd see
/// systems such as the PDP-10, which uses 18-bit *word* addressing into
/// 36-bit memory. This would demand not only a `u18` `P` and `S`, but also a
/// `u36` `MV` type.
/// 
/// The most complicated situation would probably be real-mode x86, where
/// addresses are always aliased in a bizarre segment/offset scheme. If we
/// ignored that, and modeled memory as a `u20` pointer (known contemporarily as
/// "huge memory"); we still wouldn't be able to model the machine's ported I/O
/// space. Pointers on x86 would need to be an enum of port and memory space
/// addresses, with offsets being straight integers.
struct Region<P, MV, S = P, IO = usize> {
    start: P,
    length: S,
    memtype: Behavior,
    image: Box<dyn Image<Pointer = P, Offset = IO, Data = MV>>
}

impl<P, MV, S, IO> Region<P, MV, S, IO>
    where P: Clone + PartialOrd + Add<S> + Sub + From<<P as Add<S>>::Output>,
        S: Clone + PartialOrd + From<<P as Sub>::Output> {
    
    pub fn is_ptr_within(&self, ptr: P) -> bool {
        self.start <= ptr && S::from(ptr - self.start.clone()) < self.length
    }
}

//TODO: something better than just a vec pls...
pub struct Memory<P, MV, S = P, IO = usize> {
    views: Vec<Region<P, MV, S, IO>>
}

impl<P, MV, S, IO> Memory<P, MV, S, IO> {
    pub fn new() -> Self {
        Memory {
            views: Vec::new()
        }
    }

    pub fn install_mem_image(&mut self, start: P, length: S, image: Box<dyn Image<Pointer = P, Offset = IO, Data = MV>>) {
        self.views.push(Region {
            start: start,
            length: length,
            memtype: Behavior::Storage,
            image: image
        });
    }
}

impl<P, MV, S, IO> Memory<P, MV, S, IO> where P: CheckedSub + Clone + 'static, IO: From<P> + 'static, MV: 'static {
    pub fn install_mem(&mut self, start: P, length: S) {
        self.views.push(Region {
            start: start,
            length: length,
            memtype: Behavior::Storage,
            image: Box::new(UnknownImage::new())
        });
    }

    pub fn install_io(&mut self, start: P, length: S) {
        self.views.push(Region {
            start: start,
            length: length,
            memtype: Behavior::MappedIO,
            image: Box::new(UnknownImage::new())
        });
    }

    pub fn openbus_mem(&mut self, start: P, length: S) {
        self.views.push(Region {
            start: start,
            length: length,
            memtype: Behavior::Invalid,
            image: Box::new(UnknownImage::new())
        });
    }
}

impl<P, MV, S, IO> Memory<P, MV, S, IO>
    where P: Clone + PartialOrd + Add<S> + Sub + From<<P as Add<S>>::Output>,
        MV: Clone + Not + From<<MV as Not>::Output> + Bounded + From<u8> + From<<usize as SliceIndex<[u8]>>::Output>,
        <usize as SliceIndex<[u8]>>::Output : Sized,
        IO: From<u8>,
        usize: From<P> {
    
    pub fn read_unit(&self, ptr: &Pointer<P>) -> reg::Symbolic<MV> {
        for view in &self.views {
            if let Some(offset) = view.image.decode_addr(ptr, view.start.clone()) {
                if let Some(imgdata) = view.image.retrieve(offset, IO::from(1)) {
                    if imgdata.len() > 0 {
                        return reg::Symbolic::from(imgdata[0].clone());
                    }
                }
            }
        }
        
        reg::Symbolic::default()
    }
}

impl<P, MV, S, IO> Memory<P, MV, S, IO>
    where P: Clone + PartialOrd + Add<S> + Sub + From<<P as Add<S>>::Output>,
        S: Clone + PartialOrd + From<<P as Sub>::Output> {
    pub fn minimize_context(&self, ptr: Pointer<P>) -> Pointer<P> {
        for view in &self.views {
            if view.is_ptr_within(ptr.clone().into_pointer()) {
                return view.image.minimize_context(ptr);
            }
        }

        ptr
    }
}