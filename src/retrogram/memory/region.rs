//! A special-purpose type for modeling the address decoding of a particular
//! platform.

use std::ops::{Add, Sub, Not, BitOr, BitAnd, Shl};
use std::cmp::PartialOrd;
use std::slice::SliceIndex;
use num::traits::{Zero, One, Bounded};
use num_traits::ops::checked::CheckedSub;
use crate::retrogram::{reg, mynums};
use crate::retrogram::reg::Convertable;
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

trait Desegmentable<AU> : Clone + Bounded + From<AU> + mynums::BoundWidth<usize> + Shl<usize> + BitOr + BitAnd + Zero + One + Not + From<<Self as Not>::Output> + From<<Self as Shl<usize>>::Output> + From<<Self as BitOr>::Output> + From<<Self as BitAnd>::Output> {

}

//Bounded + BitAnd + BitOr + From<<T as BitAnd>::Output> + From<<T as BitOr>::Output>
impl<T, AU> Desegmentable<AU> for T
    where T: Clone + Bounded + From<AU> + mynums::BoundWidth<usize> + Shl<usize> + BitOr + BitAnd + Zero + One + Not + From<<T as Not>::Output> + From<<T as Shl<usize>>::Output> + From<<T as BitOr>::Output> + From<<T as BitAnd>::Output> {

}

impl<P, MV, S, IO> Memory<P, MV, S, IO>
    where P: Clone + PartialOrd + Add<S> + Sub + From<<P as Add<S>>::Output>,
        MV: Clone + Not + From<<MV as Not>::Output> + Bounded + From<u8> + From<<usize as SliceIndex<[u8]>>::Output>,
        <usize as SliceIndex<[u8]>>::Output : Sized,
        IO: From<u8>,
        usize: From<P> {
    
    /// Read an arbitary little-endian integer type from memory.
    /// 
    /// All integer types involved must provide a way to reason about their
    /// widths. This is provided with the `BoundWidth` trait, which indicates how
    /// many left shifts of a given integer type are required in order to
    /// overflow it. The `BoundWidth` of the memory word type (MV) thus
    /// determines how many atomic memory units are required to be read in order
    /// to populate the expected value type.
    pub fn read_manywords_le<EV>(&self, ptr: &Pointer<P>) -> reg::Symbolic<EV>
        where EV: Desegmentable<MV>,
            P: Add<S> + From<<P as Add<S>>::Output>,
            S: From<usize>,
            MV: mynums::BoundWidth<usize>,
            <EV as Shl<usize>>::Output: BitOr + Sub<EV> + From<<<EV as Shl<usize>>::Output as BitOr>::Output> + From<<<EV as Shl<usize>>::Output as Sub<EV>>::Output>,
            reg::Symbolic<EV>: Shl<usize> + Convertable<<EV as Not>::Output> + Convertable<<EV as Shl<usize>>::Output>,
            reg::Symbolic<<EV as Shl<usize>>::Output> : From<<reg::Symbolic<EV> as Shl<usize>>::Output> {
        let units_reqd = (EV::bound_width() as f32 / MV::bound_width() as f32).round() as usize;
        let mut sum : reg::Symbolic<EV> = reg::Symbolic::<EV>::from(EV::zero());
        for i in 0..units_reqd {
            let ptr = ptr.contextualize(P::from(ptr.as_pointer().clone() + S::from(i)));
            let shifted_unit = reg::Symbolic::<EV>::convert_from(self.read_unit(&ptr)) << i * MV::bound_width();
            sum = sum | reg::Symbolic::<EV>::convert_from(reg::Symbolic::from(shifted_unit));
        }

        sum
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
    
    pub fn insert_user_context(&self, ptr: Pointer<P>, ctxts: &[u64]) -> Pointer<P> {
        for view in &self.views {
            if view.is_ptr_within(ptr.clone().into_pointer()) {
                return view.image.insert_user_context(ptr, ctxts);
            }
        }

        ptr
    }
}