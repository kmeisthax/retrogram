//! A set of types which allow analysis to model memory correctly.

use std::ops::{Add, Sub, Not};
use std::cmp::PartialOrd;
use std::slice::SliceIndex;
use num_traits::Bounded;
use crate::retrogram::reg;

/// Represents the various possible operating modes of a memory block.
/// 
/// The Behavior of a memory area bounds what analysises are considered
/// meaningful for a program.
pub enum Behavior {
    /// Storage behavior corresponds to memory which can be consistently read
    /// to and written from, such that reads always capture the last write,
    /// repeated reads always capture the same value, and there is no semantic
    /// value to writes aside from changing the value to be read back.
    /// Execution is allowed from storage only.
    Storage,
    
    /// MappedIO behavior corresponds to hardware devices on a memory bus.
    /// Reads are not consistent, writes have semantic value. The intent of
    /// such a memory block is communication with hardware. Execution is not
    /// analyzed within mapped I/O.
    MappedIO,
    
    /// Invalid behavior corresponds to memory not mapped to a hardware device
    /// at all. Platform makes no guarantees about behavior upon reading to or
    /// writing from an Invalid memory block. Execution, reads, and writes are
    /// not analyzed within invalid blocks.
    Invalid,
}

/// Represents the various possible actions that can be performed by a program
/// under analysis.
pub enum Action {
    DataRead,
    Write,
    ProgramExecute
}

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
pub struct Region<P, MV, S = P, IO = usize> {
    start: P,
    length: S,
    memtype: Behavior,
    image: Option<Box<dyn Image<Pointer = P, Offset = IO, Data = MV>>>
}

/// An Image is the contents of a given memory Region, if known.
/// 
/// An Image's contents may originate from an executable file, a ROM chip, or a
/// memory dump. The source is not important; but the data retrieved from an
/// image must match what the actual program under analysis would see if it had
/// read or executed from this memory.
pub trait Image {
    /// The architectural pointer type of the CPU architecture a given program
    /// is written or compiled for.
    type Pointer;

    /// A suitably wide type to represent an offset into any size Image we would
    /// like to model. Not required to match the architectural offset type.
    type Offset;

    /// The architectual addressing unit.
    type Data;

    /// Retrieve data from an image.
    fn retrieve(&self, offset: Self::Offset, count: Self::Offset) -> Option<Vec<Self::Data>>;

    /// Decode an architectural pointer to an image offset.
    /// 
    /// The given pointer must have a positive offset from the base pointer.
    /// If the offset is negative, this function yields None.
    /// 
    /// Images can determine the current banking in use by querying the context
    /// for the appropriately named banking value.
    fn decode_addr(&self, ptr: Self::Pointer, base: Self::Pointer, context: &reg::Context) -> Option<Self::Offset>;
}

impl<P, MV, S, IO> Region<P, MV, S, IO>
    where P: Copy + PartialOrd + Add<S> + Sub + From<<P as Add<S>>::Output>,
        S: Copy + From<<P as Sub>::Output> {
    
    pub fn is_ptr_within(&self, ptr: P) -> bool {
        self.start <= ptr && P::from(self.start + self.length) > ptr
    }
}

//TODO: something better than just a vec pls...
pub struct Memory<P, MV, S = P, IO = usize> {
    views: Vec<Region<P, MV, S, IO>>
}

impl<P, MV, S, IO> Memory<P, MV, S, IO>
    where P: Copy + PartialOrd + Add<S> + Sub + From<<P as Add<S>>::Output>,
        MV: Copy + Not + From<<MV as Not>::Output> + Bounded + From<u8> + From<<usize as SliceIndex<[u8]>>::Output>,
        <usize as SliceIndex<[u8]>>::Output : Sized,
        IO: From<u8>,
        usize: From<P> {
    
    pub fn read_unit(&self, ptr: P, ctx: &reg::Context) -> reg::Symbolic<MV> {
        for view in &self.views {
            if let Some(ref image) = view.image {
                if let Some(offset) = image.decode_addr(ptr, view.start, ctx) {
                    if let Some(imgdata) = image.retrieve(offset, IO::from(1)) {
                        if imgdata.len() > 0 {
                            return reg::Symbolic::from(imgdata[0]);
                        }
                    }
                }
            }
        }
        
        reg::Symbolic::default()
    }
}