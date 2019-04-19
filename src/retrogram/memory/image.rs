//! Types that feed data from program images, such as executable files or ROM dumps, into the memory model.

use crate::retrogram::memory::Pointer;

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
    fn retrieve(&self, offset: Self::Offset, count: Self::Offset) -> Option<&[Self::Data]>;

    /// Decode an architectural pointer to an image offset.
    /// 
    /// The given pointer must have a positive offset from the base pointer.
    /// If the offset is negative, or the pointer does not resolve to this
    /// particular image, then this function yields None.
    /// 
    /// Images can determine the current banking in use by querying the context
    /// for the appropriately named banking value.
    fn decode_addr(&self, ptr: &Pointer<Self::Pointer>, base: Self::Pointer) -> Option<Self::Offset>;

    /// Given a pointer, remove all contexts from the pointer that are not
    /// necessary to decode it to an image offset.
    fn minimize_context(&self, ptr: &Pointer<Self::Pointer>) -> Pointer<Self::Pointer>;
}