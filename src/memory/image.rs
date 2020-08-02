//! Types that feed data from program images, such as executable files or ROM dumps, into the memory model.

use crate::arch::Architecture;
use crate::memory::Pointer;
use crate::reg::State;

/// An Image is the contents of a given memory Region, if known.
///
/// An Image's contents may originate from an executable file, a ROM chip, or a
/// memory dump. The source is not important; but the data retrieved from an
/// image must match what the actual program under analysis would see if it had
/// read or executed from this memory.
pub trait Image<AR>
where
    AR: Architecture,
{
    /// Retrieve data from an image.
    fn retrieve(&self, offset: usize, count: usize) -> Option<&[AR::Byte]>;

    /// Decode an architectural pointer to an image offset.
    ///
    /// The given pointer must have a positive offset from the base pointer.
    /// If the offset is negative, or the pointer does not resolve to this
    /// particular image, then this function yields None.
    ///
    /// Images can determine the current banking in use by querying the context
    /// for the appropriately named banking value.
    fn decode_addr(&self, ptr: &Pointer<AR::PtrVal>, base: AR::PtrVal) -> Option<usize>;

    /// Given a pointer, remove all contexts from the pointer that are not
    /// necessary to decode it to an image offset.
    fn minimize_context(&self, ptr: Pointer<AR::PtrVal>) -> Pointer<AR::PtrVal> {
        ptr
    }

    /// Given a pointer, insert any additional user-specified contexts into the
    /// pointer.
    ///
    /// This function is used to parse user input. How many contexts are used,
    /// if any, and in what order, is wholly dependent on the Image type in
    /// question. Images are not required to implement this method, the default
    /// merely does not alter the pointer. Platforms should document their use
    /// of contexts as appropriate.
    fn insert_user_context(
        &self,
        ptr: Pointer<AR::PtrVal>,
        _ctxts: &[&str],
    ) -> Pointer<AR::PtrVal> {
        ptr
    }

    /// Given a pointer and data, alter the given state to contain the effects
    /// of a given memory write.
    ///
    /// The default implementation of this method treats the image as
    /// non-writable and does nothing. Overrides of this method may instead
    /// copy the data into the state as memory, or change pointer contexts in
    /// the state.
    ///
    /// Callers of this function must therefore make sure to recontextualize
    /// pointers with the new state after every memory write.
    fn write_memory(&self, _ptr: Pointer<AR::PtrVal>, _data: &[AR::Byte], _state: &mut State<AR>) {}
}
