//! Types that feed data from program images, such as executable files or ROM dumps, into the memory model.

use crate::analysis::Prerequisite;
use crate::arch::Architecture;
use crate::memory::Pointer;
use crate::reg::{State, Symbolic};

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
    ///
    /// The data retrieved from this image will be the original value of this
    /// image, not counting any state which may have altered the image.
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

    /// Encode an image offset to an architectural pointer.
    ///
    /// If the offset is invalid, then this function should yield `None`. All
    /// offsets beyond `image_size` must be `None`.
    fn encode_addr(&self, ioffset: usize, base: AR::PtrVal) -> Option<Pointer<AR::PtrVal>>;

    /// Get the size of the image.
    fn image_size(&self) -> usize;

    /// Produce a list of prerequisites necessary to fully decode a particular
    /// address.
    fn decode_prerequisites(&self, ptr: AR::PtrVal, base: AR::PtrVal) -> Vec<Prerequisite<AR>>;

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

    /// Given a pointer, retrieve the value of a particular memory location.
    ///
    /// The default implementation of this method contextualizes the pointer
    /// with the given state, and retrieves the result from the state if it has
    /// already been written. Otherwise, it falls back to the image itself.
    ///
    /// Returns `None` if this view does not include the given pointer value.
    fn read_memory(
        &self,
        ptr: AR::PtrVal,
        base: AR::PtrVal,
        state: &State<AR>,
    ) -> Option<Symbolic<AR::Byte>> {
        let cptr = self.minimize_context(state.contextualize_pointer(ptr));

        if let Some(daddr) = self.decode_addr(&cptr, base) {
            if state.memory_was_written(&cptr) {
                Some(state.get_memory(&cptr))
            } else {
                self.retrieve(daddr, 1)
                    .and_then(|b| b.get(0))
                    .map(|b| Symbolic::from(b.clone()))
            }
        } else {
            None
        }
    }

    /// Given a pointer and data, alter the given state to contain the effects
    /// of a given memory write.
    ///
    /// The default implementation of this method treats the image as
    /// writable, and uses the other methods above to copy the write into state
    /// memory. Overrides of this method may instead perform other mutations
    /// upon the state, such as changing contexts on the state or doing nothing.
    ///
    /// Callers of this function must therefore make sure to recontextualize
    /// pointers with the new state after every memory write. For this same
    /// reason, this function takes a pointer value, and we contextualize it
    /// with the state ourselves.
    ///
    /// Returns `false` if the given image does not handle the given write.
    fn write_memory(
        &self,
        ptr: AR::PtrVal,
        base: AR::PtrVal,
        data: Symbolic<AR::Byte>,
        state: &mut State<AR>,
    ) -> bool {
        let cptr = self.minimize_context(state.contextualize_pointer(ptr));

        if self.decode_addr(&cptr, base).is_some() {
            state.set_memory(cptr, data);
            true
        } else {
            false
        }
    }
}
