//! Architectural context parsing stub

use crate::memory::Pointer;

/// Inject SM83 architectural contexts into a user-specified pointer.
///
/// SM83 defines no architectural contexts, so this function does nothing.
pub fn architectural_ctxt_parse<P>(
    context_slice: &mut &[&str],
    ptr: &mut Pointer<P>,
) -> Option<()> {
    Some(())
}
