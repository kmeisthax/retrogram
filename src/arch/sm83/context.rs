//! Architectural context parsing stub

use crate::arch::sm83::types::Pointer as PValue;
use crate::memory::Pointer;

/// Inject SM83 architectural contexts into a user-specified pointer.
///
/// SM83 defines no architectural contexts, so this function does nothing.
pub fn architectural_ctxt_parse(
    _context_slice: &mut &[&str],
    _ptr: &mut Pointer<PValue>,
) -> Option<()> {
    Some(())
}
