//! Context injection for AArch32

use crate::{memory, reg};
use crate::arch::aarch32::THUMB_STATE;

fn recognize_context<P>(ctxt_str: &str, ptr: &mut memory::Pointer<P>) -> Option<()> {
    match ctxt_str {
        "T" => {
            ptr.set_arch_context(THUMB_STATE, reg::Symbolic::from(1));

            Some(())
        },
        _ => None
    }
}

/// Inject ARM architectural contexts into a user-specified pointer.
/// 
/// The following context specifiers will be recognized as architectural context
/// and injected into the given pointer:
/// 
///  * T: Indicates that the given pointer should be disassembled in THUMB mode.
/// 
/// This function recognizes architectural contexts that are specified at the
/// start or end of the context block of user input. In other words, AArch32
/// architectural contexts must be specified before or after all platform
/// contexts. Any contexts not recognized by this function will be indicated by
/// shortening the slice handed to it.
/// 
/// For example, the following are valid ways to specify that code at a
/// particular address is THUMB code:
/// 
///  * `T:80001A60` (Context slice is shortened to zero length)
///  * `160:T:A0C0A320` (Context slice is shortened to [..1])
///  * `T:2A:8000AAAA` (Context slice is shortened to [1..])
/// 
/// While the following will not be treated as an AArch32 platform context:
/// 
///  * `2:T:24:A0320002` (Context slice is returned as is)
pub fn architectural_ctxt_parse<P>(context_slice: &mut &[&str], ptr: &mut memory::Pointer<P>) -> Option<()> {
    while let Some(ctxt_str) = context_slice.get(0) {
        if recognize_context(ctxt_str, ptr) == None {
            break;
        }

        *context_slice = &context_slice[1..];
    }
    
    while context_slice.len() > 0 {
        let ctxt_str = context_slice.get(context_slice.len() - 1).expect("I already checked the length");
        
        if recognize_context(ctxt_str, ptr) == None {
            break;
        }
        
        *context_slice = &context_slice[..context_slice.len() - 1];
    }

    Some(())
}