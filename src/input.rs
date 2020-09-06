//! Input utility functions

use crate::arch::Architecture;
use crate::maths::FromStrRadix;
use crate::{ast, database, maths, memory};
use std::str::FromStr;

/// Parse any pointer specification entered in by a user.
///
/// This function returns a contextual pointer, derived from user input in two
/// ways:
///
///  1. The user specifies the contextual pointer directly, with contexts
///     specified as hexadecimal integers separated by colons and prepended to
///     the hexadecimal architectural pointer; e.g. `4:4000`. The order and
///     meaning of the prepended contexts is platform dependent; you must
///     provide a `Fn` which will assign contexts from parsed integers.
///  2. The user specifies a label, which will be looked up in the analysis
///     database.
///
/// To facilitate both functions, you must provide an analysis database and a
/// pointer contextualizer. The contextualizer must accept a `Vec` of an integer
/// type convertable to the architectural pointer type. Usually, this will be
/// the pointer type itself; though some architectures with exotic pointer types
/// will instead need to accept the offset type.
///
/// This function assumes the default context type of `u64`. No known platform
/// requires a wider context type and adding more type parameters to every user
/// of `memory::Pointer` is inadvisable.
///
/// Architectures are allowed to provide their own context parse function which
/// can consume contexts from the list and assign contexts to the pointer.
pub fn parse_ptr<AR>(
    text_str: &str,
    db: &database::Database<AR>,
    bus: &memory::Memory<AR>,
    _arch: AR,
) -> Option<memory::Pointer<AR::PtrVal>>
where
    AR: Architecture,
    AR::PtrVal: maths::FromStrRadix,
{
    if let Ok(text_lbl) = ast::Label::from_str(text_str) {
        if let Some(sym_id) = db.label_symbol(&text_lbl) {
            let sym = db.symbol(sym_id).expect("DB handed back invalid symbol ID");
            return Some(sym.as_pointer().clone());
        } else {
            //Stupid workaround for the fact that the database treats the
            //autogen bit as part of the symbol's identity

            //TODO: When the database stops treating the autogen bit as part of
            //the label's "primary key" remove this code

            let autogen_ver = ast::Label::new_placeholder(text_lbl.name(), text_lbl.parent_name());

            if let Some(sym_id) = db.label_symbol(&autogen_ver) {
                let sym = db.symbol(sym_id).expect("DB handed back invalid symbol ID");
                return Some(sym.as_pointer().clone());
            }
        }
    }

    let mut v = Vec::new();

    for piece in text_str.split(':') {
        v.push(piece);
    }

    if let Some(pival) = v.last() {
        let pval = AR::PtrVal::from_str_radix(pival, 16).ok()?;
        let mut ptr = memory::Pointer::from(pval);
        let mut context_slice = &v[..v.len() - 1];

        AR::parse_architectural_contexts(&mut context_slice, &mut ptr)?;

        Some(bus.insert_user_context(ptr, context_slice))
    } else {
        None
    }
}
