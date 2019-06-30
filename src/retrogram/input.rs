//! Input utility functions

use std::str::FromStr;
use crate::retrogram::{memory, analysis, database, ast, cli};

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
pub fn parse_ptr<P, MV, S, IO>(text_str: &str, db: &database::Database<P, S>, bus: &memory::Memory<P, MV, S, IO>) -> Option<memory::Pointer<P>>
    where P: memory::PtrNum<S> + analysis::Mappable + cli::Nameable,
        S: memory::Offset<P> {
    if let Ok(text_lbl) = ast::Label::from_str(text_str) {
        if let Some(sym_id) = db.label_symbol(&text_lbl) {
            let sym = db.symbol(sym_id).expect("DB handed back invalid symbol ID");
            return Some(sym.as_pointer().clone());
        }
    }

    let mut v = Vec::new();

    for piece in text_str.split(":") {
        v.push(u64::from_str_radix(piece, 16).ok()?);
    }

    if let Some(pival) = v.get(v.len() - 1) {
        let pval = P::try_from(*pival).ok()?;
        let ptr = memory::Pointer::from(pval);

        Some(bus.insert_user_context(ptr, &v[..v.len() - 1]))
    } else {
        None
    }
}