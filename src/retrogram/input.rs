use std::io;
use std::str::FromStr;
use std::hash::Hash;
use std::ops::{Add, Sub};
use std::convert::TryFrom;
use num_traits::Num;
use crate::retrogram::{memory, analysis, ast};

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
pub fn parse_ptr<P, MV, S, IO>(text_str: &str, db: &analysis::Database<P>, bus: &memory::Memory<P, MV, S, IO>) -> Option<memory::Pointer<P>>
    where P: Clone + Eq + Hash + FromStr + PartialOrd + Add<S> + Sub + From<<P as Add<S>>::Output> + TryFrom<u64>,
        S: Clone + PartialOrd + From<<P as Sub>::Output> {
    if let Ok(text_lbl) = ast::Label::from_str(text_str) {
        if let Some(ptr) = db.label_pointer(&text_lbl) {
            return Some(ptr.clone());
        }
    }

    let mut v = Vec::new();

    for piece in text_str.split(":") {
        v.push(u64::from_str_radix(piece, 16).ok()?);
    }

    if let Some(pival) = v.get(v.len() - 1) {
        let pval = P::try_from(*pival).ok()?;
        let ptr = memory::Pointer::from(pval);

        Some(bus.insert_user_context(ptr, &v[1..]))
    } else {
        None
    }
}