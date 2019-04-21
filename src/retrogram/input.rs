use std::io;
use std::str::FromStr;
use std::hash::Hash;
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
pub fn parse_ptr<PI, P, CX>(text_str: &str, db: &analysis::Database<P>, create_context: CX) -> io::Result<memory::Pointer<P>>
    where PI: Num,
        P: Clone + Eq + Hash + FromStr,
        CX: Fn(&Vec<PI>) -> Option<memory::Pointer<P>> {
    if let Ok(text_lbl) = ast::Label::from_str(text_str) {
        if let Some(ptr) = db.label_pointer(&text_lbl) {
            return Ok(ptr.clone());
        }
    }

    let mut v = Vec::new();

    for piece in text_str.split(":") {
        v.push(PI::from_str_radix(piece, 16).or(Err(io::Error::new(io::ErrorKind::InvalidInput, "Given analysis address is not a valid integer")))?);
    }

    create_context(&v).ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Could not create context for input pointer"))
}