//! Types needed to analyze references

use crate::cli::Nameable;
use crate::memory;
use serde::{Deserialize, Serialize};
use std::cmp::Ord;
use std::fmt::{Display, Formatter, Result};

/// Represents the type of a reference.
///
/// Reference kinds are strictly ordered: for any given symbol, you can state
/// it's strongest reference kind by taking the maximum of all incoming
/// reference kinds.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum ReferenceKind {
    Unknown,
    Data,
    Code,
    Subroutine,
    Entrypoint,
}

impl Display for ReferenceKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        //write!(f, "({}, {})", self.x, self.y)
        match self {
            ReferenceKind::Unknown => write!(f, "UNK"),
            ReferenceKind::Data => write!(f, "DAT"),
            ReferenceKind::Code => write!(f, "LOC"),
            ReferenceKind::Subroutine => write!(f, "FUN"),
            ReferenceKind::Entrypoint => write!(f, "ENTER"),
        }
    }
}

impl ReferenceKind {
    pub fn friendly_name(self) -> &'static str {
        match self {
            ReferenceKind::Unknown => "Unknown",
            ReferenceKind::Data => "Data",
            ReferenceKind::Code => "Code, branch",
            ReferenceKind::Subroutine => "Code, call",
            ReferenceKind::Entrypoint => "Code, entrypoint",
        }
    }
}

/// Represents a reference in the database.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Reference<P>
where
    P: Nameable,
{
    /// Represents a reference which has been derived from static analysis or
    /// trace execution for a specific location.
    Static {
        from: memory::Pointer<P>,
        to: memory::Pointer<P>,
        reftype: ReferenceKind,
    },

    /// Represents a reference which cannot be statically analyzed and must be
    /// traced to determine it's target(s), if any.
    Dynamic {
        at: memory::Pointer<P>,
        reftype: ReferenceKind,
    },

    /// Represents an entry point into the program.
    Entry { loc: memory::Pointer<P> },
}

impl<P> Reference<P>
where
    P: Nameable,
{
    pub fn new_static_ref(
        from: memory::Pointer<P>,
        to: memory::Pointer<P>,
        kind: ReferenceKind,
    ) -> Self {
        Reference::Static {
            from,
            to,
            reftype: kind,
        }
    }

    pub fn new_dyn_ref(at: memory::Pointer<P>, reftype: ReferenceKind) -> Self {
        Reference::Dynamic { at, reftype }
    }

    pub fn new_entrypoint(loc: memory::Pointer<P>) -> Self {
        Reference::Entry { loc }
    }

    pub fn as_source(&self) -> Option<&memory::Pointer<P>> {
        match self {
            Reference::Static {
                from,
                to: _,
                reftype: _,
            } => Some(&from),
            Reference::Dynamic { at, reftype: _ } => Some(&at),
            Reference::Entry { .. } => None,
        }
    }

    pub fn as_target(&self) -> Option<&memory::Pointer<P>> {
        match self {
            Reference::Static {
                from: _,
                to,
                reftype: _,
            } => Some(&to),
            Reference::Dynamic { at: _, reftype: _ } => None,
            Reference::Entry { loc } => Some(&loc),
        }
    }

    pub fn kind(&self) -> ReferenceKind {
        match self {
            Reference::Static {
                from: _,
                to: _,
                reftype,
            } => *reftype,
            Reference::Dynamic { at: _, reftype } => *reftype,
            Reference::Entry { .. } => ReferenceKind::Entrypoint,
        }
    }

    pub fn is_dynamic(&self) -> bool {
        matches!(self, Reference::Dynamic { .. })
    }

    /// Convert a static reference to a dynamic one, discarding information
    /// about the target in the process.
    ///
    /// Entrypoints cannot be turned into dynamic references as there is no
    /// source referrent.
    pub fn into_dynamic(self) -> Option<Self> {
        match self {
            Reference::Static {
                from,
                to: _,
                reftype,
            } => Some(Reference::Dynamic { at: from, reftype }),
            Reference::Dynamic { .. } => Some(self),
            Reference::Entry { .. } => None,
        }
    }
}
