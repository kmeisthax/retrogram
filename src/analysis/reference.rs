//! Types needed to analyze references

use crate::{analysis, memory};
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter, Result};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ReferenceKind {
    Unknown,
    Data,
    Code,
    Subroutine,
}

impl Display for ReferenceKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        //write!(f, "({}, {})", self.x, self.y)
        match self {
            ReferenceKind::Unknown => write!(f, "UNK"),
            ReferenceKind::Data => write!(f, "DAT"),
            ReferenceKind::Code => write!(f, "LOC"),
            ReferenceKind::Subroutine => write!(f, "FUN"),
        }
    }
}

/// Represents a reference in the database.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Reference<P>
where
    P: analysis::Mappable,
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
}

impl<P> Reference<P>
where
    P: analysis::Mappable,
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

    pub fn as_source(&self) -> &memory::Pointer<P> {
        match self {
            Reference::Static {
                from,
                to: _,
                reftype: _,
            } => &from,
            Reference::Dynamic { at, reftype: _ } => &at,
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
        }
    }

    pub fn is_dynamic(&self) -> bool {
        match self {
            Reference::Dynamic { .. } => true,
            _ => false,
        }
    }
}
