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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Reference<P>
where
    P: analysis::Mappable,
{
    from: memory::Pointer<P>,
    to: Option<memory::Pointer<P>>,
    reftype: ReferenceKind,
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
        Reference {
            from,
            to: Some(to),
            reftype: kind,
        }
    }

    pub fn new_dyn_ref(from: memory::Pointer<P>, kind: ReferenceKind) -> Self {
        Reference {
            from,
            to: None,
            reftype: kind,
        }
    }

    pub fn as_source(&self) -> &memory::Pointer<P> {
        &self.from
    }

    pub fn as_target(&self) -> &Option<memory::Pointer<P>> {
        &self.to
    }

    pub fn kind(&self) -> ReferenceKind {
        self.reftype
    }
}
