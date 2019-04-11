//! "Contextual" pointers that contain any platform or architectural state
//! necessary to understand them.

use std::ops::AddAssign;
use crate::retrogram::reg::Context;

/// A pointer bundled with the context necessary to resolve it to a concrete
/// value.
#[derive(Clone)]
pub struct ContextualPointer<P> {
    pointer: P,
    context: Context
}

impl<P> ContextualPointer<P> {
    /// Given a pointer and it's context, wrap them up into a contextual
    /// pointer.
    pub fn from_parts(pointer: P, context: Context) -> ContextualPointer<P> {
        ContextualPointer {
            pointer: pointer,
            context: context
        }
    }

    /// Consume the contextual pointer and return the pointer value and it's
    /// modeled context.
    pub fn into_parts(self) -> (P, Context) {
        (self.pointer, self.context)
    }

    pub fn pointer(&self) -> &P {
        &self.pointer
    }

    pub fn context(&self) -> &Context {
        &self.context
    }
}

impl<P> From<P> for ContextualPointer<P> {
    fn from(p: P) -> ContextualPointer<P> {
        ContextualPointer {
            pointer: p,
            context: Context::new()
        }
    }
}

impl<P> AddAssign<P> for ContextualPointer<P> where P: AddAssign {
    fn add_assign(&mut self, rhs: P) {
        self.pointer += rhs;
    }
}