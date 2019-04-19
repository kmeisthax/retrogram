//! "Contextual" pointers that contain any platform or architectural state
//! necessary to understand them.

use std::ops::{Add, AddAssign, Sub, SubAssign, BitAnd};
use std::cmp::{PartialEq, PartialOrd, Ord, Ordering};
use std::hash::{Hash, Hasher};
use std::collections::HashMap;
use std::convert::TryInto;
use num::traits::Bounded;
use crate::retrogram::reg::Symbolic;

/// A pointer bundled with the context necessary to resolve it to a concrete
/// value.
#[derive(Clone)]
pub struct Pointer<P, CV = u64> {
    pointer: P,
    context: HashMap<String, Symbolic<CV>>
}

impl<P, CV> Pointer<P, CV> where CV: Clone + Bounded + From<u8> {
    /// Obtain a reference to the noncontextual pointer value.
    pub fn as_pointer(&self) -> &P {
        &self.pointer
    }

    /// Strip the context entirely and yield a pointer value.
    pub fn into_pointer(self) -> P {
        self.pointer
    }

    /// Get an architecturally-defined context.
    /// 
    /// Architectural contexts are prefixed with an `A` to avoid conflicts with
    /// platform-specific contexts.
    pub fn get_arch_context(&self, context_name: &str) -> Symbolic<CV> {
        let inner_name = format!("A{}", context_name);
        if let Some(val) = self.context.get(&inner_name) {
            return val.clone();
        }

        Symbolic::default()
    }

    /// Set an architecturally-defined context.
    /// 
    /// Architectural contexts are prefixed with an `A` to avoid conflicts with
    /// platform-specific contexts.
    pub fn set_arch_context(&mut self, context_name: &str, value: Symbolic<CV>) {
        let inner_name = format!("A{}", context_name);
        self.context.insert(inner_name, value);
    }

    /// Reset an existing platform context.
    /// 
    /// The value attached to the context will be returned.
    pub fn remove_arch_context(&mut self, context_name: &str) -> Symbolic<CV> {
        let inner_name = format!("A{}", context_name);
        self.context.remove(&inner_name).unwrap_or(Symbolic::default())
    }

    /// Get a context specific to a given platform.
    /// 
    /// Platform contexts are prefixed with a `P` to avoid conflicts with
    /// architecturally defined contexts.
    pub fn get_platform_context(&self, context_name: &str) -> Symbolic<CV> {
        let inner_name = format!("P{}", context_name);
        if let Some(val) = self.context.get(&inner_name) {
            return val.clone();
        }

        Symbolic::default()
    }

    /// Set a context specific to a given platform.
    /// 
    /// Platform contexts are prefixed with a `P` to avoid conflicts with
    /// architecturally defined contexts.
    pub fn set_platform_context(&mut self, context_name: &str, value: Symbolic<CV>) {
        let inner_name = format!("P{}", context_name);
        self.context.insert(inner_name, value);
    }

    /// Reset an existing platform context.
    /// 
    /// The value attached to the context will be returned.
    pub fn remove_platform_context(&mut self, context_name: &str) -> Symbolic<CV> {
        let inner_name = format!("P{}", context_name);
        self.context.remove(&inner_name).unwrap_or(Symbolic::default())
    }

    /// List all the contexts a given pointer has.
    /// 
    /// Yields a list of tuples of booleans, strings, and context values. The
    /// boolean indicates if the context is architectural or no; the string is
    /// the context key, and the value is the context value.
    pub fn iter_contexts<'a>(&'a self) -> impl Iterator<Item = (bool, &'a str, &'a Symbolic<CV>)> + 'a {
        self.context.iter().map(|(k, v)| (k.chars().next() == Some('A'), &k[1..], v))
    }

    /// Create a new pointer with the same context as the current one.
    /// 
    /// It is not guaranteed that platform or architectural contexts will remain
    /// applicable with a different pointer. You must consult your platform to
    /// determine if the contexts selected are still applicable.
    pub fn contextualize(&self, p: P) -> Self {
        Pointer {
            pointer: p,
            context: self.context.clone()
        }
    }
}

impl<P, CV> Hash for Pointer<P, CV> where P: Hash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pointer.hash(state);
    }
}

impl<P, CV> From<P> for Pointer<P, CV> {
    fn from(p: P) -> Self {
        Pointer {
            pointer: p,
            context: HashMap::new()
        }
    }
}

impl<P, CV> Pointer<P, CV> {
    pub fn into_ptr<OP>(self) -> Pointer<OP, CV> where P: Into<OP> {
        Pointer {
            pointer: self.pointer.into(),
            context: self.context
        }
    }

    pub fn try_into_ptr<OP>(self) -> Result<Pointer<OP, CV>, <P as TryInto<OP>>::Error> where P: TryInto<OP> {
        match self.pointer.try_into() {
            Ok(into_ptr) => Ok(Pointer {
                pointer: into_ptr,
                context: self.context
            }),
            Err(e) => Err(e)
        }
    }
}

impl<P, CV> Add<P> for Pointer<P, CV> where P: Add {
    type Output = Pointer<<P as Add>::Output, CV>;

    fn add(self, rhs: P) -> Self::Output {
        Pointer {
            pointer: self.pointer + rhs,
            context: self.context
        }
    }
}

impl<P, CV> AddAssign<P> for Pointer<P, CV> where P: AddAssign {
    fn add_assign(&mut self, rhs: P) {
        self.pointer += rhs;
    }
}

impl<P, CV> Sub<P> for Pointer<P, CV> where P: Sub {
    type Output = Pointer<<P as Sub>::Output, CV>;

    fn sub(self, rhs: P) -> Self::Output {
        Pointer {
            pointer: self.pointer - rhs,
            context: self.context
        }
    }
}

impl<P, CV> SubAssign<P> for Pointer<P, CV> where P: SubAssign {
    fn sub_assign(&mut self, rhs: P) {
        self.pointer -= rhs;
    }
}

impl<P, CV> BitAnd<P> for Pointer<P, CV> where P: BitAnd {
    type Output = Pointer<<P as BitAnd>::Output, CV>;

    fn bitand(self, rhs: P) -> Self::Output {
        Pointer {
            pointer: self.pointer & rhs,
            context: self.context
        }
    }
}

impl<P, CV> PartialEq for Pointer<P, CV> where P: PartialEq {
    fn eq(&self, rhs: &Self) -> bool {
        self.pointer == rhs.pointer
    }
}

impl<P, CV> PartialOrd for Pointer<P, CV> where P: PartialOrd {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        self.pointer.partial_cmp(&rhs.pointer)
    }
}

impl<P, CV> Eq for Pointer<P, CV> where P: Eq {

}

impl<P, CV> Ord for Pointer<P, CV> where P: Ord {
    fn cmp(&self, rhs: &Self) -> Ordering {
        self.pointer.cmp(&rhs.pointer)
    }
}