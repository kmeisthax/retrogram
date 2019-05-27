//! "Contextual" pointers that contain any platform or architectural state
//! necessary to understand them.

use std::{fmt, str};
use std::ops::{Add, AddAssign, Sub, SubAssign, BitAnd};
use std::cmp::{PartialEq, PartialOrd, Ord, Ordering};
use std::hash::{Hash, Hasher};
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use num::traits::Bounded;
use serde::{Serialize, Deserialize, Deserializer};
use serde::de;
use crate::retrogram::reg;

/// A pointer bundled with the context necessary to resolve it to a concrete
/// value.
#[derive(Clone, Debug)]
pub struct Pointer<P, CV = u64> {
    pointer: P,
    context: HashMap<String, reg::Symbolic<CV>>
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
    pub fn get_arch_context(&self, context_name: &str) -> reg::Symbolic<CV> {
        let inner_name = format!("A{}", context_name);
        if let Some(val) = self.context.get(&inner_name) {
            return val.clone();
        }

        reg::Symbolic::default()
    }

    /// Set an architecturally-defined context.
    /// 
    /// Architectural contexts are prefixed with an `A` to avoid conflicts with
    /// platform-specific contexts.
    pub fn set_arch_context(&mut self, context_name: &str, value: reg::Symbolic<CV>) {
        let inner_name = format!("A{}", context_name);
        self.context.insert(inner_name, value);
    }

    /// Reset an existing platform context.
    /// 
    /// The value attached to the context will be returned.
    pub fn remove_arch_context(&mut self, context_name: &str) -> reg::Symbolic<CV> {
        let inner_name = format!("A{}", context_name);
        self.context.remove(&inner_name).unwrap_or(reg::Symbolic::default())
    }

    /// Get a context specific to a given platform.
    /// 
    /// Platform contexts are prefixed with a `P` to avoid conflicts with
    /// architecturally defined contexts.
    pub fn get_platform_context(&self, context_name: &str) -> reg::Symbolic<CV> {
        let inner_name = format!("P{}", context_name);
        if let Some(val) = self.context.get(&inner_name) {
            return val.clone();
        }

        reg::Symbolic::default()
    }

    /// Set a context specific to a given platform.
    /// 
    /// Platform contexts are prefixed with a `P` to avoid conflicts with
    /// architecturally defined contexts.
    pub fn set_platform_context(&mut self, context_name: &str, value: reg::Symbolic<CV>) {
        let inner_name = format!("P{}", context_name);
        self.context.insert(inner_name, value);
    }

    /// Reset an existing platform context.
    /// 
    /// The value attached to the context will be returned.
    pub fn remove_platform_context(&mut self, context_name: &str) -> reg::Symbolic<CV> {
        let inner_name = format!("P{}", context_name);
        self.context.remove(&inner_name).unwrap_or(reg::Symbolic::default())
    }

    /// List all the contexts a given pointer has.
    /// 
    /// Yields a list of tuples of booleans, strings, and context values. The
    /// boolean indicates if the context is architectural or no; the string is
    /// the context key, and the value is the context value.
    pub fn iter_contexts<'a>(&'a self) -> impl Iterator<Item = (bool, &'a str, &'a reg::Symbolic<CV>)> + 'a {
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

impl<P, CV> Pointer<P, CV> where CV: Clone + Bounded + From<u8>, reg::Symbolic<CV>: PartialEq {
    /// Determines if this and another contextual pointer have the same context.
    pub fn is_context_eq(&self, other: &Self) -> bool {
        //TODO: For equivalent contexts we wind up testing each key twice. How
        //do we prevent that?
        for (key, val) in self.context.iter() {
            if let Some(other_val) = other.context.get(key) {
                if other_val == val {
                    continue;
                }
            }

            return false;
        }

        for (key, other_val) in other.context.iter() {
            if let Some(val) = self.context.get(key) {
                if other_val == val {
                    continue;
                }
            }

            return false;
        }

        true
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

impl<P, CV> PartialEq for Pointer<P, CV> where P: PartialEq, CV: PartialEq {
    fn eq(&self, rhs: &Self) -> bool {
        let core_eq = self.pointer == rhs.pointer;
        let mut context_eq = true;

        for (ckey, cval) in self.context.iter() {
            context_eq = context_eq && Some(cval) == rhs.context.get(ckey);
        }

        for (foreign_ckey, foreign_cval) in rhs.context.iter() {
            context_eq = context_eq && Some(foreign_cval) == self.context.get(foreign_ckey);
        }

        core_eq && context_eq
    }
}

/// Implement partial ordering for pointers.
/// 
/// The sort order of a pointer is as follows:
/// 
/// 1. For all architectual contexts held by either pointer, in alphabetical
///    order of context keys, sort pointers without the context before pointers
///    with the context. Multiple pointers with the context are sorted by their
///    context values.
/// 2. For all platform contexts held by either pointer, in alphabetical order
///    of context keys, sort pointers without the context before pointers with
///    the context. Multiple pointers with the context are sorted by their
///    context values.
/// 3. Pointers with the same contexts sort according to their underlying
///    address type.
/// 
/// The only case in which this partial ordering returns None is if the
/// underlying address or context type's partial ordering would do the same.
impl<P, CV> PartialOrd for Pointer<P, CV> where P: PartialOrd, CV: PartialOrd + reg::Concretizable {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        let mut keys = HashSet::new();

        for key in self.context.keys() {
            keys.insert(key);
        }

        for key in rhs.context.keys() {
            keys.insert(key);
        }

        let mut keys : Vec<&String> = keys.drain().collect();
        keys.sort();
        
        for key in keys {
            let lhs_kval = self.context.get(key);
            if let None = lhs_kval {
                return Some(Ordering::Less);
            }

            let lhs_kconcrete = lhs_kval.unwrap().as_concrete();
            if let None = lhs_kconcrete {
                return Some(Ordering::Less);
            }

            let rhs_kval = rhs.context.get(key);
            if let None = rhs_kval {
                return Some(Ordering::Greater);
            }

            let rhs_kconcrete = rhs_kval.unwrap().as_concrete();
            if let None = rhs_kconcrete {
                return Some(Ordering::Greater);
            }

            let ordering = lhs_kconcrete.unwrap().partial_cmp(rhs_kconcrete.unwrap());
            if ordering != Some(Ordering::Equal) {
                return ordering;
            }
        }

        self.pointer.partial_cmp(&rhs.pointer)
    }
}

impl<P, CV> Eq for Pointer<P, CV> where P: Eq, CV: Eq {

}
/// Implement total ordering for pointers.
/// 
/// The sort order of a pointer is as follows:
/// 
/// 1. For all architectual contexts held by either pointer, in alphabetical
///    order of context keys, sort pointers without the context before pointers
///    with the context. Multiple pointers with the context are sorted by their
///    context values.
/// 2. For all platform contexts held by either pointer, in alphabetical order
///    of context keys, sort pointers without the context before pointers with
///    the context. Multiple pointers with the context are sorted by their
///    context values.
/// 3. Pointers with the same contexts sort according to their underlying
///    address type.
impl<P, CV> Ord for Pointer<P, CV> where P: Ord, CV: Ord + reg::Concretizable {
    fn cmp(&self, rhs: &Self) -> Ordering {
        let mut keys = HashSet::new();

        for key in self.context.keys() {
            keys.insert(key);
        }

        for key in rhs.context.keys() {
            keys.insert(key);
        }

        let mut keys : Vec<&String> = keys.drain().collect();
        keys.sort();
        
        for key in keys {
            let lhs_kval = self.context.get(key);
            if let None = lhs_kval {
                return Ordering::Less;
            }

            let lhs_kconcrete = lhs_kval.unwrap().as_concrete();
            if let None = lhs_kconcrete {
                return Ordering::Less;
            }

            let rhs_kval = rhs.context.get(key);
            if let None = rhs_kval {
                return Ordering::Greater;
            }

            let rhs_kconcrete = rhs_kval.unwrap().as_concrete();
            if let None = rhs_kconcrete {
                return Ordering::Greater;
            }

            let ordering = lhs_kconcrete.unwrap().cmp(rhs_kconcrete.unwrap());
            if ordering != Ordering::Equal {
                return ordering;
            }
        }

        self.pointer.cmp(&rhs.pointer)
    }
}

pub enum PointerParseError<P, CV> where P: str::FromStr, CV: str::FromStr {
    PointerWontParse(P::Err),
    ContextWontParse(CV::Err),
    EmptyContext,
    MissingPointer
}

impl<P, CV> str::FromStr for Pointer<P, CV> where P: str::FromStr, CV: str::FromStr, reg::Symbolic<CV>: Default + From<CV> {
    type Err = PointerParseError<P, CV>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut contexts = HashMap::new();
        
        for kv_str in s.split("!") {
            let mut kv_iter = kv_str.split("_");
            let k = kv_iter.next();
            let v = kv_iter.next();

            match (k, v) {
                (Some(key), Some("?")) => contexts.insert(key.to_string(), reg::Symbolic::default()),
                (Some(key), Some(value)) => contexts.insert(key.to_string(), reg::Symbolic::from(CV::from_str(value).map_err(|e| PointerParseError::ContextWontParse(e) )?)),
                (Some(ptr), None) => return Ok(Pointer{
                    pointer: P::from_str(ptr).map_err(|e| PointerParseError::PointerWontParse(e))?,
                    context: contexts
                }),
                _ => return Err(PointerParseError::EmptyContext)
            };
        }

        Err(PointerParseError::MissingPointer)
    }
}

impl<P, CV> fmt::Display for Pointer<P, CV> where P: fmt::Display, CV: fmt::Display + reg::Concretizable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (ckey, cval) in self.context.iter() {
            if let Some(concrete) = cval.as_concrete() {
                write!(f, "{}_{}!", ckey, concrete)?;
            } else {
                //TODO: Partial symbolic bounds are lost.
                write!(f, "{}_?!", ckey)?;
            }
        }

        write!(f, "{}", self.pointer)
    }
}

//Oh look, serde_plain can't work with parametric types at all.
//What a shame. Now I have to actually, uh, write a Deserializer by hand.
//I have a confession to make: I don't understand the serde deserialization API
//AT ALL - just the whole "let's define a struct and impl in the middle of a
//function" thing deeply scares me, simply because I didn't even know or even
//EXPECT Rust to have inner type syntax at all. However serde works it's black
//magic, it does so by twisting Rust's otherwise workmanlike syntax into an
//amalgamated monstrosity.
impl<'de, P, CV> Deserialize<'de> for Pointer<P, CV> where P: str::FromStr, CV: str::FromStr, reg::Symbolic<CV>: Default + From<CV>  {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'de> {
        struct V<P, CV> where P: str::FromStr, CV: str::FromStr, reg::Symbolic<CV>: Default + From<CV> {
            p: std::marker::PhantomData<P>,
            cv: std::marker::PhantomData<CV>
        }

        impl<'de, P, CV> de::Visitor<'de> for V<P, CV> where P: str::FromStr, CV: str::FromStr, reg::Symbolic<CV>: Default + From<CV> {
            type Value = Pointer<P, CV>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("valid pointer")
            }

            fn visit_str<E>(self, value: &str) -> Result<Pointer<P, CV>, E> where E: de::Error {
                value.parse().map_err(|_| de::Error::invalid_value(de::Unexpected::Str(value), &self))
            }
        }

        deserializer.deserialize_str(V {
            p: std::marker::PhantomData,
            cv: std::marker::PhantomData
        })
    }
}

impl<P, CV> Serialize for Pointer<P, CV> where P: fmt::Display, CV: fmt::Display + reg::Concretizable {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: serde::ser::Serializer {
        serializer.serialize_str(&self.to_string())
    }
}