//! "Contextual" pointers that contain any platform or architectural state
//! necessary to understand them.

use crate::maths::FromStrRadix;
use crate::reg;
use crate::reg::Symbolic;
use num::traits::Bounded;
use serde::de;
use serde::{Deserialize, Deserializer, Serialize};
use std::cmp::{Ord, Ordering, PartialEq, PartialOrd};
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::hash::{Hash, Hasher};
use std::num::ParseIntError;
use std::ops::{Add, AddAssign, BitAnd, Sub, SubAssign};
use std::{fmt, str};

pub type Contexts<CK, CV> = HashMap<CK, Symbolic<CV>>;

/// A pointer bundled with the context necessary to resolve it to a concrete
/// value.
#[derive(Clone, Debug)]
pub struct Pointer<P, CV = u64> {
    pointer: P,
    context: Contexts<String, CV>,
}

impl<P, CV> Pointer<P, CV> {
    /// Obtain a reference to the noncontextual pointer value.
    pub fn as_pointer(&self) -> &P {
        &self.pointer
    }

    /// Strip the context entirely and yield a pointer value.
    pub fn into_ptrval_and_contexts(self) -> (P, Contexts<String, CV>) {
        (self.pointer, self.context)
    }

    /// Construct a contextual pointer from it's parts.
    ///
    /// *DO NOT* use this function to construct a pointer and set contexts on
    /// it in one go. Contexts you create here will not be properly marked as
    /// the correct type.
    ///
    /// TODO: The internal representation of `Contexts` should change to use a
    /// type other than `String` to fix this problem.
    pub fn from_ptrval_and_contexts(ptrval: P, context: Contexts<String, CV>) -> Self {
        Self {
            pointer: ptrval,
            context,
        }
    }
}

impl<P, CV> Pointer<P, CV>
where
    CV: reg::Bitwise,
{
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
        self.context
            .remove(&inner_name)
            .unwrap_or_else(reg::Symbolic::default)
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
        self.context
            .remove(&inner_name)
            .unwrap_or_else(reg::Symbolic::default)
    }

    /// List all the contexts a given pointer has.
    ///
    /// Yields a list of tuples of booleans, strings, and context values. The
    /// boolean indicates if the context is architectural or no; the string is
    /// the context key, and the value is the context value.
    pub fn iter_contexts(&self) -> impl Iterator<Item = (bool, &str, &reg::Symbolic<CV>)> {
        self.context
            .iter()
            .map(|(k, v)| (k.starts_with('A'), &k[1..], v))
    }

    /// Create a new pointer with the same context as the current one.
    ///
    /// It is not guaranteed that platform or architectural contexts will remain
    /// applicable with a different pointer. You must consult your platform to
    /// determine if the contexts selected are still applicable.
    pub fn contextualize(&self, p: P) -> Self {
        Pointer {
            pointer: p,
            context: self.context.clone(),
        }
    }
}

impl<P, CV> Pointer<P, CV>
where
    CV: Clone + Bounded + From<u8>,
    reg::Symbolic<CV>: PartialEq,
{
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

impl<P, CV> Hash for Pointer<P, CV>
where
    P: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pointer.hash(state);
    }
}

impl<P, CV> From<P> for Pointer<P, CV> {
    fn from(p: P) -> Self {
        Pointer {
            pointer: p,
            context: HashMap::new(),
        }
    }
}

impl<P, CV> Pointer<P, CV> {
    pub fn into_ptr<OP>(self) -> Pointer<OP, CV>
    where
        P: Into<OP>,
    {
        Pointer {
            pointer: self.pointer.into(),
            context: self.context,
        }
    }

    pub fn try_into_ptr<OP>(self) -> Result<Pointer<OP, CV>, <P as TryInto<OP>>::Error>
    where
        P: TryInto<OP>,
    {
        match self.pointer.try_into() {
            Ok(into_ptr) => Ok(Pointer {
                pointer: into_ptr,
                context: self.context,
            }),
            Err(e) => Err(e),
        }
    }
}

impl<P, CV, S> Add<S> for Pointer<P, CV>
where
    P: Add<S>,
{
    type Output = Pointer<<P as Add<S>>::Output, CV>;

    fn add(self, rhs: S) -> Self::Output {
        Pointer {
            pointer: self.pointer + rhs,
            context: self.context,
        }
    }
}

impl<P, CV, S> AddAssign<S> for Pointer<P, CV>
where
    P: AddAssign<S>,
{
    fn add_assign(&mut self, rhs: S) {
        self.pointer += rhs;
    }
}

impl<P, CV, S> Sub<S> for Pointer<P, CV>
where
    P: Sub<S>,
{
    type Output = Pointer<<P as Sub<S>>::Output, CV>;

    fn sub(self, rhs: S) -> Self::Output {
        Pointer {
            pointer: self.pointer - rhs,
            context: self.context,
        }
    }
}

impl<P, CV, S> SubAssign<S> for Pointer<P, CV>
where
    P: SubAssign<S>,
{
    fn sub_assign(&mut self, rhs: S) {
        self.pointer -= rhs;
    }
}

impl<P, CV, S> BitAnd<S> for Pointer<P, CV>
where
    P: BitAnd<S>,
{
    type Output = Pointer<<P as BitAnd<S>>::Output, CV>;

    fn bitand(self, rhs: S) -> Self::Output {
        Pointer {
            pointer: self.pointer & rhs,
            context: self.context,
        }
    }
}

impl<P, CV> PartialEq for Pointer<P, CV>
where
    P: PartialEq,
    CV: PartialEq,
{
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
impl<P, CV> PartialOrd for Pointer<P, CV>
where
    P: PartialOrd,
    CV: PartialOrd + reg::Bitwise,
{
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        let mut keys = HashSet::new();

        for key in self.context.keys() {
            keys.insert(key);
        }

        for key in rhs.context.keys() {
            keys.insert(key);
        }

        let mut keys: Vec<&String> = keys.drain().collect();
        keys.sort();

        for key in keys {
            let lhs_kval = self.context.get(key);
            if lhs_kval.is_none() {
                return Some(Ordering::Less);
            }

            let lhs_kconcrete = lhs_kval.unwrap().as_concrete();
            if lhs_kconcrete.is_none() {
                return Some(Ordering::Less);
            }

            let rhs_kval = rhs.context.get(key);
            if rhs_kval.is_none() {
                return Some(Ordering::Greater);
            }

            let rhs_kconcrete = rhs_kval.unwrap().as_concrete();
            if rhs_kconcrete.is_none() {
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

impl<P, CV> Eq for Pointer<P, CV>
where
    P: Eq,
    CV: Eq,
{
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
impl<P, CV> Ord for Pointer<P, CV>
where
    P: Ord,
    CV: Ord + reg::Bitwise,
{
    fn cmp(&self, rhs: &Self) -> Ordering {
        let mut keys = HashSet::new();

        for key in self.context.keys() {
            keys.insert(key);
        }

        for key in rhs.context.keys() {
            keys.insert(key);
        }

        let mut keys: Vec<&String> = keys.drain().collect();
        keys.sort();

        for key in keys {
            let lhs_kval = self.context.get(key);
            if lhs_kval.is_none() {
                return Ordering::Less;
            }

            let lhs_kconcrete = lhs_kval.unwrap().as_concrete();
            if lhs_kconcrete.is_none() {
                return Ordering::Less;
            }

            let rhs_kval = rhs.context.get(key);
            if rhs_kval.is_none() {
                return Ordering::Greater;
            }

            let rhs_kconcrete = rhs_kval.unwrap().as_concrete();
            if rhs_kconcrete.is_none() {
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

pub enum PointerParseError<P, CV>
where
    P: str::FromStr,
    CV: str::FromStr,
{
    PointerWontParse(P::Err),
    ContextWontParse(CV::Err),
    EmptyContext,
    MissingPointer,
}

impl<P, CV> str::FromStr for Pointer<P, CV>
where
    P: str::FromStr,
    CV: str::FromStr,
    reg::Symbolic<CV>: Default + From<CV>,
{
    type Err = PointerParseError<P, CV>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut contexts = HashMap::new();

        for kv_str in s.split('!') {
            let mut kv_iter = kv_str.split('_');
            let k = kv_iter.next();
            let v = kv_iter.next();

            match (k, v) {
                (Some(key), Some("?")) => {
                    contexts.insert(key.to_string(), reg::Symbolic::default())
                }
                (Some(key), Some(value)) => contexts.insert(
                    key.to_string(),
                    reg::Symbolic::from(
                        CV::from_str(value).map_err(PointerParseError::ContextWontParse)?,
                    ),
                ),
                (Some(ptr), None) => {
                    return Ok(Pointer {
                        pointer: P::from_str(ptr).map_err(PointerParseError::PointerWontParse)?,
                        context: contexts,
                    })
                }
                _ => return Err(PointerParseError::EmptyContext),
            };
        }

        Err(PointerParseError::MissingPointer)
    }
}

impl<P, CV> FromStrRadix for Pointer<P, CV>
where
    P: FromStrRadix,
    CV: FromStrRadix,
    reg::Symbolic<CV>: Default + From<CV>,
{
    fn from_str_radix(s: &str, radix: u32) -> Result<Self, ParseIntError> {
        let mut contexts = HashMap::new();
        let obvious_error = u32::from_str_radix("AU!4*", 10).unwrap_err();

        for kv_str in s.split('!') {
            let mut kv_iter = kv_str.split('_');
            let k = kv_iter.next();
            let v = kv_iter.next();

            match (k, v) {
                (Some(key), Some("?")) => {
                    contexts.insert(key.to_string(), reg::Symbolic::default())
                }
                (Some(key), Some(value)) => contexts.insert(
                    key.to_string(),
                    reg::Symbolic::from(CV::from_str_radix(value, radix)?),
                ),
                (Some(ptr), None) => {
                    return Ok(Pointer {
                        pointer: P::from_str_radix(ptr, radix)?,
                        context: contexts,
                    })
                }
                _ => return Err(obvious_error),
            };
        }

        Err(obvious_error)
    }
}

impl<P, CV> fmt::Display for Pointer<P, CV>
where
    P: fmt::Display,
    CV: fmt::Display + reg::Bitwise + PartialEq,
{
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

impl<P, CV> fmt::UpperHex for Pointer<P, CV>
where
    P: fmt::UpperHex,
    CV: fmt::UpperHex + reg::Bitwise + PartialEq,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (ckey, cval) in self.context.iter() {
            if let Some(concrete) = cval.as_concrete() {
                write!(f, "{}_{:X}!", ckey, concrete)?;
            } else {
                //TODO: Partial symbolic bounds are lost.
                write!(f, "{}_?!", ckey)?;
            }
        }

        write!(f, "{:X}", self.pointer)
    }
}

impl<P, CV> fmt::LowerHex for Pointer<P, CV>
where
    P: fmt::LowerHex,
    CV: fmt::LowerHex + reg::Bitwise + PartialEq,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (ckey, cval) in self.context.iter() {
            if let Some(concrete) = cval.as_concrete() {
                write!(f, "{}_{:x}!", ckey, concrete)?;
            } else {
                //TODO: Partial symbolic bounds are lost.
                write!(f, "{}_?!", ckey)?;
            }
        }

        write!(f, "{:x}", self.pointer)
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
impl<'de, P, CV> Deserialize<'de> for Pointer<P, CV>
where
    P: str::FromStr,
    CV: str::FromStr,
    reg::Symbolic<CV>: Default + From<CV>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct V<P, CV>
        where
            P: str::FromStr,
            CV: str::FromStr,
            reg::Symbolic<CV>: Default + From<CV>,
        {
            p: std::marker::PhantomData<P>,
            cv: std::marker::PhantomData<CV>,
        }

        impl<'de, P, CV> de::Visitor<'de> for V<P, CV>
        where
            P: str::FromStr,
            CV: str::FromStr,
            reg::Symbolic<CV>: Default + From<CV>,
        {
            type Value = Pointer<P, CV>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("valid pointer")
            }

            fn visit_str<E>(self, value: &str) -> Result<Pointer<P, CV>, E>
            where
                E: de::Error,
            {
                value
                    .parse()
                    .map_err(|_| de::Error::invalid_value(de::Unexpected::Str(value), &self))
            }
        }

        deserializer.deserialize_str(V {
            p: std::marker::PhantomData,
            cv: std::marker::PhantomData,
        })
    }
}

impl<P, CV> Serialize for Pointer<P, CV>
where
    P: fmt::Display,
    CV: fmt::Display + reg::Bitwise,
    Pointer<P, CV>: ToString,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}
