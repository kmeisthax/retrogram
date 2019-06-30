//! A symbolic value type which allows placing bounds on values which have been
//! mutated.

use std::ops::{Sub, Not, BitAnd, BitOr, Shl, Shr};
use std::cmp::{min, max, PartialEq, Ord};
use num::traits::{Bounded, One};
use serde::{Serialize, Deserialize};
use crate::retrogram::reg::{Convertable, Concretizable, Validatable};

/// Represents a processor register bounded to a particular set of states.
/// 
/// A symbolic register represents specific bounds on the state of the
/// register. You may bound a register by specifying lower or upper bounds on
/// it's integer value, or by requiring certain bits be set or cleared. A basic
/// set of arithmetic operations are provided which will shift the bounds
/// appropriately, and additional bounds may be applied to further restrict the
/// register. It is not possible to directly remove bounds; certain arithmetic
/// operations may expand them, however.
/// 
/// The application of a bound to a symbolic register is analogous to a branch
/// in a concrete program; two bounded symbolic states are created based on if
/// the branch is or is not taken. Thus, the sum of all previously traced
/// register bounds within a block constitute the likely state of a processor.
/// 
/// If only one possible register value is valid, then the register is said to
/// be concrete. A register with no valid state is said to be unsatisfiable. If
/// multiple states are valid, then the register is said to be abstract.
#[derive(Copy, Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Symbolic<T> {
    lower_bound: T,
    upper_bound: T,
    bits_set: T,
    bits_cleared: T,
}

impl<T> Default for Symbolic<T> where T: Concretizable {
    fn default() -> Self {
        Symbolic {
            lower_bound: T::min_value(),
            upper_bound: T::max_value(),
            bits_set: T::zero(),
            bits_cleared: T::zero(),
        }
    }
}

impl<T> From<T> for Symbolic<T> where T: Concretizable {
    fn from(v: T) -> Self {
        Symbolic {
            lower_bound: v.clone(),
            upper_bound: v.clone(),
            bits_set: v.clone(),
            bits_cleared: T::from(!v),
        }
    }
}

impl<T, R> Convertable<R> for Symbolic<T> where T: From<R> + Concretizable, R: Concretizable {
    fn convert_from(v: Symbolic<R>) -> Self {
        let zero_extension = T::from(!T::from(R::from(!R::zero())));
        
        Symbolic {
            lower_bound: T::from(v.lower_bound),
            upper_bound: T::from(v.upper_bound),
            bits_set: T::from(v.bits_set),
            bits_cleared: T::from(zero_extension | T::from(v.bits_cleared)),
        }
    }
}

impl<T> Symbolic<T> where T: Concretizable {
    fn is_bound_concrete(&self) -> bool {
        self.lower_bound == self.upper_bound
    }

    fn is_bound_undefined(&self) -> bool {
        self.lower_bound == T::min_value() && self.upper_bound == T::max_value()
    }

    fn is_bits_concrete(&self) -> bool {
        T::from(self.bits_set.clone() | self.bits_cleared.clone()) == T::from(!T::zero())
    }

    fn is_bits_undefined(&self) -> bool {
        T::from(self.bits_set.clone() | T::from(!self.bits_cleared.clone())) == T::zero()
    }

    /// Determines if exactly one valid T matches this symbolic value
    ///
    /// TODO: This is nonexaustive, contrived cases exist which have one
    /// satisfying value but not by way of one bound mechanism or the other.
    pub fn is_concrete(&self) -> bool {
        self.is_bound_concrete() || self.is_bits_concrete()
    }

    /// Determines if any valid T matches this symbolic value
    pub fn is_undefined(&self) -> bool {
        self.is_bound_undefined() && self.is_bits_undefined()
    }

    pub fn into_concrete(self) -> Option<T> {
        if self.is_bound_concrete() {
            Some(self.lower_bound)
        } else if self.is_bits_concrete() {
            Some(self.bits_set)
        } else {
            None
        }
    }

    pub fn as_concrete(&self) -> Option<&T> {
        if self.is_bound_concrete() {
            Some(&self.lower_bound)
        } else if self.is_bits_concrete() {
            Some(&self.bits_set)
        } else {
            None
        }
    }
}

impl<T> Symbolic<T> where T: Validatable {
    /// Returns true if the given value satisfies the register constraint
    pub fn is_valid(&self, v: T) -> bool {
        let notv = !v.clone();
        v >= self.lower_bound && v <= self.upper_bound && T::from(v & self.bits_set.clone()) == self.bits_set && T::from(T::from(notv) & self.bits_cleared.clone()) == self.bits_cleared
    }
}

impl<T> Symbolic<T> where T: Clone + Ord {
    pub fn increase_lower_bound(&mut self, v: T) {
        self.lower_bound = max(self.lower_bound.clone(), v);
    }
    
    pub fn decrease_upper_bound(&mut self, v: T) {
        self.upper_bound = min(self.upper_bound.clone(), v);
    }
}

impl<T> BitAnd for Symbolic<T> where T: Bounded + BitAnd + BitOr + From<<T as BitAnd>::Output> + From<<T as BitOr>::Output> {
    type Output = Symbolic<T>;
    
    fn bitand(self, sv:Self) -> Self {
        Symbolic {
            lower_bound: T::min_value(),
            upper_bound: T::max_value(),
            bits_set: T::from(self.bits_set & sv.bits_set),
            bits_cleared: T::from(self.bits_cleared | sv.bits_cleared)
        }
    }
}

impl<T> BitOr for Symbolic<T> where T: Bounded + BitAnd + BitOr + From<<T as BitAnd>::Output> + From<<T as BitOr>::Output> {
    type Output = Symbolic<T>;
    
    fn bitor(self, sv:Self) -> Self {
        Symbolic {
            lower_bound: T::min_value(),
            upper_bound: T::max_value(),
            bits_set: T::from(self.bits_set | sv.bits_set),
            bits_cleared: T::from(self.bits_cleared & sv.bits_cleared)
        }
    }
}

impl<T,R> Shl<R> for Symbolic<T> where T: Shl<R> + One, R: Clone,
    <T as Shl<R>>::Output: BitOr + Sub<T> + From<<<T as Shl<R>>::Output as BitOr>::Output> + From<<<T as Shl<R>>::Output as Sub<T>>::Output> {
    type Output = Symbolic<<T as Shl<R>>::Output>;

    fn shl(self, rhs: R) -> Self::Output {
        Symbolic {
            lower_bound: self.lower_bound << rhs.clone(),
            upper_bound: self.upper_bound << rhs.clone(),
            bits_set: self.bits_set << rhs.clone(),
            bits_cleared: <T as Shl<R>>::Output::from(self.bits_cleared << rhs.clone() | <T as Shl<R>>::Output::from((T::one() << rhs.clone()) - T::one()))
        }
    }
}