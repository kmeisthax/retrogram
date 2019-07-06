//! A symbolic value type which allows placing bounds on values which have been
//! mutated.

use std::ops::{Sub, Not, BitAnd, BitOr, Shl, Shr};
use num::traits::{Bounded, One};
use serde::{Serialize, Deserialize};
use crate::retrogram::reg::{Convertable, Concretizable};

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
    bits_set: T,
    bits_cleared: T,
}

impl<T> Symbolic<T> {
    /// Construct a symbolic value from bits that should be set and bits that
    /// should be cleared.
    pub fn from_bits(bits_set: T, bits_cleared: T) -> Self {
        Symbolic {
            bits_set: bits_set,
            bits_cleared: bits_cleared
        }
    }
}

impl<T> Default for Symbolic<T> where T: Concretizable {
    fn default() -> Self {
        Symbolic {
            bits_set: T::zero(),
            bits_cleared: T::zero(),
        }
    }
}

impl<T> From<T> for Symbolic<T> where T: Concretizable {
    fn from(v: T) -> Self {
        Symbolic {
            bits_set: v.clone(),
            bits_cleared: T::from(!v),
        }
    }
}

impl<T, R> Convertable<R> for Symbolic<T> where T: From<R> + Concretizable, R: Concretizable {
    fn convert_from(v: Symbolic<R>) -> Self {
        let zero_extension = T::from(!T::from(R::from(!R::zero())));
        
        Symbolic {
            bits_set: T::from(v.bits_set),
            bits_cleared: T::from(zero_extension | T::from(v.bits_cleared)),
        }
    }
}

impl<T> Symbolic<T> where T: Concretizable {
    /// Construct a symbolic value from a given concrete value where we only
    /// want to specify the bits listed in `cares`. Bits in `cares` that are
    /// zero will be treated as unconstrained.
    pub fn from_cares(v: T, cares: T) -> Self {
        Symbolic {
            bits_set: v.clone(),
            bits_cleared: T::from(T::from(!v) & cares)
        }
    }

    /// Determines if this symbolic value does not constrain the values which
    /// may satisfy it.
    pub fn is_unconstrained(&self) -> bool {
        T::from(self.bits_set.clone() | self.bits_cleared.clone()) == T::zero()
    }

    /// Determine if this symbolic value cannot be satisfied.
    pub fn is_unsatisfiable(&self) -> bool {
        T::from(self.bits_set.clone() & self.bits_cleared.clone()) != T::zero()
    }

    /// Determines if this symbolic value is constrained to precisely one value.
    pub fn is_concrete(&self) -> bool {
        T::from(self.bits_set.clone() | self.bits_cleared.clone()) == T::from(!T::zero())
    }

    pub fn into_concrete(self) -> Option<T> {
        if self.is_concrete() {
            Some(self.bits_set)
        } else {
            None
        }
    }

    pub fn as_concrete(&self) -> Option<&T> {
        if self.is_concrete() {
            Some(&self.bits_set)
        } else {
            None
        }
    }

    /// Produce a concrete value where each bit of the value is `1` if and only
    /// if that bit must be ether set or cleared to satisfy the constraint of
    /// this symbolic value.
    /// 
    /// #Identities
    /// 
    /// A `cares` value equal to `!0` indicates a value that is either concrete
    /// or unsatisfiable.
    /// 
    /// A concrete value anded or ored by `cares` should satisfy the constraint
    /// of the symbolic value if and only if the original concerete value would
    /// also do so.
    pub fn cares(&self) -> T {
        T::from(self.bits_set.clone() | self.bits_cleared.clone())
    }

    /// Produce a concrete value where each bit of the value is `1` if and only
    /// if that bit can change without affecting the satisfiability of that
    /// concrete value.
    pub fn not_cares(&self) -> T {
        T::from(!self.cares())
    }
    
    /// Returns true if the given value satisfies the register constraint
    pub fn is_valid(&self, v: T) -> bool {
        let notv = !v.clone();
        T::from(v & self.bits_set.clone()) == self.bits_set && T::from(T::from(notv) & self.bits_cleared.clone()) == self.bits_cleared
    }
}

impl<T> BitAnd for Symbolic<T> where T: Bounded + BitAnd + BitOr + From<<T as BitAnd>::Output> + From<<T as BitOr>::Output> {
    type Output = Symbolic<T>;
    
    fn bitand(self, sv:Self) -> Self {
        Symbolic {
            bits_set: T::from(self.bits_set & sv.bits_set),
            bits_cleared: T::from(self.bits_cleared | sv.bits_cleared)
        }
    }
}

impl<T> BitOr for Symbolic<T> where T: Bounded + BitAnd + BitOr + From<<T as BitAnd>::Output> + From<<T as BitOr>::Output> {
    type Output = Symbolic<T>;
    
    fn bitor(self, sv:Self) -> Self {
        Symbolic {
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
            bits_set: self.bits_set << rhs.clone(),
            bits_cleared: <T as Shl<R>>::Output::from(self.bits_cleared << rhs.clone() | <T as Shl<R>>::Output::from((T::one() << rhs.clone()) - T::one()))
        }
    }
}

impl<T,R> Shr<R> for Symbolic<T> where T: Shr<R> + Bounded + Not, R: Clone,
    <T as Shr<R>>::Output: BitOr + Not + From<<<T as Shr<R>>::Output as BitOr>::Output> + From<<<T as Shr<R>>::Output as Not>::Output> {
    type Output = Symbolic<<T as Shr<R>>::Output>;

    fn shr(self, rhs: R) -> Self::Output {
        Symbolic {
            bits_set: self.bits_set >> rhs.clone(),
            bits_cleared: <T as Shr<R>>::Output::from(self.bits_cleared >> rhs.clone() | <T as Shr<R>>::Output::from(!(T::max_value() >> rhs.clone())))
        }
    }
}