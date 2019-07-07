//! A symbolic value type which allows placing bounds on values which have been
//! mutated.

use std::ops::{Sub, Not, BitAnd, BitOr, BitXor, Shl, Shr};
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
    /// Construct a symbolic value from bits that should be set and bits that
    /// should be cleared.
    pub fn from_bits(bits_set: T, bits_cleared: T) -> Self {
        Symbolic {
            bits_set: bits_set,
            bits_cleared: bits_cleared
        }
    }

    /// Break a symbolic value into it's bits-set and bits-cleared parts.
    pub fn into_bits(self) -> (T, T) {
        (self.bits_set, self.bits_cleared)
    }

    /// Reference the bits-set and bits-cleared parts of a symbolic value.
    pub fn as_bits(&self) -> (&T, &T) {
        (&self.bits_set, &self.bits_cleared)
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
        !self.is_unsatisfiable() && T::from(self.bits_set.clone() | self.bits_cleared.clone()) == T::from(!T::zero())
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

    /// Generate all possible values which satisfy the symbolic constraint.
    pub fn valid(&self) -> impl Iterator<Item = T> {
        struct SymbolicValueIterator<T> {
            not_cares: T,
            next: Option<T>,
        }

        impl<T> Iterator for SymbolicValueIterator<T> where T: Concretizable {
            type Item = T;

            fn next(&mut self) -> Option<T> {
                let r = self.next.clone();

                if let Some(mut v) = self.next.clone() {
                    let nc = self.not_cares.clone();

                    //Some explanation for this weirdness... We kind of have our
                    //hands tied syntactically here as I want to be able to use
                    //any bitwise numeral type here, including exotic signed
                    //representations. So we have to iterate through each bit,
                    //and if we find a nocare bit then we half-add it with the
                    //current value and propagate carries forward. When we run
                    //out of carries we're done and can save the new value.

                    let mut carry = T::one();
                    let mut bit = T::zero();

                    while carry != T::zero() && bit != T::bound_width() {
                        let mask = T::from(T::one() << bit.clone());

                        if T::from(nc.clone() & mask.clone()) != T::zero() {
                            let vbit = T::from(v.clone() & mask.clone());
                            let carrybit = T::from(carry << bit.clone());
                            let newcarry = match vbit == T::zero() {
                                true => T::zero(),
                                false => T::one()
                            };
                            let vcut = T::from(v & T::from(!mask));

                            v = T::from(vcut | T::from(carrybit ^ vbit));
                            carry = newcarry;
                        }

                        bit = bit + T::one();
                    }

                    if carry == T::zero() {
                        self.next = Some(v);
                    } else {
                        self.next = None;
                    }
                }

                r
            }
        }

        SymbolicValueIterator{
            not_cares: self.not_cares(),
            next: match self.is_unsatisfiable() {
                true => None,
                false => Some(self.bits_set.clone())
            }
        }
    }

    /// Calculate the lowest possible value that can satisfy the symbolic value.
    pub fn lower_bound(&self) -> Option<T> {
        if !self.is_unsatisfiable() {
            Some(self.bits_set.clone())
        } else {
            None
        }
    }

    /// Calculate the highest possible value that can satisfy the symbolic
    /// value.
    pub fn upper_bound(&self) -> Option<T> {
        if !self.is_unsatisfiable() {
            Some(T::from(self.bits_set.clone() | self.not_cares()))
        } else {
            None
        }
    }
}

impl<T, R> BitAnd<Symbolic<R>> for Symbolic<T> where T: Bounded + BitAnd<R> + BitOr<R>,
    <T as BitAnd<R>>::Output: From<<T as BitOr<R>>::Output> {
    type Output = Symbolic<<T as BitAnd<R>>::Output>;
    
    fn bitand(self, sv:Symbolic<R>) -> Self::Output {
        Symbolic {
            bits_set: self.bits_set & sv.bits_set,
            bits_cleared: <T as BitAnd<R>>::Output::from(self.bits_cleared | sv.bits_cleared)
        }
    }
}

impl<T, R> BitOr<Symbolic<R>> for Symbolic<T> where T: Bounded + BitAnd<R> + BitOr<R>,
    <T as BitOr<R>>::Output: From<<T as BitAnd<R>>::Output> {
    type Output = Symbolic<<T as BitOr<R>>::Output>;
    
    fn bitor(self, sv:Symbolic<R>) -> Self::Output {
        Symbolic {
            bits_set: self.bits_set | sv.bits_set,
            bits_cleared: <T as BitOr<R>>::Output::from(self.bits_cleared & sv.bits_cleared)
        }
    }
}

impl<T, R> BitXor<Symbolic<R>> for Symbolic<T>
    where T: Clone + BitOr + BitAnd<R> + BitXor<R> + From<<T as BitOr>::Output>,
        R: Clone + BitOr + From<<R as BitOr>::Output>,
        <T as BitXor<R>>::Output: Clone + Not + BitAnd + From<<T as BitAnd<R>>::Output> +
            From<<<T as BitXor<R>>::Output as Not>::Output> + From<<<T as BitXor<R>>::Output as BitAnd>::Output> {
    type Output = Symbolic<<T as BitXor<R>>::Output>;

    fn bitxor(self, sv:Symbolic<R>) -> Self::Output {
        let self_nocare = T::from(self.bits_set.clone() | self.bits_cleared.clone());
        let rhs_nocare = R::from(sv.bits_set.clone() | sv.bits_cleared.clone());
        let mask = <T as BitXor<R>>::Output::from(self_nocare & rhs_nocare);
        let bitset_xor = self.bits_set ^ sv.bits_set;
        let bitclear_xor = self.bits_cleared ^ sv.bits_cleared;

        Symbolic {
            bits_set: <T as BitXor<R>>::Output::from(bitset_xor & mask.clone()),
            bits_cleared: <T as BitXor<R>>::Output::from(<T as BitXor<R>>::Output::from(!bitclear_xor) & mask)
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