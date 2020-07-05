//! A symbolic value type which allows placing bounds on values which have been
//! mutated.

use crate::maths::BoundWidth;
use crate::memory::{Desegmentable, Endianness};
use crate::reg::{Bitwise, Convertable, TryConvertable};
use num::traits::{Bounded, CheckedShl, CheckedShr, One, Zero};
use serde::{Deserialize, Serialize};
use std::cmp::min;
use std::convert::TryFrom;
use std::fmt;
use std::fmt::{Formatter, LowerHex, UpperHex};
use std::ops::{Add, BitAnd, BitOr, BitXor, Not, Shl, Shr, Sub};

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
///
/// Note: The implementation of `PartialOrd` and `Ord` is intended primarily to
/// allow the use of symbolic values in sets and other structures that need to
/// produce consistent orderings of keys. It is not a logically valid ordering
/// according to symbolic bit semantics; actual symbolic execution should not
/// use orderings.
#[derive(Copy, Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbolic<T> {
    bits_set: T,
    bits_cleared: T,
}

impl<T> Symbolic<T> {
    /// Construct a symbolic value from bits that should be set and bits that
    /// should be cleared.
    pub fn from_bits(bits_set: T, bits_cleared: T) -> Self {
        Symbolic {
            bits_set,
            bits_cleared,
        }
    }
}

impl<T> Default for Symbolic<T>
where
    T: Zero,
{
    fn default() -> Self {
        Symbolic {
            bits_set: T::zero(),
            bits_cleared: T::zero(),
        }
    }
}

impl<T> From<T> for Symbolic<T>
where
    T: Clone + Not<Output = T>,
{
    fn from(v: T) -> Self {
        Symbolic {
            bits_set: v.clone(),
            bits_cleared: !v,
        }
    }
}

impl<T> Zero for Symbolic<T>
where
    T: Bitwise,
    Symbolic<T>: Add<Symbolic<T>, Output = Symbolic<T>>,
{
    fn zero() -> Self {
        Symbolic {
            bits_set: T::zero(),
            bits_cleared: !T::zero(),
        }
    }

    fn is_zero(&self) -> bool {
        self.is_concrete() && self.bits_set.is_zero()
    }
}

impl<T, R> BoundWidth<R> for Symbolic<T>
where
    T: BoundWidth<R>,
    Symbolic<T>: Shl<R>,
{
    fn bound_width() -> R {
        T::bound_width()
    }
}

impl<T, R> Convertable<R> for Symbolic<T>
where
    T: From<R> + Bitwise,
    R: Bitwise,
{
    fn convert_from(v: Symbolic<R>) -> Self {
        let zero_extension = !T::from(!R::zero());

        Symbolic {
            bits_set: T::from(v.bits_set),
            bits_cleared: zero_extension | T::from(v.bits_cleared),
        }
    }
}

impl<T, R> TryConvertable<R> for Symbolic<T>
where
    T: TryFrom<R> + Bitwise,
    R: Bitwise,
{
    type Error = <T as TryFrom<R>>::Error;

    fn try_convert_from(v: Symbolic<R>) -> Result<Self, Self::Error> {
        let conv_width = min(T::bound_width(), R::bound_width());
        let mask = !(!R::zero()).checked_shl(conv_width).unwrap_or_else(R::zero);
        let zero_extension = (!T::zero()).checked_shl(conv_width).unwrap_or_else(T::zero);

        Ok(Symbolic {
            bits_set: T::try_from(v.bits_set)?,
            bits_cleared: zero_extension | T::try_from(v.bits_cleared & mask)?,
        })
    }
}

impl<T> Symbolic<T>
where
    T: Bitwise,
{
    /// Construct a symbolic value from a given concrete value where we only
    /// want to specify the bits listed in `cares`. Bits in `cares` that are
    /// zero will be treated as unconstrained.
    pub fn from_cares(v: T, cares: T) -> Self {
        Symbolic {
            bits_set: v.clone(),
            bits_cleared: !v & cares,
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
        self.bits_set.clone() | self.bits_cleared.clone()
    }

    /// Produce a concrete value where each bit of the value is `1` if and only
    /// if that bit can change without affecting the satisfiability of that
    /// concrete value.
    pub fn not_cares(&self) -> T {
        !self.cares()
    }

    /// Determines if this symbolic value does not constrain the values which
    /// may satisfy it.
    pub fn is_unconstrained(&self) -> bool {
        (self.bits_set.clone() | self.bits_cleared.clone()).is_zero()
    }

    /// Determine if this symbolic value cannot be satisfied.
    pub fn is_unsatisfiable(&self) -> bool {
        !((self.bits_set.clone() & self.bits_cleared.clone()).is_zero())
    }

    /// Determines if this symbolic value is constrained to precisely one value.
    pub fn is_concrete(&self) -> bool {
        !self.is_unsatisfiable() && (!self.bits_set.clone() & !self.bits_cleared.clone()).is_zero()
    }

    /// Determines if specific bits of this symbolic value are constrained to
    /// precisely one value.
    pub fn bits_are_concrete(&self, mask: T) -> bool {
        !self.is_unsatisfiable()
            && (!self.bits_set.clone() & !self.bits_cleared.clone() & mask).is_zero()
    }

    /// Consumes the symbolic value to produce a concrete value, or None if it
    /// is not concrete.
    pub fn into_concrete(self) -> Option<T> {
        if self.is_concrete() {
            Some(self.bits_set)
        } else {
            None
        }
    }

    /// References the symbolic value as if it was concrete, or None if it is
    /// not concrete.
    pub fn as_concrete(&self) -> Option<&T> {
        if self.is_concrete() {
            Some(&self.bits_set)
        } else {
            None
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
            Some(self.bits_set.clone() | self.not_cares())
        } else {
            None
        }
    }
}

impl<T> Symbolic<T>
where
    T: Bitwise + PartialEq,
{
    /// Returns true if the given value satisfies the register constraint
    pub fn is_valid(&self, v: T) -> bool {
        let notv = !v.clone();
        v & self.bits_set.clone() == self.bits_set
            && notv & self.bits_cleared.clone() == self.bits_cleared
    }
}

impl<T> Symbolic<T>
where
    T: Bitwise + PartialEq + One,
{
    /// Generate all possible values which satisfy the symbolic constraint.
    pub fn valid(&self) -> impl Iterator<Item = T> {
        struct SymbolicValueIterator<T> {
            not_cares: T,
            next: Option<T>,
        }

        impl<T> Iterator for SymbolicValueIterator<T>
        where
            T: Bitwise + PartialEq + One,
        {
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
                    let mut bit = 0;

                    while carry != T::zero() && bit != T::bound_width() {
                        let mask = T::one() << bit;

                        if nc.clone() & mask.clone() != T::zero() {
                            let vbit = v.clone() & mask.clone();
                            let carrybit = carry << bit;
                            let newcarry = if vbit == T::zero() {
                                T::zero()
                            } else {
                                T::one()
                            };
                            let vcut = v & !mask;

                            v = vcut | carrybit ^ vbit;
                            carry = newcarry;
                        }

                        bit += 1;
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

        SymbolicValueIterator {
            not_cares: self.not_cares(),
            next: if self.is_unsatisfiable() {
                None
            } else {
                Some(self.bits_set.clone())
            },
        }
    }
}

impl<T, R> BitAnd<Symbolic<R>> for Symbolic<T>
where
    T: Bounded + BitAnd<R> + BitOr<R>,
    <T as BitAnd<R>>::Output: From<<T as BitOr<R>>::Output>,
{
    type Output = Symbolic<<T as BitAnd<R>>::Output>;

    fn bitand(self, sv: Symbolic<R>) -> Self::Output {
        Symbolic {
            bits_set: self.bits_set & sv.bits_set,
            bits_cleared: <T as BitAnd<R>>::Output::from(self.bits_cleared | sv.bits_cleared),
        }
    }
}

impl<T, R> BitOr<Symbolic<R>> for Symbolic<T>
where
    T: Bounded + BitAnd<R> + BitOr<R>,
    <T as BitOr<R>>::Output: From<<T as BitAnd<R>>::Output>,
{
    type Output = Symbolic<<T as BitOr<R>>::Output>;

    fn bitor(self, sv: Symbolic<R>) -> Self::Output {
        Symbolic {
            bits_set: self.bits_set | sv.bits_set,
            bits_cleared: <T as BitOr<R>>::Output::from(self.bits_cleared & sv.bits_cleared),
        }
    }
}

impl<T, R> BitXor<Symbolic<R>> for Symbolic<T>
where
    T: Clone + BitOr + BitAnd<R> + BitXor<R> + From<<T as BitOr>::Output>,
    R: Clone + BitOr + From<<R as BitOr>::Output>,
    <T as BitXor<R>>::Output: Clone
        + Not
        + BitAnd
        + From<<T as BitAnd<R>>::Output>
        + From<<<T as BitXor<R>>::Output as Not>::Output>
        + From<<<T as BitXor<R>>::Output as BitAnd>::Output>,
{
    type Output = Symbolic<<T as BitXor<R>>::Output>;

    fn bitxor(self, sv: Symbolic<R>) -> Self::Output {
        let self_nocare = T::from(self.bits_set.clone() | self.bits_cleared.clone());
        let rhs_nocare = R::from(sv.bits_set.clone() | sv.bits_cleared.clone());
        let mask = <T as BitXor<R>>::Output::from(self_nocare & rhs_nocare);
        let bitset_xor = self.bits_set ^ sv.bits_set;
        let bitclear_xor = self.bits_cleared ^ sv.bits_cleared;

        Symbolic {
            bits_set: <T as BitXor<R>>::Output::from(bitset_xor & mask.clone()),
            bits_cleared: <T as BitXor<R>>::Output::from(
                <T as BitXor<R>>::Output::from(!bitclear_xor) & mask,
            ),
        }
    }
}

impl<T, R> Shl<R> for Symbolic<T>
where
    T: Shl<R>,
    R: Clone,
    <T as Shl<R>>::Output: Zero
        + BitOr<Output = <T as Shl<R>>::Output>
        + Not<Output = <T as Shl<R>>::Output>
        + Shl<R, Output = <T as Shl<R>>::Output>,
{
    type Output = Symbolic<<T as Shl<R>>::Output>;

    fn shl(self, rhs: R) -> Self::Output {
        let extension = !(!(<T as Shl<R>>::Output::zero()) << rhs.clone());

        Symbolic {
            bits_set: self.bits_set << rhs.clone(),
            bits_cleared: self.bits_cleared << rhs | extension,
        }
    }
}

impl<T> CheckedShl for Symbolic<T>
where
    T: CheckedShl + Zero + Not<Output = T> + BitOr<Output = T>,
{
    fn checked_shl(&self, rhs: u32) -> Option<Self> {
        let extension = !((!T::zero()).checked_shl(rhs)?);

        Some(Symbolic {
            bits_set: self.bits_set.checked_shl(rhs)?,
            bits_cleared: self.bits_cleared.checked_shl(rhs)? | extension,
        })
    }
}

impl<T, R> Shr<R> for Symbolic<T>
where
    T: Shr<R>,
    R: Clone,
    <T as Shr<R>>::Output: Zero
        + BitOr<Output = <T as Shr<R>>::Output>
        + Not<Output = <T as Shr<R>>::Output>
        + Shr<R, Output = <T as Shr<R>>::Output>,
{
    type Output = Symbolic<<T as Shr<R>>::Output>;

    fn shr(self, rhs: R) -> Self::Output {
        let extension = !(!(<T as Shr<R>>::Output::zero()) >> rhs.clone());

        Symbolic {
            bits_set: self.bits_set >> rhs.clone(),
            bits_cleared: self.bits_cleared >> rhs | extension,
        }
    }
}

impl<T> CheckedShr for Symbolic<T>
where
    T: CheckedShr + Zero + Not<Output = T> + BitOr<Output = T>,
{
    fn checked_shr(&self, rhs: u32) -> Option<Self> {
        let extension = !((!T::zero()).checked_shr(rhs)?);

        Some(Symbolic {
            bits_set: self.bits_set.checked_shr(rhs)?,
            bits_cleared: self.bits_cleared.checked_shr(rhs)? | extension,
        })
    }
}

impl<T> Not for Symbolic<T>
where
    T: Not,
    <T as Not>::Output: From<T>,
{
    type Output = Symbolic<<T as Not>::Output>;

    fn not(self) -> Self::Output {
        Symbolic {
            bits_set: <T as Not>::Output::from(self.bits_cleared),
            bits_cleared: <T as Not>::Output::from(self.bits_set),
        }
    }
}

type XOROut<T, R> = <T as BitXor<R>>::Output;

#[allow(dead_code)] //rustc thinks this is unused when it clearly is...
type SymXOROut<T, R> = Symbolic<<T as BitXor<R>>::Output>;

impl<T, R> Add<Symbolic<R>> for Symbolic<T>
where
    T: Clone + Add<R> + BitXor<R> + BitAnd<R>,
    XOROut<T, R>: Clone
        + Zero
        + One
        + BoundWidth<usize>
        + Shl<usize, Output = XOROut<T, R>>
        + Not<Output = XOROut<T, R>>,
    Symbolic<T>: Clone
        + BitXor<Symbolic<R>, Output = SymXOROut<T, R>>
        + BitAnd<Symbolic<R>, Output = SymXOROut<T, R>>,
    Symbolic<R>: Clone,
    SymXOROut<T, R>: Clone
        + BoundWidth<usize>
        + Shl<usize, Output = SymXOROut<T, R>>
        + BitAnd<Output = SymXOROut<T, R>>
        + BitXor<Output = SymXOROut<T, R>>
        + BitOr<Output = SymXOROut<T, R>>
        + Not<Output = SymXOROut<T, R>>,
    Symbolic<<T as Add<R>>::Output>: Convertable<XOROut<T, R>>,
{
    type Output = Symbolic<<T as Add<R>>::Output>;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn add(self, rhs: Symbolic<R>) -> Self::Output {
        //Implementation notes:
        //
        // 1. I hope the trait bounds above did not literally kill you.
        // 2. We can't call Symbolic::zero() here, because then we'd have to
        //    require Zero on symbolic values of T^R, which requires Add on
        //    Symbolic values of T^R, which requires Zero on symbolic values of
        //    (T^R)^(T^R), which gives a 128-line long error from rustc.
        // 3. We're implementing addition literally the way you would do it in
        //    hardware - or at least, logical descriptions of hardware. This is
        //    not performant at all.
        // 4. We never call `add` so we technically don't need that trait here.
        //    I want the output types to match the concrete types, however, so
        //    we need this trait that we don't use.
        // 3. Clippy really hates that we're using binary operations in `Add`.

        let bits: usize = XOROut::<T, R>::bound_width();
        let half_adds = self.clone() ^ rhs.clone();
        let half_carries = self & rhs;
        let zero: XOROut<T, R> = XOROut::<T, R>::zero();
        let mut carry = Symbolic::from(zero);
        let mut sum = carry.clone();

        for bit in 0..bits {
            let mask = Symbolic::from(XOROut::<T, R>::one() << bit);
            sum = sum | half_adds.clone() & mask.clone() ^ carry.clone() & mask.clone();
            carry = (carry & half_adds.clone() & mask.clone() | half_carries.clone() & mask) << 1;
        }

        Symbolic::convert_from(sum)
    }
}

impl<T, R> Sub<Symbolic<R>> for Symbolic<T>
where
    T: Clone + Sub<R> + BitXor<R> + BitAnd<R>,
    XOROut<T, R>: Clone
        + Zero
        + One
        + BoundWidth<usize>
        + Shl<usize, Output = XOROut<T, R>>
        + Not<Output = XOROut<T, R>>,
    Symbolic<T>: Clone
        + BitXor<Symbolic<R>, Output = SymXOROut<T, R>>
        + BitAnd<Symbolic<R>, Output = SymXOROut<T, R>>,
    Symbolic<R>: Clone + Not<Output = Symbolic<R>>,
    SymXOROut<T, R>: Clone
        + BoundWidth<usize>
        + Shl<usize, Output = SymXOROut<T, R>>
        + BitAnd<Output = SymXOROut<T, R>>
        + BitXor<Output = SymXOROut<T, R>>
        + BitOr<Output = SymXOROut<T, R>>
        + Not<Output = SymXOROut<T, R>>,
    Symbolic<<T as Sub<R>>::Output>: Convertable<XOROut<T, R>>,
{
    type Output = Symbolic<<T as Sub<R>>::Output>;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn sub(self, rhs: Symbolic<R>) -> Self::Output {
        //Further implementation notes:
        //
        // 1. We're implementing subtraction also the same way hardware does it:
        //    flip the bits and start with a carry. This works for signed and
        //    unsigned types equally because two's compliement is great.
        // 2. If you implement an exotic signed type which emulates one's
        //    compliment or sign-and-magnitude or whatever, that becomes a
        //    standard numerical type symbolically.
        // 3. Clippy really hates that we're using binary operations in `Sub`.

        let bits: usize = XOROut::<T, R>::bound_width();
        let half_adds = self.clone() ^ !rhs.clone();
        let half_carries = self & !rhs;
        let zero: XOROut<T, R> = XOROut::<T, R>::zero();
        let mut sum = Symbolic::from(zero);
        let one: XOROut<T, R> = XOROut::<T, R>::one();
        let mut carry = Symbolic::from(one);

        for bit in 0..bits {
            let mask = Symbolic::from(XOROut::<T, R>::one() << bit);
            sum = sum | half_adds.clone() & mask.clone() ^ carry.clone() & mask.clone();
            carry = (carry & half_adds.clone() & mask.clone() | half_carries.clone() & mask) << 1;
        }

        Symbolic::convert_from(sum)
    }
}

impl<T> Bounded for Symbolic<T>
where
    T: Bounded,
    Symbolic<T>: From<T>,
{
    fn min_value() -> Self {
        Self::from(T::min_value())
    }

    fn max_value() -> Self {
        Self::from(T::max_value())
    }
}

impl<T, U> Desegmentable<Symbolic<U>> for Symbolic<T>
where
    T: Desegmentable<U>,
    U: Clone + BoundWidth<u32> + Bitwise,
    Symbolic<T>: Zero + Convertable<U> + Bitwise,
{
    fn units_reqd() -> usize {
        let self_units = <Self as BoundWidth<u32>>::bound_width();
        let from_units = <U as BoundWidth<u32>>::bound_width();
        (self_units as f32 / from_units as f32).round() as usize
    }

    fn from_segments(data: &[Symbolic<U>], endianness: Endianness) -> Option<Self> {
        let units_reqd = <Self as Desegmentable<Symbolic<U>>>::units_reqd() as u32;
        let mut sum = Self::zero();
        let i_iter: Vec<u32> = match endianness {
            Endianness::BigEndian => (0..units_reqd).rev().collect(),
            Endianness::LittleEndian => (0..units_reqd).collect(),
        };

        for i in i_iter {
            let unit = Symbolic::convert_from(data.get(i as usize)?.clone());
            sum = sum | unit << (i * <U as BoundWidth<u32>>::bound_width());
        }

        Some(sum)
    }
}

impl<U> UpperHex for Symbolic<U>
where
    U: Bitwise + From<u8> + fmt::Debug,
    u8: TryFrom<U>,
    Symbolic<U>: Bitwise,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut val = self.clone();
        let mut hexes = vec![];

        for _i in 0..Self::bound_width() / 4 {
            hexes.push(
                (val.clone() & Symbolic::from(U::from(0xF)))
                    .into_concrete()
                    .and_then(|v| u8::try_from(v).ok()),
            );
            val = val.checked_shr(4).unwrap();
        }

        for hex in hexes.iter().rev() {
            match hex {
                Some(0) => f.write_str("0")?,
                Some(1) => f.write_str("1")?,
                Some(2) => f.write_str("2")?,
                Some(3) => f.write_str("3")?,
                Some(4) => f.write_str("4")?,
                Some(5) => f.write_str("5")?,
                Some(6) => f.write_str("6")?,
                Some(7) => f.write_str("7")?,
                Some(8) => f.write_str("8")?,
                Some(9) => f.write_str("9")?,
                Some(10) => f.write_str("A")?,
                Some(11) => f.write_str("B")?,
                Some(12) => f.write_str("C")?,
                Some(13) => f.write_str("D")?,
                Some(14) => f.write_str("E")?,
                Some(15) => f.write_str("F")?,
                _ => f.write_str("?")?,
            }
        }

        Ok(())
    }
}

impl<U> LowerHex for Symbolic<U>
where
    U: Bitwise + From<u8>,
    u8: TryFrom<U>,
    Symbolic<U>: Bitwise,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut val = self.clone();
        let mut hexes = vec![];

        for _i in 0..Self::bound_width() / 4 {
            hexes.push(
                (val.clone() & Symbolic::from(U::from(0xF)))
                    .into_concrete()
                    .and_then(|v| u8::try_from(v).ok()),
            );
            val = val.checked_shr(4).unwrap();
        }

        for hex in hexes.iter().rev() {
            match hex {
                Some(0) => f.write_str("0")?,
                Some(1) => f.write_str("1")?,
                Some(2) => f.write_str("2")?,
                Some(3) => f.write_str("3")?,
                Some(4) => f.write_str("4")?,
                Some(5) => f.write_str("5")?,
                Some(6) => f.write_str("6")?,
                Some(7) => f.write_str("7")?,
                Some(8) => f.write_str("8")?,
                Some(9) => f.write_str("9")?,
                Some(10) => f.write_str("a")?,
                Some(11) => f.write_str("b")?,
                Some(12) => f.write_str("c")?,
                Some(13) => f.write_str("d")?,
                Some(14) => f.write_str("e")?,
                Some(15) => f.write_str("f")?,
                _ => f.write_str(".")?,
            }
        }

        Ok(())
    }
}
