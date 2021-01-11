use crate::reg;
use num_traits::{CheckedShl, CheckedShr};

#[test]
fn test_symbolic_default() {
    let symbolic_default: reg::Symbolic<u8> = reg::Symbolic::default();

    assert!(symbolic_default.is_unconstrained());
    assert!(!symbolic_default.is_unsatisfiable());
    assert!(!symbolic_default.is_concrete());
    assert_eq!(None, symbolic_default.into_concrete());
}

#[test]
fn test_concrete_roundtrip() {
    let start_value: u8 = 142;
    let sym_value = reg::Symbolic::from(start_value);

    assert!(sym_value.is_concrete());
    assert!(!sym_value.is_unconstrained());
    assert!(!sym_value.is_unsatisfiable());

    let concrete_value = sym_value.into_concrete();

    assert_eq!(Some(start_value), concrete_value);
}

#[test]
fn test_concrete_cares() {
    let start_value: u8 = 243;
    let sym_value = reg::Symbolic::from(start_value);

    assert_eq!(0xFF, sym_value.cares());
    assert_eq!(0x00, sym_value.not_cares());
}

#[test]
fn test_concrete_validation() {
    let start_value: u8 = 83;
    let sym_value = reg::Symbolic::from(start_value);

    assert!(sym_value.is_valid(start_value));
    assert!(!sym_value.is_valid(0));
}

#[test]
fn test_concrete_bits_concrete() {
    let start_value: u8 = 83;
    let sym_value = reg::Symbolic::from(start_value);

    assert!(sym_value.bits_are_concrete(0xF0));
    assert!(sym_value.bits_are_concrete(0x0F));
    assert!(sym_value.bits_are_concrete(0x00));
    assert!(sym_value.bits_are_concrete(0xFF));
}

#[test]
fn test_unsatisfiable() {
    let sym_value: reg::Symbolic<u8> = reg::Symbolic::from_bits(0xF0, 0x1F);

    assert!(sym_value.is_unsatisfiable());
    assert!(!sym_value.is_concrete());
    assert!(!sym_value.is_unconstrained());
}

#[test]
fn test_unsatisfiable_bits_concrete() {
    let sym_value: reg::Symbolic<u8> = reg::Symbolic::from_bits(0xF0, 0x1F);

    assert!(!sym_value.bits_are_concrete(0xF0));
    assert!(!sym_value.bits_are_concrete(0x0F));
    assert!(!sym_value.bits_are_concrete(0x00));
    assert!(!sym_value.bits_are_concrete(0xFF));
}

#[test]
fn test_concrete_bitor() {
    let value_one: u8 = 0x3C;
    let value_two: u8 = 0xF0;

    let sym_value_one = reg::Symbolic::from(value_one);
    let sym_value_two = reg::Symbolic::from(value_two);

    let value_or = value_one | value_two;
    let sym_value_or = sym_value_one | sym_value_two;

    assert_eq!(Some(value_or), sym_value_or.into_concrete());
}

#[test]
fn test_concrete_bitand() {
    let value_one: u8 = 0x3C;
    let value_two: u8 = 0xF0;

    let sym_value_one = reg::Symbolic::from(value_one);
    let sym_value_two = reg::Symbolic::from(value_two);

    let value_or = value_one & value_two;
    let sym_value_or = sym_value_one & sym_value_two;

    assert_eq!(Some(value_or), sym_value_or.into_concrete());
}

#[test]
fn test_concrete_bitxor() {
    let value_one: u8 = 0x3C;
    let value_two: u8 = 0xF0;

    let sym_value_one = reg::Symbolic::from(value_one);
    let sym_value_two = reg::Symbolic::from(value_two);

    let value_xor = value_one ^ value_two;
    let sym_value_xor = sym_value_one ^ sym_value_two;

    assert_eq!(Some(value_xor), sym_value_xor.into_concrete());
}

#[test]
fn test_concrete_not() {
    let value_one: u8 = 0x42;
    let sym_value_one = reg::Symbolic::from(value_one);

    let value_not = !value_one;
    let sym_value_not = !sym_value_one;

    assert_eq!(Some(value_not), sym_value_not.into_concrete());
}

#[test]
fn test_concrete_add() {
    let value_one: u8 = 0x76;
    let value_two: u8 = 0x15;

    let sym_value_one = reg::Symbolic::from(value_one);
    let sym_value_two = reg::Symbolic::from(value_two);

    let value_sum = value_one + value_two;
    let sym_value_sum = sym_value_one + sym_value_two;

    assert_eq!(Some(value_sum), sym_value_sum.into_concrete());
}

#[test]
fn test_concrete_sub() {
    let value_one: u8 = 0x76;
    let value_two: u8 = 0x15;

    let sym_value_one = reg::Symbolic::from(value_one);
    let sym_value_two = reg::Symbolic::from(value_two);

    let value_sum = value_one - value_two;
    let sym_value_sum = sym_value_one - sym_value_two;

    assert_eq!(Some(value_sum), sym_value_sum.into_concrete());
}

#[test]
fn test_concrete_signed_add() {
    let value_one: i8 = 32;
    let value_two: i8 = -6;

    let sym_value_one = reg::Symbolic::from(value_one);
    let sym_value_two = reg::Symbolic::from(value_two);

    let value_sum = value_one + value_two;
    let sym_value_sum = sym_value_one + sym_value_two;

    assert_eq!(Some(value_sum), sym_value_sum.into_concrete());
}

#[test]
fn test_symbolic_bits_concrete() {
    let sym_value_one: reg::Symbolic<u16> = reg::Symbolic::from_bits(0x0049, 0x0124);
    let sym_value_two: reg::Symbolic<u16> = reg::Symbolic::from_bits(0x0007, 0x01C0);

    assert!(sym_value_one.bits_are_concrete(0x016D));
    assert!(!sym_value_one.bits_are_concrete(0xFE92));
    assert!(sym_value_two.bits_are_concrete(0x01C7));
    assert!(!sym_value_two.bits_are_concrete(0xFE38));
}

#[test]
fn test_symbolic_bitor() {
    let sym_value_one: reg::Symbolic<u16> = reg::Symbolic::from_bits(0x0049, 0x0124);
    let sym_value_two: reg::Symbolic<u16> = reg::Symbolic::from_bits(0x0007, 0x01C0);
    let sym_value_or = sym_value_one | sym_value_two;

    assert_eq!((0x004F, 0x0100), sym_value_or.into_bits());
}

#[test]
fn test_symbolic_bitand() {
    let sym_value_one: reg::Symbolic<u16> = reg::Symbolic::from_bits(0x0049, 0x0124);
    let sym_value_two: reg::Symbolic<u16> = reg::Symbolic::from_bits(0x0007, 0x01C0);
    let sym_value_and = sym_value_one & sym_value_two;

    assert_eq!((0x0001, 0x01E4), sym_value_and.into_bits());
}

#[test]
fn test_symbolic_bitxor() {
    let sym_value_one: reg::Symbolic<u16> = reg::Symbolic::from_bits(0x0049, 0x0124);
    let sym_value_two: reg::Symbolic<u16> = reg::Symbolic::from_bits(0x0007, 0x01C0);
    let sym_value_and = sym_value_one ^ sym_value_two;

    assert_eq!((0x0044, 0x0101), sym_value_and.into_bits());
}

#[test]
fn test_symbolic_not() {
    let sym_value_one: reg::Symbolic<u8> = reg::Symbolic::from_bits(0x04, 0x01);
    let sym_value_not = !sym_value_one;

    assert_eq!((0x01, 0x04), sym_value_not.into_bits());
}

#[test]
fn test_concrete_shl() {
    let value_one: u8 = 0x3C;
    let sym_value_one = reg::Symbolic::from(value_one);

    let value_shift = value_one << 2;
    let sym_value_shift = sym_value_one << 2;

    assert_eq!(Some(value_shift), sym_value_shift.into_concrete());
}

#[test]
fn test_concrete_shl_arithmetic() {
    let value_one: i8 = -16;
    let sym_value_one = reg::Symbolic::from(value_one);

    let value_shift = value_one << 2;
    let sym_value_shift = sym_value_one << 2;

    assert_eq!(Some(value_shift), sym_value_shift.into_concrete());
}
#[test]
fn test_concrete_checked_shl() {
    let value_one: u8 = 0xF0;
    let sym_value_one = reg::Symbolic::from(value_one);

    let value_shift = value_one
        .checked_shl(2)
        .expect("Non-overflowing concrete value");
    let sym_value_shift = sym_value_one
        .checked_shl(2)
        .expect("Non-overflowing symbolic value");

    assert_eq!(Some(value_shift), sym_value_shift.into_concrete());
}

#[test]
fn test_concrete_checked_shl_arithmetic() {
    let value_one: i8 = -16;
    let sym_value_one = reg::Symbolic::from(value_one);

    let value_shift = value_one
        .checked_shl(2)
        .expect("Non-overflowing concrete value");
    let sym_value_shift = sym_value_one
        .checked_shl(2)
        .expect("Non-overflowing symbolic value");

    assert_eq!(Some(value_shift), sym_value_shift.into_concrete());
}

#[test]
fn test_concrete_shr() {
    let value_one: u8 = 0xF0;
    let sym_value_one = reg::Symbolic::from(value_one);

    let value_shift = value_one >> 2;
    let sym_value_shift = sym_value_one >> 2;

    assert_eq!(Some(value_shift), sym_value_shift.into_concrete());
}

#[test]
fn test_concrete_shr_arithmetic() {
    let value_one: i8 = -16;
    let sym_value_one = reg::Symbolic::from(value_one);

    let value_shift = value_one >> 2;
    let sym_value_shift = sym_value_one >> 2;

    assert_eq!(Some(value_shift), sym_value_shift.into_concrete());
}

#[test]
fn test_concrete_checked_shr() {
    let value_one: u8 = 0xF0;
    let sym_value_one = reg::Symbolic::from(value_one);

    let value_shift = value_one
        .checked_shr(2)
        .expect("Non-overflowing concrete value");
    let sym_value_shift = sym_value_one
        .checked_shr(2)
        .expect("Non-overflowing symbolic value");

    assert_eq!(Some(value_shift), sym_value_shift.into_concrete());
}

#[test]
fn test_concrete_checked_shr_arithmetic() {
    let value_one: i8 = -16;
    let sym_value_one = reg::Symbolic::from(value_one);

    let value_shift = value_one
        .checked_shr(2)
        .expect("Non-overflowing concrete value");
    let sym_value_shift = sym_value_one
        .checked_shr(2)
        .expect("Non-overflowing symbolic value");

    assert_eq!(Some(value_shift), sym_value_shift.into_concrete());
}

#[test]
fn test_symbolic_iter() {
    let sym_value: reg::Symbolic<u8> = reg::Symbolic::from_bits(0xF0, 0x05);
    let mut sym_iter = sym_value.valid();

    assert_eq!(sym_value.not_cares(), 0xA);

    assert_eq!(sym_iter.next(), Some(0xF0));
    assert_eq!(sym_iter.next(), Some(0xF2));
    assert_eq!(sym_iter.next(), Some(0xF8));
    assert_eq!(sym_iter.next(), Some(0xFA));
    assert_eq!(sym_iter.next(), None);
}

#[test]
fn test_symbolic_iter_unsatisfiable() {
    let sym_value: reg::Symbolic<u8> = reg::Symbolic::from_bits(0x6F, 0x1F);
    let mut sym_iter = sym_value.valid();

    assert_eq!(sym_iter.next(), None);
}

#[test]
fn test_symbolic_iter_concrete() {
    let sym_value = reg::Symbolic::from(0x3F_u8);
    let mut sym_iter = sym_value.valid();

    assert_eq!(sym_iter.next(), Some(0x3F));
    assert_eq!(sym_iter.next(), None);
}

#[test]
fn test_symbolic_bounds() {
    let sym_value: reg::Symbolic<u8> = reg::Symbolic::from_bits(0x7A, 0x80);

    assert_eq!(sym_value.lower_bound(), Some(0x7A));
    assert_eq!(sym_value.upper_bound(), Some(0x7F));
    assert!(sym_value.is_valid(sym_value.lower_bound().expect("Must be upper bounded")));
    assert!(sym_value.is_valid(sym_value.upper_bound().expect("Must be lower bounded")));
}

#[test]
fn test_concrete_conv() {
    use crate::reg::Convertable;

    let value: u8 = 0xC0;
    let sym_value = reg::Symbolic::from(value);

    let wide_value = u16::from(value);
    let wide_sym_value = reg::Symbolic::<u16>::convert_from(sym_value);

    assert_eq!(Some(wide_value), wide_sym_value.into_concrete());
}

#[test]
fn test_concrete_tryconv() {
    use crate::reg::TryConvertable;
    use std::convert::TryFrom;

    let value: u16 = 0xC0;
    let sym_value = reg::Symbolic::from(value);

    let narrow_value = u8::try_from(value).expect("Narrow conversion failed");
    let narrow_sym_value = reg::Symbolic::<u8>::try_convert_from(sym_value)
        .expect("Narrow symbolic conversion failed");

    assert_eq!(Some(narrow_value), narrow_sym_value.into_concrete());
}

#[test]
fn test_concrete_lowerhex() {
    let sym_value: reg::Symbolic<u8> = reg::Symbolic::from(0xC5);
    let fmtd = format!("{:x}", sym_value);

    assert_eq!("c5", fmtd);
}

#[test]
fn test_symbolic_lowerhex() {
    let sym_value: reg::Symbolic<u8> = reg::Symbolic::from_bits(0x7A, 0x80);
    let fmtd = format!("{:x}", sym_value);

    assert_eq!("7.", fmtd);
}

#[test]
fn test_concrete_upperhex() {
    let sym_value: reg::Symbolic<u8> = reg::Symbolic::from(0xC5);
    let fmtd = format!("{:X}", sym_value);

    assert_eq!("C5", fmtd);
}

#[test]
fn test_symbolic_upperhex() {
    let sym_value: reg::Symbolic<u8> = reg::Symbolic::from_bits(0x7A, 0x80);
    let fmtd = format!("{:X}", sym_value);

    assert_eq!("7?", fmtd);
}
