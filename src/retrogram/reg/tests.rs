use crate::retrogram::reg;

#[test]
fn test_symbolic_default() {
    let symbolic_default : reg::Symbolic<u8> = reg::Symbolic::default();

    assert!(symbolic_default.is_unconstrained());
    assert!(!symbolic_default.is_unsatisfiable());
    assert!(!symbolic_default.is_concrete());
    assert_eq!(None, symbolic_default.into_concrete());
}

#[test]
fn test_concrete_roundtrip() {
    let start_value = 142;
    let sym_value = reg::Symbolic::from(start_value);

    assert!(sym_value.is_concrete());
    assert!(!sym_value.is_unconstrained());
    assert!(!sym_value.is_unsatisfiable());

    let concrete_value = sym_value.into_concrete();

    assert_eq!(Some(start_value), concrete_value);
}

#[test]
fn test_concrete_cares() {
    let start_value : u8 = 243;
    let sym_value = reg::Symbolic::from(start_value);

    assert_eq!(0xFF, sym_value.cares());
    assert_eq!(0x00, sym_value.not_cares());
}

#[test]
fn test_concrete_validation() {
    let start_value = 83;
    let sym_value = reg::Symbolic::from(start_value);

    assert!(sym_value.is_valid(start_value));
    assert!(!sym_value.is_valid(0));
}

#[test]
fn test_unsatisfiable() {
    let sym_value = reg::Symbolic::from_bits(0xF0, 0x1F);

    assert!(sym_value.is_unsatisfiable());
    assert!(!sym_value.is_concrete());
    assert!(!sym_value.is_unconstrained());
}

#[test]
fn test_symbolic_bitor() {
    let value_one = 0x3C;
    let value_two = 0xF0;

    let sym_value_one = reg::Symbolic::from(value_one);
    let sym_value_two = reg::Symbolic::from(value_two);

    let value_or = value_one | value_two;
    let sym_value_or = sym_value_one | sym_value_two;

    assert_eq!(Some(value_or), sym_value_or.into_concrete());
}

#[test]
fn test_symbolic_bitand() {
    let value_one = 0x3C;
    let value_two = 0xF0;

    let sym_value_one = reg::Symbolic::from(value_one);
    let sym_value_two = reg::Symbolic::from(value_two);

    let value_or = value_one & value_two;
    let sym_value_or = sym_value_one & sym_value_two;

    assert_eq!(Some(value_or), sym_value_or.into_concrete());
}

#[test]
fn test_symbolic_shl() {
    let value_one = 0x3C;
    let sym_value_one = reg::Symbolic::from(value_one);

    let value_shift = value_one << 2;
    let sym_value_shift = sym_value_one << 2;

    assert_eq!(Some(value_shift), sym_value_shift.into_concrete());
}

#[test]
fn test_symbolic_shr() {
    let value_one = 0xF0;
    let sym_value_one = reg::Symbolic::from(value_one);

    let value_shift = value_one >> 2;
    let sym_value_shift = sym_value_one >> 2;

    assert_eq!(Some(value_shift), sym_value_shift.into_concrete());
}