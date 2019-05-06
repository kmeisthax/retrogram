use crate::retrogram::reg;

#[test]
fn test_symbolic_default() {
    let symbolic_default : reg::Symbolic<u8> = reg::Symbolic::default();

    assert_eq!(None, symbolic_default.into_concrete());
}

#[test]
fn test_concrete_roundtrip() {
    let start_value = 142;
    let sym_value = reg::Symbolic::from(start_value);
    let concrete_value = sym_value.into_concrete();

    assert_eq!(Some(start_value), concrete_value);
}

#[test]
fn test_concrete_validation() {
    let start_value = 83;
    let sym_value = reg::Symbolic::from(start_value);

    assert!(sym_value.is_valid(start_value));
    assert!(!sym_value.is_valid(0));
}

#[test]
fn test_symbolic_validation() {
    let mut sym_value = reg::Symbolic::default();

    sym_value.increase_lower_bound(20);
    sym_value.decrease_upper_bound(40);

    assert!(sym_value.is_valid(25));
    assert!(sym_value.is_valid(20));
    assert!(sym_value.is_valid(40));
    assert!(!sym_value.is_valid(60));
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
fn test_symbolic_shift() {
    let value_one = 0x3C;
    let sym_value_one = reg::Symbolic::from(value_one);

    let value_shift = value_one << 2;
    let sym_value_shift = sym_value_one << 2;

    assert_eq!(Some(value_shift), sym_value_shift.into_concrete());
}