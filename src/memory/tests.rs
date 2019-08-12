use std::cmp::Ordering;
use crate::memory::Pointer;
use crate::reg::{New, Symbolic};

#[test]
fn pointer_contextless() {
    let ptr1 : Pointer<u32> = Pointer::from(0x150);
    let ptr2 = Pointer::from(0x250);

    assert_eq!(ptr1.partial_cmp(&ptr2), Some(Ordering::Less));
    assert_eq!(ptr2.partial_cmp(&ptr1), Some(Ordering::Greater));

    assert_eq!(ptr1.cmp(&ptr2), Ordering::Less);
    assert_eq!(ptr2.cmp(&ptr1), Ordering::Greater);

    assert_eq!(ptr1.eq(&ptr2), false);
}

#[test]
fn pointer_same_context() {
    let mut ptr1 : Pointer<u32> = Pointer::from(0x150);
    let mut ptr2 = Pointer::from(0x250);

    ptr1.set_arch_context("R", Symbolic::new(1));
    ptr2.set_arch_context("R", Symbolic::new(1));

    assert_eq!(ptr1.partial_cmp(&ptr2), Some(Ordering::Less));
    assert_eq!(ptr2.partial_cmp(&ptr1), Some(Ordering::Greater));

    assert_eq!(ptr1.cmp(&ptr2), Ordering::Less);
    assert_eq!(ptr2.cmp(&ptr1), Ordering::Greater);

    assert_eq!(ptr1.eq(&ptr2), false);
}

#[test]
fn pointer_different_context() {
    let mut ptr1 : Pointer<u32> = Pointer::from(0x150);
    let mut ptr2 = Pointer::from(0x150);

    ptr1.set_arch_context("R", Symbolic::new(2));
    ptr2.set_arch_context("R", Symbolic::new(1));

    assert_eq!(ptr1.partial_cmp(&ptr2), Some(Ordering::Greater));
    assert_eq!(ptr2.partial_cmp(&ptr1), Some(Ordering::Less));

    assert_eq!(ptr1.cmp(&ptr2), Ordering::Greater);
    assert_eq!(ptr2.cmp(&ptr1), Ordering::Less);

    assert_eq!(ptr1.eq(&ptr2), false);
}

#[test]
fn pointer_unspecified_context() {
    let ptr1 : Pointer<u32> = Pointer::from(0x150);
    let mut ptr2 = Pointer::from(0x150);

    ptr2.set_arch_context("R", Symbolic::new(1));

    assert_eq!(ptr1.partial_cmp(&ptr2), Some(Ordering::Less));
    assert_eq!(ptr2.partial_cmp(&ptr1), Some(Ordering::Greater));

    assert_eq!(ptr1.cmp(&ptr2), Ordering::Less);
    assert_eq!(ptr2.cmp(&ptr1), Ordering::Greater);

    assert_eq!(ptr1.eq(&ptr2), false);
}

#[test]
fn pointer_symbolic_context() {
    let mut ptr1 : Pointer<u32> = Pointer::from(0x150);
    let mut ptr2 = Pointer::from(0x150);

    ptr1.set_arch_context("R", Symbolic::default());
    ptr2.set_arch_context("R", Symbolic::new(1));

    assert_eq!(ptr1.partial_cmp(&ptr2), Some(Ordering::Less));
    assert_eq!(ptr2.partial_cmp(&ptr1), Some(Ordering::Greater));

    assert_eq!(ptr1.cmp(&ptr2), Ordering::Less);
    assert_eq!(ptr2.cmp(&ptr1), Ordering::Greater);

    assert_eq!(ptr1.eq(&ptr2), false);
}

#[test]
fn pointer_multiple_context() {
    let mut ptr1 : Pointer<u32> = Pointer::from(0x150);
    let mut ptr2 = Pointer::from(0x150);

    ptr1.set_arch_context("R", Symbolic::new(1));
    ptr1.set_arch_context("A", Symbolic::new(2));

    ptr2.set_arch_context("R", Symbolic::new(2));
    ptr2.set_arch_context("A", Symbolic::new(1));

    assert_eq!(ptr1.partial_cmp(&ptr2), Some(Ordering::Greater));
    assert_eq!(ptr2.partial_cmp(&ptr1), Some(Ordering::Less));

    assert_eq!(ptr1.cmp(&ptr2), Ordering::Greater);
    assert_eq!(ptr2.cmp(&ptr1), Ordering::Less);

    assert_eq!(ptr1.eq(&ptr2), false);
}

#[test]
fn pointer_multiple_eq_context() {
    let mut ptr1 : Pointer<u32> = Pointer::from(0x150);
    let mut ptr2 = Pointer::from(0x150);

    ptr1.set_arch_context("R", Symbolic::new(1));
    ptr1.set_arch_context("A", Symbolic::new(1));

    ptr2.set_arch_context("R", Symbolic::new(1));
    ptr2.set_arch_context("A", Symbolic::new(1));

    assert_eq!(ptr1.partial_cmp(&ptr2), Some(Ordering::Equal));
    assert_eq!(ptr2.partial_cmp(&ptr1), Some(Ordering::Equal));

    assert_eq!(ptr1.cmp(&ptr2), Ordering::Equal);
    assert_eq!(ptr2.cmp(&ptr1), Ordering::Equal);

    assert_eq!(ptr1.eq(&ptr2), true);
}
