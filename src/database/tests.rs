use crate::analysis::{Block, ReferenceKind};
use crate::arch::tests::TestArchitecture;
use crate::ast::Label;
use crate::database::db::Symbol;
use crate::database::Database;
use crate::memory::Pointer;

#[test]
fn block_splitting() {
    let mut db = Database::<TestArchitecture>::new();
    let start_ptr = Pointer::from(0x150);

    db.insert_block(Block::from_parts(start_ptr.clone(), 0x10));

    let middle_ptr = Pointer::from(0x155);
    let target_block = db.find_block_membership(&middle_ptr);

    assert_eq!(target_block, Some(0));

    let offset = middle_ptr.as_pointer() - start_ptr.as_pointer();
    let result = db.split_block(target_block.unwrap(), offset);

    assert_eq!(result, Some(1));

    let old_block = db.block(target_block.unwrap()).unwrap();
    let new_block = db.block(result.unwrap()).unwrap();

    assert_eq!(*old_block.as_start().as_pointer(), 0x150);
    assert_eq!(*old_block.as_length(), 0x5);
    assert_eq!(*new_block.as_start().as_pointer(), 0x155);
    assert_eq!(*new_block.as_length(), 0xB);
}

#[test]
fn symbol_replaceable_by() {
    let symbol_p_unk = Symbol::new_placeholder(
        Label::new("", None),
        ReferenceKind::Unknown,
        Pointer::from(0),
    );
    let symbol_p_dat =
        Symbol::new_placeholder(Label::new("", None), ReferenceKind::Data, Pointer::from(0));
    let symbol_p_cod =
        Symbol::new_placeholder(Label::new("", None), ReferenceKind::Code, Pointer::from(0));
    let symbol_p_sub = Symbol::new_placeholder(
        Label::new("", None),
        ReferenceKind::Subroutine,
        Pointer::from(0),
    );
    let symbol_p_ent = Symbol::new_placeholder(
        Label::new("", None),
        ReferenceKind::Entrypoint,
        Pointer::from(0),
    );
    let symbol_def = Symbol::new(Label::new("", None), Pointer::from(0));
    let symbol_p_unk_other = Symbol::new_placeholder(
        Label::new("", None),
        ReferenceKind::Unknown,
        Pointer::from(1),
    );
    let symbol_p_dat_other =
        Symbol::new_placeholder(Label::new("", None), ReferenceKind::Data, Pointer::from(1));
    let symbol_p_cod_other =
        Symbol::new_placeholder(Label::new("", None), ReferenceKind::Code, Pointer::from(1));
    let symbol_p_sub_other = Symbol::new_placeholder(
        Label::new("", None),
        ReferenceKind::Subroutine,
        Pointer::from(1),
    );
    let symbol_p_ent_other = Symbol::new_placeholder(
        Label::new("", None),
        ReferenceKind::Entrypoint,
        Pointer::from(1),
    );
    let symbol_def_other = Symbol::new(Label::new("", None), Pointer::from(1));

    assert!(symbol_p_unk.is_replaceable_by(&symbol_p_unk));
    assert!(symbol_p_unk.is_replaceable_by(&symbol_p_dat));
    assert!(symbol_p_unk.is_replaceable_by(&symbol_p_cod));
    assert!(symbol_p_unk.is_replaceable_by(&symbol_p_sub));
    assert!(symbol_p_unk.is_replaceable_by(&symbol_p_ent));
    assert!(symbol_p_unk.is_replaceable_by(&symbol_def));
    assert!(!symbol_p_unk.is_replaceable_by(&symbol_p_unk_other));
    assert!(!symbol_p_unk.is_replaceable_by(&symbol_p_dat_other));
    assert!(!symbol_p_unk.is_replaceable_by(&symbol_p_cod_other));
    assert!(!symbol_p_unk.is_replaceable_by(&symbol_p_sub_other));
    assert!(!symbol_p_unk.is_replaceable_by(&symbol_p_ent_other));
    assert!(!symbol_p_unk.is_replaceable_by(&symbol_def_other));

    assert!(!symbol_p_dat.is_replaceable_by(&symbol_p_unk));
    assert!(symbol_p_dat.is_replaceable_by(&symbol_p_dat));
    assert!(symbol_p_dat.is_replaceable_by(&symbol_p_cod));
    assert!(symbol_p_dat.is_replaceable_by(&symbol_p_sub));
    assert!(symbol_p_dat.is_replaceable_by(&symbol_p_ent));
    assert!(symbol_p_dat.is_replaceable_by(&symbol_def));
    assert!(!symbol_p_dat.is_replaceable_by(&symbol_p_unk_other));
    assert!(!symbol_p_dat.is_replaceable_by(&symbol_p_dat_other));
    assert!(!symbol_p_dat.is_replaceable_by(&symbol_p_cod_other));
    assert!(!symbol_p_dat.is_replaceable_by(&symbol_p_sub_other));
    assert!(!symbol_p_dat.is_replaceable_by(&symbol_p_ent_other));
    assert!(!symbol_p_dat.is_replaceable_by(&symbol_def_other));

    assert!(!symbol_p_cod.is_replaceable_by(&symbol_p_unk));
    assert!(!symbol_p_cod.is_replaceable_by(&symbol_p_dat));
    assert!(symbol_p_cod.is_replaceable_by(&symbol_p_cod));
    assert!(symbol_p_cod.is_replaceable_by(&symbol_p_sub));
    assert!(symbol_p_cod.is_replaceable_by(&symbol_p_ent));
    assert!(symbol_p_cod.is_replaceable_by(&symbol_def));
    assert!(!symbol_p_cod.is_replaceable_by(&symbol_p_unk_other));
    assert!(!symbol_p_cod.is_replaceable_by(&symbol_p_dat_other));
    assert!(!symbol_p_cod.is_replaceable_by(&symbol_p_cod_other));
    assert!(!symbol_p_cod.is_replaceable_by(&symbol_p_sub_other));
    assert!(!symbol_p_cod.is_replaceable_by(&symbol_p_ent_other));
    assert!(!symbol_p_cod.is_replaceable_by(&symbol_def_other));

    assert!(!symbol_p_sub.is_replaceable_by(&symbol_p_unk));
    assert!(!symbol_p_sub.is_replaceable_by(&symbol_p_dat));
    assert!(!symbol_p_sub.is_replaceable_by(&symbol_p_cod));
    assert!(symbol_p_sub.is_replaceable_by(&symbol_p_sub));
    assert!(symbol_p_sub.is_replaceable_by(&symbol_p_ent));
    assert!(symbol_p_sub.is_replaceable_by(&symbol_def));
    assert!(!symbol_p_sub.is_replaceable_by(&symbol_p_unk_other));
    assert!(!symbol_p_sub.is_replaceable_by(&symbol_p_dat_other));
    assert!(!symbol_p_sub.is_replaceable_by(&symbol_p_cod_other));
    assert!(!symbol_p_sub.is_replaceable_by(&symbol_p_sub_other));
    assert!(!symbol_p_sub.is_replaceable_by(&symbol_p_ent_other));
    assert!(!symbol_p_sub.is_replaceable_by(&symbol_def_other));

    assert!(!symbol_p_ent.is_replaceable_by(&symbol_p_unk));
    assert!(!symbol_p_ent.is_replaceable_by(&symbol_p_dat));
    assert!(!symbol_p_ent.is_replaceable_by(&symbol_p_cod));
    assert!(!symbol_p_ent.is_replaceable_by(&symbol_p_sub));
    assert!(symbol_p_ent.is_replaceable_by(&symbol_p_ent));
    assert!(symbol_p_ent.is_replaceable_by(&symbol_def));
    assert!(!symbol_p_ent.is_replaceable_by(&symbol_p_unk_other));
    assert!(!symbol_p_ent.is_replaceable_by(&symbol_p_dat_other));
    assert!(!symbol_p_ent.is_replaceable_by(&symbol_p_cod_other));
    assert!(!symbol_p_ent.is_replaceable_by(&symbol_p_sub_other));
    assert!(!symbol_p_ent.is_replaceable_by(&symbol_p_ent_other));
    assert!(!symbol_p_ent.is_replaceable_by(&symbol_def_other));

    assert!(!symbol_def.is_replaceable_by(&symbol_p_unk));
    assert!(!symbol_def.is_replaceable_by(&symbol_p_dat));
    assert!(!symbol_def.is_replaceable_by(&symbol_p_cod));
    assert!(!symbol_def.is_replaceable_by(&symbol_p_sub));
    assert!(!symbol_def.is_replaceable_by(&symbol_p_ent));
    assert!(symbol_def.is_replaceable_by(&symbol_def));
    assert!(!symbol_def.is_replaceable_by(&symbol_p_unk_other));
    assert!(!symbol_def.is_replaceable_by(&symbol_p_dat_other));
    assert!(!symbol_def.is_replaceable_by(&symbol_p_cod_other));
    assert!(!symbol_def.is_replaceable_by(&symbol_p_sub_other));
    assert!(!symbol_def.is_replaceable_by(&symbol_p_ent_other));
    assert!(!symbol_def.is_replaceable_by(&symbol_def_other));
}
