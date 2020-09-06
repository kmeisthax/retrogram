use crate::analysis::Block;
use crate::arch::tests::TestArchitecture;
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
