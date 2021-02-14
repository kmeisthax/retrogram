//! Label form

use crate::arch::Architecture;
use crate::ast::Label;
use crate::database::ProjectDatabase;
use crate::memory::Pointer;
use cursive::view::{Margins, Nameable as CursiveNameable, Resizable};
use cursive::views::{Checkbox, Dialog, EditView, LinearLayout, TextView};
use cursive::Cursive;
use std::sync::{Arc, RwLock};

/// Create a label dialog.
pub fn label_dialog<AR>(
    _arch: AR,
    siv: &mut Cursive,
    address: Option<Pointer<AR::PtrVal>>,
    pjdb: Arc<RwLock<ProjectDatabase>>,
    prog_name: String,
) where
    AR: Architecture + 'static,
{
    if let Some(address) = address {
        let pjdb_lock = pjdb.read().unwrap();
        let db = pjdb_lock.get_database::<AR>(&prog_name).unwrap();
        let symbol_id = db.pointer_symbol(&address);
        let label_name = symbol_id
            .and_then(|symbol_id| Some(db.symbol(symbol_id)?.as_label().name().to_string()));
        let label_parent = symbol_id
            .and_then(|symbol_id| {
                db.symbol(symbol_id)?
                    .as_label()
                    .parent_name()
                    .map(|s| s.to_string())
            })
            .or_else(|| {
                if let Some(block_id) = db.find_block_membership(&address) {
                    let start = db.block(block_id).unwrap().as_start();
                    if start != &address {
                        db.pointer_symbol(start)
                            .map(|ps| db.symbol(ps).unwrap().as_label().name().to_string())
                    } else {
                        None
                    }
                } else {
                    None
                }
            });

        drop(pjdb_lock);

        siv.add_layer(
            Dialog::new()
                .title(format!("Label at ${:X}", address))
                .padding(Margins::lrtb(1, 1, 1, 1))
                .content(
                    LinearLayout::vertical()
                        .child(TextView::new("Label name"))
                        .child(
                            EditView::new()
                                .content(label_name.unwrap_or_else(|| "".to_string()))
                                .with_name("label_name")
                                .fixed_width(20),
                        )
                        .child(
                            LinearLayout::horizontal()
                                .child(
                                    Checkbox::new()
                                        .with_checked(label_parent.is_some())
                                        .with_name("label_has_parent"),
                                )
                                .child(TextView::new("Local name")),
                        )
                        .child(TextView::new("Label's parent"))
                        .child(
                            EditView::new()
                                .content(label_parent.unwrap_or_else(|| "".to_string()))
                                .with_name("label_parent")
                                .fixed_width(20),
                        ),
                )
                .button("OK", move |s| {
                    let new_label_name = s
                        .call_on_name("label_name", |v: &mut EditView| v.get_content())
                        .unwrap();
                    let new_label_has_parent = s
                        .call_on_name("label_has_parent", |v: &mut Checkbox| v.is_checked())
                        .unwrap();
                    let new_label_parent = s
                        .call_on_name("label_parent", |v: &mut EditView| v.get_content())
                        .unwrap();

                    let mut pjdb_lock = pjdb.write().unwrap();
                    let db = pjdb_lock.get_database_mut::<AR>(&prog_name).unwrap();

                    db.upsert_symbol(
                        Label::new(
                            &new_label_name,
                            if new_label_has_parent {
                                Some(&new_label_parent)
                            } else {
                                None
                            },
                        ),
                        None,
                        address.clone(),
                    );

                    s.pop_layer();
                })
                .button("Cancel", |s| {
                    s.pop_layer();
                }),
        );
    } else {
        siv.add_layer(
            Dialog::text("For some reason, the address of the selected line of disassembly is not valid and cannot be labeled.")
                .title("Invalid address!")
                .button("OK", |s| {
                    s.pop_layer();
                }),
        );
    }
}
