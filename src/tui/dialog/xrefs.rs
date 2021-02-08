//! Crossreference dialog

use crate::arch::Architecture;
use crate::database::ProjectDatabase;
use crate::memory::Pointer;
use cursive::view::View;
use cursive::views::{Dialog, LinearLayout, SelectView, TextView};
use num_traits::One;
use std::convert::TryFrom;
use std::io;
use std::sync::{Arc, RwLock};

/// Construct a list of crossreferences.
///
/// TODO: We need a builder pattern for the database itself being the state
pub fn xrefs_dialog<AR>(
    _arch: AR,
    pointer: Option<Pointer<AR::PtrVal>>,
    pjdb: Arc<RwLock<ProjectDatabase>>,
    prog_name: &str,
) -> io::Result<impl View>
where
    AR: Architecture,
{
    let mut db_lock = pjdb.write().unwrap();
    let db = db_lock.get_database_mut::<AR>(&prog_name).unwrap();

    let pointer = pointer.ok_or_else(|| {
        io::Error::new(io::ErrorKind::Other, "Selected memory location is invalid")
    })?;

    let inbound_xrefs = db.find_xrefs_to(&pointer, AR::Offset::one());
    let mut inbound_xref_list = SelectView::new();

    for inbound_xref in inbound_xrefs {
        let xref_obj = db.xref(inbound_xref).unwrap();
        let kind = xref_obj.kind().friendly_name();

        let source = if let Some(sym_id) = db.pointer_symbol(&xref_obj.as_source()) {
            let symbol = db.symbol(sym_id).unwrap().as_label();
            if let Some(parent) = symbol.parent_name() {
                format!("{}.{} ({})", parent, symbol.name(), kind)
            } else {
                format!("{} ({})", symbol.name(), kind)
            }
        } else if let Some(block_id) = db.find_block_membership(&xref_obj.as_source()) {
            let block = db.block(block_id).unwrap();
            let offset = AR::Offset::try_from(
                xref_obj.as_source().as_pointer().clone() - block.as_start().as_pointer().clone(),
            );

            if let Ok(offset) = offset {
                if let Some(sym_id) = db.pointer_symbol(block.as_start()) {
                    let symbol = db.symbol(sym_id).unwrap().as_label();
                    if let Some(parent) = symbol.parent_name() {
                        format!("{}.{}+${:X} ({})", parent, symbol.name(), offset, kind)
                    } else {
                        format!("{}+${:X} ({})", symbol.name(), offset, kind)
                    }
                } else {
                    format!("${:X}+${:X} ({})", block.as_start(), offset, kind)
                }
            } else {
                format!("${:X} ({})", xref_obj.as_source(), kind)
            }
        } else {
            format!("${:X} ({})", xref_obj.as_source(), kind)
        };

        inbound_xref_list.add_item(&source, inbound_xref);
    }

    let mut ll = LinearLayout::vertical();

    if inbound_xref_list.is_empty() {
        ll = ll.child(TextView::new("No backreferences exist"));
    } else {
        ll = ll.child(inbound_xref_list);
    }

    Ok(Dialog::around(ll)
        .title(format!("Backreferences for ${:X}", pointer))
        .dismiss_button("OK"))
}
