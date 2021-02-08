//! Crossreference dialog

use crate::arch::Architecture;
use crate::database::ProjectDatabase;
use crate::memory::Pointer;
use cursive::view::View;
use cursive::views::{Dialog, LinearLayout, ListView, TextView};
use num_traits::One;
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
    let mut inbound_xref_list = ListView::new();

    for inbound_xref in inbound_xrefs {
        let xref_obj = db.xref(inbound_xref).unwrap();

        inbound_xref_list.add_child(
            &format!("{}", xref_obj.as_source()),
            LinearLayout::horizontal(),
        );
    }

    Ok(Dialog::around(
        LinearLayout::vertical()
            .child(TextView::new(format!(
                "Inbound crossreferences to ${:X}",
                pointer
            )))
            .child(inbound_xref_list),
    )
    .dismiss_button("OK"))
}
