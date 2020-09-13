//! Text UI for interactive use

use crate::project::{Program, ProjectDatabase};
use crate::tui::disasm_view::DisassemblyView;
use cursive::event::Key;
use cursive::menu::MenuTree;
use cursive::views::Dialog;
use std::sync::{Arc, RwLock};
use std::{fs, io};

/// Start a TUI session.
pub fn main(program: &Program) -> io::Result<()> {
    let mut siv = cursive::default();
    let image = program
        .iter_images()
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;

    with_architecture!(program, file, |bus, arch, asm| {
        let pjdb = Arc::new(RwLock::new(
            match ProjectDatabase::read(program.as_database_path()) {
                Ok(pjdb) => pjdb,
                Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
                    eprintln!("Creating new database for project");
                    ProjectDatabase::new()
                }
                Err(e) => return Err(e),
            },
        ));
        let prog_name = program.as_name().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                "You did not specify a name for the program to disassemble.",
            )
        })?;
        let bus = Arc::new(bus);

        siv.add_fullscreen_layer(DisassemblyView::new(pjdb, prog_name, bus, arch, asm));

        Ok(())
    })?;

    siv.menubar()
        .add_subtree("File", MenuTree::new().leaf("Exit", |s| s.quit()));

    siv.add_layer(
        Dialog::text("Press <ESC> to show the menu.")
            .title("Welcome to Retrogram")
            .button("OK", |s| {
                s.pop_layer();
            }),
    );

    siv.add_global_callback(Key::Esc, |s| {
        s.select_menubar();
    });

    siv.set_autohide_menu(false);

    siv.run();

    Ok(())
}
