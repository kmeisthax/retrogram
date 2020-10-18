//! Text UI for interactive use

use crate::database::ProjectDatabase;
use crate::platform::Platform;
use crate::project::{Program, Project};
use crate::tui::disasm_view::DisassemblyView;
use cursive::event::Key;
use cursive::menu::MenuTree;
use cursive::view::Nameable;
use cursive::views::Dialog;
use cursive_tabs::TabPanel;
use std::sync::{Arc, RwLock};
use std::{fs, io};

/// Construct a new disassembly tab for a given zygote.
fn tab_zygote(
    project: &mut Project,
    name: &str,
    program: &Program,
    panel: &mut TabPanel<String>,
) -> io::Result<()> {
    let image = program
        .iter_images()
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;

    with_prog_architecture!(program, |plat, arch, asm| {
        let bus = plat.construct_platform(&mut file)?;
        let pjdb: io::Result<ProjectDatabase> =
            ProjectDatabase::read(project, &mut fs::File::open(program.as_database_path())?)
                .map_err(|e| e.into());

        let pjdb = Arc::new(RwLock::new(match pjdb {
            Ok(pjdb) => pjdb,
            Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
                eprintln!("Creating new database for project");
                ProjectDatabase::new()
            }
            Err(e) => return Err(e),
        }));
        let bus = Arc::new(bus);

        panel.add_tab(
            name.to_string(),
            DisassemblyView::new(pjdb, name, bus, arch, asm),
        );

        Ok(())
    })
}

/// Start a TUI session.
pub fn main(mut project: Project) -> io::Result<()> {
    let mut siv = cursive::default();

    siv.menubar()
        .add_subtree("File", MenuTree::new().leaf("Exit", |s| s.quit()));

    let mut panel = TabPanel::new();
    let programs = project
        .iter_programs()
        .map(|(k, v)| (k.to_string(), v.clone()))
        .collect::<Vec<(String, Program)>>();

    for (name, program) in programs {
        tab_zygote(&mut project, &name, &program, &mut panel)?;
    }

    siv.add_fullscreen_layer(panel.with_name("tabs"));

    siv.set_autohide_menu(false);

    siv.add_layer(
        Dialog::text("Press <ESC> to show the menu.")
            .title("Welcome to Retrogram")
            .button("OK", |s| {
                s.pop_layer();
            }),
    );

    siv.run();

    Ok(())
}
