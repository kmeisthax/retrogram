//! Text UI for interactive use

use crate::database::ProjectDatabase;
use crate::platform::Platform;
use crate::project::{Program, Project};
use crate::tui::disasm_view::DisassemblyView;
use cursive::menu::MenuTree;
use cursive::view::Nameable;
use cursive::views::Dialog;
use cursive_tabs::TabPanel;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::{fs, io};

/// Construct a new disassembly tab for a given zygote.
fn tab_zygote(
    project: &mut Project,
    name: &str,
    panel: &mut TabPanel<String>,
    databases: &HashMap<String, Arc<RwLock<ProjectDatabase>>>,
) -> io::Result<()> {
    let program = project.program(name).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("Program {} does not exist", name),
        )
    })?;
    let image = program
        .iter_images()
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;
    let pjdb = databases
        .get(program.as_database_path())
        .cloned()
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!(
                    "Unexpected database file {} encountered",
                    program.as_database_path()
                ),
            )
        })?;

    with_prog_architecture!(program, |plat, arch, asm| {
        let bus = plat.construct_platform(&mut file)?;
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
    let programs = project
        .iter_programs()
        .map(|(k, v)| (k.to_string(), v.clone()))
        .collect::<Vec<(String, Program)>>();
    let mut databases = HashMap::new();

    for (_name, program) in programs.iter() {
        if !databases.contains_key(program.as_database_path()) {
            let pjdb: io::Result<ProjectDatabase> = ProjectDatabase::read(
                &mut project,
                &mut fs::File::open(program.as_database_path())?,
            )
            .map_err(|e| e.into());

            let pjdb = Arc::new(RwLock::new(match pjdb {
                Ok(pjdb) => pjdb,
                Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
                    eprintln!("Creating new database for project");
                    ProjectDatabase::new()
                }
                Err(e) => return Err(e),
            }));

            databases.insert(program.as_database_path().to_string(), pjdb);
        }
    }

    let mut siv = cursive::default();

    siv.menubar()
        .add_subtree("File", MenuTree::new().leaf("Exit", |s| s.quit()));

    let mut panel = TabPanel::new();

    for (name, _program) in programs.iter() {
        tab_zygote(&mut project, &name, &mut panel, &databases)?;
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
