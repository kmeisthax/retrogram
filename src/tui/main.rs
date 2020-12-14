//! Text UI for interactive use

use crate::analysis::Response;
use crate::database::ProjectDatabase;
use crate::platform::Platform;
use crate::project::{Program, Project};
use crate::tui::context::{AnyProgramContext, ProgramContext};
use crate::tui::disasm_view::DisassemblyView;
use backtrace::Backtrace;
use cursive::menu::MenuTree;
use cursive::view::Nameable;
use cursive::views::{Dialog, ScrollView, TextView};
use cursive::Cursive;
use cursive_tabs::TabPanel;
use std::collections::HashMap;
use std::panic::set_hook;
use std::sync::{Arc, RwLock};
use std::thread::spawn;
use std::{fs, io};

/// Open a program and construct a context that holds it's mutable state for
/// later use.
///
/// The `siv` is used to trigger refreshes whenever the analysis queue for this
/// context has mutated the database.
fn context_zygote(
    program: Program,
    database: Arc<RwLock<ProjectDatabase>>,
    siv: &Cursive,
) -> io::Result<Box<dyn AnyProgramContext>> {
    let image = program
        .iter_images()
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;
    let prog_borrow = &program;

    with_prog_architecture!(prog_borrow, |plat, arch, asm| {
        let bus = plat.construct_platform(&mut file)?;
        let bus = Arc::new(bus);

        let (ctxt, recv) =
            ProgramContext::from_program_and_database(arch, asm, program, database, bus);
        let sink = siv.cb_sink().clone();

        spawn(move || loop {
            if matches!(recv.recv().unwrap(), Response::Fence) {
                sink.send(Box::new(|siv| {
                    siv.refresh();
                }))
                .unwrap();
            }
        });

        Ok(Box::new(ctxt))
    })
}

/// Construct a new disassembly tab for a given program and add it to the TUI
fn tab_zygote(context: &mut dyn AnyProgramContext, panel: &mut TabPanel<String>) -> io::Result<()> {
    with_context_architecture!(context, |context, arch, asm| {
        let name = context.program_name();

        panel.add_tab(
            name.to_string(),
            DisassemblyView::new(context.clone(), arch, asm),
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

    for (_name, program) in programs {
        let program_db = databases
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
        let mut context = context_zygote(program, program_db, &siv)?;

        tab_zygote(&mut *context, &mut panel)?;
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

    set_hook(Box::new(|panic_info| {
        let backtrace = format!("{:?}", Backtrace::new());
        let mut emergency_siv = cursive::default();
        let error = if let Some(reason) = panic_info.payload().downcast_ref::<String>() {
            format!(
                "Retrogram died due to an error: {}\n\n{}",
                reason, backtrace
            )
        } else {
            format!("Retrogram died due to an unknown error.\n\n{}", backtrace)
        };

        emergency_siv.add_layer(
            Dialog::around(ScrollView::new(TextView::new(error)))
                .title("Flagrant System Error")
                .button("Exit", |s| {
                    s.quit();
                }),
        );

        emergency_siv.run();
    }));

    siv.run();

    Ok(())
}
