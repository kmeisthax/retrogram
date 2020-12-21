//! Text UI for interactive use

use crate::analysis::Response;
use crate::arch::{Architecture, CompatibleLiteral};
use crate::asm::Assembler;
use crate::database::ProjectDatabase;
use crate::platform::Platform;
use crate::project::{Program, Project};
use crate::tui::context::{AnyProgramContext, ProgramContext};
use crate::tui::disasm_view::DisassemblyView;
use crate::tui::jump::jump_dialog;
use crate::tui::label::label_dialog;
use backtrace::Backtrace;
use cursive::menu::MenuTree;
use cursive::view::{Nameable, View};
use cursive::views::{Dialog, ScrollView, TextView};
use cursive::Cursive;
use cursive_tabs::TabPanel;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::panic::set_hook;
use std::sync::{Arc, RwLock};
use std::thread::spawn;
use std::{fmt, fs, io};

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

/// Information about a particular open tab.
#[derive(Clone)]
struct TabHandle {
    /// The program this tab is associated with.
    program: Program,

    /// A nonce to separate multiple tabs open and viewing the same program.
    nonce: u64,
}

impl PartialEq for TabHandle {
    fn eq(&self, rhs: &Self) -> bool {
        self.program.as_name() == rhs.program.as_name() && self.nonce == rhs.nonce
    }
}

impl Eq for TabHandle {}

impl Hash for TabHandle {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.program.as_name().hash(state);
        self.nonce.hash(state);
    }
}

impl Display for TabHandle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.program.as_name().unwrap_or("?"))
    }
}

impl TabHandle {
    fn from_program(program: Program, nonce: u64) -> Self {
        Self { program, nonce }
    }

    /// Calculate a string form of this handle to use as the view name of any
    /// tab referenced by this handle.
    ///
    /// This is separate from the `Display` impl so that we can just have the
    /// program title in the tab.
    fn to_view_name(&self) -> String {
        format!(
            "tab_{}_{}",
            self.program.as_name().unwrap_or(""),
            self.nonce
        )
    }

    fn program(&self) -> &Program {
        &self.program
    }
}

/// Construct a new disassembly tab for a given program and add it to the TUI
fn tab_zygote(
    context: &mut dyn AnyProgramContext,
    panel: &mut TabPanel<TabHandle>,
    nonce: &mut u64,
) -> io::Result<()> {
    with_context_architecture!(context, |context, arch, asm| {
        let handle = TabHandle::from_program(context.program().clone(), *nonce);
        let name = handle.to_view_name();

        *nonce += 1;

        panel.add_tab(
            handle,
            DisassemblyView::new(context.clone(), &name, arch, asm).with_name(name),
        );

        Ok(())
    })
}

fn call_on_tab<AR, ASM, CBK, R>(
    _arch: AR,
    _asm: ASM,
    siv: &mut Cursive,
    handle: &TabHandle,
    cbk: CBK,
) -> Option<R>
where
    AR: Architecture,
    ASM: Assembler,
    ASM::Literal: CompatibleLiteral<AR> + Clone,
    DisassemblyView<AR, ASM>: View,
    CBK: FnOnce(&mut DisassemblyView<AR, ASM>) -> R,
{
    let name = handle.to_view_name();

    siv.call_on_name(&name, cbk)
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
        .add_subtree("File", MenuTree::new().leaf("Exit", |s| s.quit()))
        .add_subtree(
            "Edit",
            MenuTree::new()
                .leaf("Declare code...", |s| {
                    let handle = s
                        .call_on_name("tabs", |v: &mut TabPanel<TabHandle>| {
                            v.active_tab().cloned()
                        })
                        .flatten();

                    if let Some(handle) = handle {
                        let program = handle.program();

                        with_prog_architecture!(program, |_plat, arch, asm| {
                            call_on_tab(arch, asm, s, &handle, |v| {
                                v.declare_code();
                            })
                            .unwrap();

                            Ok(())
                        })
                        .unwrap();
                    }
                })
                .leaf("Declare label...", |s| {
                    let handle = s
                        .call_on_name("tabs", |v: &mut TabPanel<TabHandle>| {
                            v.active_tab().cloned()
                        })
                        .flatten();

                    if let Some(handle) = handle {
                        let program = handle.program();

                        with_prog_architecture!(program, |_plat, arch, asm| {
                            let (mem, pjdb, prog_name) = call_on_tab(arch, asm, s, &handle, |v| {
                                let mem = v.memory_location();
                                let pjdb = v.context().project_database();
                                let prog_name = v.context().program_name().to_string();

                                (mem, pjdb, prog_name)
                            })
                            .unwrap();

                            label_dialog(arch, s, mem, pjdb, prog_name);

                            Ok(())
                        })
                        .unwrap();
                    }
                }),
        )
        .add_subtree(
            "View",
            MenuTree::new().leaf("Jump to...", |s| {
                let handle = s
                    .call_on_name("tabs", |v: &mut TabPanel<TabHandle>| {
                        v.active_tab().cloned()
                    })
                    .flatten();

                if let Some(handle) = handle {
                    let program = handle.program();

                    with_prog_architecture!(program, |_plat, arch, asm| {
                        let context =
                            call_on_tab(arch, asm, s, &handle, |v| v.context().clone()).unwrap();

                        jump_dialog(arch, s, &context, move |s, scroll| {
                            call_on_tab(arch, asm, s, &handle, |v| v.scroll_to(scroll)).unwrap();

                            true
                        });

                        Ok(())
                    })
                    .unwrap();
                }
            }),
        );

    let mut panel = TabPanel::new();
    let mut tab_nonce = 0;

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

        tab_zygote(&mut *context, &mut panel, &mut tab_nonce)?;
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
