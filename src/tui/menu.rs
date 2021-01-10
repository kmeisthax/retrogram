//! TUI menu tree utils

use crate::tui::context::SessionContext;
use crate::tui::error_dialog::error_dialog;
use crate::tui::jump::jump_dialog;
use crate::tui::label::label_dialog;
use crate::tui::pickers::directory_picker;
use crate::tui::tabs::{call_on_tab, repopulate_tabs, TabHandle};
use cursive::menu::MenuTree;
use cursive::Cursive;
use cursive_tabs::TabPanel;
use std::path::Path;
use std::{env, fs};

/// Process a user command to save the current session.
///
/// The `overwrite` flag indicates if the user wants to overwrite the on-disk
/// session or save it in a new directory.
fn save_intent(siv: &mut Cursive, overwrite: bool) {
    let session = siv
        .user_data::<SessionContext>()
        .expect("Session should exist");
    let read_path = if overwrite {
        session.path().map(|p| p.to_path_buf())
    } else {
        None
    };

    let then = |s: &mut Cursive, project_path: &Path| {
        let session = s
            .user_data::<SessionContext>()
            .expect("Session should exist");
        let mut project_file_path = project_path.to_path_buf();

        project_file_path.push("retrogram.json");

        if let Err(e) = session.project_mut().write(project_file_path) {
            s.add_layer(error_dialog(e));
            return;
        }

        let mut reportable_error = None;

        for (db_path, db) in session.iter_databases() {
            let mut db_lock = db.write().unwrap();
            let mut db_file = match fs::File::create(db_path.to_path(project_path)) {
                Err(e) => {
                    reportable_error = Some(e);
                    break;
                }
                Ok(file) => file,
            };

            if let Err(e) = db_lock.write(&mut db_file) {
                reportable_error = Some(e.into());
                break;
            }
        }

        if let Some(reportable_error) = reportable_error {
            s.add_layer(error_dialog(reportable_error));
            return;
        }
    };

    if let Some(base_path) = read_path {
        then(siv, &base_path)
    } else {
        directory_picker(
            siv,
            "Select save directory",
            &env::current_dir().unwrap(),
            move |s, path| then(s, path),
        );
    }
}

/// Regenerate the global menu in Cursive.
pub fn repopulate_menu(siv: &mut Cursive) {
    siv.menubar().clear();
    siv.menubar()
        .add_subtree(
            "File",
            MenuTree::new()
                .leaf("New", |s| {
                    s.set_user_data(SessionContext::empty_session());

                    repopulate_tabs(s);
                    repopulate_menu(s);
                })
                .delimiter()
                .leaf("Open...", |s| {
                    let session = s
                        .user_data::<SessionContext>()
                        .expect("Session should exist");
                    let mut path = env::current_dir().unwrap();
                    if let Some(read_path) = session.read_from() {
                        if read_path.is_absolute() {
                            path = read_path.to_path_buf();
                        } else {
                            path = path.join(read_path);
                        }
                    }

                    while !path.is_dir() && path.parent().is_some() {
                        path = path.parent().unwrap().to_path_buf();
                    }

                    directory_picker(s, "Select project directory", &path, |s, path| {
                        let new_session =
                            SessionContext::from_filename(path.join("retrogram.json"));

                        match new_session {
                            Ok(new_session) => s.set_user_data(new_session),
                            Err(e) => {
                                s.add_layer(error_dialog(e));
                                return;
                            }
                        };

                        repopulate_tabs(s);
                        repopulate_menu(s);
                    });
                })
                .leaf("Save", |s| save_intent(s, true))
                .leaf("Save as...", |s| save_intent(s, false))
                .delimiter()
                .leaf("Exit", |s| s.quit()),
        )
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

    let windows = siv
        .call_on_name("tabs", |v: &mut TabPanel<TabHandle>| {
            let mut windows = MenuTree::new();

            for (i, handle) in v.tab_order().into_iter().enumerate() {
                let closure_handle = handle.clone();
                windows = windows.leaf(format!("{}: {}", i + 1, handle.clone()), move |s| {
                    let closure_handle = closure_handle.clone();
                    s.call_on_name("tabs", move |v: &mut TabPanel<TabHandle>| {
                        v.set_active_tab(closure_handle).unwrap();
                    });
                });
            }

            windows
        })
        .expect("Could not read tabs list in menu");

    siv.menubar().add_subtree("Windows", windows);
}
