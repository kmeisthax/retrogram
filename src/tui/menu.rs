//! TUI menu tree utils

use crate::tui::context::SessionContext;
use crate::tui::jump::jump_dialog;
use crate::tui::label::label_dialog;
use crate::tui::tabs::{call_on_tab, TabHandle};
use cursive::menu::MenuTree;
use cursive::views::Dialog;
use cursive::Cursive;
use cursive_tabs::TabPanel;

/// Regenerate the global menu in Cursive.
pub fn repopulate_menu(siv: &mut Cursive) {
    siv.menubar().clear();
    siv.menubar()
        .add_subtree(
            "File",
            MenuTree::new()
                .leaf("New", |s| {
                    s.call_on_name("tabs", |v: &mut TabPanel<TabHandle>| {
                        for tab in v.tab_order() {
                            v.remove_tab(&tab).unwrap()
                        }
                    });

                    let new_session = SessionContext::empty_session();

                    s.set_user_data(new_session);
                    repopulate_menu(s);
                })
                .leaf("Open", |s| {
                    s.add_layer(Dialog::text("Not yet implemented.").title("Error").button(
                        "OK",
                        |s| {
                            s.pop_layer();
                        },
                    ));
                })
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
