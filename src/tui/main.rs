//! Text UI for interactive use

use crate::project::Project;
use crate::tui::context::SessionContext;
use crate::tui::menu::repopulate_menu;
use crate::tui::tabs::tab_zygote;
use backtrace::Backtrace;
use cursive::view::Nameable;
use cursive::views::{Dialog, ScrollView, TextView};
use cursive_tabs::TabPanel;
use std::io;
use std::panic::set_hook;

/// Start a TUI session.
pub fn main(project: Project) -> io::Result<()> {
    let mut session = SessionContext::from_project(project)?;
    let mut siv = cursive::default();
    let mut panel = TabPanel::new();
    let mut tab_nonce = 0;
    let program_names = session
        .iter_programs()
        .map(|(k, _v)| k.to_string())
        .collect::<Vec<String>>();

    for name in program_names {
        let mut context = session.program_context(&siv, &name)?;

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

    repopulate_menu(&mut siv);

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
