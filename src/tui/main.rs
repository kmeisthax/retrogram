//! Text UI for interactive use

use crate::project::Project;
use crate::tui::context::SessionContext;
use crate::tui::menu::repopulate_menu;
use crate::tui::tabs::{repopulate_tabs, TabHandle};
use backtrace::Backtrace;
use cursive::view::{Nameable, Resizable};
use cursive::views::{Dialog, LinearLayout, ScrollView, TextView};
use cursive_tabs::TabPanel;
use std::io;
use std::panic::set_hook;

/// Start a TUI session.
pub fn main(project: Project) -> io::Result<()> {
    let session = SessionContext::from_project(project)?;
    let mut siv = cursive::default();
    let panel: TabPanel<TabHandle> = TabPanel::new();

    siv.set_user_data(session);

    siv.add_fullscreen_layer(
        LinearLayout::vertical()
            .child(panel.with_name("tabs"))
            .child(
                TextView::new("Welcome to Retrogram!")
                    .with_name("status")
                    .min_height(3),
            ),
    );

    siv.set_autohide_menu(false);

    siv.add_layer(
        Dialog::text("Press <ESC> to show the menu.")
            .title("Welcome to Retrogram")
            .button("OK", |s| {
                s.pop_layer();
            }),
    );

    repopulate_tabs(&mut siv);
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
