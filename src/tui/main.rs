//! Text UI for interactive use

use cursive::event::Key;
use cursive::menu::MenuTree;
use cursive::views::Dialog;

/// Start a TUI session.
pub fn main() {
    let mut siv = cursive::default();

    siv.menubar()
        .add_subtree("File", MenuTree::new().leaf("Exit", |s| s.quit()));

    siv.add_global_callback(Key::Esc, |s| s.select_menubar());

    siv.add_layer(
        Dialog::text("Press <ESC> to show the menu.")
            .title("Welcome to Retrogram")
            .button("OK", |s| {
                s.pop_layer();
            }),
    );

    siv.run();
}
