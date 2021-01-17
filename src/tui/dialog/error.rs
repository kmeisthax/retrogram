use cursive::views::Dialog;
use std::error::Error;

/// Dismissable error dialog box.
pub fn error_dialog<E>(error: E) -> Dialog
where
    E: Error,
{
    Dialog::text(format!("{}", error))
        .title("Error")
        .dismiss_button("OK")
}
