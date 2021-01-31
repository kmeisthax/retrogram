//! TUI File Picker

use crate::tui::builder::{BoxedMergeable, Builder};
use cursive::event::Event;
use cursive::view::{Nameable, Resizable};
use cursive::views::{Dialog, EditView, LinearLayout, Panel, ScrollView, SelectView};
use cursive::Cursive;
use std::fs::{read_dir, FileType};
use std::io;
use std::path::{Component, Path, PathBuf, Prefix};

/// Determine if a path is a valid filesystem directory.
///
/// If `Some`, then the returned path is some prefix of the given path that
/// corresponds to a valid directory on the filesystem. Otherwise, `None`
/// indicates that no prefix of the given path can be used.
///
/// We treat all paths that have a Windows prefix component of non-Disk as
/// invalid. This is to avoid locking the UI thread on server timeouts.
fn validate_directory(maybe_good_path: &Path) -> Option<&Path> {
    let mut known_good_path = maybe_good_path;
    if let Some(Component::Prefix(prefix)) = known_good_path.components().next() {
        if !matches!(prefix.kind(), Prefix::Disk(_))
            && !matches!(prefix.kind(), Prefix::VerbatimDisk(_))
        {
            return None;
        }
    }

    while read_dir(known_good_path).is_err() {
        if let Some(parent) = known_good_path.parent() {
            known_good_path = parent;
        } else {
            return None;
        }
    }

    Some(known_good_path)
}

/// Produce an error dialog for an IO error.
fn error_dialog(error: io::Error, path: &Path) -> Dialog {
    Dialog::text(format!(
        "{}\n(when reading contents of: {})",
        error,
        path.to_string_lossy()
    ))
    .title("Error")
    .button("OK", |s| {
        s.pop_layer();
    })
}

/// Construct a directory tree selector.
///
/// This directory selector goes through great pains to avoid yielding an error
/// when the user types an invalid path. It will assume the path has not yet
/// been completed, and instead continue with the longest valid subset of the
/// path. If the path is completely invalid, then an empty select view is
/// shown.
///
/// UNC paths are always treated as invalid to avoid locking up the UI thread
/// by trying to connect remote servers that don't exist.
#[allow(clippy::needless_collect)]
fn directory_tree<P: AsRef<Path>, CHANGE>(
    maybe_good_path: P,
    on_change: CHANGE,
) -> io::Result<SelectView<PathBuf>>
where
    CHANGE: Fn(&mut Cursive, &PathBuf) + 'static,
{
    let mut list = SelectView::new();
    let known_good_path = if let Some(p) = validate_directory(maybe_good_path.as_ref()) {
        p
    } else {
        return Ok(list);
    };

    let mut depth = 0;
    let ancestors = known_good_path.ancestors().collect::<Vec<_>>();

    for ancestor in ancestors.into_iter().rev() {
        if let Some(file_name) = ancestor.file_name() {
            list.add_item(
                format!(
                    "{}{}",
                    " ".repeat(depth),
                    file_name.to_string_lossy().into_owned()
                ),
                ancestor.to_path_buf(),
            );
            depth += 1;
        } else if ancestor.has_root() && ancestor.parent().is_none() {
            list.add_item(
                format!("{}{}", " ".repeat(depth), ancestor.display()),
                ancestor.to_path_buf(),
            );
            depth += 1;
        }
    }

    let directory = read_dir(known_good_path)?;

    for entry in directory {
        let entry = entry?;
        let child_path = entry.path();

        if child_path.is_dir() {
            list.add_item(
                format!(
                    "{}{}",
                    " ".repeat(depth),
                    entry.file_name().to_string_lossy().into_owned()
                ),
                child_path,
            );
        }
    }

    let list = list.on_submit(on_change);

    Ok(list)
}

/// Open a directory picker.
///
/// The directory picker will start at the given path, which must be a
/// directory. If a valid path is not given or the user later navigates to an
/// invalid path, the directory picker will be dismissed without action.
///
/// When a directory is selected, the `then` callback will be given with the
/// selected path after the directory picker has been dismissed.
pub fn directory_picker<THEN>(
    siv: &mut Cursive,
    title: &'static str,
    starting_path: &Path,
    then: THEN,
) where
    THEN: Fn(&mut Cursive, &Path) + 'static + Clone,
{
    siv.add_layer(
        Builder::from_state_and_builder(starting_path.to_path_buf(), move |path: &PathBuf| {
            let on_change = |s: &mut Cursive, path: &PathBuf| {
                s.call_on_name("file_picker", |v: &mut Builder<PathBuf>| {
                    v.with_state_mut(|pathbuf: &mut PathBuf| {
                        *pathbuf = path.clone();
                    })
                });

                s.on_event(Event::Refresh);
            };
            let directory_list = match directory_tree(path, on_change) {
                Ok(directory_list) => directory_list,
                Err(e) => return BoxedMergeable::boxed(error_dialog(e, path)),
            };

            let confirm_path = path.clone();
            let confirm_then = then.clone();

            BoxedMergeable::boxed(
                Dialog::around(
                    LinearLayout::vertical()
                        .child(EditView::new().content(path.to_string_lossy()).on_edit(
                            |s, new_path, _pos| {
                                let new_path = PathBuf::from(new_path);

                                s.call_on_name("file_picker", move |v: &mut Builder<PathBuf>| {
                                    v.with_state_mut(|pathbuf| {
                                        *pathbuf = new_path;
                                    })
                                });

                                s.on_event(Event::Refresh);
                            },
                        ))
                        .child(Panel::new(
                            ScrollView::new(directory_list.min_width(30))
                                .scroll_y(true)
                                .full_height(),
                        )),
                )
                .title(title)
                .button("OK", move |s| {
                    s.pop_layer();
                    confirm_then(s, &confirm_path);
                })
                .button("Cancel", |s| {
                    s.pop_layer();
                }),
            )
        })
        .with_name("file_picker"),
    );
}

/// Select an emoji to represent this file type.
fn type_emoji(file_type: FileType) -> &'static str {
    if file_type.is_dir() {
        "(DIR)"
    } else if file_type.is_symlink() {
        "(LNK)"
    } else {
        ""
    }
}

/// Construct a file listing selector.
///
/// Much like the directory tree, we go through great pains to present some
/// kind of usable view when showing files.
#[allow(clippy::needless_collect)]
fn file_listing<P: AsRef<Path>, CHANGE>(
    maybe_good_path: P,
    on_change: CHANGE,
) -> io::Result<SelectView<PathBuf>>
where
    CHANGE: Fn(&mut Cursive, &PathBuf) + 'static,
{
    let mut list = SelectView::new();
    let known_good_dir = if let Some(p) = validate_directory(maybe_good_path.as_ref()) {
        p
    } else {
        return Ok(list);
    };

    let directory = read_dir(known_good_dir)?;

    for entry in directory {
        let entry = entry?;
        let child_path = entry.path();
        let file_type = entry.file_type()?;

        list.add_item(
            format!(
                "{} {}",
                type_emoji(file_type),
                entry.file_name().to_string_lossy().into_owned()
            ),
            child_path,
        );
    }

    let list = list.on_submit(on_change);

    Ok(list)
}

/// Open a file picker.
///
/// The directory picker will start at the given path, which must be a
/// directory. If a valid path is not given or the user later navigates to an
/// invalid path, the directory picker will be dismissed without action.
///
/// When a directory is selected, the `then` callback will be given with the
/// selected path after the directory picker has been dismissed.
pub fn file_picker<THEN>(siv: &mut Cursive, title: &'static str, starting_path: &Path, then: THEN)
where
    THEN: Fn(&mut Cursive, &Path) + 'static + Clone,
{
    siv.add_layer(
        Builder::from_state_and_builder(starting_path.to_path_buf(), move |path: &PathBuf| {
            let on_change = |s: &mut Cursive, path: &PathBuf| {
                s.call_on_name("file_picker", |v: &mut Builder<PathBuf>| {
                    v.with_state_mut(|pathbuf: &mut PathBuf| {
                        *pathbuf = path.clone();
                    })
                });

                s.on_event(Event::Refresh);
            };
            let directory_list = match directory_tree(path, on_change) {
                Ok(directory_list) => directory_list,
                Err(e) => return BoxedMergeable::boxed(error_dialog(e, path)),
            };
            let file_list = match file_listing(path, on_change) {
                Ok(file_list) => file_list,
                Err(e) => return BoxedMergeable::boxed(error_dialog(e, path)),
            };

            let confirm_path = path.clone();
            let confirm_then = then.clone();

            BoxedMergeable::boxed(
                Dialog::around(
                    LinearLayout::vertical()
                        .child(EditView::new().content(path.to_string_lossy()).on_edit(
                            |s, new_path, _pos| {
                                let new_path = PathBuf::from(new_path);

                                s.call_on_name("file_picker", move |v: &mut Builder<PathBuf>| {
                                    v.with_state_mut(|pathbuf| {
                                        *pathbuf = new_path;
                                    })
                                });

                                s.on_event(Event::Refresh);
                            },
                        ))
                        .child(
                            LinearLayout::horizontal()
                                .child(Panel::new(
                                    ScrollView::new(directory_list.min_width(30))
                                        .scroll_y(true)
                                        .full_height(),
                                ))
                                .child(Panel::new(
                                    ScrollView::new(file_list.min_width(60))
                                        .scroll_y(true)
                                        .full_height(),
                                )),
                        ),
                )
                .title(title)
                .button("OK", move |s| {
                    s.pop_layer();
                    confirm_then(s, &confirm_path);
                })
                .button("Cancel", |s| {
                    s.pop_layer();
                }),
            )
        })
        .with_name("file_picker"),
    );
}
