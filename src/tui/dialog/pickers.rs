//! TUI File Picker

use cursive::event::{Callback, Event, EventResult, Key};
use cursive::view::{Nameable, Resizable};
use cursive::views::{
    BoxedView, Dialog, LinearLayout, OnEventView, Panel, ScrollView, SelectView, TextArea,
};
use cursive::Cursive;
use std::fs::read_dir;
use std::io;
use std::path::{Path, PathBuf};

/// Construct a directory tree selector.
#[allow(clippy::needless_collect)]
fn directory_tree<P: AsRef<Path>, CHANGE>(
    path: P,
    on_change: CHANGE,
) -> io::Result<SelectView<PathBuf>>
where
    CHANGE: Fn(&mut Cursive, &PathBuf) + 'static,
{
    let mut list = SelectView::new();
    let mut depth = 0;
    let ancestors = path.as_ref().ancestors().collect::<Vec<_>>();

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

    let directory = read_dir(path)?;

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
                Err(e) => {
                    return BoxedView::boxed(
                        Dialog::text(format!(
                            "{}\n(when reading contents of: {})",
                            e,
                            path.to_string_lossy()
                        ))
                        .title("Error")
                        .button("OK", |s| {
                            s.pop_layer();
                        }),
                    )
                }
            };

            let confirm_path = path.clone();
            let confirm_then = then.clone();

            BoxedView::boxed(
                Dialog::around(
                    LinearLayout::vertical()
                        .child(
                            OnEventView::new(
                                TextArea::new()
                                    .content(path.to_string_lossy())
                                    .fixed_height(1),
                            )
                            .on_pre_event_inner(
                                Key::Enter,
                                |ta, _evt| {
                                    let new_path = PathBuf::from(ta.get_inner().get_content());

                                    Some(EventResult::Consumed(Some(Callback::from_fn(move |s| {
                                        let new_path = new_path.clone();
                                        s.call_on_name(
                                            "file_picker",
                                            move |v: &mut Builder<PathBuf>| {
                                                v.with_state_mut(|pathbuf| {
                                                    *pathbuf = new_path;
                                                })
                                            },
                                        );

                                        s.on_event(Event::Refresh);
                                    }))))
                                },
                            ),
                        )
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
