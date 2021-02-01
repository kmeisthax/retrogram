//! Program configurator dialog

use crate::arch::ArchName;
use crate::asm::AssemblerName;
use crate::platform::PlatformName;
use crate::project::Program;
use crate::tui::builder::{BoxedMergeable, Builder};
use crate::tui::dialog::pickers::file_picker;
use cursive::event::Event;
use cursive::view::{Nameable, Resizable};
use cursive::views::{Button, Dialog, EditView, LinearLayout, ListView, SelectView, TextView};
use cursive::Cursive;
use std::env;

/// Build the platform selector
fn platform_selector<CHANGE>(
    platform: Option<PlatformName>,
    current_arch: Option<ArchName>,
    change: CHANGE,
) -> SelectView<Option<PlatformName>>
where
    CHANGE: Fn(&mut Cursive, Option<PlatformName>) + 'static,
{
    let mut select = SelectView::new();

    select.add_item("Select platform...", None);

    for (i, pn) in PlatformName::iter().into_iter().enumerate() {
        if let Some(arch) = current_arch {
            if !pn.is_compatible_with_arch(arch) {
                continue;
            }
        }

        select.add_item(pn.friendly_name(), Some(pn));

        if let Some(platform) = platform {
            if platform == pn {
                select.set_selection(i + 1);
            }
        }
    }

    select.popup().on_submit(move |s, p| change(s, *p))
}

/// Build the architecture selector
fn arch_selector<CHANGE>(arch: Option<ArchName>, change: CHANGE) -> SelectView<Option<ArchName>>
where
    CHANGE: Fn(&mut Cursive, Option<ArchName>) + 'static,
{
    let mut select = SelectView::new();

    select.add_item("Select architecture...", None);

    for (i, an) in ArchName::iter().into_iter().enumerate() {
        select.add_item(an.friendly_name(), Some(an));

        if let Some(arch) = arch {
            if arch == an {
                select.set_selection(i + 1);
            }
        }
    }

    select.popup().on_submit(move |s, a| change(s, *a))
}

/// Build the assembler selector
fn asm_selector<CHANGE>(
    arch: Option<AssemblerName>,
    current_arch: Option<ArchName>,
    change: CHANGE,
) -> SelectView<Option<AssemblerName>>
where
    CHANGE: Fn(&mut Cursive, Option<AssemblerName>) + 'static,
{
    let mut select = SelectView::new();

    select.add_item("Select assembler...", None);

    for (i, an) in AssemblerName::iter().into_iter().enumerate() {
        if let Some(arch) = current_arch {
            if !an.is_compatible_with_arch(arch) {
                continue;
            }
        }

        select.add_item(an.friendly_name(), Some(an));

        if let Some(arch) = arch {
            if arch == an {
                select.set_selection(i + 1);
            }
        }
    }

    select.popup().on_submit(move |s, a| change(s, *a))
}

/// Build the image list for a program configurator.
///
///
fn image_list<REMOVE>(images: &[&str], remove: REMOVE) -> ListView
where
    REMOVE: Fn(&mut Cursive, usize) + Clone + 'static,
{
    let mut list = ListView::new();

    for (i, image) in images.iter().enumerate() {
        let my_remove = remove.clone();

        list.add_child(image, Button::new("Remove", move |s| my_remove(s, i)));
    }

    list
}

/// Open a program configurator dialog, and call `then` if the dialog was
/// confirmed.
pub fn program_config_dialog<THEN>(siv: &mut Cursive, program: Program, then: THEN)
where
    THEN: Fn(&mut Cursive, &Program) + 'static + Clone,
{
    siv.add_layer(
        Builder::from_state_and_builder(program, move |program: &Program| {
            let then_program = program.clone();
            let then = then.clone();
            let old_name = program.as_name().unwrap_or("").to_string();

            BoxedMergeable::boxed(
                Dialog::around(
                    LinearLayout::vertical()
                        .child(
                            LinearLayout::horizontal()
                                .child(TextView::new("Name").min_width(25))
                                .weight(1)
                                .child(
                                    EditView::new()
                                        .content(&old_name)
                                        .on_edit(|s, new_name, _pos| {
                                            let new_name = new_name.to_string();

                                            s.call_on_name(
                                                "program_configurator",
                                                move |v: &mut Builder<Program>| {
                                                    v.with_state_mut(|program| {
                                                        program.set_name(&new_name);
                                                    });
                                                },
                                            );

                                            s.on_event(Event::Refresh);
                                        })
                                        .min_width(30),
                                )
                                .weight(5),
                        )
                        .child(
                            LinearLayout::horizontal()
                                .child(TextView::new("Analysis Architecture").min_width(25))
                                .weight(1)
                                .child(
                                    arch_selector(program.arch(), |s, new_ar| {
                                        s.call_on_name(
                                            "program_configurator",
                                            move |v: &mut Builder<Program>| {
                                                v.with_state_mut(|program| {
                                                    if let Some(arch) = new_ar {
                                                        program.set_arch(arch);
                                                    } else {
                                                        program.unset_arch();
                                                    }
                                                });
                                            },
                                        );

                                        s.on_event(Event::Refresh);
                                    })
                                    .min_width(30),
                                )
                                .weight(5),
                        )
                        .child(
                            LinearLayout::horizontal()
                                .child(TextView::new("Platform").min_width(25))
                                .weight(1)
                                .child(
                                    platform_selector(
                                        program.platform(),
                                        program.arch(),
                                        |s, new_pf| {
                                            s.call_on_name(
                                                "program_configurator",
                                                move |v: &mut Builder<Program>| {
                                                    v.with_state_mut(|program| {
                                                        if let Some(pf) = new_pf {
                                                            program.set_platform(pf);
                                                        } else {
                                                            program.unset_platform();
                                                        }
                                                    });
                                                },
                                            );

                                            s.on_event(Event::Refresh);
                                        },
                                    )
                                    .min_width(30),
                                )
                                .weight(5),
                        )
                        .child(
                            LinearLayout::horizontal()
                                .child(TextView::new("Assembler Syntax").min_width(25))
                                .weight(1)
                                .child(
                                    asm_selector(
                                        program.assembler(),
                                        program.arch(),
                                        |s, new_asm| {
                                            s.call_on_name(
                                                "program_configurator",
                                                move |v: &mut Builder<Program>| {
                                                    v.with_state_mut(|program| {
                                                        if let Some(asm) = new_asm {
                                                            program.set_assembler(asm);
                                                        } else {
                                                            program.unset_assembler();
                                                        }
                                                    });
                                                },
                                            );

                                            s.on_event(Event::Refresh);
                                        },
                                    )
                                    .min_width(30),
                                )
                                .weight(5),
                        )
                        .child(
                            LinearLayout::horizontal()
                                .child(TextView::new("Images").min_width(25))
                                .weight(1)
                                .child(
                                    Button::new("Add Image", |s| {
                                        file_picker(
                                            s,
                                            "Select Image File",
                                            &env::current_dir().unwrap(),
                                            move |s, path| {
                                                s.call_on_name(
                                                    "program_configurator",
                                                    move |v: &mut Builder<Program>| {
                                                        v.with_state_mut(|program| {
                                                            program.add_image_path(
                                                                &path.to_string_lossy(),
                                                            );
                                                        });
                                                    },
                                                );

                                                s.on_event(Event::Refresh);
                                            },
                                        )
                                    })
                                    .min_width(15),
                                )
                                .weight(5),
                        )
                        .child(image_list(
                            &program.iter_images().collect::<Vec<&str>>(),
                            |s, i| {
                                s.call_on_name(
                                    "program_configurator",
                                    move |v: &mut Builder<Program>| {
                                        v.with_state_mut(|program| {
                                            program.remove_image_index(i);
                                        });
                                    },
                                );

                                s.on_event(Event::Refresh);
                            },
                        )),
                )
                .title("")
                .button("OK", move |s| {
                    if let Err(e) = then_program.validate() {
                        s.add_layer(Dialog::info(format!(
                            "Your program is currently invalid: {}.\nPlease reconfigure it first.",
                            e
                        )));
                    } else {
                        s.pop_layer();
                        then(s, &then_program);
                    }
                })
                .button("Cancel", |s| {
                    s.pop_layer();
                }),
            )
        })
        .with_name("program_configurator"),
    )
}
