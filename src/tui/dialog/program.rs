//! Program configurator dialog

use crate::arch::ArchName;
use crate::asm::AssemblerName;
use crate::platform::PlatformName;
use crate::project::Program;
use crate::tui::builder::{BoxedMergeable, Builder};
use crate::tui::dialog::pickers::file_picker;
use cursive::event::Event;
use cursive::view::Nameable;
use cursive::views::{Button, Dialog, EditView, LinearLayout, ListView, SelectView, TextView};
use cursive::Cursive;
use std::env;

/// Build the platform selector
fn platform_selector<CHANGE>(
    platform: Option<PlatformName>,
    change: CHANGE,
) -> SelectView<PlatformName>
where
    CHANGE: Fn(&mut Cursive, &PlatformName) + 'static,
{
    let mut select = SelectView::new();

    for (i, pn) in PlatformName::iter().into_iter().enumerate() {
        select.add_item(pn.friendly_name(), pn);

        if let Some(platform) = platform {
            if platform == pn {
                select.set_selection(i);
            }
        }
    }

    select.popup().on_submit(change)
}

/// Build the architecture selector
fn arch_selector<CHANGE>(arch: Option<ArchName>, change: CHANGE) -> SelectView<ArchName>
where
    CHANGE: Fn(&mut Cursive, &ArchName) + 'static,
{
    let mut select = SelectView::new();

    for (i, an) in ArchName::iter().into_iter().enumerate() {
        select.add_item(an.friendly_name(), an);

        if let Some(arch) = arch {
            if arch == an {
                select.set_selection(i);
            }
        }
    }

    select.popup().on_submit(change)
}

/// Build the assembler selector
fn asm_selector<CHANGE>(arch: Option<AssemblerName>, change: CHANGE) -> SelectView<AssemblerName>
where
    CHANGE: Fn(&mut Cursive, &AssemblerName) + 'static,
{
    let mut select = SelectView::new();

    for (i, an) in AssemblerName::iter().into_iter().enumerate() {
        select.add_item(an.friendly_name(), an);

        if let Some(arch) = arch {
            if arch == an {
                select.set_selection(i);
            }
        }
    }

    select.popup().on_submit(change)
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
                        .child(TextView::new("Name"))
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
                                }),
                        )
                        .child(TextView::new("Analysis Architecture"))
                        .child(arch_selector(program.arch(), |s, new_ar| {
                            s.call_on_name(
                                "program_configurator",
                                move |v: &mut Builder<Program>| {
                                    v.with_state_mut(|program| {
                                        program.set_arch(*new_ar);
                                    });
                                },
                            );

                            s.on_event(Event::Refresh);
                        }))
                        .child(TextView::new("Platform"))
                        .child(platform_selector(program.platform(), |s, new_pf| {
                            s.call_on_name(
                                "program_configurator",
                                move |v: &mut Builder<Program>| {
                                    v.with_state_mut(|program| {
                                        program.set_platform(*new_pf);
                                    });
                                },
                            );

                            s.on_event(Event::Refresh);
                        }))
                        .child(TextView::new("Assembler Syntax"))
                        .child(asm_selector(program.assembler(), |s, new_asm| {
                            s.call_on_name(
                                "program_configurator",
                                move |v: &mut Builder<Program>| {
                                    v.with_state_mut(|program| {
                                        program.set_assembler(*new_asm);
                                    });
                                },
                            );

                            s.on_event(Event::Refresh);
                        }))
                        .child(TextView::new("Images"))
                        .child(
                            LinearLayout::horizontal().child(Button::new("Add Image", |s| {
                                file_picker(
                                    s,
                                    "Select Image File",
                                    &env::current_dir().unwrap(),
                                    move |s, path| {
                                        s.call_on_name(
                                            "program_configurator",
                                            move |v: &mut Builder<Program>| {
                                                v.with_state_mut(|program| {
                                                    program.add_image_path(&path.to_string_lossy());
                                                });
                                            },
                                        );

                                        s.on_event(Event::Refresh);
                                    },
                                )
                            })),
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
                    s.pop_layer();
                    then(s, &then_program)
                })
                .button("Cancel", |s| {
                    s.pop_layer();
                }),
            )
        })
        .with_name("program_configurator"),
    )
}
