//! Program configurator dialog

use crate::arch::ArchName;
use crate::asm::AssemblerName;
use crate::platform::PlatformName;
use crate::project::Program;
use crate::tui::builder::Builder;
use cursive::event::{Callback, EventResult, Key};
use cursive::view::{Nameable, View};
use cursive::views::{
    BoxedView, Dialog, EditView, LinearLayout, OnEventView, SelectView, TextView,
};
use cursive::Cursive;

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

            BoxedView::boxed(
                Dialog::around(
                    LinearLayout::vertical()
                        .child(TextView::new("Name"))
                        .child(
                            OnEventView::new(EditView::new().content(&old_name).on_submit(
                                |s, new_name| {
                                    let new_name = new_name.to_string();

                                    s.call_on_name(
                                        "program_configurator",
                                        move |v: &mut Builder<Program>| {
                                            v.with_state_mut(|program| {
                                                program.set_name(&new_name);
                                            });
                                        },
                                    );

                                    s.refresh();
                                },
                            ))
                            .on_pre_event_inner(
                                Key::Down,
                                move |ev, evt| {
                                    let new_name = ev.get_content().to_string();
                                    let callback = if let EventResult::Consumed(Some(cbk)) =
                                        ev.on_event(evt.clone())
                                    {
                                        Some(cbk)
                                    } else {
                                        None
                                    };

                                    if new_name != old_name {
                                        Some(EventResult::Consumed(Some(Callback::from_fn(
                                            move |s| {
                                                let new_name = new_name.clone();

                                                if let Some(cbk) = &callback {
                                                    cbk(s);
                                                }

                                                s.call_on_name(
                                                    "program_configurator",
                                                    move |v: &mut Builder<Program>| {
                                                        v.with_state_mut(|program| {
                                                            program.set_name(&new_name);
                                                        });
                                                    },
                                                );

                                                s.refresh();
                                            },
                                        ))))
                                    } else {
                                        None
                                    }
                                },
                            ),
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

                            s.refresh();
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

                            s.refresh();
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

                            s.refresh();
                        })),
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
