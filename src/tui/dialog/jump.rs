//! Jump form

use crate::arch::Architecture;
use crate::memory::Tumbler;
use crate::tui::context::ProgramContext;
use cursive::view::{Nameable, Resizable};
use cursive::views::{Dialog, EditView};
use cursive::Cursive;

/// Construct a form for accepting a jump-to target from a user.
///
/// This function accepts one callback, which will be called if the form has
/// successfully captured a jump target to go to. It will be given a tumbler
/// to scroll to, and should return `true` if it was able to scroll. Otherwise,
/// it should tell the user why it couldn't scroll by adding another dialog box
/// and returning `false` to keep the jump box on screen.
pub fn jump_dialog<AR, CBK>(
    _arch: AR,
    siv: &mut Cursive,
    context: &ProgramContext<AR>,
    on_jump: CBK,
) where
    AR: Architecture,
    CBK: 'static + Clone + Fn(&mut Cursive, Tumbler) -> bool,
{
    let on_jump_submit = on_jump.clone();
    let context_submit = context.clone();
    let context_button = context.clone();

    siv.add_layer(
        Dialog::new()
            .title("Jump")
            .padding_lrtb(1, 1, 1, 0)
            .content(
                EditView::new()
                    .on_submit(move |s, loc| jump_intent(s, &context_submit, loc, &on_jump_submit))
                    .with_name("jump_target")
                    .fixed_width(20),
            )
            .button("OK", move |s| {
                let name = s.call_on_name("jump_target", |v: &mut EditView| v.get_content());

                if let Some(name) = name {
                    jump_intent(s, &context_button, &name, &on_jump)
                }
            })
            .button("Cancel", |s| {
                s.pop_layer();
            }),
    )
}

fn jump_intent<AR, CBK>(
    siv: &mut Cursive,
    context: &ProgramContext<AR>,
    target: &str,
    on_jump: &CBK,
) where
    AR: Architecture,
    CBK: 'static + Clone + Fn(&mut Cursive, Tumbler) -> bool,
{
    let parsed_ptr = context.parse_ptr(target);

    if let Some(parsed_ptr) = parsed_ptr {
        let scroll = context.bus().decode_tumbler(parsed_ptr.clone());

        if let Some(scroll) = scroll {
            if on_jump(siv, scroll) {
                siv.pop_layer();
            }
        } else {
            siv.add_layer(Dialog::text(format!("Could not convert pointer ${:X} into a scroll position.\nThis can happen if a pointer doesn't point to valid memory,\nor if the memory is banked and needs context information.\nYou can indicate banks by prefixing your pointer with a colon.", parsed_ptr))
                .title("Could not calculate scroll position")
                .button("OK", |s| {
                    s.pop_layer();
                }));
        }
    } else {
        siv.add_layer(Dialog::text("Could not parse pointer.\nPointers must be provided in hexdecimal, with banks and other contexts separated by colon prefixes.\nDo not prefix the pointer with $ or 0x.")
            .title("Could not parse pointer")
            .button("OK", |s| {
                s.pop_layer();
            }));
    }
}
