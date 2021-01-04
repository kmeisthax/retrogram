//! TUI tab handling

use crate::arch::{Architecture, CompatibleLiteral};
use crate::asm::Assembler;
use crate::project::Program;
use crate::tui::context::{AnyProgramContext, SessionContext};
use crate::tui::disasm_view::DisassemblyView;
use crate::tui::error_dialog::error_dialog;
use cursive::view::{Nameable, View};
use cursive::Cursive;
use cursive_tabs::TabPanel;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::{fmt, io};

/// Information about a particular open tab.
#[derive(Clone)]
pub struct TabHandle {
    /// The program this tab is associated with.
    program: Program,

    /// A nonce to separate multiple tabs open and viewing the same program.
    nonce: u64,
}

impl PartialEq for TabHandle {
    fn eq(&self, rhs: &Self) -> bool {
        self.program.as_name() == rhs.program.as_name() && self.nonce == rhs.nonce
    }
}

impl Eq for TabHandle {}

impl Hash for TabHandle {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.program.as_name().hash(state);
        self.nonce.hash(state);
    }
}

impl Display for TabHandle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.program.as_name().unwrap_or("?"))
    }
}

impl TabHandle {
    fn from_program(program: Program, nonce: u64) -> Self {
        Self { program, nonce }
    }

    /// Calculate a string form of this handle to use as the view name of any
    /// tab referenced by this handle.
    ///
    /// This is separate from the `Display` impl so that we can just have the
    /// program title in the tab.
    fn to_view_name(&self) -> String {
        format!(
            "tab_{}_{}",
            self.program.as_name().unwrap_or(""),
            self.nonce
        )
    }

    pub fn program(&self) -> &Program {
        &self.program
    }
}

/// Construct a new disassembly tab for a given program and add it to the TUI
fn tab_zygote(
    context: &mut dyn AnyProgramContext,
    panel: &mut TabPanel<TabHandle>,
    nonce: u64,
) -> io::Result<()> {
    with_context_architecture!(context, |context, arch, asm| {
        let handle = TabHandle::from_program(context.program().clone(), nonce);
        let name = handle.to_view_name();

        panel.add_tab(
            handle,
            DisassemblyView::new(context.clone(), &name, arch, asm).with_name(name),
        );

        Ok(())
    })
}

pub fn call_on_tab<AR, ASM, CBK, R>(
    _arch: AR,
    _asm: ASM,
    siv: &mut Cursive,
    handle: &TabHandle,
    cbk: CBK,
) -> Option<R>
where
    AR: Architecture,
    ASM: Assembler,
    ASM::Literal: CompatibleLiteral<AR> + Clone,
    DisassemblyView<AR, ASM>: View,
    CBK: FnOnce(&mut DisassemblyView<AR, ASM>) -> R,
{
    let name = handle.to_view_name();

    siv.call_on_name(&name, cbk)
}

/// Repopulate tabs with the current set of tabs in the session context.
///
/// This should only be called when a new session has been instantiated; doing
/// so will be highly disruptive to all user view state.
pub fn repopulate_tabs(siv: &mut Cursive) {
    siv.call_on_name("tabs", |v: &mut TabPanel<TabHandle>| {
        for tab in v.tab_order() {
            v.remove_tab(&tab).unwrap()
        }
    });

    let cb_sink = siv.cb_sink().clone();
    let program_names = siv
        .with_user_data(|session: &mut SessionContext| {
            let programs = session
                .iter_programs()
                .map(|(k, _v)| k.to_string())
                .collect::<Vec<String>>();
            let mut out = vec![];

            for program in programs {
                let context = session.program_context(cb_sink.clone(), &program);
                out.push((context, session.nonce()));
            }

            out
        })
        .unwrap();
    let mut errors = vec![];

    for (maybe_ctxt, nonce) in program_names {
        match maybe_ctxt {
            Err(e) => errors.push(e),
            Ok(mut ctxt) => {
                siv.call_on_name("tabs", |v: &mut TabPanel<TabHandle>| {
                    if let Err(e) = tab_zygote(&mut *ctxt, v, nonce) {
                        errors.push(e);
                    }
                });
            }
        }
    }

    for error in errors {
        siv.add_layer(error_dialog(error));
    }
}
