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
use std::borrow::Borrow;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::{fmt, io};

/// Names what kind of view a particular tab shows.
pub enum TabViewType {
    /// This tab contains a view of disassembled code.
    DisassemblyView {
        /// The program this disassembly view is displaying.
        program: Box<dyn AnyProgramContext>,
    },
}

impl Clone for TabViewType {
    fn clone(&self) -> Self {
        match self {
            TabViewType::DisassemblyView { program } => TabViewType::DisassemblyView {
                program: program.duplicate(),
            },
        }
    }
}

/// Information about a particular open tab.
#[derive(Clone)]
pub struct TabHandle {
    /// What view this tab contains.
    view_type: TabViewType,

    /// A nonce to separate multiple otherwise-identical tabs.
    nonce: u64,
}

impl PartialEq for TabHandle {
    fn eq(&self, rhs: &Self) -> bool {
        let view = match (&self.view_type, &rhs.view_type) {
            (
                TabViewType::DisassemblyView {
                    program: self_program,
                },
                TabViewType::DisassemblyView {
                    program: rhs_program,
                },
            ) => (self_program.as_program_name() == rhs_program.as_program_name()),
        };

        view && (self.nonce == rhs.nonce)
    }
}

impl Eq for TabHandle {}

impl Hash for TabHandle {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.view_type {
            TabViewType::DisassemblyView { program } => {
                "DisassemblyView".hash(state);
                program.as_program_name().hash(state);
            }
        };

        self.nonce.hash(state);
    }
}

impl Display for TabHandle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.view_type {
            TabViewType::DisassemblyView { program } => {
                write!(f, "Disasm View ({})", program.as_program_name())
            }
        }
    }
}

impl TabHandle {
    /// Create a handle for a disassembly view of a particular program.
    fn disasm_from_program(program: Box<dyn AnyProgramContext>, nonce: u64) -> Self {
        Self {
            view_type: TabViewType::DisassemblyView { program },
            nonce,
        }
    }

    /// Calculate a string form of this handle to use as the view name of any
    /// tab referenced by this handle.
    ///
    /// This is separate from the `Display` impl so that we can just have the
    /// program title in the tab.
    fn to_view_name(&self) -> String {
        match &self.view_type {
            TabViewType::DisassemblyView { program } => {
                format!("tab_disasm_{}_{}", program.as_program_name(), self.nonce)
            }
        }
    }

    pub fn program(&mut self) -> Option<impl '_ + Borrow<Program>> {
        match &mut self.view_type {
            TabViewType::DisassemblyView { program } => Some(program.as_program()),
        }
    }
}

/// Construct a new disassembly tab for a given program and add it to the TUI
fn disasm_tab_zygote(
    context: &mut dyn AnyProgramContext,
    panel: &mut TabPanel<TabHandle>,
    nonce: u64,
) -> io::Result<()> {
    with_context_architecture!(context, |context, arch, asm| {
        let handle = TabHandle::disasm_from_program(context.duplicate(), nonce);
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
                .project()
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
                    if let Err(e) = disasm_tab_zygote(&mut *ctxt, v, nonce) {
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
