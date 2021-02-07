//! TUI tab handling

use crate::arch::{Architecture, CompatibleLiteral};
use crate::asm::Assembler;
use crate::project::Program;
use crate::tui::context::{AnyProgramContext, SessionContext};
use crate::tui::dialog::error_dialog;
use crate::tui::disasm_view::DisassemblyView;
use cursive::traits::Resizable;
use cursive::view::{Nameable, View};
use cursive::views::{BoxedView, TextView};
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

    /// This tab exists to keep the disassembly view from crashing.
    EmptyView,
}

impl Clone for TabViewType {
    fn clone(&self) -> Self {
        match self {
            TabViewType::DisassemblyView { program } => TabViewType::DisassemblyView {
                program: program.duplicate(),
            },
            TabViewType::EmptyView => TabViewType::EmptyView,
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
            (TabViewType::DisassemblyView { .. }, _) => false,
            (TabViewType::EmptyView, TabViewType::EmptyView) => true,
            (TabViewType::EmptyView, _) => false,
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
            TabViewType::EmptyView => {
                "EmptyView".hash(state);
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
            TabViewType::EmptyView => {
                write!(f, "Empty")
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

    /// Create a handle for an empty view of a particular program.
    fn empty(nonce: u64) -> Self {
        Self {
            view_type: TabViewType::EmptyView,
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
            TabViewType::EmptyView => {
                format!("tab_empty_{}", self.nonce)
            }
        }
    }

    pub fn program(&mut self) -> Option<impl '_ + Borrow<Program>> {
        match &mut self.view_type {
            TabViewType::DisassemblyView { program } => Some(program.as_program()),
            TabViewType::EmptyView => None,
        }
    }

    /// Construct the view that should go with this tab.
    pub fn construct(&mut self) -> io::Result<BoxedView> {
        let name = self.to_view_name();

        match &mut self.view_type {
            TabViewType::DisassemblyView { program } => {
                let context = &mut **program;
                with_context_architecture!(context, |program, arch, asm| {
                    Ok(BoxedView::boxed(
                        DisassemblyView::new(program.clone(), &name, arch, asm).with_name(name),
                    ))
                })
            }
            TabViewType::EmptyView => Ok(BoxedView::boxed(
                TextView::new("Please add a program to this project.").full_screen(),
            )),
        }
    }

    /// Determine if this tab handle is an EmptyView type of tab
    pub fn is_empty(&self) -> bool {
        matches!(self.view_type, TabViewType::EmptyView)
    }
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
    let mut errors = vec![];
    let tab_handles = siv
        .with_user_data(|session: &mut SessionContext| {
            let programs = session
                .project()
                .iter_programs()
                .map(|(k, _v)| k.to_string())
                .collect::<Vec<String>>();
            let mut out = vec![];

            for program in programs {
                match session.program_context(cb_sink.clone(), &program) {
                    Ok(context) => {
                        out.push(TabHandle::disasm_from_program(context, session.nonce()))
                    }
                    Err(e) => errors.push(e),
                }
            }

            if out.is_empty() {
                out.push(TabHandle::empty(session.nonce()))
            }

            out
        })
        .unwrap();

    siv.call_on_name("tabs", |v: &mut TabPanel<TabHandle>| {
        for mut handle in tab_handles {
            match handle.construct() {
                Ok(view) => v.add_tab(handle, view),
                Err(e) => errors.push(e),
            }
        }
    });

    for error in errors {
        siv.add_layer(error_dialog(error));
    }
}

/// Open a new disasm tab for a context program with the given name.
pub fn open_disasm_tab(siv: &mut Cursive, for_program_name: &str) -> io::Result<()> {
    let cb_sink = siv.cb_sink().clone();
    let session = siv
        .user_data::<SessionContext>()
        .expect("Session should exist");

    let ctxt = session.program_context(cb_sink, for_program_name)?;
    let mut handle = TabHandle::disasm_from_program(ctxt, session.nonce());

    siv.call_on_name::<_, _, io::Result<()>>("tabs", |v: &mut TabPanel<TabHandle>| {
        for tab in v.tab_order() {
            if tab.is_empty() {
                v.remove_tab(&tab).unwrap();
            }
        }

        let view = handle.construct()?;
        v.add_tab(handle.clone(), view);
        v.set_active_tab(handle).unwrap();

        Ok(())
    })
    .ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::Other,
            format!(
                "Could not find program {} in session context",
                for_program_name
            ),
        )
    })??;

    Ok(())
}
