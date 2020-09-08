//! Disassembly view

use crate::analysis::replace_labels;
use crate::arch::{Architecture, CompatibleLiteral};
use crate::ast::{Directive, Section};
use crate::database::Database;
use crate::memory::{Memory, Pointer, Tumbler};
use crate::project::ProjectDatabase;
use cursive::event::{Callback, Event, EventResult, Key};
use cursive::{Printer, View, XY};
use std::convert::{TryFrom, TryInto};
use std::marker::PhantomData;
use std::sync::{Arc, RwLock};

/// Renders a disassembly in a TUI context.
pub struct DisassemblyView<L, AR, FMT>
where
    L: CompatibleLiteral<AR>,
    AR: Architecture,
    FMT: Fn(&Section<L, AR::PtrVal, AR::Byte, AR::Offset>) -> String,
{
    /// The database repository to pull information from.
    pjdb: Arc<RwLock<ProjectDatabase<AR>>>,

    /// The name of the program to show.
    prog_name: String,

    /// The memory bus containing all of the images to inspect.
    bus: Arc<Memory<AR>>,

    /// The index of the region at the top of the screen.
    position: Tumbler,

    /// The current view size.
    size: XY<usize>,

    /// The section-formatting callback.
    fmt_section: FMT,

    /// The architecture itself.
    arch: AR,

    /// Phantom literal data.
    phantom_literal: PhantomData<L>,
}

impl<L, AR, FMT> DisassemblyView<L, AR, FMT>
where
    L: CompatibleLiteral<AR>,
    L::PtrVal: Into<AR::PtrVal>,
    AR: Architecture,
    FMT: Fn(&Section<L, AR::PtrVal, AR::Byte, AR::Offset>) -> String,
{
    pub fn new(
        pjdb: Arc<RwLock<ProjectDatabase<AR>>>,
        prog_name: &str,
        bus: Arc<Memory<AR>>,
        fmt_section: FMT,
        arch: AR,
    ) -> Self {
        let s = Self {
            pjdb,
            prog_name: prog_name.to_string(),
            bus,
            position: Tumbler::default(),
            size: (0, 0).into(),
            fmt_section,
            arch,
            phantom_literal: PhantomData,
        };

        s.pjdb
            .write()
            .unwrap()
            .get_database_mut(&s.prog_name)
            .update_indexes();

        s
    }

    /// Move the cursor down by one.
    pub fn cursor_down(&mut self) {
        let mut db_lock = self.pjdb.write().unwrap();
        let db = db_lock.get_database_mut(&self.prog_name);

        if let Some(position) = self.position.scroll_forward_by_lines(&self.bus, db, 1) {
            self.position = position;
        }
    }

    /// Move the cursor down by one page.
    pub fn page_down(&mut self) {
        let mut db_lock = self.pjdb.write().unwrap();
        let db = db_lock.get_database_mut(&self.prog_name);

        if let Some(position) = self
            .position
            .scroll_forward_by_lines(&self.bus, db, self.size.y)
        {
            self.position = position;
        }
    }

    /// Move the cursor up by one.
    pub fn cursor_up(&mut self) {
        let mut db_lock = self.pjdb.write().unwrap();
        let db = db_lock.get_database_mut(&self.prog_name);

        if let Some(position) = self.position.scroll_backward_by_lines(&self.bus, db, 1) {
            self.position = position;
        }
    }

    /// Move the cursor down by one page.
    pub fn page_up(&mut self) {
        let mut db_lock = self.pjdb.write().unwrap();
        let db = db_lock.get_database_mut(&self.prog_name);

        if let Some(position) = self
            .position
            .scroll_backward_by_lines(&self.bus, db, self.size.y)
        {
            self.position = position;
        }
    }

    /// Draw an address to the screen.
    fn draw_addr(&self, printer: &Printer, pos: XY<usize>, enc: &Pointer<AR::PtrVal>) -> usize {
        let address = format!("${:X}: ", enc);

        printer.print(pos, &address);

        address.len()
    }

    /// Draw an instruction to the screen.
    fn draw_instr(
        &self,
        printer: &Printer,
        pos: XY<usize>,
        enc: &Pointer<AR::PtrVal>,
        db: &mut Database<AR>,
    ) {
        let disasm = self.arch.disassemble(&enc, &self.bus);

        match disasm {
            Ok(disasm) => {
                let mut instr_directive: Section<L, AR::PtrVal, AR::Byte, AR::Offset> =
                    Section::new("");
                instr_directive.append_directive(
                    Directive::EmitInstr(disasm.as_instr().clone(), disasm.next_offset()),
                    enc.clone(),
                );

                instr_directive = replace_labels(instr_directive, db, &self.bus);

                let instr = (self.fmt_section)(&instr_directive);
                printer.print(pos, &instr.trim().to_string());
            }
            Err(e) => printer.print(pos, &format!("Error ({})", e)),
        }
    }
}

impl<L, AR, FMT> View for DisassemblyView<L, AR, FMT>
where
    L: 'static + CompatibleLiteral<AR>,
    AR: 'static + Architecture,
    L::PtrVal: Into<AR::PtrVal>,
    FMT: 'static + Fn(&Section<L, AR::PtrVal, AR::Byte, AR::Offset>) -> String,
    AR::Offset: TryInto<usize>,
    <AR::Offset as TryFrom<u8>>::Error: std::fmt::Debug,
    <AR::Offset as TryInto<usize>>::Error: std::fmt::Debug,
{
    fn draw(&self, printer: &Printer) {
        let mut position = self.position;

        let mut db_lock = self.pjdb.write().unwrap();

        let mut db = db_lock.get_database_mut(&self.prog_name);

        for line in 0..printer.size.y {
            if self.bus.region_count() == 0 {
                printer.print((0, line), "Bus is empty!");
                break;
            }

            if let Some(enc) = self.bus.encode_tumbler(position) {
                let addr_width = self.draw_addr(printer, XY::new(0, line), &enc);
                if let Some(_block) = db.find_block_membership(&enc).and_then(|bid| db.block(bid)) {
                    self.draw_instr(printer, XY::new(addr_width, line), &enc, &mut db);
                } else if let Some(data) = self.bus.retrieve_at_tumbler(position, 1) {
                    let mut data_directive: Section<L, AR::PtrVal, AR::Byte, AR::Offset> =
                        Section::new("");
                    data_directive
                        .append_directive(Directive::EmitData(data.to_vec()), enc.clone());

                    let data = (self.fmt_section)(&data_directive);
                    printer.print(XY::new(addr_width, line), &data.trim().to_string());
                } else {
                    let mut space_directive: Section<L, AR::PtrVal, AR::Byte, AR::Offset> =
                        Section::new("");
                    space_directive.append_directive(
                        Directive::EmitSpace(AR::Offset::try_from(1).unwrap()),
                        enc.clone(),
                    );

                    let space = (self.fmt_section)(&space_directive);
                    printer.print(XY::new(addr_width, line), &space.trim().to_string());
                }
            } else {
                printer.print(
                    (0, line),
                    &format!("Invalid offset! ${:X}", position.image_index()),
                );
            }

            position = position.scroll_forward_by_lines(&self.bus, db, 1).unwrap();
        }
    }

    fn layout(&mut self, size: XY<usize>) {
        self.size = size;
    }

    fn required_size(&mut self, constraint: XY<usize>) -> XY<usize> {
        constraint
    }

    fn on_event(&mut self, evt: Event) -> EventResult {
        match evt {
            Event::Key(Key::Up) => {
                self.cursor_up();
                EventResult::Consumed(Some(Callback::from_fn(|s| {
                    s.refresh();
                })))
            }
            Event::Key(Key::PageUp) => {
                self.page_up();
                EventResult::Consumed(Some(Callback::from_fn(|s| {
                    s.refresh();
                })))
            }
            Event::Key(Key::Down) => {
                self.cursor_down();
                EventResult::Consumed(Some(Callback::from_fn(|s| {
                    s.refresh();
                })))
            }
            Event::Key(Key::PageDown) => {
                self.page_down();
                EventResult::Consumed(Some(Callback::from_fn(|s| {
                    s.refresh();
                })))
            }
            _ => EventResult::Ignored,
        }
    }
}
