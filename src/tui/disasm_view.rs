//! Disassembly view

use crate::arch::{Architecture, CompatibleLiteral};
use crate::asm::Assembler;
use crate::memory::{Memory, Tumbler};
use crate::project::ProjectDatabase;
use cursive::event::{Callback, Event, EventResult, Key};
use cursive::{Printer, View, XY};
use std::convert::{TryFrom, TryInto};
use std::sync::{Arc, RwLock};

/// Renders a disassembly in a TUI context.
pub struct DisassemblyView<AR, ASM>
where
    AR: Architecture,
    ASM: Assembler,
    ASM::Literal: CompatibleLiteral<AR>,
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

    /// The architecture itself.
    arch: AR,

    /// The assembler syntax to format things with.
    asm: ASM,
}

impl<AR, ASM> DisassemblyView<AR, ASM>
where
    AR: Architecture,
    ASM: Assembler,
    ASM::Literal: CompatibleLiteral<AR>,
{
    pub fn new(
        pjdb: Arc<RwLock<ProjectDatabase<AR>>>,
        prog_name: &str,
        bus: Arc<Memory<AR>>,
        arch: AR,
        asm: ASM,
    ) -> Self {
        let s = Self {
            pjdb,
            prog_name: prog_name.to_string(),
            bus,
            position: Tumbler::default(),
            size: (0, 0).into(),
            arch,
            asm,
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
}

impl<AR, ASM> View for DisassemblyView<AR, ASM>
where
    AR: 'static + Architecture,
    AR::Offset: TryInto<usize>,
    <AR::Offset as TryFrom<u8>>::Error: std::fmt::Debug,
    <AR::Offset as TryInto<usize>>::Error: std::fmt::Debug,
    ASM: 'static + Assembler,
    ASM::Literal: CompatibleLiteral<AR>,
{
    fn draw(&self, printer: &Printer) {
        let mut position = self.position;

        let mut db_lock = self.pjdb.write().unwrap();

        let db = db_lock.get_database_mut(&self.prog_name);

        for line in 0..printer.size.y {
            if self.bus.region_count() == 0 {
                printer.print((0, line), "Bus is empty!");
                break;
            }

            if let Some(enc) = self.bus.encode_tumbler(position) {
                if let Some(_block) = db.find_block_membership(&enc).and_then(|bid| db.block(bid)) {
                    let disasm = self.arch.disassemble(&enc, &self.bus);

                    match disasm {
                        Ok(disasm) => {
                            let mut instr_data = Vec::new();
                            self.asm
                                .emit_instr(&mut instr_data, &disasm.as_instr())
                                .unwrap();

                            let instr = String::from_utf8(instr_data).unwrap();
                            printer.print((0, line), &format!("${:X}: {}", enc, instr.trim()));
                        }
                        Err(e) => printer.print((0, line), &format!("${:?}: Error ({})", enc, e)),
                    }
                } else if let Some(data) = self.bus.retrieve_at_tumbler(position, 1) {
                    let mut data_data = Vec::new();
                    self.asm.emit_data(&mut data_data, data).unwrap();

                    let data = String::from_utf8(data_data).unwrap();
                    printer.print((0, line), &format!("${:X}: {}", enc, data.trim()));
                } else {
                    let mut space_data = Vec::new();
                    self.asm.emit_space(&mut space_data, 1).unwrap();

                    let space = String::from_utf8(space_data).unwrap();
                    printer.print((0, line), &format!("${:X}: {}", enc, space.trim()));
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
