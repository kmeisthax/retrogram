//! Disassembly view

use crate::arch::{Architecture, CompatibleLiteral};
use crate::ast::{Directive, Section};
use crate::memory::Memory;
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
    region_cursor: usize,

    /// The image offset of the top of the screen.
    image_offset_cursor: usize,

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
            region_cursor: 0,
            image_offset_cursor: 0,
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

    /// Get the index and length of the next region.
    ///
    /// If `None`, then the bus is empty.
    pub fn next_region(&self, region: usize) -> Option<(usize, usize)> {
        let mut next_r = region.checked_add(1).unwrap_or(0);
        let mut max_io = self.bus.region_image_size(next_r);
        if max_io.is_none() {
            next_r = 0;
            max_io = self.bus.region_image_size(next_r);
        }

        Some((next_r, max_io?))
    }

    /// Get the index and length of the current region, or if it's invalid,
    /// then the index of some other region.
    ///
    /// This returns a new region index, image offset, and max offset.
    ///
    /// If `None`, then the bus is empty.
    pub fn valid_region(&self) -> Option<(usize, usize, usize)> {
        let mut this_r = self.region_cursor;
        let mut this_i = self.image_offset_cursor;
        let mut max_io = self.bus.region_image_size(this_r);
        if max_io.is_none() {
            this_r = 0;
            this_i = 0;
            max_io = self.bus.region_image_size(this_r);
        }

        Some((this_r, this_i, max_io?))
    }

    /// Get the index and length of the previous region.
    ///
    /// If `None`, then the bus is empty.
    pub fn prev_region(&self, region: usize) -> Option<(usize, usize)> {
        let max_r = self.bus.region_count();
        let mut prev_r = region
            .checked_sub(1)
            .unwrap_or_else(|| max_r.saturating_sub(1));
        let mut max_io = self.bus.region_image_size(prev_r);
        if max_io.is_none() {
            prev_r = max_r.saturating_sub(1);
            max_io = self.bus.region_image_size(prev_r);
        }

        Some((prev_r, max_io?))
    }

    /// Calculate the region and image offset indicies some number of bytes
    /// ahead.
    ///
    /// If `None`, then the bus is empty.
    pub fn scroll_forward_by_offset(&mut self, mut amount: usize) -> Option<(usize, usize)> {
        let (mut next_r, mut next_io, mut max_io) = self.valid_region()?;

        while amount > 0 {
            let remaining_io = max_io.saturating_sub(next_io);
            if remaining_io < amount {
                let (r_plus, r_plus_max_io) = self.next_region(next_r).unwrap();

                max_io = r_plus_max_io;
                next_r = r_plus;
                next_io = 0;
                amount = amount.saturating_sub(remaining_io);
            } else {
                next_io += amount;
                amount = 0;
            }
        }

        Some((next_r, next_io))
    }

    /// Move the cursor down by one.
    ///
    /// This is written to wrap the disassembly view around when it gets to the
    /// end.
    pub fn cursor_down(&mut self) {
        if let Some((next_r, next_io)) = self.scroll_forward_by_offset(1) {
            self.region_cursor = next_r;
            self.image_offset_cursor = next_io;
        }
    }

    /// Move the cursor down by one page.
    ///
    /// This is written to wrap the disassembly view around when it gets to the
    /// end.
    pub fn page_down(&mut self) {
        if let Some((next_r, next_io)) = self.scroll_forward_by_offset(self.size.y) {
            self.region_cursor = next_r;
            self.image_offset_cursor = next_io;
        }
    }

    /// Calculate the region and image offset indicies some number of bytes
    /// behind.
    ///
    /// If `None`, then the bus is empty.
    pub fn scroll_backward_by_offset(&mut self, mut amount: usize) -> Option<(usize, usize)> {
        let (mut next_r, mut next_io, mut _max_io) = self.valid_region()?;

        while amount > 0 {
            if next_io < amount {
                let (r_minus, r_minus_max_io) = self.prev_region(next_r).unwrap();

                next_r = r_minus;
                next_io = r_minus_max_io.saturating_sub(1);
                amount = amount.saturating_sub(next_io);
            } else {
                next_io -= amount;
                amount = 0;
            }
        }

        Some((next_r, next_io))
    }

    /// Move the cursor up by one.
    ///
    /// This is written to wrap the disassembly view around when it gets to the
    /// start.
    pub fn cursor_up(&mut self) {
        if let Some((next_r, next_io)) = self.scroll_backward_by_offset(1) {
            self.region_cursor = next_r;
            self.image_offset_cursor = next_io;
        }
    }

    /// Move the cursor down by one page.
    ///
    /// This is written to wrap the disassembly view around when it gets to the
    /// end.
    pub fn page_up(&mut self) {
        if let Some((next_r, next_io)) = self.scroll_backward_by_offset(self.size.y) {
            self.region_cursor = next_r;
            self.image_offset_cursor = next_io;
        }
    }
}

impl<L, AR, FMT> View for DisassemblyView<L, AR, FMT>
where
    L: 'static + CompatibleLiteral<AR>,
    AR: 'static + Architecture,
    FMT: 'static + Fn(&Section<L, AR::PtrVal, AR::Byte, AR::Offset>) -> String,
    AR::Offset: TryInto<usize>,
    <AR::Offset as TryFrom<u8>>::Error: std::fmt::Debug,
    <AR::Offset as TryInto<usize>>::Error: std::fmt::Debug,
{
    fn draw(&self, printer: &Printer) {
        let mut ioffset = self.image_offset_cursor;
        let mut roffset = self.region_cursor;

        let mut db_lock = self.pjdb.write().unwrap();

        let db = db_lock.get_database_mut(&self.prog_name);

        for line in 0..printer.size.y {
            if self.bus.region_count() == 0 {
                printer.print((0, line), "Bus is empty!");
                break;
            }

            if ioffset >= self.bus.region_image_size(roffset).unwrap() {
                roffset += 1;
                ioffset = 0;
            }

            if roffset >= self.bus.region_count() {
                roffset = 0;
                ioffset = 0;
            }

            if let Some(enc) = self.bus.region_encode_image_offset(roffset, ioffset) {
                if let Some(_block) = db.find_block_membership(&enc).and_then(|bid| db.block(bid)) {
                    let disasm = self.arch.disassemble(&enc, &self.bus);

                    match disasm {
                        Ok(disasm) => {
                            let mut instr_directive: Section<L, AR::PtrVal, AR::Byte, AR::Offset> =
                                Section::new("");
                            instr_directive.append_directive(
                                Directive::EmitInstr(
                                    disasm.as_instr().clone(),
                                    disasm.next_offset(),
                                ),
                                enc.clone(),
                            );

                            let instr = (self.fmt_section)(&instr_directive);
                            printer.print((0, line), &format!("${:X}: {}", enc, instr.trim()));

                            ioffset += disasm.next_offset().try_into().unwrap();
                        }
                        Err(e) => printer.print((0, line), &format!("${:?}: Error ({})", enc, e)),
                    }
                } else if let Some(data) = self.bus.region_retrieve(roffset, ioffset, 1) {
                    let mut data_directive: Section<L, AR::PtrVal, AR::Byte, AR::Offset> =
                        Section::new("");
                    data_directive
                        .append_directive(Directive::EmitData(data.to_vec()), enc.clone());

                    let data = (self.fmt_section)(&data_directive);
                    printer.print((0, line), &format!("${:X}: {}", enc, data.trim()));
                } else {
                    let mut space_directive: Section<L, AR::PtrVal, AR::Byte, AR::Offset> =
                        Section::new("");
                    space_directive.append_directive(
                        Directive::EmitSpace(AR::Offset::try_from(1).unwrap()),
                        enc.clone(),
                    );

                    let space = (self.fmt_section)(&space_directive);
                    printer.print((0, line), &format!("${:X}: {}", enc, space.trim()));
                }
            } else {
                printer.print((0, line), &format!("Invalid offset! ${:X}", ioffset));
            }

            ioffset += 1;
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
