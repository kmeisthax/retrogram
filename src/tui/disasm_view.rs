//! Disassembly view

use crate::arch::Architecture;
use crate::memory::{Memory, Pointer};
use crate::project::ProjectDatabase;
use cursive::event::{Callback, Event, EventResult, Key};
use cursive::{Printer, View, XY};
use std::sync::{Arc, RwLock};

/// Renders a disassembly in a TUI context.
pub struct DisassemblyView<AR>
where
    AR: Architecture,
{
    pjdb: Arc<RwLock<ProjectDatabase<AR::PtrVal, AR::Offset>>>,
    prog_name: String,
    bus: Arc<Memory<AR>>,

    /// The index of the region at the top of the screen.
    region_cursor: usize,

    /// The image offset of the top of the screen.
    image_offset_cursor: usize,
}

impl<AR> DisassemblyView<AR>
where
    AR: Architecture,
{
    pub fn new(
        pjdb: Arc<RwLock<ProjectDatabase<AR::PtrVal, AR::Offset>>>,
        prog_name: &str,
        bus: Arc<Memory<AR>>,
    ) -> Self {
        let s = Self {
            pjdb,
            prog_name: prog_name.to_string(),
            bus,
            region_cursor: 0,
            image_offset_cursor: 0,
        };

        s.pjdb
            .write()
            .unwrap()
            .get_database_mut(&s.prog_name)
            .update_indexes();

        s
    }

    /// Move the cursor down by one.
    ///
    /// This is written to wrap the disassembly view around when it gets to the
    /// end.
    pub fn cursor_down(&mut self) {
        let mut next_io = self.image_offset_cursor.checked_add(1);
        let mut next_r = self.region_cursor;

        let mut max_io = self.bus.region_image_size(next_r);
        if max_io.is_none() {
            // Can only happen if we're already out of bounds.
            next_io = Some(0);
            next_r = 0;

            max_io = self.bus.region_image_size(next_r);
            if max_io.is_none() {
                // This is clearly an empty bus.
                return;
            }
        }

        if next_io.is_none() || next_io.unwrap() >= max_io.unwrap() {
            next_io = Some(0);
            next_r += 1;

            if next_r >= self.bus.region_count() {
                next_r = 0;
            }
        }

        self.image_offset_cursor = next_io.unwrap();
        self.region_cursor = next_r;
    }

    /// Move the cursor up by one.
    ///
    /// This is written to wrap the disassembly view around when it gets to the
    /// start.
    pub fn cursor_up(&mut self) {
        let mut next_io = self.image_offset_cursor.checked_sub(1);
        let mut next_r = self.region_cursor;

        let mut max_io = self.bus.region_image_size(next_r);
        if max_io.is_none() {
            // Can only happen if we're already out of bounds.
            next_r = self.bus.region_count().saturating_sub(1);
            max_io = self.bus.region_image_size(next_r);
            if max_io.is_none() {
                // This is clearly an empty bus.
                return;
            }

            next_io = Some(max_io.unwrap().saturating_sub(1));
        }

        if next_io.is_none() {
            next_r = next_r
                .checked_sub(1)
                .unwrap_or_else(|| self.bus.region_count().saturating_sub(1));
            max_io = self.bus.region_image_size(next_r);
            if max_io.is_none() {
                // This is clearly an empty bus.
                return;
            }

            next_io = Some(max_io.unwrap().saturating_sub(1));
        }

        self.image_offset_cursor = next_io.unwrap();
        self.region_cursor = next_r;
    }
}

impl<AR> View for DisassemblyView<AR>
where
    AR: 'static + Architecture,
{
    fn draw(&self, printer: &Printer) {
        let mut ioffset = self.image_offset_cursor;
        let mut roffset = self.region_cursor;

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
                if let Some(data) = self.bus.region_retrieve(roffset, ioffset, 1) {
                    printer.print((0, line), &format!("${:X}: db ${:X}", enc, data[0]));
                } else {
                    printer.print((0, line), &format!("${:X}: ds 1", enc));
                }
            } else {
                printer.print((0, line), &format!("Invalid offset! ${:X}", ioffset));
            }

            ioffset += 1;
        }
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
            Event::Key(Key::Down) => {
                self.cursor_down();
                EventResult::Consumed(Some(Callback::from_fn(|s| {
                    s.refresh();
                })))
            }
            _ => EventResult::Ignored,
        }
    }
}
