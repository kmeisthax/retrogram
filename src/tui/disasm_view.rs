//! Disassembly view

use crate::analysis::{replace_labels, Error};
use crate::arch::{Architecture, CompatibleLiteral};
use crate::asm::{AnnotatedText, AnnotationKind, Assembler};
use crate::ast::{Directive, Literal, Section};
use crate::database::Database;
use crate::memory::{Memory, Pointer, Tumbler};
use crate::project::ProjectDatabase;
use cursive::direction::Direction;
use cursive::event::{Callback, Event, EventResult, Key};
use cursive::theme::{BaseColor, Color, ColorStyle, ColorType, PaletteColor};
use cursive::{Printer, View, XY};
use cursive_tabs::TabPanel;
use std::cmp::max;
use std::convert::{TryFrom, TryInto};
use std::io::Write;
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

    /// The location that we are scrolling to.
    scroll: Tumbler,

    /// The location of the cursor on the screen.
    cursor: Tumbler,

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
    <ASM::Literal as Literal>::PtrVal: Clone + Into<AR::PtrVal> + From<AR::PtrVal>,
{
    /// Produce annotated text for a particular location.
    pub fn disasm_at_location(
        &self,
        bus: &Memory<AR>,
        db: &mut Database<AR>,
        loc: &Pointer<AR::PtrVal>,
    ) -> Result<AnnotatedText, Error<AR>> {
        let mut at = AnnotatedText::new();
        write!(
            at.as_annotated_writer(AnnotationKind::Syntactic),
            "${:X}: ",
            loc
        )?;

        if let Some(sym) = db.pointer_symbol(loc).and_then(|id| db.symbol(id)) {
            at.emit_label_decl(self.asm.clone(), sym.as_label())?;
        }

        if db
            .find_block_membership(loc)
            .and_then(|id| db.block(id))
            .is_some()
        {
            let disasm = self.arch.disassemble(loc, bus)?;
            let mut instr_directive = Section::new("");

            instr_directive.append_directive(
                Directive::EmitInstr(disasm.as_instr().clone(), disasm.next_offset()),
                loc.clone(),
            );

            instr_directive = replace_labels(instr_directive, db, bus);

            for (directive, _size) in instr_directive.iter_directives() {
                match directive {
                    Directive::EmitInstr(instr, _size) => at.emit_instr(self.asm.clone(), instr)?,
                    Directive::EmitData(data) => at.emit_data(self.asm.clone(), data)?,
                    Directive::EmitSpace(s) => at.emit_space(self.asm.clone(), s)?,
                    Directive::DeclareLabel(label) => {
                        at.emit_label_decl(self.asm.clone(), label)?
                    }
                    Directive::DeclareOrg(ptr) => {
                        let (ptrval, ctxt) = ptr.clone().into_ptrval_and_contexts();
                        let desired_ptr = Pointer::from_ptrval_and_contexts(ptrval.into(), ctxt);
                        at.emit_org(self.asm.clone(), "", &desired_ptr)?
                    }
                    Directive::DeclareComment(com) => at.emit_comment(self.asm.clone(), com)?,
                }
            }
        } else if let Some(data) = self.bus.read_unit(loc).into_concrete() {
            at.emit_data(self.asm.clone(), &[data]).unwrap();
        } else {
            at.emit_space(self.asm.clone(), 1).unwrap();
        }

        Ok(at)
    }

    /// Determine the number of lines at a particular memory location.
    pub fn disasm_lines_at_location(
        &self,
        bus: &Memory<AR>,
        db: &mut Database<AR>,
        loc: &Pointer<AR::PtrVal>,
    ) -> usize {
        match self.disasm_at_location(bus, db, loc) {
            Ok(at) => {
                let mut lines = 0;

                match at.unannotated_text() {
                    Ok(text) => {
                        for (i, _text_line) in text.split('\n').enumerate() {
                            if i > 0 {
                                lines += 1;
                            }
                        }
                    }
                    Err(_e) => return 1,
                }

                lines
            }
            Err(_e) => 1,
        }
    }

    pub fn new(
        pjdb: Arc<RwLock<ProjectDatabase<AR>>>,
        prog_name: &str,
        bus: Arc<Memory<AR>>,
        arch: AR,
        asm: ASM,
    ) -> Self {
        Self {
            pjdb,
            prog_name: prog_name.to_string(),
            bus,
            scroll: Tumbler::default(),
            cursor: Tumbler::default(),
            size: (0, 0).into(),
            arch,
            asm,
        }
    }

    /// Returns the scroll start, wrap point, and scroll end.
    fn scroll_params(&self, db: &mut Database<AR>) -> Option<(Tumbler, Tumbler, Tumbler)> {
        let scroll_end = self.scroll.scroll_forward_by_lines(
            &self.bus,
            db,
            &mut |bus, db, pos| self.disasm_lines_at_location(bus, db, pos),
            self.size.y.saturating_sub(1),
        )?;
        let wrap_point = Tumbler::default().scroll_backward_by_lines(
            &self.bus,
            db,
            &mut |bus, db, pos| self.disasm_lines_at_location(bus, db, pos),
            1,
        )?;

        Some((self.scroll, wrap_point, scroll_end))
    }

    /// Move the cursor down by one.
    pub fn cursor_down(&mut self) {
        let mut db_lock = self.pjdb.write().unwrap();
        let db = db_lock.get_database_mut(&self.prog_name);

        if let Some((scroll, wrap_point, scroll_end)) = self.scroll_params(db) {
            if let Some(cursor) = self.cursor.scroll_forward_by_lines(
                &self.bus,
                db,
                &mut |bus, db, pos| self.disasm_lines_at_location(bus, db, pos),
                1,
            ) {
                if self.cursor < scroll_end || (scroll_end < scroll && self.cursor >= scroll) {
                    self.cursor = cursor;
                } else if let Some(scroll) = scroll.scroll_forward_by_lines(
                    &self.bus,
                    db,
                    &mut |bus, db, pos| self.disasm_lines_at_location(bus, db, pos),
                    1,
                ) {
                    self.cursor = cursor;
                    self.scroll = scroll;
                }
            }
        }
    }

    /// Move the cursor down by one page.
    pub fn page_down(&mut self) {
        let mut db_lock = self.pjdb.write().unwrap();
        let db = db_lock.get_database_mut(&self.prog_name);

        if let Some((scroll, wrap_point, scroll_end)) = self.scroll_params(db) {
            if self.cursor < scroll_end || (scroll_end < scroll && self.cursor >= scroll) {
                self.cursor = scroll_end;
            } else if let Some(scroll) = scroll.scroll_forward_by_lines(
                &self.bus,
                db,
                &mut |bus, db, pos| self.disasm_lines_at_location(bus, db, pos),
                self.size.y,
            ) {
                self.scroll = scroll;
                self.cursor = scroll
                    .scroll_forward_by_lines(
                        &self.bus,
                        db,
                        &mut |bus, db, pos| self.disasm_lines_at_location(bus, db, pos),
                        self.size.y.saturating_sub(1),
                    )
                    .unwrap();
            }
        }
    }

    /// Move the cursor up by one.
    pub fn cursor_up(&mut self) {
        let mut db_lock = self.pjdb.write().unwrap();
        let db = db_lock.get_database_mut(&self.prog_name);

        if let Some(cursor) = self.cursor.scroll_backward_by_lines(
            &self.bus,
            db,
            &mut |bus, db, pos| self.disasm_lines_at_location(bus, db, pos),
            1,
        ) {
            if self.cursor == self.scroll {
                self.scroll = cursor;
            }
            self.cursor = cursor;
        }
    }

    /// Move the cursor up by one page.
    pub fn page_up(&mut self) {
        let mut db_lock = self.pjdb.write().unwrap();
        let db = db_lock.get_database_mut(&self.prog_name);

        if self.cursor == self.scroll {
            if let Some(cursor) = self.cursor.scroll_backward_by_lines(
                &self.bus,
                db,
                &mut |bus, db, pos| self.disasm_lines_at_location(bus, db, pos),
                self.size.y,
            ) {
                self.cursor = cursor;
                self.scroll = cursor;
            }
        } else {
            self.cursor = self.scroll;
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
    ASM::Literal: CompatibleLiteral<AR> + Clone,
    <ASM::Literal as Literal>::PtrVal: Clone + Into<AR::PtrVal> + From<AR::PtrVal>,
{
    fn draw(&self, printer: &Printer) {
        let mut position = self.scroll;

        let mut db_lock = self.pjdb.write().unwrap();

        let db = db_lock.get_database_mut(&self.prog_name);
        let mut line = 0;

        while line < printer.size.y {
            if self.bus.region_count() == 0 {
                printer.print((0, line), "Bus is empty!");
                break;
            }

            let (at, lines_to_skip) = if let Some(enc) = self.bus.encode_tumbler(position) {
                let lines_at_loc = self.disasm_lines_at_location(&self.bus, db, &enc);
                let lines_to_skip = lines_at_loc.saturating_sub(position.line_index());

                match self.disasm_at_location(&self.bus, db, &enc) {
                    Ok(at) => (at, lines_to_skip),
                    Err(e) => {
                        let mut at = AnnotatedText::new();

                        writeln!(
                            at.as_annotated_writer(AnnotationKind::Error),
                            "Disassembly error! {}",
                            e
                        )
                        .unwrap();

                        (at, lines_to_skip)
                    }
                }
            } else {
                let mut at = AnnotatedText::new();

                writeln!(
                    at.as_annotated_writer(AnnotationKind::Error),
                    "Invalid offset! ${:X}",
                    position.image_index()
                )
                .unwrap();

                (at, 1)
            };

            let mut pos = 0;
            let line_offset = position.line_index();
            let mut seen_lines = 0;
            let mut rendered_lines = 0;

            for res in at.iter_annotations() {
                let (text, annotation) = res.unwrap();
                let foreground: ColorType = match annotation {
                    AnnotationKind::Syntactic => PaletteColor::Primary.into(),
                    AnnotationKind::Comment => Color::Dark(BaseColor::Green).into(),
                    AnnotationKind::Label => Color::Dark(BaseColor::Blue).into(),
                    AnnotationKind::Data => Color::Dark(BaseColor::Magenta).into(),
                    AnnotationKind::Opcode => Color::Dark(BaseColor::Cyan).into(),
                    AnnotationKind::Error => Color::Dark(BaseColor::Red).into(),
                };

                for (i, text_line) in text.split('\n').enumerate() {
                    let (pos_region, pos_io, _) = position.into();
                    let (cur_region, cur_io, cur_line) = self.cursor.into();

                    let background =
                        if pos_region == cur_region && pos_io == cur_io && seen_lines == cur_line {
                            PaletteColor::Shadow
                        } else {
                            PaletteColor::View
                        };

                    if seen_lines >= line_offset {
                        printer.with_color(ColorStyle::new(foreground, background), |printer| {
                            printer.print((pos, line + rendered_lines), text_line);
                        });

                        if i > 0 {
                            rendered_lines += 1;
                            pos = 0;
                        } else {
                            pos += text_line.len();
                        }
                    }

                    if i > 0 {
                        seen_lines += 1;
                    }
                }
            }

            line += max(rendered_lines, 1);

            position = position
                .scroll_forward_by_lines(
                    &self.bus,
                    db,
                    &mut |bus, db, pos| self.disasm_lines_at_location(bus, db, pos),
                    max(lines_to_skip, 1),
                )
                .unwrap();
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
            Event::Key(Key::Tab) => EventResult::Consumed(Some(Callback::from_fn(|s| {
                s.find_name::<TabPanel<String>>("tabs").unwrap().next()
            }))),
            _ => EventResult::Ignored,
        }
    }

    fn take_focus(&mut self, _source: Direction) -> bool {
        true
    }
}
