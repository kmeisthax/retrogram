//! Disassembly view

use crate::analysis::replace_labels;
use crate::arch::{Architecture, CompatibleLiteral};
use crate::asm::{AnnotatedText, AnnotationKind, Assembler};
use crate::ast::{Directive, Literal, Section};
use crate::database::Database;
use crate::memory::{Memory, Pointer, Tumbler};
use crate::project::ProjectDatabase;
use cursive::event::{Callback, Event, EventResult, Key};
use cursive::theme::{BaseColor, Color, ColorStyle, PaletteColor};
use cursive::{Printer, View, XY};
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
    <ASM::Literal as Literal>::PtrVal: Clone + Into<AR::PtrVal>,
{
    /// Determine how many lines of disassembly exist at a particular memory
    /// location.
    pub fn disasm_lines_at_location(
        &self,
        bus: &Memory<AR>,
        db: &mut Database<AR>,
        loc: &Pointer<AR::PtrVal>,
    ) -> usize {
        if let Some(_) = db.find_block_membership(loc).and_then(|id| db.block(id)) {
            match self.arch.disassemble(loc, bus) {
                Ok(disasm) => {
                    let mut instr_directive = Section::new("");

                    instr_directive.append_directive(
                        Directive::EmitInstr(disasm.as_instr().clone(), disasm.next_offset()),
                        loc.clone(),
                    );

                    instr_directive = replace_labels(instr_directive, db, bus);

                    let mut disasm = Vec::new();

                    for (directive, _size) in instr_directive.iter_directives() {
                        if directive.is_emit_instr() {
                            //TODO: Become errors.
                            self.asm
                                .emit_instr(&mut disasm, directive.as_emit_instr().unwrap().0)
                                .unwrap();
                        }
                    }

                    std::str::from_utf8(&disasm).unwrap().matches("\n").count()
                }
                Err(_e) => 1,
            }
        } else {
            1
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
            position: Tumbler::default(),
            size: (0, 0).into(),
            arch,
            asm,
        }
    }

    /// Move the cursor down by one.
    pub fn cursor_down(&mut self) {
        let mut db_lock = self.pjdb.write().unwrap();
        let db = db_lock.get_database_mut(&self.prog_name);

        if let Some(position) = self.position.scroll_forward_by_lines(
            &self.bus,
            db,
            &mut |bus, db, pos| self.disasm_lines_at_location(bus, db, pos),
            1,
        ) {
            self.position = position;
        }
    }

    /// Move the cursor down by one page.
    pub fn page_down(&mut self) {
        let mut db_lock = self.pjdb.write().unwrap();
        let db = db_lock.get_database_mut(&self.prog_name);

        if let Some(position) = self.position.scroll_forward_by_lines(
            &self.bus,
            db,
            &mut |bus, db, pos| self.disasm_lines_at_location(bus, db, pos),
            self.size.y,
        ) {
            self.position = position;
        }
    }

    /// Move the cursor up by one.
    pub fn cursor_up(&mut self) {
        let mut db_lock = self.pjdb.write().unwrap();
        let db = db_lock.get_database_mut(&self.prog_name);

        if let Some(position) = self.position.scroll_backward_by_lines(
            &self.bus,
            db,
            &mut |bus, db, pos| self.disasm_lines_at_location(bus, db, pos),
            1,
        ) {
            self.position = position;
        }
    }

    /// Move the cursor down by one page.
    pub fn page_up(&mut self) {
        let mut db_lock = self.pjdb.write().unwrap();
        let db = db_lock.get_database_mut(&self.prog_name);

        if let Some(position) = self.position.scroll_backward_by_lines(
            &self.bus,
            db,
            &mut |bus, db, pos| self.disasm_lines_at_location(bus, db, pos),
            self.size.y,
        ) {
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
    ASM::Literal: CompatibleLiteral<AR> + Clone,
    <ASM::Literal as Literal>::PtrVal: Clone + Into<AR::PtrVal>,
{
    fn draw(&self, printer: &Printer) {
        let mut position = self.position;

        let mut db_lock = self.pjdb.write().unwrap();

        let db = db_lock.get_database_mut(&self.prog_name);
        let mut line = 0;

        while line < printer.size.y {
            if self.bus.region_count() == 0 {
                printer.print((0, line), "Bus is empty!");
                break;
            }

            let mut at = AnnotatedText::new();

            if let Some(enc) = self.bus.encode_tumbler(position) {
                at.change_annotation(AnnotationKind::Syntactic);
                write!(at.as_writer(), "${:X}: ", enc);

                if let Some(sym) = db.pointer_symbol(&enc).and_then(|id| db.symbol(id)) {
                    at.emit_label_decl(self.asm.clone(), sym.as_label());
                }

                if let Some(_block) = db.find_block_membership(&enc).and_then(|bid| db.block(bid)) {
                    let disasm = self.arch.disassemble(&enc, &self.bus);

                    match disasm {
                        Ok(disasm) => {
                            let mut instr_directive: Section<
                                ASM::Literal,
                                AR::PtrVal,
                                AR::Byte,
                                AR::Offset,
                            > = Section::new("");

                            instr_directive.append_directive(
                                Directive::EmitInstr(
                                    disasm.as_instr().clone(),
                                    disasm.next_offset(),
                                ),
                                enc.clone(),
                            );

                            instr_directive = replace_labels(instr_directive, db, &self.bus);

                            for (directive, _size) in instr_directive.iter_directives() {
                                if directive.is_emit_instr() {
                                    at.emit_instr(self.asm.clone(), &disasm.as_instr()).unwrap();
                                }
                            }
                        }
                        Err(e) => {
                            at.change_annotation(AnnotationKind::Error);
                            writeln!(at.as_writer(), "Error ({})", e);
                        }
                    }
                } else if let Some(data) = self.bus.retrieve_at_tumbler(position, 1) {
                    at.emit_data(self.asm.clone(), data).unwrap();
                } else {
                    at.emit_space(self.asm.clone(), 1).unwrap();
                }
            } else {
                at.change_annotation(AnnotationKind::Error);
                writeln!(
                    at.as_writer(),
                    "Invalid offset! ${:X}",
                    position.image_index()
                );
            }

            let mut pos = 0;

            for res in at.iter_annotations() {
                let (text, annotation) = res.unwrap();
                let color_style = match annotation {
                    AnnotationKind::Syntactic => {
                        ColorStyle::new(PaletteColor::Primary, PaletteColor::View)
                    }
                    AnnotationKind::Comment => {
                        ColorStyle::new(Color::Dark(BaseColor::Green), PaletteColor::View)
                    }
                    AnnotationKind::Label => {
                        ColorStyle::new(Color::Dark(BaseColor::Blue), PaletteColor::View)
                    }
                    AnnotationKind::Data => {
                        ColorStyle::new(Color::Dark(BaseColor::Yellow), PaletteColor::View)
                    }
                    AnnotationKind::Opcode => {
                        ColorStyle::new(Color::Dark(BaseColor::Cyan), PaletteColor::View)
                    }
                    AnnotationKind::Error => {
                        ColorStyle::new(Color::Dark(BaseColor::Red), PaletteColor::View)
                    }
                };

                for (i, text_line) in text.split('\n').enumerate() {
                    if i > 0 {
                        line += 1;
                        pos = 0;
                    }

                    printer.with_color(color_style, |printer| {
                        printer.print((pos, line), text_line.trim());
                    });

                    if i == 0 {
                        pos += text.trim().len();
                    }
                }
            }

            position = position
                .scroll_forward_by_lines(
                    &self.bus,
                    db,
                    &mut |bus, db, pos| self.disasm_lines_at_location(bus, db, pos),
                    1,
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
            _ => EventResult::Ignored,
        }
    }
}
