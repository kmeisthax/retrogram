//! High-level CLI routines

use std::{io, fs};
use std::str::FromStr;
use std::convert::TryFrom;
use std::fmt::{Debug, Display, LowerHex, UpperHex};
use std::collections::HashSet;
use crate::retrogram::{asm, ast, arch, analysis, project, platform, input, memory, cli};

fn dis_inner<I, S, F, P, MV, MS, IO, DIS, FMT>(prog: &project::Program,
        start_spec: &str,
        bus: &memory::Memory<P, MV, MS, IO>,
        disassemble: &DIS,
        format_and_print: &FMT) -> io::Result<()>
    where for <'dw> P: memory::PtrNum<MS> + analysis::Mappable + cli::Nameable + serde::Deserialize<'dw>,
        for <'dw> MS: memory::Offset<P> + Debug + serde::Deserialize<'dw>,
        memory::Pointer<P>: Debug,
        I: Clone + Display,
        S: Clone + Display,
        F: Clone + Display,
        DIS: Fn(&memory::Pointer<P>, &memory::Memory<P, MV, MS, IO>) -> (Option<ast::Instruction<I, S, F, P>>, MS, bool, bool, Vec<analysis::Reference<P>>),
        FMT: Fn(&ast::Section<I, S, F, P>) {
    
    let mut pjdb = match project::ProjectDatabase::read(prog.as_database_path()) {
        Ok(pjdb) => pjdb,
        Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
            eprintln!("Creating new database for project");
            project::ProjectDatabase::new()
        },
        Err(e) => return Err(e)
    };

    let mut db = pjdb.get_database_mut(prog.as_name().expect("Projects must be named!"));
    db.update_indexes();

    let start_pc = input::parse_ptr(start_spec, db, bus);
    let start_pc = start_pc.expect("Must specify a valid address to analyze");
    let (orig_asm, xrefs, pc_offset, blocks) = analysis::disassemble_block(start_pc.clone(), bus, disassemble)?;

    for block in blocks {
        db.insert_block(block);
    }

    for xref in xrefs {
        if let Some(target) = xref.as_target() {
            if let None = db.pointer_label(&target) {
                db.insert_placeholder_label(target.clone(), xref.kind());
            }

            if let Some(id) = db.find_block_membership(target) {
                let mut xref_offset = MS::zero();

                if let Some(block) = db.block(id) {
                    xref_offset = MS::try_from(target.as_pointer().clone() - block.as_start().as_pointer().clone()).unwrap_or(MS::zero());
                }
                
                if xref_offset > MS::zero() {
                    db.split_block(id, xref_offset);
                }
            }

            db.insert_crossreference(xref);
        }
    }

    match orig_asm.iter_lines().next() {
        Some(line) => {
            db.insert_placeholder_label(line.source_address().clone(), analysis::ReferenceKind::Unknown);
        },
        _ => {}
    }
    
    let labeled_asm = analysis::replace_labels(orig_asm, db, bus);
    let injected_asm = analysis::inject_labels(labeled_asm, db);
    format_and_print(&injected_asm);

    Ok(())
}

pub fn dis(prog: &project::Program, start_spec: &str) -> io::Result<()> {
    let platform = prog.platform().ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified platform, analysis cannot continue."))?;
    let arch = prog.arch().or_else(|| platform.default_arch()).ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified architecture, analysis cannot continue."))?;
    let asm = prog.assembler().or_else(|| arch.default_asm()).ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified assembler for architecture, analysis cannot continue."))?;
    let image = prog.iter_images().next().ok_or(io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;

    match (arch, platform) {
        (arch::ArchName::LR35902, platform::PlatformName::GB) => dis_inner(prog, start_spec,
            &platform::gb::construct_platform(&mut file, platform::gb::PlatformVariant::MBC5Mapper)?,
            &arch::lr35902::disassemble,
            &|asm| println!("{}", asm::rgbds::RGBDSAstFormatee::wrap(&asm))),
        (arch::ArchName::AARCH32, platform::PlatformName::AGB) => dis_inner(prog, start_spec,
            &platform::agb::construct_platform(&mut file)?,
            &arch::aarch32::disassemble,
            &|asm| println!("")),
        _ => return Err(io::Error::new(io::ErrorKind::Other, "oops"))
    }
}