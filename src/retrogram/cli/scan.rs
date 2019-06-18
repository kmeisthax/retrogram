//! CLI command: scan

use std::{io, fs};
use std::collections::HashSet;
use std::fmt::Debug;
use crate::retrogram::{project, platform, arch, asm, ast, input, analysis, database, memory, cli};

/// Scan a specific starting PC and add the results of the analysis to the
/// database.
fn scan_pc_for_arch<I, SI, F, P, MV, S, IO, DIS>(db: &mut database::Database<P, S>, start_pc: &memory::Pointer<P>,
        disassembler: &DIS,
        bus: &memory::Memory<P, MV, S, IO>) -> io::Result<()>
    where P: memory::PtrNum<S> + analysis::Mappable + cli::Nameable,
        S: memory::Offset<P>,
        DIS: Fn(&memory::Pointer<P>, &memory::Memory<P, MV, S, IO>) -> (Option<ast::Instruction<I, SI, F, P>>, S, bool,
            bool, Vec<analysis::Reference<P>>) {
    let (orig_asm, xrefs, pc_offset, blocks) = analysis::disassemble_block(start_pc.clone(), bus, disassembler)?;
    
    for block in blocks {
        db.insert_block(block);
    }

    for xref in xrefs {
        if let Some(target) = xref.as_target() {
            if let None = db.pointer_label(&target) {
                db.insert_placeholder_label(target.clone(), xref.kind());
            }

            if let Some(id) = db.find_block_membership(target) {
                let mut xref_offset = S::zero();

                if let Some(block) = db.block(id) {
                    xref_offset = S::try_from(target.as_pointer().clone() - block.as_start().as_pointer().clone()).unwrap_or(S::zero());
                }
                
                if xref_offset > S::zero() {
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
    };

    Ok(())
}

/// Given a program, analyze a given routine with the given memory model and
/// disassembler and populate the database with the results.
/// 
/// This function exists to isolate as much as possible of the top-level scan
/// implementation from generic typing concerns. You call it with a compatible
/// disassembler and memory and the database gets updated as you wished.
/// 
/// TODO: The current set of lifetime bounds preclude the use of zero-copy
/// deserialization. We should figure out a way around that.
fn scan_for_arch<I, SI, F, P, MV, S, IO, DIS, IMP>(prog: &project::Program, start_spec: &str, disassembler: &DIS,
    bus: &memory::Memory<P, MV, S, IO>, importer: Option<&IMP>) -> io::Result<()>
    where for <'dw> P: memory::PtrNum<S> + analysis::Mappable + cli::Nameable + serde::Deserialize<'dw> + serde::Serialize,
        for <'dw> S: memory::Offset<P> + Debug + serde::Deserialize<'dw> + serde::Serialize,
        DIS: Fn(&memory::Pointer<P>, &memory::Memory<P, MV, S, IO>) -> (Option<ast::Instruction<I, SI, F, P>>, S, bool, bool, Vec<analysis::Reference<P>>),
        IMP: Fn(io::BufReader<fs::File>, &mut database::Database<P, S>) -> io::Result<()> {
    let image = prog.iter_images().next().ok_or(io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;

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

    if let Some(importer) = importer {
        db.import_symbols(prog, importer)?;
    }
    
    let start_pc = input::parse_ptr(start_spec, db, bus).expect("Must specify a valid address to analyze");
    println!("Starting scan from {:X}", start_pc);
    scan_pc_for_arch(&mut db, &start_pc, disassembler, bus)?;

    loop {
        let unanalyzed = db.unanalyzed_static_xrefs();

        if unanalyzed.len() == 0 {
            break;
        }

        for xref_id in unanalyzed {
            let mut target_pc = None;
            let xref = db.xref(xref_id);

            if let Some(xref) = xref {
                target_pc = xref.as_target().clone();
            }

            if let Some(target_pc) = target_pc {
                println!("Found missing xref at {:X}", target_pc);
                scan_pc_for_arch(&mut db, &target_pc, disassembler, bus)?;
            }
        }
    }

    pjdb.write(prog.as_database_path())?;

    Ok(())
}

/// Scan a given program for control flow, symbols, and so on.
pub fn scan(prog: &project::Program, start_spec: &str) -> io::Result<()> {
    let platform = prog.platform().ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified platform, analysis cannot continue."))?;
    let arch = prog.arch().or_else(|| platform.default_arch()).ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified architecture, analysis cannot continue."))?;
    let asm = prog.assembler().or_else(|| arch.default_asm()).ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified assembler for architecture, analysis cannot continue."))?;
    let image = prog.iter_images().next().ok_or(io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;
    
    //TODO: how the hell do we use None as the assembler symbol import
    match (arch, platform, asm) {
        (arch::ArchName::LR35902, platform::PlatformName::GB, asm::AssemblerName::RGBDS) => scan_for_arch(prog, start_spec, &arch::lr35902::disassemble, &platform::gb::construct_platform(&mut file, platform::gb::PlatformVariant::MBC5Mapper)?, Some(&database::rgbds::parse_symbol_file)),
        //(arch::ArchName::AARCH32, platform::PlatformName::AGB, _) => scan_for_arch(prog, start_spec, &arch::aarch32::disassemble, &platform::agb::construct_platform(&mut file)?, None),
        _ => return Err(io::Error::new(io::ErrorKind::Other, "The given combination of architecture, platform, and/or assembler are not compatible."))
    }
}