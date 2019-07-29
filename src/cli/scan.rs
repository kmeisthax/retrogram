//! CLI command: scan

use std::{io, fs};
use std::collections::HashSet;
use num_traits::Zero;
use crate::{project, platform, arch, ast, input, analysis, database, memory, cli};

/// Scan a specific starting PC and add the results of the analysis to the
/// database.
fn scan_pc_for_arch<I, SI, F, P, MV, S, IO, DIS>(db: &mut database::Database<P, S>, start_pc: &memory::Pointer<P>,
        disassembler: &DIS,
        bus: &memory::Memory<P, MV, S, IO>) -> io::Result<()>
    where P: memory::PtrNum<S> + analysis::Mappable + cli::Nameable,
        S: memory::Offset<P> + cli::Nameable + Zero,
        DIS: Fn(&memory::Pointer<P>, &memory::Memory<P, MV, S, IO>) -> (Option<ast::Instruction<I, SI, F, P>>, S, bool,
            bool, Vec<analysis::Reference<P>>) {
    let (orig_asm, xrefs, pc_offset, blocks, is_improperly_ended) = analysis::disassemble_block(start_pc.clone(), bus, disassembler)?;

    if is_improperly_ended {
        if pc_offset == Some(S::zero()) {
            return Err(io::Error::new(io::ErrorKind::InvalidData, format!("There is no valid code at {:X}", start_pc)));
        } else if pc_offset == None {
            return Err(io::Error::new(io::ErrorKind::Other, format!("Disassembly size cannot be expressed in current type system, caused by analysis of {:X}", start_pc)));
        } else {
            eprintln!("Improperly terminated block discovered in {:X}", start_pc);
        }
    }
    
    for block in blocks {
        db.insert_block(block);
    }

    for xref in xrefs {
        if let Some(target) = xref.as_target() {
            if let None = db.pointer_symbol(&target) {
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

    //TODO: This seems to be polluting the symbol table for no reason.
    match orig_asm.iter_directives().next() {
        Some((_dir, loc)) => {
            db.insert_placeholder_label(loc.clone(), analysis::ReferenceKind::Unknown);
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
fn scan_for_arch<I, SI, F, P, MV, S, IO, DIS, APARSE>(prog: &project::Program, start_spec: &str, disassembler: DIS,
    bus: &memory::Memory<P, MV, S, IO>, architectural_ctxt_parse: APARSE) -> io::Result<()>
    where for <'dw> P: memory::PtrNum<S> + analysis::Mappable + cli::Nameable + serde::Deserialize<'dw> + serde::Serialize,
        for <'dw> S: memory::Offset<P> + cli::Nameable + serde::Deserialize<'dw> + serde::Serialize,
        DIS: Fn(&memory::Pointer<P>, &memory::Memory<P, MV, S, IO>) -> (Option<ast::Instruction<I, SI, F, P>>, S, bool, bool, Vec<analysis::Reference<P>>),
        APARSE: FnOnce(&mut &[&str], &mut memory::Pointer<P>) -> Option<()> {

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
    
    let start_pc = input::parse_ptr(start_spec, db, bus, architectural_ctxt_parse).expect("Must specify a valid address to analyze");
    eprintln!("Starting scan from {:X}", start_pc);
    match scan_pc_for_arch(&mut db, &start_pc, &disassembler, bus) {
        Ok(_) => {},
        Err(e) => {
            eprintln!("Initial scan failed due to {}", e);

            return Err(e);
        }
    };

    let mut failed_analysis = HashSet::new();

    loop {
        let unanalyzed = db.unanalyzed_static_xrefs();
        let mut more_analysis_done = false;

        for xref_id in unanalyzed {
            let mut target_pc = None;
            let xref = db.xref(xref_id);

            if let Some(xref) = xref {
                target_pc = xref.as_target().clone();
            }

            if let Some(target_pc) = target_pc {
                if !failed_analysis.contains(&target_pc) {
                    match scan_pc_for_arch(&mut db, &target_pc, &disassembler, bus) {
                        Ok(_) => {
                            eprintln!("Found additional code at {:X}", target_pc);
                            more_analysis_done = true;
                        },
                        Err(e) => {
                            eprintln!("{}", e);

                            failed_analysis.insert(target_pc);
                        }
                    }
                }
            }
        }

        if !more_analysis_done {
            break;
        }
    }

    eprintln!("Scan complete, writing database");

    pjdb.write(prog.as_database_path())?;

    Ok(())
}

/// Scan a given program for control flow, symbols, and so on.
pub fn scan(prog: &project::Program, start_spec: &str) -> io::Result<()> {
    let platform = prog.platform().ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified platform, analysis cannot continue."))?;
    let arch = prog.arch().or_else(|| platform.default_arch()).ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified architecture, analysis cannot continue."))?;
    let image = prog.iter_images().next().ok_or(io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;
    
    match (arch, platform) {
        (arch::ArchName::SM83, platform::PlatformName::GB) =>
            scan_for_arch(prog, start_spec, arch::sm83::disassemble,
                &platform::gb::construct_platform(&mut file, platform::gb::PlatformVariant::MBC5Mapper)?,
                |_, _| Some(())),
        (arch::ArchName::AARCH32, platform::PlatformName::AGB) =>
            scan_for_arch(prog, start_spec, arch::aarch32::disassemble,
                &platform::agb::construct_platform(&mut file)?,
                arch::aarch32::architectural_ctxt_parse),
        _ => return Err(io::Error::new(io::ErrorKind::Other, "The given combination of architecture, platform, and/or assembler are not compatible."))
    }
}