//! CLI command: scan

use crate::{analysis, arch, ast, cli, database, input, maths, memory, platform, project, reg};
use crate::cli::common::resolve_program_config;
use num_traits::{One, Zero};
use std::collections::HashSet;
use std::{fmt, fs, io};

/// Scan a specific starting PC and add the results of the analysis to the
/// database.
fn scan_pc_for_arch<I, SI, F, P, MV, S, IO, DIS>(
    db: &mut database::Database<P, S>,
    start_pc: &memory::Pointer<P>,
    disassembler: &DIS,
    bus: &memory::Memory<P, MV, S, IO>,
) -> io::Result<()>
where
    P: memory::PtrNum<S> + analysis::Mappable + cli::Nameable,
    S: memory::Offset<P> + cli::Nameable + Zero + One,
    IO: One,
    MV: reg::Bitwise + fmt::UpperHex,
    reg::Symbolic<MV>: Default,
    ast::Instruction<I, SI, F, P>: Clone,
    DIS: analysis::Disassembler<I, SI, F, P, MV, S, IO>,
{
    let (orig_asm, xrefs, pc_offset, blocks, terminating_error) =
        analysis::disassemble_block(start_pc.clone(), bus, disassembler);

    if let Some(err) = terminating_error {
        match (pc_offset, err) {
            (Some(pc_offset), analysis::Error::Misinterpretation(size, true)) => {
                let mut values = String::new();
                let bad_pc = start_pc.clone() + pc_offset.clone();
                let mut iv_offset = start_pc.clone() + pc_offset;
                let end_offset = iv_offset.clone() + size;

                while iv_offset < end_offset {
                    //TODO: This generates incorrect results if MV::bound_width is not divisible by four
                    if let Some(nval) = bus.read_unit(&iv_offset).into_concrete() {
                        values = format!("{}{:X}", &values, nval);
                    } else {
                        //TODO: This assumes MV is always u8.
                        values = format!("{}??", &values);
                    }

                    iv_offset = iv_offset + S::one();
                }

                return Err(io::Error::new(io::ErrorKind::Other, format!("Decoding error at {:X} (from {:X}) on value {}", bad_pc, start_pc, values)));
            },
            (Some(pc_offset), analysis::Error::Misinterpretation(size, false)) => { //Little-endian
                let mut values = String::new();
                let bad_pc = start_pc.clone() + pc_offset.clone();
                let mut iv_offset = start_pc.clone() + pc_offset;
                let end_offset = iv_offset.clone() + size;

                while iv_offset < end_offset {
                    //TODO: This generates incorrect results if MV::bound_width is not divisible by four
                    if let Some(nval) = bus.read_unit(&iv_offset).into_concrete() {
                        values = format!("{:X}{}", nval, &values);
                    } else {
                        //TODO: This assumes MV is always u8.
                        values = format!("??{}", &values);
                    }

                    iv_offset = iv_offset + S::one();
                }

                return Err(io::Error::new(io::ErrorKind::Other, format!("Decoding error at {:X} (from {:X}) on value {}", bad_pc, start_pc, values)));
            },
            (Some(ref s), ref e) if *s == S::zero() => return Err(io::Error::new(io::ErrorKind::InvalidData, format!("There is no valid code at {:X} due to {}", start_pc, e))),
            (None, _) => return Err(io::Error::new(io::ErrorKind::Other, format!("Disassembly size cannot be expressed in current type system, caused by analysis of {:X}", start_pc))),
            _ => eprintln!("Improperly terminated block discovered in {:X}", start_pc)
        };
    }

    for block in blocks {
        db.insert_block(block);
    }

    for xref in xrefs {
        if let Some(target) = xref.as_target() {
            if db.pointer_symbol(&target).is_none() {
                db.insert_placeholder_label(target.clone(), xref.kind());
            }

            if let Some(id) = db.find_block_membership(target) {
                let mut xref_offset = S::zero();

                if let Some(block) = db.block(id) {
                    xref_offset = S::try_from(
                        target.as_pointer().clone() - block.as_start().as_pointer().clone(),
                    )
                    .unwrap_or_else(|_| S::zero());
                }

                if xref_offset > S::zero() {
                    db.split_block(id, xref_offset);
                }
            }

            db.insert_crossreference(xref);
        }
    }

    //TODO: This seems to be polluting the symbol table for no reason.
    if let Some((_dir, loc)) = orig_asm.iter_directives().next() {
        db.insert_placeholder_label(loc.clone(), analysis::ReferenceKind::Unknown);
    }

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
fn scan_for_arch<I, SI, F, P, MV, S, IO, DIS, APARSE>(
    prog: &project::Program,
    start_spec: &str,
    disassembler: DIS,
    bus: &memory::Memory<P, MV, S, IO>,
    architectural_ctxt_parse: APARSE,
) -> io::Result<()>
where
    for<'dw> P: memory::PtrNum<S>
        + analysis::Mappable
        + cli::Nameable
        + serde::Deserialize<'dw>
        + serde::Serialize
        + maths::FromStrRadix,
    for<'dw> S:
        memory::Offset<P> + cli::Nameable + serde::Deserialize<'dw> + serde::Serialize + One,
    IO: One,
    MV: reg::Bitwise + fmt::UpperHex,
    reg::Symbolic<MV>: Default,
    ast::Instruction<I, SI, F, P>: Clone,
    DIS: analysis::Disassembler<I, SI, F, P, MV, S, IO>,
    APARSE: FnOnce(&mut &[&str], &mut memory::Pointer<P>) -> Option<()>,
{
    let mut pjdb = match project::ProjectDatabase::read(prog.as_database_path()) {
        Ok(pjdb) => pjdb,
        Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
            eprintln!("Creating new database for project");
            project::ProjectDatabase::new()
        }
        Err(e) => return Err(e),
    };

    let mut db = pjdb.get_database_mut(prog.as_name().expect("Projects must be named!"));
    db.update_indexes();

    let start_pc = input::parse_ptr(start_spec, db, bus, architectural_ctxt_parse)
        .expect("Must specify a valid address to analyze");
    eprintln!("Starting scan from {:X}", start_pc);
    match scan_pc_for_arch(&mut db, &start_pc, &disassembler, bus) {
        Ok(_) => {}
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
                        }
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
    let image = prog
        .iter_images()
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;

    match resolve_program_config(prog)? {
        (arch::ArchName::SM83, platform::PlatformName::GB, _) => scan_for_arch(
            prog,
            start_spec,
            arch::sm83::disassemble,
            &platform::gb::construct_platform(
                &mut file,
                platform::gb::PlatformVariant::MBC5Mapper,
            )?,
            |_, _| Some(()),
        ),
        (arch::ArchName::AARCH32, platform::PlatformName::AGB, _) => scan_for_arch(
            prog,
            start_spec,
            arch::aarch32::disassemble,
            &platform::agb::construct_platform(&mut file)?,
            arch::aarch32::architectural_ctxt_parse,
        ),
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "The given combination of architecture, platform, and/or assembler are not compatible.",
        )),
    }
}
