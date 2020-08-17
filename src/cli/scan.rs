//! CLI command: scan

use crate::analysis::{analyze_trace_log, trace_until_fork, Fork, Trace};
use crate::arch::{Architecture, CompatibleLiteral};
use crate::ast::Instruction;
use crate::database::Database;
use crate::memory::{Offset, Pointer};
use crate::reg::{Bitwise, State};
use crate::{analysis, ast, database, input, maths, memory, project, reg};
use clap::ArgMatches;
use num_traits::{One, Zero};
use std::collections::{BinaryHeap, HashSet};
use std::convert::{TryFrom, TryInto};
use std::{fmt, fs, io};

/// Scan a specific starting PC and add the results of the analysis to the
/// database.
fn scan_pc_for_arch<L, AR>(
    db: &mut database::Database<AR::PtrVal, AR::Offset>,
    start_pc: &memory::Pointer<AR::PtrVal>,
    bus: &memory::Memory<AR>,
    arch: AR,
) -> io::Result<()>
where
    L: CompatibleLiteral<AR>,
    AR: Architecture,
    AR::Offset: Offset<AR::PtrVal>, //I shouldn't have to do this.
    reg::Symbolic<AR::Byte>: Default,
    ast::Instruction<L>: Clone,
{
    let (orig_asm, xrefs, pc_offset, blocks, terminating_error) =
        analysis::disassemble_block::<L, AR>(start_pc.clone(), bus, arch);

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

                    iv_offset = iv_offset + AR::Offset::one();
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

                    iv_offset = iv_offset + AR::Offset::one();
                }

                return Err(io::Error::new(io::ErrorKind::Other, format!("Decoding error at {:X} (from {:X}) on value {}", bad_pc, start_pc, values)));
            },
            (Some(ref s), ref e) if *s == AR::Offset::zero() => return Err(io::Error::new(io::ErrorKind::InvalidData, format!("There is no valid code at {:X} due to {}", start_pc, e))),
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
                let mut xref_offset = AR::Offset::zero();

                if let Some(block) = db.block(id) {
                    xref_offset = AR::Offset::try_from(
                        target.as_pointer().clone() - block.as_start().as_pointer().clone(),
                    )
                    .unwrap_or_else(|_| AR::Offset::zero());
                }

                if xref_offset > AR::Offset::zero() {
                    db.split_block(id, xref_offset);
                }
            }
        }

        db.insert_crossreference(xref);
    }

    //TODO: This seems to be polluting the symbol table for no reason.
    if let Some((_dir, loc)) = orig_asm.iter_directives().next() {
        db.insert_placeholder_label(loc.clone(), analysis::ReferenceKind::Unknown);
    }

    Ok(())
}

/// Scan all unanalyzed static crossreferences present within a given database,
/// recursively, until the control graph is maximally extended.
///
/// This function yields false if it's execution yielded no additional code. It
/// will also yield the addresses of any code that threw errors when analyzed.
fn exhaust_all_static_scans<L, AR>(
    db: &mut Database<AR::PtrVal, AR::Offset>,
    bus: &memory::Memory<AR>,
    arch: AR,
) -> (bool, HashSet<Pointer<AR::PtrVal>>)
where
    L: CompatibleLiteral<AR>,
    AR: Architecture,
    AR::Byte: reg::Bitwise + fmt::UpperHex,
    ast::Instruction<L>: Clone,
{
    let mut failed_analysis = HashSet::new();
    let mut any_analysis_done = false;

    loop {
        let unanalyzed = db.unanalyzed_static_xrefs();
        let mut more_analysis_done = false;

        for xref_id in unanalyzed {
            let mut target_pc = None;
            let xref = db.xref(xref_id);

            if let Some(xref) = xref {
                target_pc = xref.as_target().cloned();
            }

            if let Some(target_pc) = target_pc {
                if !failed_analysis.contains(&target_pc) {
                    match scan_pc_for_arch(db, &target_pc, bus, arch) {
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

        any_analysis_done |= more_analysis_done;

        if !more_analysis_done {
            break;
        }
    }

    (any_analysis_done, failed_analysis)
}

/// Given a program database, analyze all blocks in need of dynamic tracing.
///
/// This function yields true if any dynamic analysis was done. You will need
/// to check for any new unanalyzed static references after the tracing has
/// completed.
fn exhaust_all_dynamic_scans<L, AR>(
    db: &mut Database<AR::PtrVal, AR::Offset>,
    bus: &memory::Memory<AR>,
    arch: AR,
) -> analysis::Result<bool, AR>
where
    L: CompatibleLiteral<AR>,
    AR: Architecture,
    AR::Word: TryInto<u64>,
    AR::Byte: TryInto<u64>,
    AR::Offset: TryInto<usize>,
{
    let mut did_trace = false;

    for block_id in db.undertraced_blocks().iter() {
        let block = db.block(*block_id).unwrap();

        eprintln!("Starting trace from {}", block.as_start());

        let mut forks = BinaryHeap::new();
        let mut first_state = State::default();

        first_state.contextualize_self(block.as_start());

        forks.push(Fork::initial_fork(
            block.as_start().as_pointer().clone(),
            first_state,
        ));

        while let Some(fork) = forks.pop() {
            let (branches, pc, state, trace) = fork.into_parts();

            if branches > 5.0 {
                //TODO: better heuristic please
                //TODO: this should retrieve the block's total fork score
                //TODO: what happens if we overtrace a block (say a utility fn)
                //      while it's still in the undertraced list?
                continue;
            }

            eprintln!("Processing trace fork from {}", pc);

            let (new_pc, trace, post_state, prerequisites) =
                trace_until_fork(&pc, trace, bus, &state, arch)?;

            did_trace = true;

            let traced_blocks = analyze_trace_log::<L, AR>(&trace, bus, db, arch)?;

            db.insert_trace_counts(traced_blocks, 1);

            let context_new_pc = post_state.contextualize_pointer(new_pc.clone());

            eprintln!("Prerequisites missing: {:?}", prerequisites);

            for result_fork in Fork::make_forks(
                branches,
                new_pc.clone(),
                post_state,
                bus,
                Trace::begin_at(context_new_pc),
                &prerequisites,
            ) {
                forks.push(result_fork);
            }
        }
    }

    Ok(did_trace)
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
fn scan_for_arch<L, AR, FMTI>(
    prog: &project::Program,
    start_spec: &str,
    bus: &memory::Memory<AR>,
    arch: AR,
    _fmt_i: FMTI, // This shouldn't be necessary...
) -> io::Result<()>
where
    L: CompatibleLiteral<AR>,
    AR: Architecture,
    AR::Word: TryInto<u64>,
    for<'dw> AR::PtrVal: serde::Deserialize<'dw> + serde::Serialize + maths::FromStrRadix,
    AR::Byte: TryInto<u64>,
    for<'dw> AR::Offset: serde::Deserialize<'dw> + serde::Serialize + TryInto<usize>,
    reg::Symbolic<AR::Word>: Bitwise,
    reg::Symbolic<AR::Byte>: Default + Bitwise,
    analysis::Error<AR>: Into<io::Error>,
    FMTI: Fn(&Instruction<L>) -> String,
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

    let start_pc = input::parse_ptr(start_spec, db, bus, arch)
        .expect("Must specify a valid address to analyze");
    eprintln!("Starting scan from {:X}", start_pc);
    match scan_pc_for_arch::<L, AR>(&mut db, &start_pc, bus, arch) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("Initial scan failed due to {}", e);

            return Err(e);
        }
    };

    loop {
        let (did_find_code, _) = exhaust_all_static_scans::<L, AR>(&mut db, bus, arch);
        if !did_find_code {
            break;
        }

        let did_trace_branches = exhaust_all_dynamic_scans::<L, AR>(&mut db, bus, arch)
            .map_err(Into::<io::Error>::into)?;
        if !did_trace_branches {
            break;
        }
    }

    eprintln!("Scan complete, writing database");

    pjdb.write(prog.as_database_path())?;

    Ok(())
}

/// Scan a given program for control flow, symbols, and so on.
pub fn scan<'a>(prog: &project::Program, argv: &ArgMatches<'a>) -> io::Result<()> {
    let start_spec = argv
        .value_of("start_pc")
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Did not provide a start PC"))?;
    let image = prog
        .iter_images()
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;

    with_architecture!(prog, file, |bus, _fmt_s, fmt_i, arch| {
        scan_for_arch(prog, start_spec, bus, arch, fmt_i)
    })
}
