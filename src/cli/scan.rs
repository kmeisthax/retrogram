//! CLI command: scan

use crate::analysis::{
    analyze_trace_log, trace_until_fork, Disassembler, Fork, Mappable, PrerequisiteAnalysis, Trace,
    Tracer,
};
use crate::cli::Nameable;
use crate::database::Database;
use crate::maths::{Numerical, Popcount};
use crate::memory::{Offset, Pointer, PtrNum};
use crate::reg::{Bitwise, State, Symbolic};
use crate::{analysis, ast, cli, database, input, maths, memory, project, reg};
use clap::ArgMatches;
use num_traits::{One, Zero};
use std::collections::{BinaryHeap, HashSet};
use std::convert::TryInto;
use std::hash::Hash;
use std::{fmt, fs, io};

/// Scan a specific starting PC and add the results of the analysis to the
/// database.
fn scan_pc_for_arch<L, P, MV, S, IO, DIS>(
    db: &mut database::Database<P, S>,
    start_pc: &memory::Pointer<P>,
    disassembler: &DIS,
    bus: &memory::Memory<P, MV, S, IO>,
) -> io::Result<()>
where
    L: ast::Literal<PtrVal = P>,
    P: memory::PtrNum<S> + analysis::Mappable + cli::Nameable,
    S: memory::Offset<P> + cli::Nameable + Zero + One,
    IO: One,
    MV: reg::Bitwise + fmt::UpperHex,
    reg::Symbolic<MV>: Default,
    ast::Instruction<L>: Clone,
    DIS: analysis::Disassembler<L, P, MV, S, IO>,
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

/// Scan all unanalyzed static crossreferences present within a given database,
/// recursively, until the control graph is maximally extended.
///
/// This function yields false if it's execution yielded no additional code. It
/// will also yield the addresses of any code that threw errors when analyzed.
fn exhaust_all_static_scans<L, P, MV, S, IO, DIS>(
    db: &mut Database<P, S>,
    bus: &memory::Memory<P, MV, S, IO>,
    disassembler: &DIS,
) -> (bool, HashSet<Pointer<P>>)
where
    L: ast::Literal<PtrVal = P>,
    P: PtrNum<S> + Mappable + Nameable,
    S: Offset<P> + Nameable + Zero + One,
    IO: One,
    MV: reg::Bitwise + fmt::UpperHex,
    reg::Symbolic<MV>: Default,
    ast::Instruction<L>: Clone,
    DIS: analysis::Disassembler<L, P, MV, S, IO>,
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
                    match scan_pc_for_arch(db, &target_pc, &disassembler, bus) {
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
fn exhaust_all_dynamic_scans<L, RK, I, P, MV, S, IO, PREREQ, TRACER, DISASM>(
    db: &mut Database<P, S>,
    bus: &memory::Memory<P, MV, S, IO>,
    prereq: &PREREQ,
    tracer: &TRACER,
    disasm: &DISASM,
) -> analysis::Result<bool, P, S>
where
    L: ast::Literal<PtrVal = P>,
    RK: Mappable,
    I: Bitwise + Numerical + TryInto<u64> + Popcount<Output = I>,
    P: Mappable + PtrNum<S> + Numerical,
    MV: Bitwise + Numerical + TryInto<u64> + Popcount<Output = MV>,
    S: Offset<P> + TryInto<usize>,
    IO: One,
    Symbolic<I>: Bitwise,
    Symbolic<MV>: Bitwise,
    Fork<RK, I, P, MV>: Ord,
    State<RK, I, P, MV>: Clone + Eq + Hash,
    PREREQ: PrerequisiteAnalysis<RK, I, P, MV, S, IO>,
    TRACER: Tracer<RK, I, P, MV, S, IO>,
    DISASM: Disassembler<L, P, MV, S, IO>,
{
    let mut did_trace = false;

    for block_id in db.undertraced_blocks().iter() {
        let block = db.block(*block_id).unwrap();
        let mut forks = BinaryHeap::new();

        forks.push(Fork::initial_fork(
            block.as_start().clone(),
            Default::default(),
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

            let (new_pc, trace, post_state, prerequisites) =
                trace_until_fork(&pc, trace, bus, &state, prereq, tracer)?;

            did_trace = true;

            let traced_blocks = analyze_trace_log(&trace, bus, db, disasm)?;

            db.insert_trace_counts(traced_blocks, 1);

            for result_fork in Fork::make_forks(
                branches,
                new_pc.clone(),
                post_state,
                bus,
                Trace::begin_at(new_pc),
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
fn scan_for_arch<L, RK, I, P, MV, S, IO, DIS, APARSE, PREREQ, TRACER>(
    prog: &project::Program,
    start_spec: &str,
    disassembler: DIS,
    bus: &memory::Memory<P, MV, S, IO>,
    architectural_ctxt_parse: APARSE,
    prereq: PREREQ,
    tracer: TRACER,
) -> io::Result<()>
where
    L: ast::Literal<PtrVal = P>,
    RK: Mappable,
    I: Bitwise + Numerical + TryInto<u64> + Popcount<Output = I>,
    for<'dw> P: memory::PtrNum<S>
        + analysis::Mappable
        + cli::Nameable
        + serde::Deserialize<'dw>
        + serde::Serialize
        + maths::FromStrRadix
        + Numerical,
    MV: Bitwise + Numerical + TryInto<u64> + Popcount<Output = MV> + fmt::UpperHex,
    for<'dw> S: memory::Offset<P>
        + cli::Nameable
        + serde::Deserialize<'dw>
        + serde::Serialize
        + One
        + TryInto<usize>,
    IO: One,
    reg::Symbolic<I>: Bitwise,
    reg::Symbolic<MV>: Default + Bitwise,
    ast::Instruction<L>: Clone,
    Fork<RK, I, P, MV>: Ord,
    State<RK, I, P, MV>: Clone + Eq + Hash,
    DIS: analysis::Disassembler<L, P, MV, S, IO>,
    PREREQ: PrerequisiteAnalysis<RK, I, P, MV, S, IO>,
    TRACER: Tracer<RK, I, P, MV, S, IO>,
    APARSE: FnOnce(&mut &[&str], &mut memory::Pointer<P>) -> Option<()>,
    analysis::Error<P, S>: Into<io::Error>,
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

    loop {
        let (did_find_code, _) = exhaust_all_static_scans(&mut db, bus, &disassembler);
        if !did_find_code {
            break;
        }

        let did_trace_branches =
            exhaust_all_dynamic_scans(&mut db, bus, &prereq, &tracer, &disassembler)
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

    with_architecture!(prog, file, |bus,
                                    dis,
                                    _fmt_s,
                                    _fmt_i,
                                    aparse,
                                    prereq,
                                    tracer| {
        scan_for_arch(prog, start_spec, dis, bus, aparse, prereq, tracer)
    })
}
