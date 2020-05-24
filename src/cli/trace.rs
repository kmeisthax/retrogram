//! Single-run tracing command

use crate::analysis::{trace_until_fork, Trace};
use crate::input::parse_ptr;
use crate::project;
use crate::reg::State;
use clap::ArgMatches;
use std::fs;
use std::io;

/// Trace execution of a particular program and display the results to the
/// user.
///
/// The `start_spec` is provided by the user and is interpreted by the program's
/// architecture to produce a valid start pointer.
pub fn trace<'a>(prog: &project::Program, argv: &ArgMatches<'a>) -> io::Result<()> {
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
                                    fmt_i,
                                    aparse,
                                    prereq,
                                    tracer| {
        let mut pjdb = match project::ProjectDatabase::read(prog.as_database_path()) {
            Ok(pjdb) => pjdb,
            Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
                eprintln!("Creating new database for project");
                project::ProjectDatabase::new()
            }
            Err(e) => return Err(e),
        };

        let db = pjdb.get_database_mut(prog.as_name().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                "You did not specify a name for the program to disassemble.",
            )
        })?);
        db.update_indexes();

        let start_pc = parse_ptr(start_spec, db, bus, aparse).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                "Must specify a valid address to analyze",
            )
        })?;
        let trace = Trace::begin_at(start_pc.clone());
        let state = State::default();

        let (_busaddr, trace, _state) =
            trace_until_fork(&start_pc, trace, bus, &state, prereq, tracer)
                .map_err(Into::<io::Error>::into)?;

        for pc in trace.iter() {
            let disasm = dis(pc, bus).map_err(Into::<io::Error>::into)?;
            println!("{}: {}", pc, fmt_i(disasm.as_instr()));
        }

        Ok(())
    })
}
