//! Single-run tracing command

use crate::analysis::{trace_until_fork, Trace, TraceEvent};
use crate::input::parse_ptr;
use crate::project;
use crate::reg::{State, Symbolic};
use clap::{ArgMatches, Values};
use std::cmp::max;
use std::fs;
use std::hash::Hash;
use std::io;
use std::str::FromStr;

/// Parse a bunch of registers from a multiple-value argument into a State.
fn reg_parse<RK, RV, P, MV>(state: &mut State<RK, RV, P, MV>, regs: Values<'_>) -> io::Result<()>
where
    RK: Eq + Hash + FromStr,
    RV: FromStr,
    Symbolic<RV>: From<RV>,
    P: Eq + Hash,
{
    for reg_spec in regs {
        let values = reg_spec.split('=').collect::<Vec<&str>>();
        if let Some(v) = values.get(0..2) {
            let reg: RK = v[0].parse().map_err(|_e| {
                io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("Register {} is invalid", v[0]),
                )
            })?;
            let value: RV = v[1].parse().map_err(|_e| {
                io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("Value {} for register {} is invalid", v[1], v[0]),
                )
            })?;

            state.set_register(reg, Symbolic::from(value))
        }
    }

    Ok(())
}

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
        let mut state = State::default();

        if let Some(regs) = argv.values_of("register") {
            reg_parse(&mut state, regs)?;
        }

        let (_busaddr, trace, _state, missing_reg, missing_mem) =
            trace_until_fork(&start_pc, trace, bus, &state, prereq, tracer)
                .map_err(Into::<io::Error>::into)?;

        let mut pc_list = vec![];
        let mut instr_list = vec![];
        let mut event_list = vec![];

        for event in trace.iter() {
            match event {
                TraceEvent::Execute(pc) => {
                    let disasm = dis(pc, bus).map_err(Into::<io::Error>::into)?;

                    pc_list.push(format!("${:X}", pc));
                    instr_list.push(fmt_i(disasm.as_instr()).to_string());
                    event_list.push("".to_string());
                }
                TraceEvent::RegisterSet(reg, val) => {
                    if let Some(evt) = event_list.last_mut() {
                        if !evt.is_empty() {
                            *evt = format!("{}, ", evt);
                        }

                        *evt = format!("{}={:X}", reg, val);
                    }
                }
                TraceEvent::MemoryWrite(ptr, values) => {
                    if let Some(evt) = event_list.last_mut() {
                        if !evt.is_empty() {
                            *evt = format!("{}, ", evt);
                        }

                        *evt = format!(
                            "{:X}={}",
                            ptr,
                            values
                                .iter()
                                .map(|v| format!("{:X}", v))
                                .collect::<Vec<String>>()
                                .join("")
                        );
                    }
                }
            }
        }

        let pc_width = pc_list.iter().fold(0, |m, s| max(m, s.len()));
        let instr_width = instr_list.iter().fold(0, |m, s| max(m, s.len()));
        let event_width = event_list.iter().fold(0, |m, s| max(m, s.len()));

        for ((pc, instr), event) in pc_list.iter().zip(instr_list.iter()).zip(event_list.iter()) {
            println!(
                "{0:1$} | {2:3$} | {4:5$}",
                pc, pc_width, instr, instr_width, event, event_width
            );
        }

        if !missing_reg.is_empty() {
            println!(
                "Halted on unconstrainted register: {}",
                missing_reg
                    .iter()
                    .map(|m| format!("{}", m))
                    .collect::<Vec<String>>()
                    .join(", ")
            );
        }

        if !missing_mem.is_empty() {
            println!(
                "Halted on unconstrainted memory: {}",
                missing_mem
                    .iter()
                    .map(|m| format!("${:X}", m))
                    .collect::<Vec<String>>()
                    .join(", ")
            );
        }

        Ok(())
    })
}
