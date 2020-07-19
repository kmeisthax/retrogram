//! Single-run tracing command

use crate::analysis::{analyze_trace_log, trace_until_fork, Prerequisite, Trace, TraceEvent};
use crate::arch::{Architecture, CompatibleLiteral};
use crate::ast::Instruction;
use crate::input::parse_ptr;
use crate::maths::{FromStrRadix, Numerical};
use crate::memory::{Memory, Pointer};
use crate::project;
use crate::reg::{State, Symbolic};
use clap::{ArgMatches, Values};
use num::One;
use serde::Deserialize;
use std::cmp::max;
use std::collections::HashSet;
use std::fmt::{Display, UpperHex};
use std::fs;
use std::hash::Hash;
use std::io;
use std::io::{stdin, stdout, BufRead, Write};
use std::str::FromStr;

enum NextAction {
    Trace,
    Ask,
    SetRegister,
    SetMemory,
    Quit,
    Help,
    Invalid,
}

impl NextAction {
    pub fn step(self) -> Self {
        match self {
            NextAction::Trace => NextAction::Ask,
            NextAction::Ask => NextAction::Invalid,
            NextAction::SetRegister => NextAction::Ask,
            NextAction::SetMemory => NextAction::Ask,
            NextAction::Quit => NextAction::Quit,
            NextAction::Help => NextAction::Ask,
            NextAction::Invalid => NextAction::Ask,
        }
    }
}

/// Parse a bunch of registers from a multiple-value argument into a State.
fn reg_parse<RK, RV, P, MV>(state: &mut State<RK, RV, P, MV>, regs: Values<'_>) -> io::Result<()>
where
    RK: Eq + Hash + FromStr,
    RV: Numerical + FromStrRadix,
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
            let value = RV::from_str_radix(v[1], 16).map_err(|_e| {
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

/// Print a tracelog out to the console as a table.
fn print_tracelog<L, AR, IO, FMTI>(
    trace: Trace<AR::Register, AR::Word, AR::PtrVal, AR::Byte>,
    bus: &Memory<AR::PtrVal, AR::Byte, AR::Offset, IO>,
    arch: AR,
    fmt_i: FMTI,
) -> io::Result<()>
where
    L: CompatibleLiteral<AR>,
    AR: Architecture,
    IO: One,
    FMTI: Fn(&Instruction<L>) -> String,
{
    let mut pc_list = vec![];
    let mut instr_list = vec![];
    let mut event_list = vec![];

    for event in trace.iter() {
        match event {
            TraceEvent::Execute(pc) => {
                let disasm = arch.disassemble(pc, bus).map_err(Into::<io::Error>::into)?;

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

    Ok(())
}

/// Print out the prerequisites that a trace operation stopped on.
fn print_prereqs<RK, I, P, MV, S>(halt_pc: Pointer<P>, missing: &[Prerequisite<RK, I, P, MV, S>])
where
    RK: Display,
    P: UpperHex,
{
    println!("Halted at ${:X}", halt_pc);

    if !missing.is_empty() {
        let missing_reg = missing
            .iter()
            .filter_map(|p| match p {
                Prerequisite::Register {
                    register,
                    mask: _mask,
                } => Some(format!("{}", register)),
                _ => None,
            })
            .collect::<Vec<String>>()
            .join(", ");

        if !missing_reg.is_empty() {
            println!("Halted on unconstrained register: {}", missing_reg);
        }

        let missing_mem = missing
            .iter()
            .filter_map(|p| match p {
                Prerequisite::Memory {
                    ptr,
                    length: _length,
                    mask: _mask,
                } => Some(format!("${:X}", ptr)),
                _ => None,
            })
            .collect::<Vec<String>>()
            .join(", ");

        if !missing_mem.is_empty() {
            println!("Halted on unconstrained memory: {}", missing_mem);
        }
    }
}

/// Prompt the user for the next action.
fn get_next_action() -> io::Result<NextAction> {
    print!("What to do next? [T/R/M/Q/?]: ");

    let mut buf = String::new();

    {
        stdout().lock().flush()?;
        stdin().lock().read_line(&mut buf)?;
    }

    Ok(match buf.trim().to_lowercase().as_str() {
        "t" => NextAction::Trace,
        "r" => NextAction::SetRegister,
        "m" => NextAction::SetMemory,
        "q" => NextAction::Quit,
        "?" => NextAction::Help,
        _ => NextAction::Invalid,
    })
}

/// Collect a register key and value from the user and set that register to
/// that value in the state.
fn set_register<RK, RV, P, MV, S>(
    state: &mut State<RK, RV, P, MV>,
    missing: &[Prerequisite<RK, RV, P, MV, S>],
) -> io::Result<()>
where
    RK: Eq + Hash + Display + FromStr,
    RV: FromStrRadix,
    Symbolic<RV>: From<RV>,
    P: Eq + Hash,
{
    let options = missing
        .iter()
        .filter_map(|m| match m {
            Prerequisite::Register {
                register,
                mask: _mask,
            } => Some(format!("{}", register)),
            _ => None,
        })
        .collect::<Vec<String>>()
        .join("/");

    let rkey = {
        print!("Register name? [{}]: ", options);

        let mut rkey_buf = String::new();
        {
            stdout().lock().flush()?;
            stdin().lock().read_line(&mut rkey_buf)?;
        }

        match RK::from_str(&rkey_buf.trim()) {
            Ok(r) => r,
            Err(_) => {
                println!("Invalid register name");
                return Ok(());
            }
        }
    };

    let rval = {
        print!("Register value? [$ABCD]: $");

        let mut rval_buf = String::new();
        {
            stdout().lock().flush()?;
            stdin().lock().read_line(&mut rval_buf)?;
        }

        match RV::from_str_radix(&rval_buf.trim(), 16) {
            Ok(r) => r,
            Err(_) => {
                println!("Invalid register value");
                return Ok(());
            }
        }
    };

    state.set_register(rkey, Symbolic::from(rval));

    Ok(())
}

/// Collect a memory pointer and value from the user and set that address to
/// that value in the state memory.
fn set_memory<RK, RV, P, MV, S>(
    state: &mut State<RK, RV, P, MV>,
    missing: &[Prerequisite<RK, RV, P, MV, S>],
) -> io::Result<()>
where
    RK: Eq + Hash,
    MV: FromStrRadix,
    Symbolic<MV>: From<MV>,
    P: Eq + Hash + Display,
    Pointer<P>: FromStrRadix,
{
    let options = missing
        .iter()
        .filter_map(|m| match m {
            Prerequisite::Memory {
                ptr,
                mask: _mask,
                length: _length,
            } => Some(format!("{}", ptr)),
            _ => None,
        })
        .collect::<Vec<String>>()
        .join("/");

    let ptr = {
        print!("Memory address? [{}]: ", options);

        let mut ptr_buf = String::new();
        {
            stdout().lock().flush()?;
            stdin().lock().read_line(&mut ptr_buf)?;
        }

        match Pointer::from_str_radix(&ptr_buf.trim(), 16) {
            Ok(r) => r,
            Err(_) => {
                println!("Invalid memory location");
                return Ok(());
            }
        }
    };

    let rval = {
        print!("Memory value? [$ABCD]: $");

        let mut rval_buf = String::new();
        {
            stdout().lock().flush()?;
            stdin().lock().read_line(&mut rval_buf)?;
        }

        match MV::from_str_radix(&rval_buf.trim(), 16) {
            Ok(r) => r,
            Err(_) => {
                println!("Invalid register value");
                return Ok(());
            }
        }
    };

    state.set_memory(ptr, Symbolic::from(rval));

    Ok(())
}

fn trace_for_arch<'a, L, AR, IO, FMTI>(
    prog: &project::Program,
    argv: &ArgMatches<'a>,
    start_spec: &str,
    bus: &Memory<AR::PtrVal, AR::Byte, AR::Offset, IO>,
    fmt_i: FMTI,
    arch: AR,
) -> io::Result<()>
where
    AR: Architecture,
    AR::Word: FromStrRadix,
    AR::Byte: FromStrRadix,
    for<'dw> AR::PtrVal: Deserialize<'dw> + FromStrRadix,
    for<'dw> AR::Offset: Deserialize<'dw>,
    L: CompatibleLiteral<AR>,
    IO: One,
    FMTI: Copy + Fn(&Instruction<L>) -> String,
{
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

    let mut pc = parse_ptr(start_spec, db, bus, arch).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "Must specify a valid address to analyze",
        )
    })?;

    let mut action = NextAction::Trace;
    let mut state = State::default();
    let mut missing = Vec::new();
    let mut traced_blocks = HashSet::new();

    if let Some(regs) = argv.values_of("register") {
        reg_parse(&mut state, regs)?;
    }

    loop {
        match action {
            NextAction::Trace => {
                let (halt_pc, trace, new_state, new_missing) =
                    trace_until_fork(&pc, Trace::begin_at(pc.clone()), bus, &state, arch)
                        .map_err(Into::<io::Error>::into)?;

                traced_blocks = traced_blocks
                    .union(
                        &analyze_trace_log::<L, AR, IO>(&trace, bus, db, arch)
                            .map_err(Into::<io::Error>::into)?,
                    )
                    .copied()
                    .collect();

                state = new_state;
                missing = new_missing;
                pc = halt_pc.clone();

                print_tracelog(trace, bus, arch, fmt_i)?;

                print_prereqs(halt_pc, &missing);
            }
            NextAction::Ask => {
                action = get_next_action()?;
                continue;
            }
            NextAction::SetRegister => set_register(&mut state, &missing)?,
            NextAction::SetMemory => set_memory(&mut state, &missing)?,
            NextAction::Quit => break,
            NextAction::Help => {
                println!("All of the listed prerequisites must be resolved before continuing.");
                println!("Use the following commands to do so:");
                println!(" R = Resolve a register to a concrete value");
                println!(" M = Resolve memory to a concrete value");
                println!(" T = Continue trace (if all values have been resolved)");
                println!(" ? = Print help screen");
                println!(" Q = End tracing");
            }
            NextAction::Invalid => println!("Please type a valid command."),
        }

        action = action.step();
    }

    db.insert_trace_counts(traced_blocks, 1);

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

    with_architecture!(prog, file, |bus, _fmt_s, fmt_i, arch| {
        trace_for_arch(prog, argv, start_spec, bus, fmt_i, arch)
    })
}
