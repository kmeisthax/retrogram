//! CLI command: scan

use crate::arch::{Architecture, CompatibleLiteral};
use crate::asm::Assembler;
use crate::cli::common::reg_parse;
use crate::database::ProjectDatabase;
use crate::maths::FromStrRadix;
use crate::memory::{Memory, Offset};
use crate::platform::Platform;
use crate::project::{Program, Project};
use crate::queue::{start_analysis_queue, Command, Response};
use crate::reg::{Bitwise, State};
use crate::{analysis, ast, input, maths, memory, reg};
use clap::ArgMatches;
use num_traits::{One, Zero};
use std::convert::TryInto;
use std::sync::{Arc, RwLock};
use std::{fs, io};

/// Scan a specific starting PC and add the results of the analysis to the
/// database.
fn describe_scan_error<L, AR>(
    bus: &memory::Memory<AR>,
    start_pc: &memory::Pointer<AR::PtrVal>,
    pc_offset: Option<AR::Offset>,
    err: analysis::Error<AR>,
) -> String
where
    L: CompatibleLiteral<AR>,
    AR: Architecture,
    AR::Offset: Offset<AR::PtrVal>, //I shouldn't have to do this.
    reg::Symbolic<AR::Byte>: Default,
    ast::Instruction<L>: Clone,
{
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

            format!("Decoding error at {:X} (from {:X}) on value {}", bad_pc, start_pc, values)
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

            format!("Decoding error at {:X} (from {:X}) on value {}", bad_pc, start_pc, values)
        },
        (Some(ref s), ref e) if *s == AR::Offset::zero() => format!("There is no valid code at {:X} due to {}", start_pc, e),
        (None, _) => format!("Disassembly size cannot be expressed in current type system, caused by analysis of {:X}", start_pc),
        _ => format!("Improperly terminated block discovered in {:X}", start_pc)
    }
}

fn read_shareable_db(
    project: &mut Project,
    prog: &Program,
) -> io::Result<Arc<RwLock<ProjectDatabase>>> {
    let project_path = project.implicit_path()?;
    let database_path = prog.as_database_path().to_path(&project_path);
    let mut maybe_file = fs::File::open(prog.as_database_path().to_path(database_path));
    let pjdb: io::Result<ProjectDatabase> = match maybe_file {
        Ok(ref mut file) => ProjectDatabase::read(project, file).map_err(|e| e.into()),
        Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
            eprintln!("Creating new database for project");
            Ok(ProjectDatabase::new())
        }
        Err(e) => Err(e),
    };

    Ok(Arc::new(RwLock::new(pjdb?)))
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
fn scan_for_arch<'a, AR, ASM>(
    project: &mut Project,
    prog: &Program,
    start_spec: &str,
    bus: Arc<Memory<AR>>,
    arch: AR,
    _asm: ASM,
    argv: &ArgMatches<'a>,
) -> io::Result<()>
where
    AR: Architecture + 'static,
    AR::Word: TryInto<u64> + FromStrRadix,
    for<'dw> AR::PtrVal: serde::Deserialize<'dw> + serde::Serialize + maths::FromStrRadix,
    AR::Byte: TryInto<u64>,
    for<'dw> AR::Offset: serde::Deserialize<'dw> + serde::Serialize + TryInto<usize>,
    reg::Symbolic<AR::Word>: Bitwise,
    reg::Symbolic<AR::Byte>: Default + Bitwise,
    analysis::Error<AR>: Into<io::Error>,
    ASM: Assembler,
    ASM::Literal: CompatibleLiteral<AR>,
    Memory<AR>: Send + Sync,
{
    let pjdb = read_shareable_db(project, prog)?;
    let program_name = prog.as_name().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "You did not specify a name for the program to disassemble.",
        )
    })?;

    let (cmd_send, resp_recv) = start_analysis_queue::<ASM::Literal, AR>(
        arch,
        pjdb.clone(),
        program_name.to_string(),
        bus.clone(),
    );

    let mut db_lock = pjdb.write().unwrap();
    let db = db_lock.get_database_mut::<AR>(program_name).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "The architecture of the current program's database does not match the program's architecture."
        )
    })?;

    let is_tracing_allowed = argv.is_present("dynamic");
    let mut poweron_state = State::<AR>::default();

    if let Some(regs) = argv.values_of("register") {
        reg_parse(&mut poweron_state, regs)?;
    }

    let start_pc = input::parse_ptr(start_spec, db, &bus, arch)
        .expect("Must specify a valid address to analyze");
    let mut last_num_scans = None;

    cmd_send
        .send(Command::SetPowerOnState(poweron_state))
        .unwrap();
    cmd_send.send(Command::StaticScanCode(start_pc)).unwrap();
    cmd_send.send(Command::Fence).unwrap();
    cmd_send
        .send(Command::ExtractAllScans(is_tracing_allowed))
        .unwrap();
    cmd_send.send(Command::Fence).unwrap();

    drop(db_lock);

    loop {
        match resp_recv.recv().unwrap() {
            Response::StaticScanCode {
                scan_start,
                scan_end_offset,
                error,
            } => {
                if let Some(error) = error {
                    eprintln!(
                        "{}",
                        describe_scan_error::<ASM::Literal, AR>(
                            &bus,
                            &scan_start,
                            scan_end_offset,
                            error
                        )
                    );
                } else if let Some(scan_end_offset) = scan_end_offset {
                    eprintln!(
                        "Static scan at ${:X} got ${:X} bytes",
                        scan_start, scan_end_offset
                    );
                } else {
                    eprintln!("Static scan at ${:X}", scan_start);
                }
            }
            Response::PowerOnStateSet => {}
            Response::DynamicScanCode {
                scan_start,
                scan_end,
                error,
            } => {
                if let Some(error) = error {
                    eprintln!(
                        "Dynamic scan at ${:X} failed at ${:X} due to {}",
                        scan_start, scan_end, error
                    );
                } else {
                    eprintln!(
                        "Dynamic scan at ${:X} forked at ${:X}",
                        scan_start, scan_end
                    );
                }
            }
            Response::ExtractScanCount(_with_dynamic, how_much) => {
                eprintln!("Found {} more scans to complete...", how_much);
                last_num_scans = Some(how_much)
            }
            Response::Fence => {
                if last_num_scans == Some(0) {
                    eprintln!("Finishing scan.");
                    break;
                }

                eprintln!("Looking for more scans to run...");
                cmd_send
                    .send(Command::ExtractAllScans(is_tracing_allowed))
                    .unwrap();
                cmd_send.send(Command::Fence).unwrap();
            }
        }
    }

    eprintln!("Scan complete, writing database");

    let project_path = project.implicit_path()?;
    let database_path = prog.as_database_path().to_path(&project_path);

    if let Err(e) = pjdb
        .write()
        .unwrap()
        .write(&mut fs::File::create(database_path)?)
    {
        return Err(e.into());
    }

    Ok(())
}

/// Scan a given program for control flow, symbols, and so on.
pub fn scan<'a>(project: &mut Project, prog: &Program, argv: &ArgMatches<'a>) -> io::Result<()> {
    let start_spec = argv
        .value_of("start_pc")
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Did not provide a start PC"))?;
    let image = prog
        .iter_images()
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;

    with_prog_architecture!(prog, |plat, arch, asm| {
        let bus = Arc::new(plat.construct_platform(&mut file)?);

        scan_for_arch(project, prog, start_spec, bus, arch, asm, argv)
    })
}
