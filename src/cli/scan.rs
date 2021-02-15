//! CLI command: scan

use crate::arch::{Architecture, CompatibleLiteral};
use crate::asm::Assembler;
use crate::cli::common::reg_parse;
use crate::database::ProjectDatabase;
use crate::maths::FromStrRadix;
use crate::memory::Memory;
use crate::platform::Platform;
use crate::project::{Program, Project};
use crate::queue::{start_analysis_queue, Command, Response};
use crate::reg::{Bitwise, State};
use crate::{analysis, input, maths, reg};
use clap::ArgMatches;
use std::convert::TryInto;
use std::sync::{Arc, RwLock};
use std::{fs, io};

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
    asm: ASM,
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
    cmd_send
        .send(Command::DeclareEntryPoint(start_pc.clone()))
        .unwrap();
    cmd_send.send(Command::StaticScanCode(start_pc)).unwrap();
    cmd_send.send(Command::Fence).unwrap();
    cmd_send
        .send(Command::ExtractAllScans(is_tracing_allowed))
        .unwrap();
    cmd_send.send(Command::Fence).unwrap();

    drop(db_lock);

    loop {
        let resp = resp_recv.recv().unwrap();
        eprintln!("{}", resp.describe_response(bus.clone(), asm.clone()));

        match resp {
            Response::ExtractScanCount(_with_dynamic, how_much) => last_num_scans = Some(how_much),
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
            _ => {}
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
