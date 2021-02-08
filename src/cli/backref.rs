//! Backreference list command for retrogram

use crate::arch::{Architecture, CompatibleLiteral};
use crate::asm::Assembler;
use crate::database::ProjectDatabase;
use crate::platform::Platform;
use crate::project::{Program, Project};
use crate::{input, maths, memory};
use clap::ArgMatches;
use num_traits::One;
use std::{fs, io};

fn backref_inner<AR, ASM>(
    project: &mut Project,
    prog: &Program,
    start_spec: &str,
    bus: &memory::Memory<AR>,
    arch: AR,
    asm: ASM,
) -> io::Result<()>
where
    AR: Architecture + 'static,
    AR::PtrVal: maths::FromStrRadix,
    ASM: Assembler,
    ASM::Literal: CompatibleLiteral<AR>,
{
    let project_path = project.implicit_path()?;
    let database_path = prog.as_database_path().to_path(project_path);
    let pjdb: io::Result<ProjectDatabase> =
        ProjectDatabase::read(project, &mut fs::File::open(database_path)?).map_err(|e| e.into());
    let mut pjdb = pjdb?;
    let db = pjdb.get_database_mut(prog.as_name().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "You did not specify a name for the program to disassemble.",
        )
    })?).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "The architecture of the current program's database does not match the program's architecture."
        )
    })?;

    let start_pc = input::parse_ptr(start_spec, db, bus, arch).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "Must specify a valid address to analyze",
        )
    })?;

    for xref_id in db.find_xrefs_to(&start_pc, AR::Offset::one()) {
        if let Some(xref) = db.xref(xref_id) {
            //let (instr_asm, _, _, _, _) = dis(xref.as_source(), bus);

            match arch.disassemble(xref.as_source(), bus) {
                Ok(disasm) => {
                    let instr_asm = disasm.as_instr();

                    println!("{:X}: ", xref.as_source());

                    asm.emit_instr(&mut io::stdout(), instr_asm)?;

                    println!(" ({})", xref.kind().friendly_name());
                }
                Err(_) => {
                    println!(
                        "{:X}: ??? ({})",
                        xref.as_source(),
                        xref.kind().friendly_name()
                    );
                }
            }
        }
    }

    Ok(())
}

pub fn backref<'a>(project: &mut Project, prog: &Program, argv: &ArgMatches<'a>) -> io::Result<()> {
    let start_spec = argv
        .value_of("start_pc")
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Did not provide a start PC"))?;
    let image = prog
        .iter_images()
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;

    with_prog_architecture!(prog, |plat, arch, asm| {
        let bus = plat.construct_platform(&mut file)?;

        backref_inner(project, prog, start_spec, &bus, arch, asm)
    })
}
