//! Backreference list command for retrogram

use crate::arch::{Architecture, CompatibleLiteral};
use crate::asm::Assembler;
use crate::{analysis, input, maths, memory, project};
use clap::ArgMatches;
use num_traits::One;
use std::{fs, io};

fn backref_inner<AR, ASM>(
    prog: &project::Program,
    start_spec: &str,
    bus: &memory::Memory<AR>,
    arch: AR,
    asm: ASM,
) -> io::Result<()>
where
    AR: Architecture,
    AR::PtrVal: maths::FromStrRadix,
    ASM: Assembler,
    ASM::Literal: CompatibleLiteral<AR>,
{
    let mut pjdb = project::ProjectDatabase::read(prog.as_database_path())?;
    let db = pjdb.get_database_mut(prog.as_name().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "You did not specify a name for the program to disassemble.",
        )
    })?);

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

                    println!(
                        " ({})",
                        match xref.kind() {
                            analysis::ReferenceKind::Unknown => "Unknown",
                            analysis::ReferenceKind::Data => "Data",
                            analysis::ReferenceKind::Code => "Code, branch",
                            analysis::ReferenceKind::Subroutine => "Code, call",
                        }
                    );
                }
                Err(_) => {
                    println!(
                        "{:X}: ??? ({})",
                        xref.as_source(),
                        match xref.kind() {
                            analysis::ReferenceKind::Unknown => "Unknown",
                            analysis::ReferenceKind::Data => "Data",
                            analysis::ReferenceKind::Code => "Code, branch",
                            analysis::ReferenceKind::Subroutine => "Code, call",
                        }
                    );
                }
            }
        }
    }

    Ok(())
}

pub fn backref<'a>(prog: &project::Program, argv: &ArgMatches<'a>) -> io::Result<()> {
    let start_spec = argv
        .value_of("start_pc")
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Did not provide a start PC"))?;
    let image = prog
        .iter_images()
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;

    with_architecture!(prog, file, |bus, arch, asm| {
        backref_inner(prog, start_spec, &bus, arch, asm)
    })
}
