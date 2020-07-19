//! Backreference list command for retrogram

use crate::arch::{Architecture, CompatibleLiteral};
use crate::{analysis, ast, cli, input, maths, memory, project};
use clap::ArgMatches;
use num_traits::One;
use std::{fs, io};

fn backref_inner<L, FMT, AR, IO>(
    prog: &project::Program,
    start_spec: &str,
    bus: &memory::Memory<AR::PtrVal, AR::Byte, AR::Offset, IO>,
    fmt: FMT,
    arch: AR,
) -> io::Result<()>
where
    L: CompatibleLiteral<AR>,
    AR: Architecture,
    for<'dw> AR::PtrVal: serde::Deserialize<'dw> + serde::Serialize + maths::FromStrRadix,
    for<'dw> AR::Offset: cli::Nameable + serde::Deserialize<'dw> + serde::Serialize,
    for<'dw> AR::Byte: serde::Deserialize<'dw>,
    FMT: Fn(&ast::Instruction<L>) -> String,
    IO: One,
{
    let mut pjdb = project::ProjectDatabase::read(prog.as_database_path())?;
    let db = pjdb.get_database_mut(prog.as_name().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "You did not specify a name for the program to disassemble.",
        )
    })?);
    db.update_indexes();

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

                    println!(
                        "{:X}: {} ({})",
                        xref.as_source(),
                        fmt(instr_asm),
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

    with_architecture!(prog, file, |bus, _fmt_sec, fmt_instr, arch| {
        backref_inner(prog, start_spec, bus, fmt_instr, arch)
    })
}
