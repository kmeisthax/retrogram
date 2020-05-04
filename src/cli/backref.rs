//! Backreference list command for retrogram

use crate::{analysis, arch, asm, ast, cli, input, maths, memory, platform, project};
use num_traits::One;
use std::{fs, io};

fn backref_inner<I, SI, F, P, MV, S, IO, DIS, FMT, APARSE>(
    prog: &project::Program,
    start_spec: &str,
    bus: &memory::Memory<P, MV, S, IO>,
    dis: DIS,
    fmt: FMT,
    architectural_ctxt_parse: APARSE,
) -> io::Result<()>
where
    for<'dw> P: memory::PtrNum<S>
        + analysis::Mappable
        + cli::Nameable
        + serde::Deserialize<'dw>
        + serde::Serialize
        + maths::FromStrRadix,
    for<'dw> S:
        memory::Offset<P> + cli::Nameable + serde::Deserialize<'dw> + serde::Serialize + One,
    for<'dw> MV: serde::Deserialize<'dw>,
    DIS: analysis::Disassembler<I, SI, F, P, MV, S, IO>,
    FMT: Fn(&ast::Instruction<I, SI, F, P>) -> String,
    APARSE: FnOnce(&mut &[&str], &mut memory::Pointer<P>) -> Option<()>,
{
    let mut pjdb = project::ProjectDatabase::read(prog.as_database_path())?;
    let db = pjdb.get_database_mut(prog.as_name().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "You did not specify a name for the program to disassemble.",
        )
    })?);
    db.update_indexes();

    let start_pc =
        input::parse_ptr(start_spec, db, bus, architectural_ctxt_parse).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                "Must specify a valid address to analyze",
            )
        })?;

    for xref_id in db.find_xrefs_to(&start_pc, S::one()) {
        if let Some(xref) = db.xref(xref_id) {
            //let (instr_asm, _, _, _, _) = dis(xref.as_source(), bus);

            match dis(xref.as_source(), bus) {
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

pub fn backref(prog: &project::Program, start_spec: &str) -> io::Result<()> {
    let platform = prog.platform().ok_or(io::Error::new(
        io::ErrorKind::InvalidInput,
        "Unspecified platform, analysis cannot continue.",
    ))?;
    let arch = prog
        .arch()
        .or_else(|| platform.default_arch())
        .ok_or(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Unspecified architecture, analysis cannot continue.",
        ))?;
    let asm = prog
        .assembler()
        .or_else(|| arch.default_asm())
        .ok_or(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Unspecified assembler for architecture, analysis cannot continue.",
        ))?;
    let image = prog.iter_images().next().ok_or(io::Error::new(
        io::ErrorKind::Other,
        "Did not specify an image",
    ))?;
    let mut file = fs::File::open(image)?;

    match (arch, platform, asm) {
        (arch::ArchName::SM83, platform::PlatformName::GB, asm::AssemblerName::RGBDS) => {
            backref_inner(
                prog,
                start_spec,
                &platform::gb::construct_platform(
                    &mut file,
                    platform::gb::PlatformVariant::MBC5Mapper,
                )?,
                arch::sm83::disassemble,
                |asm| format!("{}", asm::rgbds::InstrFmtWrap::wrap(asm)),
                |_, _| Some(()),
            )
        }
        (arch::ArchName::AARCH32, platform::PlatformName::AGB, asm::AssemblerName::ARMIPS) => {
            backref_inner(
                prog,
                start_spec,
                &platform::agb::construct_platform(&mut file)?,
                arch::aarch32::disassemble,
                |asm| format!("{}", asm::armips::InstrFmtWrap::wrap(asm)),
                arch::aarch32::architectural_ctxt_parse,
            )
        }
        _ => return Err(io::Error::new(io::ErrorKind::Other, "oops")),
    }
}
