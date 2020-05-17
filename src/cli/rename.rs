//! Symbol rename command for retrogram

use crate::{analysis, arch, ast, cli, input, maths, memory, platform, project};
use crate::cli::common::resolve_program_config;
use num_traits::One;
use std::str::FromStr;
use std::{fs, io};

fn rename_inner<P, MV, S, IO, APARSE>(
    prog: &project::Program,
    from_spec: &str,
    to_spec: &str,
    bus: &memory::Memory<P, MV, S, IO>,
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

    let from_pc =
        input::parse_ptr(from_spec, db, bus, architectural_ctxt_parse).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                "Must specify a valid address to analyze",
            )
        })?;
    let into_label = ast::Label::from_str(to_spec).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "Label specified by user as location name is invalid",
        )
    })?;

    db.upsert_symbol(into_label, from_pc);

    pjdb.write(prog.as_database_path())
}

pub fn rename(prog: &project::Program, from_spec: &str, to_spec: &str) -> io::Result<()> {
    let image = prog
        .iter_images()
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;

    match resolve_program_config(prog)? {
        (arch::ArchName::SM83, platform::PlatformName::GB, _) => rename_inner(
            prog,
            from_spec,
            to_spec,
            &platform::gb::construct_platform(
                &mut file,
                platform::gb::PlatformVariant::MBC5Mapper,
            )?,
            |_, _| Some(()),
        ),
        (arch::ArchName::AARCH32, platform::PlatformName::AGB, _) => rename_inner(
            prog,
            from_spec,
            to_spec,
            &platform::agb::construct_platform(&mut file)?,
            arch::aarch32::architectural_ctxt_parse,
        ),
        _ => Err(io::Error::new(io::ErrorKind::Other, "oops")),
    }
}
