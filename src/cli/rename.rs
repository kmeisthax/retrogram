//! Symbol rename command for retrogram

use crate::arch::Architecture;
use crate::{ast, input, maths, memory, project};
use clap::ArgMatches;
use num_traits::One;
use std::str::FromStr;
use std::{fs, io};

fn rename_inner<AR>(
    prog: &project::Program,
    from_spec: &str,
    to_spec: &str,
    bus: &memory::Memory<AR>,
    arch: AR,
) -> io::Result<()>
where
    AR: Architecture,
    for<'dw> AR::PtrVal: serde::Deserialize<'dw> + serde::Serialize + maths::FromStrRadix,
    for<'dw> AR::Offset: serde::Deserialize<'dw> + serde::Serialize + One,
    for<'dw> AR::Byte: serde::Deserialize<'dw>,
{
    let mut pjdb = project::ProjectDatabase::read(prog.as_database_path())?;
    let db = pjdb.get_database_mut(prog.as_name().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "You did not specify a name for the program to disassemble.",
        )
    })?);
    db.update_indexes();

    let from_pc = input::parse_ptr(from_spec, db, bus, arch).ok_or_else(|| {
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

pub fn rename<'a>(prog: &project::Program, argv: &ArgMatches<'a>) -> io::Result<()> {
    let from_spec = argv
        .value_of("start_pc")
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Did not provide a start PC"))?;
    let to_spec = argv.value_of("new_label").ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "Did not provide a target label",
        )
    })?;
    let image = prog
        .iter_images()
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;

    with_architecture!(prog, file, |bus, _fmt_section, _fmt_instr, arch| {
        rename_inner(prog, from_spec, to_spec, &bus, arch)
    })
}
