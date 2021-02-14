//! Symbol rename command for retrogram

use crate::arch::Architecture;
use crate::database::ProjectDatabase;
use crate::platform::Platform;
use crate::project::{Program, Project};
use crate::{ast, input, maths, memory};
use clap::ArgMatches;
use num_traits::One;
use std::str::FromStr;
use std::{fs, io};

fn rename_inner<AR>(
    project: &mut Project,
    prog: &Program,
    from_spec: &str,
    to_spec: &str,
    bus: &memory::Memory<AR>,
    arch: AR,
) -> io::Result<()>
where
    AR: Architecture + 'static,
    for<'dw> AR::PtrVal: serde::Deserialize<'dw> + serde::Serialize + maths::FromStrRadix,
    for<'dw> AR::Offset: serde::Deserialize<'dw> + serde::Serialize + One,
    for<'dw> AR::Byte: serde::Deserialize<'dw>,
{
    let project_path = project.implicit_path()?;
    let database_path = prog.as_database_path().to_path(&project_path);
    let pjdb: io::Result<ProjectDatabase> =
        ProjectDatabase::read(project, &mut fs::File::open(&database_path)?).map_err(|e| e.into());
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

    db.upsert_symbol(into_label, None, from_pc);

    if let Err(e) = pjdb.write(&mut fs::File::create(database_path)?) {
        return Err(e.into());
    }

    Ok(())
}

pub fn rename<'a>(project: &mut Project, prog: &Program, argv: &ArgMatches<'a>) -> io::Result<()> {
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

    with_prog_architecture!(prog, |plat, arch, _asm| {
        let bus = plat.construct_platform(&mut file)?;

        rename_inner(project, prog, from_spec, to_spec, &bus, arch)
    })
}
