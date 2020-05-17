//! Symbol rename command for retrogram

use crate::{analysis, ast, cli, input, maths, memory, project};
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

    with_architecture!(prog, file, |bus,
                                    _dis,
                                    _fmt_section,
                                    _fmt_instr,
                                    aparse,
                                    _prereq,
                                    _tracer| {
        rename_inner(prog, from_spec, to_spec, bus, aparse)
    })
}
