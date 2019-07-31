//! Symbol rename command for retrogram

use std::{io, fs};
use std::str::FromStr;
use num_traits::One;
use crate::{ast, project, platform, memory, cli, analysis, arch, input, maths};

fn rename_inner<P, MV, S, IO, APARSE>
    (prog: &project::Program,
        from_spec: &str,
        to_spec: &str,
        bus: &memory::Memory<P, MV, S, IO>,
        architectural_ctxt_parse: APARSE) -> io::Result<()>
    where for <'dw> P: memory::PtrNum<S> + analysis::Mappable + cli::Nameable + serde::Deserialize<'dw> +
            serde::Serialize + maths::FromStrRadix,
        for <'dw> S: memory::Offset<P> + cli::Nameable + serde::Deserialize<'dw> + serde::Serialize + One,
        for <'dw> MV: serde::Deserialize<'dw>,
        APARSE: FnOnce(&mut &[&str], &mut memory::Pointer<P>) -> Option<()> {
    
    let mut pjdb = project::ProjectDatabase::read(prog.as_database_path())?;
    let db = pjdb.get_database_mut(prog.as_name().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "You did not specify a name for the program to disassemble."))?);
    db.update_indexes();

    let from_pc = input::parse_ptr(from_spec, db, bus, architectural_ctxt_parse).ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Must specify a valid address to analyze"))?;
    let into_label = ast::Label::from_str(to_spec).map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "Label specified by user as location name is invalid"))?;
    
    db.upsert_symbol(into_label, from_pc);

    pjdb.write(prog.as_database_path())
}

pub fn rename(prog: &project::Program, from_spec: &str, to_spec: &str) -> io::Result<()> {
    let platform = prog.platform().ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified platform, analysis cannot continue."))?;
    let arch = prog.arch().or_else(|| platform.default_arch()).ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified architecture, analysis cannot continue."))?;
    let image = prog.iter_images().next().ok_or(io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;

    match (arch, platform) {
        (arch::ArchName::SM83, platform::PlatformName::GB) => 
            rename_inner(prog, from_spec, to_spec,
                &platform::gb::construct_platform(&mut file, platform::gb::PlatformVariant::MBC5Mapper)?,
                |_, _| Some(())),
        (arch::ArchName::AARCH32, platform::PlatformName::AGB) =>
            rename_inner(prog, from_spec, to_spec,
                &platform::agb::construct_platform(&mut file)?,
                arch::aarch32::architectural_ctxt_parse),
        _ => return Err(io::Error::new(io::ErrorKind::Other, "oops"))
    }
}