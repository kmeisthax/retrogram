//! Import command: Copies data from an external data source into the Retrogram
//! analysis database

use crate::arch::Architecture;
use crate::cli::common::resolve_source;
use crate::database::ProjectDatabase;
use crate::project::{DataSource, Program, Project};
use crate::{arch, database, platform};
use clap::ArgMatches;
use std::{fs, io};

pub fn import_for_arch<AR, IMP>(
    project: &mut Project,
    prog: &Program,
    datasrc: &DataSource,
    imp: IMP,
) -> io::Result<()>
where
    AR: Architecture + 'static,
    IMP: Fn(
        &Program,
        &DataSource,
        &mut [io::BufReader<fs::File>],
        &mut database::Database<AR>,
    ) -> io::Result<()>,
{
    let project_path = project.implicit_path()?;
    let database_path = prog.as_database_path().to_path(project_path);
    let pjdb: io::Result<ProjectDatabase> =
        ProjectDatabase::read(project, &mut fs::File::open(database_path)?).map_err(|e| e.into());
    let mut pjdb = match pjdb {
        Ok(pjdb) => pjdb,
        Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
            eprintln!("Creating new database for project");
            ProjectDatabase::new()
        }
        Err(e) => return Err(e),
    };

    let mut db = pjdb.get_database_mut(prog.as_name().ok_or_else(|| {
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

    let mut files = Vec::new();
    for filename in datasrc.iter_files() {
        files.push(io::BufReader::new(fs::File::open(filename)?));
    }

    imp(
        prog,
        datasrc,
        files.get_mut(..).expect("No files to import"),
        &mut db,
    )
}

pub fn import<'a>(project: &mut Project, prog: &Program, argv: &ArgMatches<'a>) -> io::Result<()> {
    let mut source = DataSource::from_arg_matches(&argv);
    let source_name = argv.value_of("external_db");

    source = resolve_source(project, source_name, &prog, source)?;

    let platform = prog.platform().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "Unspecified platform, analysis cannot continue.",
        )
    })?;
    let arch = prog
        .arch()
        .or_else(|| platform.default_arch())
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                "Unspecified architecture, analysis cannot continue.",
            )
        })?;
    let format = source.format().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "Unspecified data source format",
        )
    })?;

    match (arch, platform, format) {
        (
            arch::ArchName::Sm83,
            platform::PlatformName::Gb,
            database::ExternalFormat::RgbdsSymbolFile,
        ) => import_for_arch(project, prog, &source, &database::rgbds::parse_symbol_file),
        //(arch::ArchName::AArch32, platform::PlatformName::Agb) => scan_for_arch(prog, start_spec, &arch::aarch32::disassemble, &platform::agb::construct_platform(&mut file)?),
        _ => Err(io::Error::new(
            io::ErrorKind::Other,
            "The given combination of architecture, platform, and/or assembler are not compatible.",
        )),
    }
}
