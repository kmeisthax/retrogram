//! Import command: Copies data from an external data source into the Retrogram
//! analysis database

use std::{io, fs};
use crate::retrogram::{project, database, arch, platform, analysis};

//pub fn parse_symbol_file<F>(file: F, db: &mut Database<lr35902::Pointer, lr35902::Offset>) -> io::Result<()> where F: io::BufRead

pub fn import_for_arch<P, S, IMP>(prog: &project::Program, datasrc: &project::DataSource, imp: IMP) -> io::Result<()>
    where for <'dw> P: analysis::Mappable + serde::Deserialize<'dw>,
        for <'dw> S: serde::Deserialize<'dw>,
        IMP: Fn(&project::Program, &project::DataSource, &mut [io::BufReader<fs::File>], &mut database::Database<P, S>) -> io::Result<()> {
    
    let mut pjdb = match project::ProjectDatabase::read(prog.as_database_path()) {
        Ok(pjdb) => pjdb,
        Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
            eprintln!("Creating new database for project");
            project::ProjectDatabase::new()
        },
        Err(e) => return Err(e)
    };

    let mut db = pjdb.get_database_mut(prog.as_name().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "You did not specify a name for the program to disassemble."))?);
    db.update_indexes();

    let mut files = Vec::new();
    for filename in datasrc.iter_files() {
        files.push(io::BufReader::new(fs::File::open(filename)?));
    }

    imp(prog, datasrc, files.get_mut(..).expect("No files to import"), &mut db)
}

pub fn import(prog: &project::Program, datasrc: &project::DataSource) -> io::Result<()> {
    let platform = prog.platform().ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified platform, analysis cannot continue."))?;
    let arch = prog.arch().or_else(|| platform.default_arch()).ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified architecture, analysis cannot continue."))?;
    let format = datasrc.format().ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified data source format"))?;

    match (arch, platform, format) {
        (arch::ArchName::LR35902, platform::PlatformName::GB, database::ExternalFormat::RGBDSSymbolFile) => import_for_arch(prog, datasrc, &database::rgbds::parse_symbol_file),
        //(arch::ArchName::AARCH32, platform::PlatformName::AGB) => scan_for_arch(prog, start_spec, &arch::aarch32::disassemble, &platform::agb::construct_platform(&mut file)?),
        _ => return Err(io::Error::new(io::ErrorKind::Other, "The given combination of architecture, platform, and/or assembler are not compatible."))
    }
}