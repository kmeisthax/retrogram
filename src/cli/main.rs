//! CLI support for non-command bits

use std::io;
use std::str::FromStr;
use clap::{Arg, SubCommand, ArgSettings};
use crate::{project, cli};

fn resolve_program(project: &mut project::Project, version: &Option<String>, mut prog: project::Program) -> io::Result<project::Program> {
    //TODO: If the user specifies no program, we must select one from the DB,
    //otherwise disassembly fails because the program is unnamed.
    if let Some(version) = version {
        match project.program(&version) {
            Some(project_program) => prog = project_program.apply_override(&prog),
            None => eprintln!("The specified program version {} does not exist.", version)
        }
    } else if let Some((_, default_program)) = project.default_program() {
        prog = default_program.apply_override(&prog);
    }

    Ok(prog)
}

fn resolve_source(command: cli::Command, project: &mut project::Project, source_name: &Option<String>, prog: &project::Program, mut source: project::DataSource) -> io::Result<project::DataSource> {
    if let Some(source_name) = source_name {
        match project.data_source(&source_name) {
            Some(project_source) => source = project_source.apply_override(&source),
            None if command == cli::Command::Import => eprintln!("The specified data source {} does not exist.", source_name),
            None => {}
        }
    } else if let Some(first_datasource_name) = prog.iter_sources().next() {
        match project.data_source(&first_datasource_name) {
            Some(project_source) => source = project_source.apply_override(&source),
            None if command == cli::Command::Import => eprintln!("No project data source was configured or mentioned and the project's first data source does not exist."),
            None => {}
        }
    } else if command == cli::Command::Import {
        eprintln!("No project data source was configured or mentioned and the given program does not have any sources.");
    }

    Ok(source)
}

pub fn main() -> io::Result<()> {
    let mut app = app_from_crate!();
    app = app.arg(Arg::with_name("program").long("program").value_name("myapp").takes_value(true).help("Which program to analyze").set(ArgSettings::Global));
    app = project::Program::configure_app(app);
    app = project::DataSource::configure_app(app);
    app = app.arg(Arg::with_name("project").long("project").value_name("retrogram.json").takes_value(true).help("The project file to load").set(ArgSettings::Global));
    app = app.subcommand(SubCommand::with_name("scan").about("Scan for code at a given address")
        .arg(Arg::with_name("start_pc").value_name("1234").index(1).required(true).help("The PC value to start analysis from")));
    app = app.subcommand(SubCommand::with_name("dis").about("Display code for a given address or label")
        .arg(Arg::with_name("start_pc").value_name("1234").index(1).required(true).help("The PC value or label to list code for")));
    app = app.subcommand(SubCommand::with_name("import").about("Import data from an external data source")
        .arg(Arg::with_name("external_db").value_name("myapp_sym").index(1).required(true).help("The name of an external data source in the project to import from")));
    app = app.subcommand(SubCommand::with_name("backref").about("List backreferences to a given address or label")
        .arg(Arg::with_name("start_pc").value_name("1234").index(1).required(true).help("The PC value or label to list backreferences for")));
    app = app.subcommand(SubCommand::with_name("rename").about("Assign a label to a memory location (or reassign an existing one)")
        .arg(Arg::with_name("start_pc").value_name("1234").index(1).required(true).help("The PC value (or existing label) to (re)label"))
        .arg(Arg::with_name("new_label").value_name("MyLabel").index(2).required(true).help("What to name the PC value or label")));
    
    let matches = app.get_matches();

    let project_filename = matches.value_of("project").unwrap_or("retrogram.json");
    let version = matches.value_of("program").map(|s| s.to_string());
    let mut prog = project::Program::from_arg_matches(&matches);
    let mut source = project::DataSource::from_arg_matches(&matches);

    let (command, submatches) = matches.subcommand();
    let command = cli::Command::from_str(command);

    if let Ok(command) = command {
        let start_pc = submatches.unwrap().value_of("start_pc").map(|s| s.to_string());
        let end_pc = submatches.unwrap().value_of("new_label").map(|s| s.to_string());
        let source_name = submatches.unwrap().value_of("external_db").map(|s| s.to_string());

        match project::Project::read(&project_filename) {
            Ok(mut project) => {
                prog = resolve_program(&mut project, &version, prog)?;
                source = resolve_source(command, &mut project, &source_name, &prog, source)?;
            },
            Err(e) => eprintln!("Cannot open project file, got error {}", e) //TODO: You shouldn't need a project file if you specified everything else correctly.
        };

        match command {
            cli::Command::Scan => cli::scan(&prog, &start_pc.ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Did not provide a start PC"))?)?,
            cli::Command::Disassemble => cli::dis(&prog, &start_pc.ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Did not provide a start PC"))?)?,
            cli::Command::Import => cli::import(&prog, &source)?,
            cli::Command::Backreference => cli::backref(&prog, &start_pc.ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Did not provide a start PC"))?)?,
            cli::Command::Rename => cli::rename(&prog, &start_pc.ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Did not provide a start PC"))?, &end_pc.ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Did not provide a target label"))?)?
        };
    } else {
        eprintln!("Please enter a command");
    }

    Ok(())
}