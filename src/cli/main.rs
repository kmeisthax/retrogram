//! CLI support for non-command bits

use std::io;
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
    let mut command = None;
    let mut start_pc : String = "".to_string();
    let mut version : Option<String> = None;
    let mut source_name : Option<String> = None;
    let mut prog = project::Program::default();
    let mut source = project::DataSource::default();
    let mut project_filename = "retrogram.json".to_string();

    {
        let mut ap = argparse::ArgumentParser::new();

        ap.refer(&mut command).add_argument("command", argparse::StoreOption, "The command to run.");
        ap.refer(&mut start_pc).add_argument("start_pc", argparse::Store, "The PC value to start analysis from");
        ap.refer(&mut version).add_option(&["--program"], argparse::StoreOption, "Which program to analyze");
        ap.refer(&mut source_name).add_option(&["--external_db"], argparse::StoreOption, "The name of an external data source in the project to use");
        ap.refer(&mut project_filename).add_option(&["--project"], argparse::Store, "The project file to load (usually not needed)");
        prog.refer_args(&mut ap);
        source.refer_args(&mut ap);

        ap.parse_args_or_exit();
    }

    if let Some(command) = command {
        match project::Project::read(&project_filename) {
            Ok(mut project) => {
                prog = resolve_program(&mut project, &version, prog)?;
                source = resolve_source(command, &mut project, &source_name, &prog, source)?;
            },
            Err(e) => eprintln!("Cannot open project file, got error {}", e) //TODO: You shouldn't need a project file if you specified everything else correctly.
        }

        match command {
            cli::Command::Scan => cli::scan(&prog, &start_pc)?,
            cli::Command::Disassemble => cli::dis(&prog, &start_pc)?,
            cli::Command::Import => cli::import(&prog, &source)?,
            cli::Command::Backreference => cli::backref(&prog, &start_pc)?
        };
    } else {
        eprintln!("Please enter a command");
    }

    Ok(())
}