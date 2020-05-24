//! CLI support for non-command bits

use crate::cli::common::{resolve_program, Command};
use crate::{cli, project};
use clap::{Arg, ArgSettings};
use std::io;
use std::str::FromStr;

pub fn main() -> io::Result<()> {
    let mut app = app_from_crate!();
    app = app.arg(
        Arg::with_name("program")
            .long("program")
            .value_name("myapp")
            .takes_value(true)
            .help("Which program to analyze")
            .set(ArgSettings::Global),
    );
    app = project::Program::configure_app(app);
    app = project::DataSource::configure_app(app);
    app = app.arg(
        Arg::with_name("project")
            .long("project")
            .value_name("retrogram.json")
            .takes_value(true)
            .help("The project file to load")
            .set(ArgSettings::Global),
    );

    for cmd in Command::enumerate().iter() {
        app = app.subcommand(cmd.into_clap_subcommand());
    }

    let matches = app.get_matches();

    let project_filename = matches.value_of("project").unwrap_or("retrogram.json");
    let version = matches.value_of("program");
    let mut prog = project::Program::from_arg_matches(&matches);

    let (command, submatches) = matches.subcommand();
    let command = cli::Command::from_str(command);

    if let Ok(command) = command {
        match project::Project::read(&project_filename) {
            Ok(mut project) => {
                prog = resolve_program(&mut project, version, prog)?;
            }
            Err(e) => eprintln!("Cannot open project file, got error {}", e), //TODO: You shouldn't need a project file if you specified everything else correctly.
        };

        match command {
            cli::Command::Scan => cli::scan(&prog, submatches.unwrap())?,
            cli::Command::Disassemble => cli::dis(&prog, submatches.unwrap())?,
            cli::Command::Import => cli::import(&prog, submatches.unwrap())?,
            cli::Command::Backreference => cli::backref(&prog, submatches.unwrap())?,
            cli::Command::Rename => cli::rename(&prog, submatches.unwrap())?,
            cli::Command::Trace => cli::trace(&prog, submatches.unwrap())?,
        };
    } else {
        eprintln!("Please enter a command");
    }

    Ok(())
}
