#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate serde_plain;

mod retrogram;

use std::{str, io};
use crate::retrogram::{cli, project};

enum Commands {
    Scan,
    Disassemble,
    Import
}

impl str::FromStr for Commands {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            "dis" => Ok(Commands::Disassemble),
            "scan" => Ok(Commands::Scan),
            "chadtronic-scan" => Ok(Commands::Scan),
            "import" => Ok(Commands::Import),
            _ => Err(())
        }
    }
}

fn main() -> io::Result<()> {
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

    //TODO: If the user specifies no program, we must select one from the DB,
    //otherwise disassembly fails because the program is unnamed.
    match project::Project::read(&project_filename) {
        Ok(mut project) => {
            if let Some(version) = version {
                match project.program(&version) {
                    Some(project_program) => prog = project_program.apply_override(&prog),
                    None => eprintln!("The specified program version {} does not exist.", version)
                }
            } else if let Some((_, default_program)) = project.default_program() {
                prog = default_program.apply_override(&prog);
            }

            if let Some(source_name) = source_name {
                match project.data_source(&source_name) {
                    Some(project_source) => source = project_source.apply_override(&source),
                    None => eprintln!("The specified data source {} does not exist.", source_name)
                }
            }
        },
        Err(e) => eprintln!("Cannot open project file, got error {}", e) //TODO: You shouldn't need a project file if you specified everything else correctly.
    }

    let image = prog.iter_images().next().ok_or(io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;

    match command {
        Some(Commands::Scan) => cli::scan(&prog, &start_pc)?,
        Some(Commands::Disassemble) => cli::dis(&prog, &start_pc)?,
        Some(Commands::Import) => cli::import(&prog, &source)?,
        _ => eprintln!("Please enter a command"),
    };

    Ok(())
}
