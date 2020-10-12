//! Common utilities for command implementations

use crate::arch::{ArchName, Architecture};
use crate::asm::AssemblerName;
use crate::maths::FromStrRadix;
use crate::platform::PlatformName;
use crate::project::{DataSource, Program, Project};
use crate::reg::{State, Symbolic};
use clap::{App, Arg, SubCommand, Values};
use std::io;
use std::str;
use std::str::FromStr;

/// Enumeration of all CLI commands
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Command {
    Scan,
    Disassemble,
    Import,
    Backreference,
    Rename,
    Trace,
}

impl Command {
    /// Enumerate all commands that Retrogram recognizes.
    pub fn enumerate() -> Vec<Self> {
        use Command::*;

        vec![Scan, Disassemble, Import, Backreference, Rename, Trace]
    }

    /// Construct the subcommand object for this particular `Command`.
    pub fn into_clap_subcommand<'a, 'b>(self) -> App<'a, 'b> {
        match self {
            Command::Scan => SubCommand::with_name("scan")
                .about("Scan for code at a given address")
                .arg(
                    Arg::with_name("start_pc")
                        .value_name("1234")
                        .index(1)
                        .required(true)
                        .help("The PC value to start analysis from"),
                )
                .arg(
                    Arg::with_name("dynamic")
                        .long("dynamic")
                        .help("When specified, unresolvable references will trigger dynamic tracing analysis."),
                )
                .arg(
                    Arg::with_name("register")
                        .value_name("A=8F")
                        .short("R")
                        .multiple(true)
                        .takes_value(true)
                        .value_terminator("--")
                        .help("One or more registers to initialize to particular values during dynamic analysis"),
                ),
            Command::Disassemble => SubCommand::with_name("dis")
                .about("Display code for a given address or label")
                .arg(
                    Arg::with_name("start_pc")
                        .value_name("1234")
                        .index(1)
                        .required(true)
                        .help("The PC value or label to list code for"),
                ),
            Command::Import => SubCommand::with_name("import")
                .about("Import data from an external data source")
                .arg(
                    Arg::with_name("external_db")
                        .value_name("myapp_sym")
                        .index(1)
                        .required(true)
                        .help("The name of an external data source in the project to import from"),
                ),
            Command::Backreference => SubCommand::with_name("backref")
                .about("List backreferences to a given address or label")
                .arg(
                    Arg::with_name("start_pc")
                        .value_name("1234")
                        .index(1)
                        .required(true)
                        .help("The PC value or label to list backreferences for"),
                ),
            Command::Rename => SubCommand::with_name("rename")
                .about("Assign a label to a memory location (or reassign an existing one)")
                .arg(
                    Arg::with_name("start_pc")
                        .value_name("1234")
                        .index(1)
                        .required(true)
                        .help("The PC value (or existing label) to (re)label"),
                )
                .arg(
                    Arg::with_name("new_label")
                        .value_name("MyLabel")
                        .index(2)
                        .required(true)
                        .help("What to name the PC value or label"),
                ),
            Command::Trace => SubCommand::with_name("trace")
                .about("Execute a portion of the program with a particular register state")
                .arg(
                    Arg::with_name("start_pc")
                        .value_name("1234")
                        .index(1)
                        .required(true)
                        .help("The PC value or label to trace"),
                )
                .arg(
                    Arg::with_name("register")
                        .value_name("A=8F")
                        .short("R")
                        .multiple(true)
                        .takes_value(true)
                        .value_terminator("--")
                        .help("One or more registers to set to particular values"),
                ),
        }
    }
}

impl FromStr for Command {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            "dis" => Ok(Command::Disassemble),
            "scan" => Ok(Command::Scan),
            "chadtronic-scan" => Ok(Command::Scan),
            "import" => Ok(Command::Import),
            "backref" => Ok(Command::Backreference),
            "name" => Ok(Command::Rename),
            "rename" => Ok(Command::Rename),
            "trace" => Ok(Command::Trace),
            _ => Err(()),
        }
    }
}

/// Resolve the program the user may have specified.
pub fn resolve_program(
    project: &mut Project,
    version: Option<&str>,
    mut prog: Program,
) -> io::Result<Program> {
    //TODO: If the user specifies no program, we must select one from the DB,
    //otherwise disassembly fails because the program is unnamed.
    if let Some(version) = version {
        match project.program(&version) {
            Some(project_program) => prog = project_program.apply_override(&prog),
            None => eprintln!("The specified program version {} does not exist.", version),
        }
    } else if let Some((_, default_program)) = project.default_program() {
        prog = default_program.apply_override(&prog);
    }

    Ok(prog)
}

/// Resolve the data source the user may have specified.
pub fn resolve_source(
    project: &mut Project,
    source_name: Option<&str>,
    prog: &Program,
    mut source: DataSource,
) -> io::Result<DataSource> {
    if let Some(source_name) = source_name {
        match project.data_source(&source_name) {
            Some(project_source) => source = project_source.apply_override(&source),
            None => {
                return Err(io::Error::new(
                    io::ErrorKind::NotFound,
                    format!("The specified data source {} does not exist.", source_name),
                ));
            }
        }
    } else if let Some(first_datasource_name) = prog.iter_sources().next() {
        match project.data_source(&first_datasource_name) {
            Some(project_source) => source = project_source.apply_override(&source),
            None => return Err(io::Error::new(io::ErrorKind::NotFound, "No project data source was configured or mentioned and the project's first data source does not exist.".to_string())),
        }
    } else {
        return Err(io::Error::new(io::ErrorKind::NotFound, "No project data source was configured or mentioned and the given program does not have any sources.".to_string()));
    }

    Ok(source)
}

/// Resolve a program's platform, architecture, and assembler.
///
/// This function yields an error if any of the three could not be determined.
pub fn resolve_program_config(
    prog: &Program,
) -> io::Result<(ArchName, PlatformName, AssemblerName)> {
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
    let asm = prog
        .assembler()
        .or_else(|| arch.default_asm())
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                "Unspecified assembler for architecture, analysis cannot continue.",
            )
        })?;

    Ok((arch, platform, asm))
}

/// Parse a bunch of registers from a multiple-value argument into a State.
pub fn reg_parse<AR>(state: &mut State<AR>, regs: Values<'_>) -> io::Result<()>
where
    AR: Architecture,
    AR::Word: FromStrRadix,
{
    for reg_spec in regs {
        let values = reg_spec.split('=').collect::<Vec<&str>>();
        if let Some(v) = values.get(0..2) {
            let reg: AR::Register = v[0].parse().map_err(|_e| {
                io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("Register {} is invalid", v[0]),
                )
            })?;
            let value = AR::Word::from_str_radix(v[1], 16).map_err(|_e| {
                io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("Value {} for register {} is invalid", v[1], v[0]),
                )
            })?;

            state.set_register(reg, Symbolic::from(value))
        }
    }

    Ok(())
}
