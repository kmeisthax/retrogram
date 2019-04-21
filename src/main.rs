#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate serde_plain;

mod retrogram;

use std::{str, fs, io};
use std::str::FromStr;
use std::hash::Hash;
use num_traits::Num;
use crate::retrogram::{ast, arch, platform, asm, analysis, memory};
use crate::retrogram::platform::PlatformName;
use crate::retrogram::project;

enum Commands {
    Disassemble
}

impl str::FromStr for Commands {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_ref() {
            "dis" => Ok(Commands::Disassemble),
            _ => Err(())
        }
    }
}

fn parse_pcspec<P, C>(text_str: &str, db: &analysis::Database<P>, create_context: C) -> io::Result<memory::Pointer<P>>
    where P: Clone + Eq + Hash + FromStr + Num, C: Fn(&Vec<P>) -> Option<memory::Pointer<P>> {
    if let Ok(text_lbl) = ast::Label::from_str(text_str) {
        if let Some(ptr) = db.label_pointer(&text_lbl) {
            return Ok(ptr.clone());
        }
    }

    let mut v = Vec::new();

    for piece in text_str.split(":") {
        v.push(P::from_str_radix(piece, 16).or(Err(io::Error::new(io::ErrorKind::InvalidInput, "Given analysis address is not a valid integer")))?);
    }

    create_context(&v).ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Could not create context for input pointer"))
}

fn main() -> io::Result<()> {
    let mut command = None;
    let mut start_pc : String = "".to_string();
    let mut version : Option<String> = None;
    let mut prog = project::Program::default();

    {
        let mut ap = argparse::ArgumentParser::new();

        ap.refer(&mut command).add_argument("command", argparse::StoreOption, "The command to run.");
        ap.refer(&mut start_pc).add_argument("start_pc", argparse::Store, "The PC value to start analysis from");
        ap.refer(&mut version).add_option(&["--program"], argparse::StoreOption, "Which program to analyze");
        prog.refer_args(&mut ap);

        ap.parse_args_or_exit();
    }

    match project::Project::read() {
        Ok(project) => if let Some(version) = version {
            match project.program(&version) {
                Some(project_program) => prog = project_program.apply_override(&prog),
                None => eprintln!("The specified program version {} does not exist.", version)
            }
        } else if let Some((_, default_program)) = project.default_program() {
            prog = default_program.apply_override(&prog);
        },
        Err(e) => eprintln!("Cannot open project file, got error {}", e) //TODO: You shouldn't need a project file if you specified everything else correctly.
    }

    let image = prog.iter_images().next().ok_or(io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;

    match command {
        Some(Commands::Disassemble) => {
            let mut file = fs::File::open(image)?;

            match prog.platform() {
                Some(PlatformName::GB) => {
                    let mut db = analysis::Database::new();
                    let bus = platform::gb::construct_platform(&mut file, platform::gb::PlatformVariant::MBC5Mapper)?;

                    for symbol_file in prog.iter_symbol_files() {
                        if let Ok(file) = fs::File::open(symbol_file) {
                            asm::rgbds::parse_symbol_file(io::BufReader::new(file), &mut db)?;
                        }
                    }

                    let orig_asm = arch::lr35902::disassemble_block(parse_pcspec(&start_pc, &db, platform::gb::create_context).ok(), &bus)?;

                    match orig_asm.iter_lines().next() {
                        Some(line) => {
                            db.insert_placeholder_label(line.source_address().clone(), analysis::ReferenceKind::Unknown);
                        },
                        _ => {}
                    }
                    
                    let labeled_asm = analysis::replace_labels(orig_asm, &mut db, &bus);
                    let injected_asm = analysis::inject_labels(labeled_asm, &db);

                    println!("{}", injected_asm);
                },
                _ => eprintln!("Unknown platform")
            }
        },
        _ => eprintln!("Please enter a command")
    }

    Ok(())
}
