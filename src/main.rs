#[macro_use]
extern crate lazy_static;

mod retrogram;

use std::{str, fs, io};
use std::str::FromStr;
use serde_json;
use crate::retrogram::{arch, platform, asm, reg, analysis};
use crate::retrogram::arch::ArchName;
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

fn main() -> io::Result<()> {
    let mut command = None;
    let mut start_pc : Option<String> = None;
    let mut version : Option<String> = None;
    let mut prog = project::Program::default();

    {
        let mut ap = argparse::ArgumentParser::new();

        ap.refer(&mut command).add_argument("command", argparse::StoreOption, "The command to run.");
        ap.refer(&mut start_pc).add_argument("start_pc", argparse::StoreOption, "The PC value to start analysis from");
        ap.refer(&mut version).add_option(&["--program"], argparse::StoreOption, "Which program to analyze");
        prog.refer_args(&mut ap);

        ap.parse_args_or_exit();
    }

    let image = prog.iter_images().next().ok_or(io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;

    match command {
        Some(Commands::Disassemble) => {
            let mut file = fs::File::open(image)?;
            let pc_pieces = if let Some(start_pc) = start_pc {
                let mut v = Vec::new();

                for piece in start_pc.split(":") {
                    v.push(u16::from_str(piece).or(Err(io::Error::new(io::ErrorKind::InvalidInput, "Given analysis address is not a valid integer")))?);
                }

                Some(v)
            } else {
                None
            };

            match prog.platform() {
                Some(PlatformName::GB) => {
                    let mut db = analysis::Database::new();
                    let bus = platform::gb::construct_platform(&mut file, platform::gb::PlatformVariant::MBC5Mapper)?;

                    for symbol_file in prog.iter_symbol_files() {
                        if let Ok(mut file) = fs::File::open(symbol_file) {
                            asm::rgbds::parse_symbol_file(io::BufReader::new(file), &mut db)?;
                        }
                    }
                    
                    let orig_asm = match pc_pieces.map(|p| platform::gb::create_context(&p)) {
                        Some(Some(cptr)) => arch::lr35902::disassemble_block(&mut file, Some(cptr), &bus)?,
                        _ => arch::lr35902::disassemble_block(&mut file, None, &bus)?
                    };
                    
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
