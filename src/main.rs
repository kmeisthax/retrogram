#[macro_use]
extern crate lazy_static;

mod retrogram;

use std::{str, fs, io};
use std::str::FromStr;
use argparse;
use crate::retrogram::{arch, platform, reg};
use crate::retrogram::arch::ArchName;
use crate::retrogram::platform::PlatformName;

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
    let mut image = "".to_string();
    let mut platform = None;
    let mut arch : Option<arch::ArchName> = None;
    let mut start_pc : Option<String> = None;

    {
        let mut ap = argparse::ArgumentParser::new();

        ap.refer(&mut command).add_argument("command", argparse::StoreOption, "The command to run.");
        ap.refer(&mut image).add_argument("image", argparse::Store, "The program image file to analyze");
        ap.refer(&mut platform).add_option(&["--platform"], argparse::StoreOption, "What platform to expect");
        ap.refer(&mut arch).add_option(&["--arch"], argparse::StoreOption, "What architecture to expect");
        ap.refer(&mut start_pc).add_option(&["--start_pc"], argparse::StoreOption, "The PC value to start analysis from");

        ap.parse_args_or_exit();
    }

    match command {
        Some(Commands::Disassemble) => {
            let mut file = fs::File::open(image)?;
            let mut pc_pieces = if let Some(start_pc) = start_pc {
                let mut v = Vec::new();

                for piece in start_pc.split(":") {
                    v.push(u16::from_str(piece).or(Err(io::Error::new(io::ErrorKind::InvalidInput, "Given analysis address is not a valid integer")))?);
                }

                Some(v)
            } else {
                None
            };

            match platform {
                Some(PlatformName::GB) => {
                    match pc_pieces.map(|p| platform::gb::create_context(&p)) {
                        Some(Some((start_pc, ctxt))) => platform::gb::analyze(&mut file, Some(start_pc), Some(&ctxt))?,
                        _ => platform::gb::analyze(&mut file, Some(0x0100), None)?
                    }
                },
                _ => eprintln!("Unknown platform")
            }
        },
        _ => eprintln!("Please enter a command")
    }

    Ok(())
}
