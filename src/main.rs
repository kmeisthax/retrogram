mod retrogram;

use std::{str, fs, io};
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
    let mut start_pc = None;

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

            match platform {
                Some(PlatformName::GB) => platform::gb::analyze(&mut file, start_pc)?,
                _ => eprintln!("Unknown platform")
            }
        },
        _ => eprintln!("Please enter a command")
    }

    Ok(())
}
