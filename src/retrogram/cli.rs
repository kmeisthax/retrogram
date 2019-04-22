//! High-level CLI routines

use std::{io, fs};
use crate::retrogram::{asm, arch, analysis, project, platform, input};

pub fn dis(prog: &project::Program, start_pc: &str) -> io::Result<()> {
    let image = prog.iter_images().next().ok_or(io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;

    match prog.platform() {
        Some(platform::PlatformName::GB) => {
            let mut db = analysis::Database::new();
            let bus = platform::gb::construct_platform(&mut file, platform::gb::PlatformVariant::MBC5Mapper)?;

            for symbol_file in prog.iter_symbol_files() {
                if let Ok(file) = fs::File::open(symbol_file) {
                    asm::rgbds::parse_symbol_file(io::BufReader::new(file), &mut db)?;
                }
            }

            let orig_asm = arch::lr35902::disassemble_block(input::parse_ptr(start_pc, &db, platform::gb::create_context::<u16>).ok(), &bus)?;

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

    Ok(())
}