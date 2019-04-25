//! High-level CLI routines

use std::{io, fs};
use std::ops::{Add, Sub};
use std::hash::Hash;
use std::str::FromStr;
use std::convert::TryFrom;
use std::fmt::{Display, LowerHex, UpperHex};
use crate::retrogram::{asm, ast, arch, analysis, project, platform, input, memory};

fn dis_inner<I, F, P, MV, S, IO, DIS>(start_spec: &str, db: &mut analysis::Database<P>, bus: &memory::Memory<P, MV, S, IO>, disassemble_block: DIS) -> io::Result<ast::Assembly<I, F, P>>
    where P: memory::PtrNum<S> + Clone + Eq + Hash + FromStr + Display + LowerHex + UpperHex + TryFrom<u64>,
        S: memory::Offset<P>,
        I: Clone + Display,
        F: Clone + Display,
        DIS: Fn(Option<memory::Pointer<P>>, &memory::Memory<P, MV, S, IO>) -> io::Result<ast::Assembly<I, F, P>> {
    let start_pc = input::parse_ptr(start_spec, db, bus);
    let orig_asm = disassemble_block(start_pc, bus)?;

    match orig_asm.iter_lines().next() {
        Some(line) => {
            db.insert_placeholder_label(line.source_address().clone(), analysis::ReferenceKind::Unknown);
        },
        _ => {}
    }
    
    let labeled_asm = analysis::replace_labels(orig_asm, db, bus);
    let injected_asm = analysis::inject_labels(labeled_asm, db);

    Ok(injected_asm)
}

pub fn dis(prog: &project::Program, start_spec: &str) -> io::Result<()> {
    let image = prog.iter_images().next().ok_or(io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;
    let arch = match prog.arch() {
        Some(a) => a,
        None => match prog.platform() {
            Some(platform::PlatformName::GB) => arch::ArchName::LR35902,
            _ => return Err(io::Error::new(io::ErrorKind::Other, "No viable default architecture for platform and none was selected"))
        }
    };

    match arch {
        arch::ArchName::LR35902 => {
            let mut db = analysis::Database::for_program(prog, asm::rgbds::parse_symbol_file)?;
            let bus = match prog.platform() {
                Some(platform::PlatformName::GB) => platform::gb::construct_platform(&mut file, platform::gb::PlatformVariant::MBC5Mapper)?,
                _ => return Err(io::Error::new(io::ErrorKind::Other, "Invalid platform for architecture"))
            };

            let asm = dis_inner(start_spec, &mut db, &bus, arch::lr35902::disassemble_block)?;

            match prog.assembler() {
                Some(asm::AssemblerName::RGBDS) => println!("{}", asm::rgbds::RGBDSAstFormatee::wrap(&asm)),
                _ => eprintln!("Please specify an assembler to print with")
            };
        },
        _ => return Err(io::Error::new(io::ErrorKind::Other, "oops"))
    }

    match prog.platform() {
        Some(platform::PlatformName::GB) => {
        },
        _ => eprintln!("Unknown platform")
    }

    Ok(())
}