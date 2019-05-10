//! High-level CLI routines

use std::{io, fs};
use std::str::FromStr;
use std::convert::TryFrom;
use std::fmt::{Display, LowerHex, UpperHex};
use crate::retrogram::{asm, ast, arch, analysis, project, platform, input, memory};

/// Guard trait for pointer values users can input to us and we can output.
/// 
/// Effectively, this covers all of the operations we need to do in order for
/// both the program and it's users to be able to name locations within a
/// program.
/// 
/// The trait bounds, in plain english, require that we can:
/// 
///  * Parse the value from a string
///  * Display the value, with upper or lowercase hexdecimal notation if needed
///  + Attempt to convert the value from a u64 (for user input contexts)
pub trait Nameable : Clone + FromStr + Display + LowerHex + UpperHex + TryFrom<u64> {

}

impl<T> Nameable for T where T: Clone + FromStr + Display + LowerHex + UpperHex + TryFrom<u64> {

}

fn dis_inner<I, S, F, P, MV, MS, IO, DIS>(start_spec: &str, db: &mut analysis::Database<P, MS>, bus: &memory::Memory<P, MV, MS, IO>, disassemble: &DIS) -> io::Result<ast::Section<I, S, F, P>>
    where P: memory::PtrNum<MS> + analysis::Mappable + Nameable,
        MS: memory::Offset<P>,
        memory::Pointer<P>: std::fmt::Debug,
        I: Clone + Display,
        S: Clone + Display,
        F: Clone + Display,
        DIS: Fn(&memory::Pointer<P>, &memory::Memory<P, MV, MS, IO>) -> (Option<ast::Instruction<I, S, F, P>>, MS, bool, Vec<Option<memory::Pointer<P>>>) {
    let start_pc = input::parse_ptr(start_spec, db, bus);
    let (orig_asm, targets, xrefs) = analysis::disassemble_block(start_pc.expect("Must specify a valid address to analyze"), bus, disassemble)?;

    for target in targets {
        if let Some(target) = target {
            if let None = db.pointer_label(&target) {
                db.insert_placeholder_label(target, analysis::ReferenceKind::Code);
            }
        }
    }

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
            Some(platform::PlatformName::AGB) => arch::ArchName::AARCH32,
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

            let asm = dis_inner(start_spec, &mut db, &bus, &arch::lr35902::disassemble)?;

            match prog.assembler() {
                Some(asm::AssemblerName::RGBDS) => println!("{}", asm::rgbds::RGBDSAstFormatee::wrap(&asm)),
                _ => eprintln!("Please specify an assembler to print with")
            };
        },
        arch::ArchName::AARCH32 => {
            let mut db = analysis::Database::new(); //analysis::Database::for_program(prog, asm::armips::parse_symbol_file)?;
            let bus = match prog.platform() {
                Some(platform::PlatformName::AGB) => platform::agb::construct_platform(&mut file)?,
                _ => return Err(io::Error::new(io::ErrorKind::Other, "Invalid platform for architecture"))
            };

            let asm = dis_inner(start_spec, &mut db, &bus, &arch::aarch32::disassemble)?;

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