//! High-level CLI routines

use std::{io, fs};
use std::collections::BTreeSet;
use crate::{asm, ast, arch, analysis, project, platform, input, memory, cli, maths};

fn dis_inner<I, S, F, P, MV, MS, IO, DIS, FMT, APARSE>(prog: &project::Program,
        start_spec: &str,
        bus: &memory::Memory<P, MV, MS, IO>,
        disassemble: DIS,
        format_and_print: FMT,
        architectural_ctxt_parse: APARSE) -> io::Result<()>
    where for <'dw> P: memory::PtrNum<MS> + analysis::Mappable + cli::Nameable + serde::Deserialize<'dw> +
            maths::FromStrRadix,
        for <'dw> MS: memory::Offset<P> + analysis::Mappable + serde::Deserialize<'dw>,
        ast::Operand<I, S, F, P>: Clone,
        ast::Instruction<I, S, F, P>: Clone,
        ast::Directive<I, S, F, P, MV, MS>: Clone,
        DIS: analysis::Disassembler<I, S, F, P, MV, MS, IO>,
        FMT: Fn(&ast::Section<I, S, F, P, MV, MS>),
        APARSE: FnOnce(&mut &[&str], &mut memory::Pointer<P>) -> Option<()> {
    
    let mut pjdb = match project::ProjectDatabase::read(prog.as_database_path()) {
        Ok(pjdb) => pjdb,
        Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
            eprintln!("Creating new database for project");
            project::ProjectDatabase::new()
        },
        Err(e) => return Err(e)
    };

    let db = pjdb.get_database_mut(prog.as_name().ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "You did not specify a name for the program to disassemble."))?);
    db.update_indexes();

    let start_pc = input::parse_ptr(start_spec, db, bus, architectural_ctxt_parse).ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Must specify a valid address to analyze"))?;
    let start_block_id = db.find_block_membership(&start_pc).ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "The given memory location has not yet been successfully analyzed. Please scan it first."))?;

    let mut disassembly_blocks = BTreeSet::new();

    {
        let start_block = db.block(start_block_id).ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "LOGIC ERROR: The given PC returned a block ID that doesn't exist."))?;
        disassembly_blocks.insert(start_block.clone());
    }
    
    let mut found_targets = true;

    while found_targets {
        found_targets = false;
        
        let mut target_blocks = BTreeSet::new();

        //TODO: This routine unnecessarily scans blocks that already exist for
        //each run-through. We should find a way to stop that.
        for block in disassembly_blocks.iter() {
            for xref_id in db.find_xrefs_from(block.as_start(), block.as_length().clone()) {
                let xref = db.xref(xref_id).ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "LOGIC ERROR: The given PC returned an xref ID that doesn't exist."))?;
                
                match (xref.kind(), xref.as_target()) {
                    (analysis::ReferenceKind::Code, Some(target)) => {
                        let target_block_id = db.find_block_membership(target).ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "The given memory location has not yet been successfully analyzed. Please scan it first."))?;
                        let target_block = db.block(target_block_id).ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "LOGIC ERROR: The given PC returned a block ID that doesn't exist."))?;

                        if !disassembly_blocks.contains(target_block) {
                            found_targets = true;
                            target_blocks.insert(target_block);
                        }
                    },
                    _ => {}
                }
            }
        }

        for block in target_blocks {
            disassembly_blocks.insert(block.clone());
        }
    }

    for block in disassembly_blocks {
        let (orig_asm, _xrefs, pc_offset, _blocks, terminating_error) = analysis::disassemble_block(block.as_start().clone(), bus, &disassemble);
        if let Some(pc_offset) = pc_offset {
            if pc_offset > block.as_length().clone() {
                if let Ok(too_big) = MS::try_from(pc_offset - block.as_length().clone()) {
                    eprintln!("WARN: Block at {} is too large by {}", block.as_start(), too_big);
                }
            } else if pc_offset < block.as_length().clone() {
                if let Ok(too_small) = MS::try_from(block.as_length().clone() - pc_offset) {
                    eprintln!("WARN: Block at {} is too small by {}", block.as_start(), too_small);
                }
            }
        }

        if let Some(e) = terminating_error {
            eprintln!("WARN: Block at {} terminates at an invalid instruction", block.as_start());
        }

        let labeled_asm = analysis::replace_labels(orig_asm, db, bus);
        let injected_asm = analysis::inject_labels(labeled_asm, db);
        let orgd_asm = analysis::inject_orgs(injected_asm);
        format_and_print(&orgd_asm);
    }

    Ok(())
}

pub fn dis(prog: &project::Program, start_spec: &str) -> io::Result<()> {
    let platform = prog.platform().ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified platform, analysis cannot continue."))?;
    let arch = prog.arch().or_else(|| platform.default_arch()).ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified architecture, analysis cannot continue."))?;
    let asm = prog.assembler().or_else(|| arch.default_asm()).ok_or(io::Error::new(io::ErrorKind::InvalidInput, "Unspecified assembler for architecture, analysis cannot continue."))?;
    let image = prog.iter_images().next().ok_or(io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;

    match (arch, platform, asm) {
        (arch::ArchName::SM83, platform::PlatformName::GB, _) => dis_inner(prog, start_spec,
            &platform::gb::construct_platform(&mut file, platform::gb::PlatformVariant::MBC5Mapper)?,
            arch::sm83::disassemble,
            |asm| println!("{}", asm::rgbds::SectionFmtWrap::wrap(&asm)),
            |_, _| Some(())),
        (arch::ArchName::AARCH32, platform::PlatformName::AGB, _) => dis_inner(prog, start_spec,
            &platform::agb::construct_platform(&mut file)?,
            arch::aarch32::disassemble,
            |asm| println!("{}", asm::armips::SectionFmtWrap::wrap(&asm)),
            arch::aarch32::architectural_ctxt_parse),
        _ => return Err(io::Error::new(io::ErrorKind::Other, "oops"))
    }
}