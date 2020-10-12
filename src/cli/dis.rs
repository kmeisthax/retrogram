//! High-level CLI routines

use crate::analysis::{Block, ReferenceKind};
use crate::arch::{Architecture, CompatibleLiteral};
use crate::asm::Assembler;
use crate::database::ProjectDatabase;
use crate::platform::Platform;
use crate::project::{Program, Project};
use crate::{analysis, input, maths, memory};
use clap::ArgMatches;
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::{fs, io};

fn dis_inner<AR, ASM>(
    project: &mut Project,
    prog: &Program,
    start_spec: &str,
    bus: &memory::Memory<AR>,
    arch: AR,
    asm: ASM,
) -> io::Result<()>
where
    AR: Architecture + 'static,
    for<'dw> AR::PtrVal: serde::Deserialize<'dw> + maths::FromStrRadix,
    for<'dw> AR::Offset: serde::Deserialize<'dw>,
    ASM: Assembler,
    ASM::Literal: CompatibleLiteral<AR, PtrVal = AR::PtrVal>,
{
    let pjdb: io::Result<ProjectDatabase> =
        ProjectDatabase::read(project, &mut fs::File::open(prog.as_database_path())?)
            .map_err(|e| e.into());
    let mut pjdb = match pjdb {
        Ok(pjdb) => pjdb,
        Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
            eprintln!("Creating new database for project");
            ProjectDatabase::new()
        }
        Err(e) => return Err(e),
    };

    let db = pjdb.get_database_mut(prog.as_name().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "You did not specify a name for the program to disassemble.",
        )
    })?).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "The architecture of the current program's database does not match the program's architecture."
        )
    })?;

    let start_pc = input::parse_ptr(start_spec, db, bus, arch).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            "Must specify a valid address to analyze",
        )
    })?;
    let start_block_id = db.find_block_membership(&start_pc).ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "The given memory location has not yet been successfully analyzed. Please scan it first."))?;

    let mut disassembly_blocks = BTreeSet::new();

    {
        let start_block = db.block(start_block_id).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                "LOGIC ERROR: The given PC returned a block ID that doesn't exist.",
            )
        })?;
        disassembly_blocks.insert(start_block.clone());
    }

    let mut found_targets = true;

    while found_targets {
        found_targets = false;

        let mut target_blocks = BTreeSet::new();

        //TODO: This routine unnecessarily scans blocks that already exist for
        //each run-through. We should find a way to stop that.
        for block in disassembly_blocks.iter() {
            for target in db
                .find_xrefs_from(block.as_start(), block.as_length().clone())
                .filter_map(|xref_id| db.xref(xref_id))
                .filter(|xref| matches!(xref.kind(), ReferenceKind::Code))
                .filter_map(|xref| xref.as_target())
            {
                let target_block_id = db.find_block_membership(target).ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "The given memory location has not yet been successfully analyzed. Please scan it first."))?;
                let target_block = db.block(target_block_id).ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::NotFound,
                        "LOGIC ERROR: The given PC returned a block ID that doesn't exist.",
                    )
                })?;

                if !disassembly_blocks.contains(target_block) {
                    found_targets = true;
                    target_blocks.insert(target_block);
                }
            }

            let mut has_subroutine_xrefs = false;
            for xref_id in db.find_xrefs_to(block.as_start(), block.as_length().clone()) {
                let xref = db.xref(xref_id).ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::NotFound,
                        "LOGIC ERROR: The given PC returned an xref ID that doesn't exist.",
                    )
                })?;

                if matches!(xref.kind(), analysis::ReferenceKind::Code) {
                    has_subroutine_xrefs = true;
                }
            }

            if !has_subroutine_xrefs {
                for source in db
                    .find_xrefs_from(block.as_start(), block.as_length().clone())
                    .filter_map(|xref_id| db.xref(xref_id))
                    .filter(|xref| matches!(xref.kind(), ReferenceKind::Code))
                    .map(|xref| xref.as_source())
                {
                    let source_block_id = db.find_block_membership(source).ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "The given memory location has not yet been successfully analyzed. Please scan it first."))?;
                    let source_block = db.block(source_block_id).ok_or_else(|| {
                        io::Error::new(
                            io::ErrorKind::NotFound,
                            "LOGIC ERROR: The given PC returned a block ID that doesn't exist.",
                        )
                    })?;

                    if !disassembly_blocks.contains(source_block) {
                        found_targets = true;
                        target_blocks.insert(source_block);
                    }
                }
            }
        }

        for block in target_blocks {
            disassembly_blocks.insert(block.clone());
        }
    }

    let mut merged_blocks = Vec::new();
    let mut last_block: Option<Block<AR>> = None;
    for block in disassembly_blocks {
        if let Some(lblock) = last_block {
            if lblock.can_coalesce(&block) {
                last_block = Some(lblock.coalesce(block));
            } else {
                merged_blocks.push(lblock);
                last_block = Some(block);
            }
        } else {
            last_block = Some(block);
            continue;
        }
    }

    if let Some(lblock) = last_block {
        merged_blocks.push(lblock);
    }

    for block in merged_blocks {
        let (orig_asm, _xrefs, pc_offset, _blocks, terminating_error) =
            analysis::disassemble_block(block.as_start().clone(), bus, arch);
        if let Some(pc_offset) = pc_offset {
            match pc_offset.cmp(&block.as_length()) {
                Ordering::Greater => {
                    let too_big = pc_offset - block.as_length().clone();
                    eprintln!(
                        "WARN: Block at {} is too large by {}",
                        block.as_start(),
                        too_big
                    );
                }
                Ordering::Less => {
                    let too_small = block.as_length().clone() - pc_offset;
                    eprintln!(
                        "WARN: Block at {} is too small by {}",
                        block.as_start(),
                        too_small
                    );
                }
                Ordering::Equal => {}
            }
        }

        if terminating_error.is_some() {
            eprintln!(
                "WARN: Block at {} terminates at an invalid instruction",
                block.as_start()
            );
        }

        let labeled_asm = analysis::replace_labels(orig_asm, db, bus);
        let injected_asm = analysis::inject_labels(labeled_asm, db);
        let orgd_asm = analysis::inject_orgs(injected_asm);

        asm.emit_section(&mut io::stdout(), &orgd_asm)?;
    }

    Ok(())
}

pub fn dis<'a>(project: &mut Project, prog: &Program, argv: &ArgMatches<'a>) -> io::Result<()> {
    let start_spec = argv
        .value_of("start_pc")
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Did not provide a start PC"))?;
    let image = prog
        .iter_images()
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Did not specify an image"))?;
    let mut file = fs::File::open(image)?;

    with_prog_architecture!(prog, |plat, arch, asm| {
        let bus = plat.construct_platform(&mut file)?;

        dis_inner(project, prog, start_spec, &bus, arch, asm)
    })
}
