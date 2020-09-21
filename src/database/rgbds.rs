//! Database import support for RGBDS build artifacts (sym/map)

use crate::arch::sm83;
use crate::ast;
use crate::database::Database;
use crate::platform::gb;
use crate::project;
use std::io::BufRead;
use std::ops::{BitOr, Shl};
use std::{fs, io};

fn str2hex<I>(thestr: &str) -> Option<I>
where
    I: From<u8> + Shl + From<<I as Shl>::Output> + BitOr + From<<I as BitOr>::Output>,
{
    let mut out = I::from(0);

    for char in thestr.chars() {
        match char {
            '0' => out = I::from(I::from(out << I::from(4)) | I::from(0)),
            '1' => out = I::from(I::from(out << I::from(4)) | I::from(1)),
            '2' => out = I::from(I::from(out << I::from(4)) | I::from(2)),
            '3' => out = I::from(I::from(out << I::from(4)) | I::from(3)),
            '4' => out = I::from(I::from(out << I::from(4)) | I::from(4)),
            '5' => out = I::from(I::from(out << I::from(4)) | I::from(5)),
            '6' => out = I::from(I::from(out << I::from(4)) | I::from(6)),
            '7' => out = I::from(I::from(out << I::from(4)) | I::from(7)),
            '8' => out = I::from(I::from(out << I::from(4)) | I::from(8)),
            '9' => out = I::from(I::from(out << I::from(4)) | I::from(9)),
            'A' => out = I::from(I::from(out << I::from(4)) | I::from(10)),
            'B' => out = I::from(I::from(out << I::from(4)) | I::from(11)),
            'C' => out = I::from(I::from(out << I::from(4)) | I::from(12)),
            'D' => out = I::from(I::from(out << I::from(4)) | I::from(13)),
            'E' => out = I::from(I::from(out << I::from(4)) | I::from(14)),
            'F' => out = I::from(I::from(out << I::from(4)) | I::from(15)),
            _ => return None,
        }
    }

    Some(out)
}

/// Read the symbols from an RGBDS symbol file.
pub fn parse_symbol_file(
    _prog: &project::Program,
    _dsrc: &project::DataSource,
    files: &mut [io::BufReader<fs::File>],
    db: &mut Database<sm83::SM83>,
) -> io::Result<()> {
    for file in files {
        for line in file.lines() {
            let line = line?;
            let mut split = line.split(' ');

            if let Some(ptr_str) = split.next() {
                if ptr_str.starts_with(';') {
                    continue;
                }

                let mut ptr_split = ptr_str.split(':');
                let mut bank_addr: u16 = 0;
                let mut ptr_addr: u16 = 0;
                if let Some(bank_part) = ptr_split.next() {
                    bank_addr = str2hex(bank_part).unwrap_or(0);
                }

                if let Some(ptr_part) = ptr_split.next() {
                    ptr_addr = str2hex(ptr_part).unwrap_or(0);
                }

                if let Some(ctxt_ptr) = gb::create_context(&[bank_addr, ptr_addr]) {
                    if let Some(label_str) = split.next() {
                        let mut name_split = label_str.split('.');

                        if let Some(global_part) = name_split.next() {
                            if let Some(local_part) = name_split.next() {
                                db.upsert_symbol(
                                    ast::Label::new(local_part, Some(global_part)),
                                    ctxt_ptr,
                                );
                            } else {
                                db.upsert_symbol(ast::Label::new(global_part, None), ctxt_ptr);
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(())
}
