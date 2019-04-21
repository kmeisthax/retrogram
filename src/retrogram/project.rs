//! Project file structures

use std::{io, fs};
use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use argparse;
use serde_json;
use crate::retrogram::platform::PlatformName;
use crate::retrogram::arch::ArchName;
use crate::retrogram::asm::AssemblerName;

#[derive(Serialize, Deserialize)]
pub struct Program {
    platform: Option<PlatformName>,
    arch: Option<ArchName>,
    assembler: Option<AssemblerName>,
    images: Vec<String>,
    symbol_files: Vec<String>,
}

impl Default for Program {
    fn default() -> Self {
        Program {
            platform: None,
            arch: None,
            assembler: None,
            images: Vec::new(),
            symbol_files: Vec::new()
        }
    }
}

impl Program {
    pub fn refer_args<'a, 'b>(&'a mut self, ap: &'b mut argparse::ArgumentParser<'a>) {
        ap.refer(&mut self.images).add_option(&["--image"], argparse::Collect, "The program image file(s) to analyze.");
        ap.refer(&mut self.symbol_files).add_option(&["--symbol_file"], argparse::Collect, "Symbol files, if any, to look up symbols from.");
        ap.refer(&mut self.platform).add_option(&["--platform"], argparse::StoreOption, "What platform to expect");
        ap.refer(&mut self.arch).add_option(&["--arch"], argparse::StoreOption, "What architecture to expect");
        ap.refer(&mut self.assembler).add_option(&["--asm"], argparse::StoreOption, "What assembler to output");
    }

    pub fn platform(&self) -> Option<PlatformName> {
        self.platform
    }

    /// List all the image files related to a given program.
    /// 
    /// TODO: This should return &str, not &String, no?
    pub fn iter_images<'a>(&'a self) -> impl Iterator<Item = &String> + 'a {
        self.images.iter()
    }

    /// List all the symbol files related to a given program.
    /// 
    /// TODO: This should return &str, not &String, no?
    pub fn iter_symbol_files<'a>(&'a self) -> impl Iterator<Item = &String> + 'a {
        self.symbol_files.iter()
    }
}

#[derive(Serialize, Deserialize)]
pub struct Project {
    programs: HashMap<String, Program>
}

impl Project {
    pub fn read() -> io::Result<Self> {
        let mut project_file = fs::File::open("retrogram.json")?;
        let project = serde_json::from_reader(project_file)?;

        Ok((project))
    }
}