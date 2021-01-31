//! Program identifier

use crate::arch::ArchName;
use crate::asm::AssemblerName;
use crate::platform::PlatformName;
use clap::{App, Arg, ArgMatches, ArgSettings};
use relative_path::{RelativePath, RelativePathBuf};
use serde::{Deserialize, Serialize};
use std::str::FromStr;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Program {
    platform: Option<PlatformName>,
    arch: Option<ArchName>,
    assembler: Option<AssemblerName>,
    images: Vec<String>,

    #[serde(skip)]
    name: Option<String>,

    #[serde(default)]
    data_sources: Vec<String>,

    #[serde(default = "default_db_filename")]
    database_path: RelativePathBuf,
}

fn default_db_filename() -> RelativePathBuf {
    RelativePathBuf::from("retrogram.db")
}

impl Default for Program {
    fn default() -> Self {
        Program {
            platform: None,
            arch: None,
            assembler: None,
            images: Vec::new(),
            name: None,
            data_sources: Vec::new(),
            database_path: default_db_filename(),
        }
    }
}

impl Program {
    pub fn configure_app<'a, 'b>(app: App<'a, 'b>) -> App<'a, 'b> {
        app.arg(
            Arg::with_name("image")
                .long("image")
                .value_name("image.bin")
                .help("The program image file(s) to analyze.")
                .takes_value(true)
                .set(ArgSettings::Global),
        )
        .arg(
            Arg::with_name("platform")
                .long("platform")
                .value_name("PLATFORM")
                .help("What platform to expect.")
                .takes_value(true)
                .set(ArgSettings::Global),
        )
        .arg(
            Arg::with_name("arch")
                .long("arch")
                .value_name("ARCH")
                .help("What architecture to expect.")
                .takes_value(true)
                .set(ArgSettings::Global),
        )
        .arg(
            Arg::with_name("asm")
                .long("asm")
                .value_name("ASM")
                .help("What assembler syntax to output.")
                .takes_value(true)
                .set(ArgSettings::Global),
        )
    }

    /// Construct a Program from clap ArgMatches
    pub fn from_arg_matches(args: &ArgMatches) -> Program {
        Program {
            platform: args
                .value_of("platform")
                .and_then(|s| PlatformName::from_str(s).ok()),
            arch: args
                .value_of("arch")
                .and_then(|s| ArchName::from_str(s).ok()),
            assembler: args
                .value_of("asm")
                .and_then(|s| AssemblerName::from_str(s).ok()),
            images: args
                .values_of("image")
                .map_or(Vec::new(), |v| v.map(|s| s.to_string()).collect()),
            name: None,
            data_sources: Vec::new(),
            database_path: default_db_filename(),
        }
    }

    pub fn platform(&self) -> Option<PlatformName> {
        self.platform
    }

    pub fn set_platform(&mut self, platform: PlatformName) {
        self.platform = Some(platform);
    }

    pub fn unset_platform(&mut self) {
        self.platform = None;
    }

    pub fn arch(&self) -> Option<ArchName> {
        self.arch
    }

    pub fn set_arch(&mut self, arch: ArchName) {
        self.arch = Some(arch);
    }

    pub fn unset_arch(&mut self) {
        self.arch = None;
    }

    pub fn assembler(&self) -> Option<AssemblerName> {
        self.assembler
    }

    pub fn set_assembler(&mut self, asm: AssemblerName) {
        self.assembler = Some(asm);
    }

    pub fn unset_assembler(&mut self) {
        self.assembler = None;
    }

    /// List all the image files related to a given program.
    pub fn iter_images(&self) -> impl Iterator<Item = &str> {
        self.images.iter().map(|s| s.as_ref())
    }

    /// Add an image path.
    pub fn add_image_path(&mut self, path: &str) {
        self.images.push(path.to_string());
    }

    /// Remove an image by index.
    pub fn remove_image_index(&mut self, index: usize) {
        self.images.remove(index);
    }

    /// List all the external data sources this program pulls data from.
    pub fn iter_sources(&self) -> impl Iterator<Item = &str> {
        self.data_sources.iter().map(|s| s.as_ref())
    }

    pub fn as_database_path(&self) -> &RelativePath {
        self.database_path.as_ref()
    }

    pub fn as_name(&self) -> Option<&str> {
        match &self.name {
            Some(name) => Some(&name),
            None => None,
        }
    }

    pub fn set_name(&mut self, name: &str) {
        self.name = Some(name.to_string());
    }

    pub fn apply_override(&self, other: &Program) -> Program {
        Program {
            platform: other.platform.or(self.platform),
            arch: other.arch.or(self.arch),
            assembler: other.assembler.or(self.assembler),
            name: other.name.clone().or_else(|| self.name.clone()),
            images: match other.images.len() {
                0 => self.images.clone(),
                _ => other.images.clone(),
            },
            data_sources: match other.data_sources.len() {
                0 => self.data_sources.clone(),
                _ => other.data_sources.clone(),
            },
            database_path: default_db_filename(),
        }
    }
}
