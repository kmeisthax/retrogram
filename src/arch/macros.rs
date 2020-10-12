//! Architecture-related macros

/// Execute a callback with a given set of architectural, platform, and
/// assembler related functions.
///
/// This macro must be invoked in order to almost anything generic with a
/// particular architecture. It is responsible for instantiating your code
/// across each architecture's particular type system.
macro_rules! with_prog_architecture {
    ($prog:ident, |$plat:ident, $arch:ident, $asm:ident| $callback:block) => {
        match crate::cli::resolve_program_config($prog)? {
            (
                crate::arch::ArchName::SM83,
                crate::platform::PlatformName::GB,
                crate::asm::AssemblerName::RGBDS,
            ) => {
                let $plat = crate::platform::gb::GBPlatform();
                let $asm = crate::asm::rgbds::RGBDS();
                let $arch = crate::arch::sm83::SM83();
                $callback
            }
            (
                crate::arch::ArchName::AARCH32,
                crate::platform::PlatformName::AGB,
                crate::asm::AssemblerName::ARMIPS,
            ) => {
                let $plat = crate::platform::agb::AGBPlatform();
                let $asm = crate::asm::armips::ARMIPS();
                let $arch = crate::arch::aarch32::AArch32();
                $callback
            }
            _ => Err(::std::io::Error::new(
                ::std::io::ErrorKind::Other,
                "Unsupported combination of architecture, platform, or assembler syntax.",
            )),
        }
    };
}

/// Execute a callback with the architecture for a given database.
///
/// It is expected that `$db` is a boxed `AnyDatabase`. You must also expand
/// this macro in a context that is currently using the `Any` trait.
///
/// Your callback will be given a mutable reference to a "concrete",
/// type-bearing version of the database.
///
/// This yields an IO error if the database type could not be determined.
macro_rules! with_db_architecture {
    ($db:ident, |$concrete_db: ident, $arch:ident| $callback:block) => {
        if let Some($concrete_db) = $db.as_any().downcast_ref::<crate::arch::sm83::SM83>() {
            let $arch = crate::arch::sm83::SM83();
            $callback
        } else if let Some($concrete_db) =
            $db.as_any().downcast_ref::<crate::arch::aarch32::AArch32>()
        {
            let $arch = crate::arch::aarch32::AArch32();
            $callback
        } else {
            Err(::std::io::Error::new(
                ::std::io::ErrorKind::Other,
                "Unsupported architecture for database.",
            ))
        }
    };
}
