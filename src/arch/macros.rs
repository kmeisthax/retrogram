//! Architecture-related macros

/// Determine the architecture of an object and yield the architecture's type
/// for use in generic contexts.
///
/// This macro is intended to be invoked with something that implements
/// `AnyArch`.
macro_rules! with_architecture {
    ($erased:ident, |$arch:ident| $callback:block) => {
        match $erased.arch() {
            crate::arch::ArchName::Sm83 => {
                let $arch = crate::arch::sm83::Sm83();
                $callback
            }
            crate::arch::ArchName::AArch32 => {
                let $arch = crate::arch::aarch32::AArch32();
                $callback
            }

            #[cfg(test)]
            crate::arch::ArchName::Test => {
                let $arch = crate::arch::tests::TestArchitecture;
                $callback
            }
        }
    };
}

/// Execute a callback with a given set of architectural, platform, and
/// assembler related functions.
///
/// This macro must be invoked in order to almost anything generic with a
/// particular architecture. It is responsible for instantiating your code
/// across each architecture's particular type system.
macro_rules! with_prog_architecture {
    ($prog:ident, |$plat:ident, $arch:ident, $asm:ident| $callback:block) => {
        match crate::cli::resolve_program_config($prog) {
            Ok((
                crate::arch::ArchName::Sm83,
                crate::platform::PlatformName::Gb,
                crate::asm::AssemblerName::Rgbds,
            )) => {
                let $plat = crate::platform::gb::GbPlatform();
                let $asm = crate::asm::rgbds::Rgbds();
                let $arch = crate::arch::sm83::Sm83();
                $callback
            }
            Ok((
                crate::arch::ArchName::AArch32,
                crate::platform::PlatformName::Agb,
                crate::asm::AssemblerName::Armips,
            )) => {
                let $plat = crate::platform::agb::AgbPlatform();
                let $asm = crate::asm::armips::Armips();
                let $arch = crate::arch::aarch32::AArch32();
                $callback
            }
            Err(e) => Err(e),
            _ => Err(::std::io::Error::new(
                ::std::io::ErrorKind::Other,
                "Unsupported combination of architecture, platform, or assembler syntax.",
            )),
        }
    };
}
