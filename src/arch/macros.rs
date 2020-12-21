//! Architecture-related macros

/// Determine the architecture of an object and yield the architecture's type
/// for use in generic contexts.
///
/// This macro is intended to be invoked with something that implements
/// `AnyArch`.
macro_rules! with_architecture {
    ($erased:ident, |$arch:ident| $callback:block) => {
        match $erased.arch() {
            crate::arch::ArchName::SM83 => {
                let $arch = crate::arch::sm83::SM83();
                $callback
            }
            crate::arch::ArchName::AARCH32 => {
                let $arch = crate::arch::aarch32::AArch32();
                $callback
            }

            #[cfg(test)]
            crate::arch::ArchName::TEST => {
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
                crate::arch::ArchName::SM83,
                crate::platform::PlatformName::GB,
                crate::asm::AssemblerName::RGBDS,
            )) => {
                let $plat = crate::platform::gb::GBPlatform();
                let $asm = crate::asm::rgbds::RGBDS();
                let $arch = crate::arch::sm83::SM83();
                $callback
            }
            Ok((
                crate::arch::ArchName::AARCH32,
                crate::platform::PlatformName::AGB,
                crate::asm::AssemblerName::ARMIPS,
            )) => {
                let $plat = crate::platform::agb::AGBPlatform();
                let $asm = crate::asm::armips::ARMIPS();
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
