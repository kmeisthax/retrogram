//! Queue Response Type

use crate::analysis::Error;
use crate::arch::{Architecture, CompatibleLiteral};
use crate::asm::Assembler;
use crate::ast::Instruction;
use crate::memory::{Memory, Offset, Pointer};
use crate::reg::Symbolic;
use num_traits::{One, Zero};
use std::sync::Arc;

/// Scan a specific starting PC and add the results of the analysis to the
/// database.
fn describe_scan_error<L, AR>(
    bus: &Memory<AR>,
    start_pc: &Pointer<AR::PtrVal>,
    pc_offset: Option<AR::Offset>,
    err: &Error<AR>,
) -> String
where
    L: CompatibleLiteral<AR>,
    AR: Architecture,
    AR::Offset: Offset<AR::PtrVal>, //I shouldn't have to do this.
    Symbolic<AR::Byte>: Default,
    Instruction<L>: Clone,
{
    match (pc_offset, err) {
        (Some(pc_offset), Error::Misinterpretation(size, true)) => {
            let mut values = String::new();
            let bad_pc = start_pc.clone() + pc_offset.clone();
            let mut iv_offset = start_pc.clone() + pc_offset;
            let end_offset = iv_offset.clone() + size.clone();

            while iv_offset < end_offset {
                //TODO: This generates incorrect results if MV::bound_width is not divisible by four
                if let Some(nval) = bus.read_unit(&iv_offset).into_concrete() {
                    values = format!("{}{:X}", &values, nval);
                } else {
                    //TODO: This assumes MV is always u8.
                    values = format!("{}??", &values);
                }

                iv_offset = iv_offset + AR::Offset::one();
            }

            format!("Decoding error at {:X} (from {:X}) on value {}", bad_pc, start_pc, values)
        },
        (Some(pc_offset), Error::Misinterpretation(size, false)) => { //Little-endian
            let mut values = String::new();
            let bad_pc = start_pc.clone() + pc_offset.clone();
            let mut iv_offset = start_pc.clone() + pc_offset;
            let end_offset = iv_offset.clone() + size.clone();

            while iv_offset < end_offset {
                //TODO: This generates incorrect results if MV::bound_width is not divisible by four
                if let Some(nval) = bus.read_unit(&iv_offset).into_concrete() {
                    values = format!("{:X}{}", nval, &values);
                } else {
                    //TODO: This assumes MV is always u8.
                    values = format!("??{}", &values);
                }

                iv_offset = iv_offset + AR::Offset::one();
            }

            format!("Decoding error at {:X} (from {:X}) on value {}", bad_pc, start_pc, values)
        },
        (Some(ref s), ref e) if *s == AR::Offset::zero() => format!("There is no valid code at {:X} due to {}", start_pc, e),
        (None, _) => format!("Disassembly size cannot be expressed in current type system, caused by analysis of {:X}", start_pc),
        _ => format!("Improperly terminated block discovered in {:X}", start_pc)
    }
}

/// A response to a queue command.
pub enum Response<AR>
where
    AR: Architecture,
{
    /// Result of declaring a new entrypoint.
    DeclaredEntryPoint(Pointer<AR::PtrVal>),

    /// Result of a static scan at a given address.
    StaticScanCode {
        /// The start of the scan.
        scan_start: Pointer<AR::PtrVal>,

        /// The length from the start to the end of the scan.
        ///
        /// A length of `None` signals a scan too long to represent as an
        /// `AR::Offset`.
        scan_end_offset: Option<AR::Offset>,

        /// The analysis error that occured analyzing the block, if any.
        error: Option<Error<AR>>,
    },

    /// Acknowledges that the power-on state was changed.
    PowerOnStateSet,

    /// Results of a dynamic trace at a given address.
    DynamicScanCode {
        /// The start of the scan.
        scan_start: Pointer<AR::PtrVal>,

        /// The end of the scan.
        scan_end: Pointer<AR::PtrVal>,

        /// The analysis error that occured tracing the block, if any.
        error: Option<Error<AR>>,
    },

    /// The number of scans extracted from the database.
    ///
    /// The first parameter indicates if the extract operation given allowed
    /// dynamic tracing.
    ///
    /// A count of zero indicates that scanning has completed. A non-zero count
    /// may indicate that further scans may be extractable from the database.
    ExtractScanCount(bool, usize),

    /// Acknowledges an issued `Fence` command, and restricts the reordering of
    /// responses as follows:
    ///
    /// 1. All responses before the `Fence` response originate from commands
    ///    before the acknowledged `Fence` command.
    /// 2. All responses after the `Fence` response originate from commands
    ///    after the acknowledged `Fence` response.
    ///
    /// It follows that one can determine if a command completed by issuing a
    /// `Fence` after it and waiting for it's response to come back.
    Fence,
}

impl<AR> Response<AR>
where
    AR: Architecture,
{
    /// Flatten the response into a text string suitable for logging.
    pub fn describe_response<ASM>(&self, bus: Arc<Memory<AR>>, _asm: ASM) -> String
    where
        ASM: Assembler,
        ASM::Literal: CompatibleLiteral<AR>,
    {
        match self {
            Response::DeclaredEntryPoint(p) => {
                format!("Marked ${:X} as an entry point", p)
            }
            Response::StaticScanCode {
                scan_start,
                scan_end_offset,
                error,
            } => {
                if let Some(error) = error {
                    describe_scan_error::<ASM::Literal, AR>(
                        &bus,
                        &scan_start,
                        scan_end_offset.clone(),
                        error,
                    )
                } else if let Some(scan_end_offset) = scan_end_offset {
                    format!(
                        "Static scan at ${:X} got ${:X} bytes",
                        scan_start, scan_end_offset
                    )
                } else {
                    format!("Static scan at ${:X}", scan_start)
                }
            }
            Response::PowerOnStateSet => "Power-on state set".to_string(),
            Response::DynamicScanCode {
                scan_start,
                scan_end,
                error,
            } => {
                if let Some(error) = error {
                    format!(
                        "Dynamic scan at ${:X} failed at ${:X} due to {}",
                        scan_start, scan_end, error
                    )
                } else {
                    format!(
                        "Dynamic scan at ${:X} forked at ${:X}",
                        scan_start, scan_end
                    )
                }
            }
            Response::ExtractScanCount(_with_dynamic, how_much) => {
                format!("Found {} more scans to complete...", how_much)
            }
            Response::Fence => "".to_string(),
        }
    }
}
