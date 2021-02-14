//! Queue Response Type

use crate::analysis::Error;
use crate::arch::Architecture;
use crate::memory::Pointer;

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
