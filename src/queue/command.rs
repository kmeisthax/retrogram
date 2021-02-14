//! Queue Command Type

use crate::analysis::Fork;
use crate::arch::Architecture;
use crate::memory::Pointer;
use crate::reg::State;

/// A command issued by some interactive thread into the analysis process.
///
/// Each command generates a subsequent `Response<AR>`. Responses will be
/// returned in any order in the response channel as the underlying work is
/// completed. Commands are permitted to issue multiple responses.
#[derive(Eq, PartialEq)]
pub enum Command<AR>
where
    AR: Architecture,
{
    /// Declare a new entrypoint into the program.
    DeclareEntryPoint(Pointer<AR::PtrVal>),

    /// Start a static scan at a given address.
    StaticScanCode(Pointer<AR::PtrVal>),

    /// Set the power-on state that all dynamic analysis in any future command
    /// will use.
    SetPowerOnState(State<AR>),

    /// Start a dynamic trace on a given address.
    DynamicScanCode(Fork<AR>),

    /// Execute all scans implied by the database.
    ///
    /// The `bool` parameter is `true` to indicate if dynamic scans should also
    /// be extracted.
    ///
    /// This command generates multiple responses: first, the number of scans
    /// that were found, followed by the responses generated by those scans.
    ///
    /// It is highly recommended to surround this command with fences so that
    /// you know that it has completed.
    ExtractAllScans(bool),

    /// Restrict the reordering of work such that all previously-issued
    /// commands have completed before any subsequently-issued commands are
    /// executed.
    ///
    /// This also emits a corresponding `Fence` response with the same
    /// reordering restriction on the responses for said work. You can use this
    /// response to determine when a given set of commands have completed.
    Fence,
}
