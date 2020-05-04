//! Special-purpose types for modeling program and memory behavior

/// Indicates the semantics of data reads, data writes, and execution on a given
/// memory region.
///
/// Each action listed above can have different semantics.
///
/// The Behavior of a memory area bounds what analysises are considered
/// meaningful for a program.
#[derive(PartialEq, Eq)]
pub enum Behavior {
    /// Memory behavior corresponds to regions which obey the semantics of
    /// normal memory devices (e.g. RAM, ROM, etc):
    ///
    /// 1. Writes, if allowed, have no semantic value beyond changing the memory
    ///    at that address
    /// 2. Reads from writable memory return the last value written, or an
    ///    unconstrained symbol if they have not yet been written
    /// 3. Reads from read-only memory return constrained symbols
    /// 4. Execution is semantically valid
    Memory,

    /// MappedIO behavior corresponds to regions which do not obey memory
    /// semantics:
    ///
    /// 1. Writes and reads have arbitrary semantic value
    /// 2. Repeated writes and reads have semantic value regardless of the value
    ///    written or read
    /// 3. Execution is semantically invalid (though may be possible)
    MappedIO,

    /// Invalid behavior corresponds to regions which do not respond to this
    /// particular action. If multiple regions are valid for a given address,
    /// the highest non-Invalid region determines the semantics of the action.
    /// If all regions are invalid then the analysis is invalid and should be
    /// cancelled.
    Invalid,
}

/// Represents the various possible actions that can be performed by a program
/// under analysis.
///
/// Each block has a slot for each action.
pub enum Action {
    DataRead,
    DataWrite,
    ProgramExecute,
}
