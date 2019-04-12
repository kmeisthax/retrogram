//! Special-purpose types for modeling program and memory behavior

/// Represents the various possible operating modes of a memory block.
/// 
/// The Behavior of a memory area bounds what analysises are considered
/// meaningful for a program.
pub enum Behavior {
    /// Storage behavior corresponds to memory which can be consistently read
    /// to and written from, such that reads always capture the last write,
    /// repeated reads always capture the same value, and there is no semantic
    /// value to writes aside from changing the value to be read back.
    /// Execution is allowed from storage only.
    Storage,
    
    /// MappedIO behavior corresponds to hardware devices on a memory bus.
    /// Reads are not consistent, writes have semantic value. The intent of
    /// such a memory block is communication with hardware. Execution is not
    /// analyzed within mapped I/O.
    MappedIO,
    
    /// Invalid behavior corresponds to memory not mapped to a hardware device
    /// at all. Platform makes no guarantees about behavior upon reading to or
    /// writing from an Invalid memory block. Execution, reads, and writes are
    /// not analyzed within invalid blocks.
    Invalid,
}

/// Represents the various possible actions that can be performed by a program
/// under analysis.
pub enum Action {
    DataRead,
    Write,
    ProgramExecute
}