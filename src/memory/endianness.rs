//! Types to represent endianness.

/// Indicates the byte (or memory unit) order when reading words from memory.
pub enum Endianness {
    /// Words are stored in memory least significant bit first.
    ///
    /// This is "backwards" compared to how English writes numbers.
    LittleEndian,

    /// Words are stored in memory most significant bit first.
    ///
    /// This is the same order as how English writes numbers.
    BigEndian,
}
