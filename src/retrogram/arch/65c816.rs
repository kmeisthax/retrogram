//! 65C816 CPU architecture. Extension of the 6502 to ""16-bit"" operation.
//! 
//! I swear, if this counts as 16-bit, then the god damned Sega Genesis was
//! 32-bit. Do the math.
//! 
//! Used as the main CPU for Apple ][gs and Super Famicom platforms.
//! Also used as accelerator chips for Commodore 64 and Super Famicom.

/// Names of all 65C816 registers we care about.
/// 
/// A, X, Y, and S are variable-width: various bits of P either shorten or
/// lengthen them to 8 or 16 bits. The rest of the registers work as follows:
/// 
///  * PC is always 16 bits, and PB is always 8. They combine to form a 24-bit
///    memory address to retrieve the next instruction from.
///  * P is always 8 bits.
///  * DB is always 8 bits. It combines with the calculated address of any data
///    read or write instruction to form a 24-bit memory address to retrieve the
///    next instruction from.
/// 
/// The variable width nature of certain registers also means that we need an
/// architectural context for the widths of certain registers.
enum Register {
    /// General-purpose Accumulator.
    A,
    /// Index register, type X
    X,
    /// Index register, type Y
    Y,
    /// Stack pointer register
    S,
    /// Processor Status register
    P,
    /// Zero page pointer register
    D,
    /// Data bank
    DB,
    /// Program bank
    PB
};

/// Enum type representing a register value.
/// 
/// Data width varies based on context, so this needs to be able to hold either
/// u8 or u16s.
enum Data {
    u8(u8),
    u16(u16)
};

