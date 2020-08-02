//! Architecture trait

use crate::analysis::{Disasm, Mappable, Prerequisite, Result, Trace};
use crate::ast::Literal;
use crate::cli::Nameable;
use crate::maths::{Numerical, Popcount};
use crate::memory::{Memory, Offset, Pointer, PtrNum};
use crate::reg::{Bitwise, State};
use num::Bounded;
use std::convert::TryInto;
use std::fmt::Display;
use std::str::FromStr;

/// Indicates a `Literal` that is specifically compatible with a given
/// architecture's formatting needs.
pub trait CompatibleLiteral<AR>:
    Literal + From<AR::Word> + From<AR::Byte> + From<AR::Offset> + From<Pointer<AR::PtrVal>>
where
    AR: Architecture,
{
}

impl<T, AR> CompatibleLiteral<AR> for T
where
    AR: Architecture,
    T: Literal + From<AR::Word> + From<AR::Byte> + From<AR::Offset> + From<Pointer<AR::PtrVal>>,
{
}

/// Trait which represents all of the analysis methods an architecture
/// must provide in order to be supported.
pub trait Architecture
where
    Self: Copy,
    Self::Register: Mappable + Display + FromStr,
    Self::Word: Bitwise
        + Numerical
        + Popcount<Output = Self::Word>
        + TryInto<u64>
        + Mappable
        + Nameable
        + Ord
        + Bounded,
    Self::Byte: Bitwise
        + Numerical
        + Popcount<Output = Self::Byte>
        + TryInto<u64>
        + Mappable
        + Nameable
        + Ord
        + Bounded,
    Self::PtrVal: PtrNum<Self::Offset> + Mappable + Nameable,
    Self::Offset: Offset<Self::PtrVal> + Mappable + Nameable + Numerical,
{
    /// The type which represents all possible register names in a given
    /// architecture.
    ///
    /// In some architectures, notably AArch32, the program counter is treated
    /// as a normal register that can be operated upon. In such architectures,
    /// you must either leave off that register from this type, or ensure that
    /// it is always in synchronization with the contextualized program counter
    /// when tracing code.
    ///
    /// This type is customarily referred to as `RK` in other trait bounds.
    type Register;

    /// The type which represents a register value.
    ///
    /// In the case that an architecture has multiple widths of registers, then
    /// this type must either enumerate all possible register widths, or it
    /// must use a representation wide enough to hold all of them and ensure
    /// that any unused bits do not affect the results of tracing. It must also
    /// ensure that register values intended for one type or width of register
    /// do not get set on registers which cannot architecturally contain them
    /// without being first converted.
    ///
    /// This type is customarily referred to as `I` in other trait bounds.
    type Word;

    /// The type which represents a byte as addressed by memory.
    ///
    /// In most modern architectures, bytes are 8 bits wide, and this should be
    /// `u8`. Some exotic architectures are "word-addressed": incrementing an
    /// address by one results in skipping more or less than eight bits in the
    /// resulting memory. In that case, `Byte` would need to be wider or
    /// narrower than 8 bits.
    ///
    /// Note that most processors whose memory buses read or write more than
    /// one byte at a time do *not* qualify as word-addressed; as reading the
    /// next address still returns a byte even though the memory device it
    /// comes from works in wider units of data.
    ///
    /// This type is customarily referred to as `MV` in other trait bounds.
    type Byte;

    /// The type which represents this architecture's memory addresses.
    ///
    /// An architecture is permitted to have non-numerical memory addresses,
    /// such as architectures with separate I/O and memory address spaces. In
    /// this case, you would use an enum type with an option for each separate
    /// bus, and provide a separate `Offset` type which can be added to any
    /// address to get a new one within the same bus.
    ///
    /// This type is customarily referred to as `P` in other trait bounds.
    type PtrVal;

    /// The type which represents an offset from a given pointer value.
    ///
    /// While architectures are allowed to provide multiple pointer value
    /// representations, bundled together in an `enum`, every arm of the enum
    /// must be able to support a numerical offset type that can be added to
    /// any address to get a new one that many bytes further along.
    ///
    /// This type is customarily referred to as `S` in other trait bounds.
    type Offset;

    /// Inject architectural contexts from user-provided input intended to form
    /// a valid contextual pointer.
    ///
    /// Each architecture is allowed to specify it's own architectural
    /// contexts, which are stored alongside platform contexts in the
    /// `Pointer`. This function allows architectures to participate in context
    /// parsing.
    ///
    /// After parsing has completed, the context list given should be shortened
    /// to exclude the contexts this function has processed, and those parsed
    /// contexts should be provided to the `Pointer`. As a practical
    /// convention, architectures should only parse contexts at the start or
    /// end of a context list.
    ///
    /// TODO: Why does this return `Option<()>`?!
    fn parse_architectural_contexts(
        contexts: &mut &[&str],
        ptr: &mut Pointer<Self::PtrVal>,
    ) -> Option<()>;

    /// Statically disassemble instructions from a given address on a given
    /// platform.
    ///
    /// The `L` type parameter is the literal type of the given assembler to be
    /// used when disassembling the program. The `IO` type parameter represents
    /// an offset into a program image, which may be a wider type than `Offset`
    /// (e.g. if bank switchig is in use). It is almost always `usize`.
    fn disassemble<L>(
        &self,
        at: &Pointer<Self::PtrVal>,
        bus: &Memory<Self>,
    ) -> Result<Disasm<L, Self::PtrVal, Self::Offset>, Self>
    where
        L: Literal
            + From<Self::Word>
            + From<Self::Byte>
            + From<Self::Offset>
            + From<Pointer<Self::PtrVal>>;

    /// Determine what register values or memory addresses are required to be
    /// resolved in order for symbolic execution to continue at a given PC.
    ///
    /// This function returns a list of `Prerequisite`s, as well as a flag to
    /// indicate if the prerequisite list is complete or not. If the list is
    /// incomplete, then after resolving those prerequisites, you must
    /// reanalyze the program at the same position with the new state in order
    /// to find more prerequisites. Otherwise, you may continue tracing.
    ///
    /// A prerequisite should only be listed if symbolic tracing cannot
    /// continue otherwise. If every symbolic value is listed as a prerequisite,
    /// then the state space of symbolic tracing will explode far faster than
    /// if symbolic execution is occurring. When a value is listed as a
    /// prerequisite, the state is said to have been forced into forking. It is
    /// permissible to force a fork for the following reasons:
    ///
    ///  * The program counter or a context it needs is unresolved
    ///  * Instruction contents are unresolved in memory
    ///  * Memory addresses being read or written to are unresolved
    ///  * The target address of a jump or call is unresolved
    ///  * Flags or other information necessary to determine if a jump or call
    ///    is taken or not taken are unresolved
    ///
    /// The `IO` type parameter represents an offset into a program image,
    /// which may be a wider type than `Offset` (e.g. if bank switchig is in
    /// use). It is almost always `usize`.
    fn prerequisites(
        &self,
        at: &Pointer<Self::PtrVal>,
        bus: &Memory<Self>,
        state: &State<Self>,
    ) -> Result<(Vec<Prerequisite<Self>>, bool), Self>;

    /// Advance the state of program execution by one instruction, producing a
    /// new state and program counter to continue from.
    ///
    /// This function may error if the given state is ambiguous enough to
    /// disallow further execution. In order to find out why, you need to ask
    /// the `prerequisites` function to get what needs to be fixed about the
    /// state. In fact, you should always call it before calling this one.
    ///
    /// A state and program counter that produce an empty prerequisites list
    /// for a given program must always cause `trace` to return a valid
    /// continuation of the program.
    ///
    /// TODO: There is currently no representation of states that halt the
    /// program.
    fn trace(
        &self,
        at: &Pointer<Self::PtrVal>,
        bus: &Memory<Self>,
        state: State<Self>,
        trace: &mut Trace<Self>,
    ) -> Result<(State<Self>, Pointer<Self::PtrVal>), Self>;
}
