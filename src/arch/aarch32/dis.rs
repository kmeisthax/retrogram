//! Disassembly of all AArch32 code

use crate::arch::aarch32;
use crate::arch::aarch32::arm::disassemble as armdis;
use crate::arch::aarch32::thumb::disassemble as thumbdis;
use crate::arch::aarch32::{Bus, Disasm, Literal, Pointer, THUMB_STATE};
use crate::{analysis, memory};

/// Disassemble the instruction at `p` in `mem`.
///
/// This function returns:
///
///  * A string representation of the instruction encountered, if there is a
///    valid instruction at P; otherwise `None`
///  * The offset to the next instruction, usually also the size of the current
///    instruction, except for architectures with polynomial program counters.
///  * is_nonfinal: If true, the next instruction is implicitly a target of the
///    current instruction.
///  * is_nonbranching: If true, the current instruction does not end the
///    current block.
///  * A list of pointers containing all statically known jump and call targets
///    from the instruction. Instructions with dynamic or unknown jump targets
///    must be expressed as None. The next instruction is implied as a target
///    if is_nonfinal is returned as True and does not need to be provided here.
pub fn disassemble<L>(p: &memory::Pointer<Pointer>, mem: &Bus) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    match p.get_arch_context(THUMB_STATE).into_concrete() {
        Some(0) => armdis(p, mem),
        Some(1) => thumbdis(p, mem),
        _ => Err(analysis::Error::UnconstrainedMemory(p.clone())),
    }
}
