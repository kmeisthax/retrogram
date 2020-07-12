//! Symbolic fork analysis for SM83

use crate::arch::sm83;
use crate::arch::sm83::dis::{AbstractOperand, ALU_TARGET_MEM, ALU_TARGET_REGS};
use crate::arch::sm83::{Bus, Prerequisite, PtrVal, Register, State};
use crate::{analysis, memory};
use num::One;

/// Return a prerequisite list for an instruction that reads or writes the
/// address situated in the opcode of this instruction.
fn memlist_rw_op16(p: &memory::Pointer<PtrVal>) -> sm83::Result<(Vec<Prerequisite>, bool)> {
    Ok((vec![Prerequisite::memory(p.clone(), 2)], true))
}

/// Return a prerequisite list for an instruction that jumps to or calls the
/// address situated in the opcode of this instruction.
fn memlist_call_op16<IO>(
    mut preq: Vec<Prerequisite>,
    p: &memory::Pointer<PtrVal>,
    mem: &Bus<IO>,
) -> sm83::Result<(Vec<Prerequisite>, bool)>
where
    IO: One,
{
    if let Some(val) = mem.read_leword::<u16>(p).into_concrete() {
        preq.push(Prerequisite::memory(
            mem.minimize_context(p.contextualize(val)),
            1,
        ));
        return Ok((preq, true));
    }

    preq.push(Prerequisite::memory(p.clone(), 2));

    Ok((preq, false))
}

/// Return a prerequisite list for an instruction that reads or writes the
/// high memory address situated in the opcode of this instruction.
fn memlist_rw_hi8(p: &memory::Pointer<PtrVal>) -> sm83::Result<(Vec<Prerequisite>, bool)> {
    Ok((vec![Prerequisite::memory(p.clone(), 1)], true))
}

/// Return a prerequisite list for a PC-relative jump.
fn memlist_pc8<IO>(
    mut preq: Vec<Prerequisite>,
    p: &memory::Pointer<PtrVal>,
    mem: &Bus<IO>,
) -> sm83::Result<(Vec<Prerequisite>, bool)>
where
    IO: One,
{
    let is_complete = match mem
        .read_unit(p)
        .into_concrete()
        .map(|target| ((p.as_pointer() + 1) as i16 + (target as i8) as i16) as u16)
    {
        Some(val) => {
            preq.push(Prerequisite::memory(
                mem.minimize_context(p.contextualize(val)),
                1,
            ));
            true
        }
        None => {
            preq.push(Prerequisite::memory(p.clone(), 1));
            false
        }
    };

    Ok((preq, is_complete))
}

/// Return a prerequisite list for an instruction that jumps to or calls the
/// address at a particular 16-bit register pair (e.g. jp [hl] or ret).
///
/// Flags should be indicated with `include_flags` rather than `Register::F` so
/// that only the `z` and `c` flags are actually counted as prerequisites.
fn memlist_call_indir16<IO>(
    regs: Vec<Register>,
    include_flags: bool,
    p: &memory::Pointer<PtrVal>,
    mem: &Bus<IO>,
    state: &State,
) -> sm83::Result<(Vec<Prerequisite>, bool)>
where
    IO: One,
{
    let mut preqs: Vec<Prerequisite> = regs.iter().map(|r| Prerequisite::from(*r)).collect();
    if include_flags {
        preqs.push(Prerequisite::register(Register::F, 0x90));
    }

    match (regs.get(0), regs.get(1)) {
        (Some(r0), Some(r1)) => match (
            state.get_register(r0).into_concrete(),
            state.get_register(r1).into_concrete(),
        ) {
            (Some(h), Some(l)) => {
                preqs.push(Prerequisite::memory(
                    mem.minimize_context(p.contextualize((h as PtrVal) << 8 | l as PtrVal)),
                    1,
                ));
                Ok((preqs, true))
            }
            _ => Ok((preqs, false)),
        },
        _ => Ok((preqs, false)),
    }
}

/// Determine what registers need to be known in order to execute an
/// instruction at a given address.
///
/// This function returns:
///
///  * A list of architectural registers necessary to execute the instruction
///  * A list of memory locations necessary to execute the instruction
///  * Whether or not the list is complete (i.e. resolving a Register might add
///    a memory location if this is false)
///
/// `prereq` will not return a particular register or memory location if
/// execution can continue symbolically. The only things which absolutely must
/// be concretely resolved are:
///
///  * The current program counter
///  * Instruction contents (unless they can be represented symbolically)
///  * Memory addresses (for both loads and stores)
///  * Target addresses for jumps and calls
///  * Whether or not jumps and calls are taken
///
/// When this function requests forking on the concrete values of certain
/// registers or memory locations, the tracing routine must consider how many
/// forks will be created and if such forking is "worth it". This is a heuristic
/// policy not covered by the tracing implementation of this architecture.
pub fn prereq<IO>(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus<IO>,
    state: &State,
) -> sm83::Result<(Vec<Prerequisite>, bool)>
where
    IO: One,
{
    match mem.read_unit(p).into_concrete() {
        Some(0xCB) => match mem.read_unit(&(p.clone() + 1)).into_concrete() {
            Some(subop) => match ALU_TARGET_REGS[(subop & 0x07) as usize] {
                AbstractOperand::Symbol(_) => Ok((vec![], true)),
                AbstractOperand::Indirect("hl") => Ok((Register::prereqs_from_sym("hl"), true)),
                _ => Ok((vec![], false)),
            },
            _ => Ok((vec![Prerequisite::memory(p.clone() + 1, 1)], false)),
        },

        //Z80 instructions that don't fit the pattern decoder below
        Some(0x00) => Ok((vec![], true)),                //nop
        Some(0x08) => memlist_rw_op16(&(p.clone() + 1)), //ld [u16], sp
        Some(0x10) => Ok((vec![], true)),                //stop
        Some(0x18) => memlist_pc8(vec![], &(p.clone() + 1), mem), //jr u8
        Some(0x76) => Ok((vec![], true)),                //halt

        Some(0xC3) => memlist_call_op16(vec![], &(p.clone() + 1), mem), //jp u16
        Some(0xCD) => memlist_call_op16(Register::prereqs_from_sym("sp"), &(p.clone() + 1), mem), //call u16

        Some(0xC9) => memlist_call_indir16(vec![Register::S, Register::P], false, p, mem, state), //ret
        Some(0xD9) => memlist_call_indir16(vec![Register::S, Register::P], false, p, mem, state), //reti
        Some(0xE9) => memlist_call_indir16(vec![Register::H, Register::L], false, p, mem, state), //jp [hl]
        Some(0xF9) => Ok((vec![], true)), //ld sp, hl

        Some(0xE0) => memlist_rw_hi8(&(p.clone() + 1)), //ldh [u8], a
        Some(0xE8) => Ok((vec![], true)),               //add sp, reg
        Some(0xF0) => memlist_rw_hi8(&(p.clone() + 1)), //ldh a, [u8]
        Some(0xF8) => Ok((vec![], true)),               //ld hl, sp+r8

        Some(0xE2) => Ok((vec![Prerequisite::from(Register::C)], true)), //ldh [c], a
        Some(0xEA) => memlist_rw_op16(&(p.clone() + 1)),                 //ld [u16], a
        Some(0xF2) => Ok((vec![Prerequisite::from(Register::C)], true)), //ldh a, [c]
        Some(0xFA) => memlist_rw_op16(&(p.clone() + 1)),                 //ld a, [u16]
        //TODO: Should memory reads prereq on the target of the read?
        Some(0xF3) => Ok((vec![], true)),
        Some(0xFB) => Ok((vec![], true)),

        //Z80 instructions that follow a particular pattern
        Some(op) => {
            let targetmem = ALU_TARGET_MEM[((op >> 4) & 0x03) as usize];

            //decode `op` into aab?cddd. This creates a nice visual table for
            //the Z80's semiperiodic instruction encoding
            match (
                (op >> 6) & 0x03,
                (op >> 5) & 0x01,
                (op >> 3) & 0x01,
                op & 0x07,
            ) {
                (0, 0, _, 0) => Err(analysis::Error::Misinterpretation(1, false)), /* 00, 08, 10, 18 */
                (0, 1, _, 0) => memlist_pc8(
                    vec![Prerequisite::register(Register::F, 0x90)],
                    &(p.clone() + 1),
                    mem,
                ), //jr cond, pc8
                (0, _, 0, 1) => Ok((vec![], true)),                                //ld r16, u16
                (0, _, 1, 1) => Ok((vec![], true)),                                //add hl, r16
                (0, _, 0, 2) => Ok((Register::prereqs_from_sym(targetmem), true)), //ld [r16], a
                (0, _, 1, 2) => Ok((Register::prereqs_from_sym(targetmem), true)), //ld a, [r16]
                (0, _, 0, 3) => Ok((vec![], true)),                                //inc r16
                (0, _, 1, 3) => Ok((vec![], true)),                                //dec r16
                (0, _, _, 4) if ((op >> 3) & 0x07) == 6 => {
                    Ok((Register::prereqs_from_sym("hl"), true))
                } //inc [hl]
                (0, _, _, 4) => Ok((vec![], true)),                                //inc r8
                (0, _, _, 5) if ((op >> 3) & 0x07) == 6 => {
                    Ok((Register::prereqs_from_sym("hl"), true))
                } //dec [hl]
                (0, _, _, 5) => Ok((vec![], true)),                                //dec r8
                (0, _, _, 6) if ((op >> 3) & 0x07) == 6 => {
                    Ok((Register::prereqs_from_sym("hl"), true))
                } //ld [hl], d8
                (0, _, _, 6) => Ok((vec![], true)),                                //ld r8, d8
                (0, _, _, 7) => Ok((vec![], true)), //8080 bitwise ops
                (1, _, _, _) if ((op >> 3) & 0x07) == 6 => {
                    Ok((Register::prereqs_from_sym("hl"), true))
                } //ld [hl], r8
                (1, _, _, _) if (op & 0x07) == 6 => Ok((Register::prereqs_from_sym("hl"), true)), //ld r8, [hl]
                (1, _, _, _) => Ok((vec![], true)), //ld r8, r8
                (2, _, _, _) if (op & 0x07) == 6 => Ok((Register::prereqs_from_sym("hl"), true)), //aluops r8, [hl]
                (2, _, _, _) => Ok((vec![], true)), //aluops a, r8
                (3, 0, _, 0) => {
                    memlist_call_indir16(vec![Register::S, Register::P], true, p, mem, state)
                } //ret cond... TODO: Can we specify which bit of F we want?
                (3, 1, _, 0) => Err(analysis::Error::Misinterpretation(1, false)), /* E0, E8, F0, F8 */
                (3, _, 0, 1) => Ok((Register::prereqs_from_sym("sp"), true)),      //pop r16
                (3, _, 1, 1) => Err(analysis::Error::Misinterpretation(1, false)), /* C9, D9, E9, F9 */
                (3, 0, _, 2) => memlist_call_op16(
                    vec![Prerequisite::register(Register::F, 0x90)],
                    &(p.clone() + 1),
                    mem,
                ), //jp cond, d16
                (3, 1, _, 2) => Err(analysis::Error::Misinterpretation(1, false)), /* E2, EA, F2, FA */
                (3, _, _, 3) => Err(analysis::Error::InvalidInstruction),          //invalid
                (3, 0, _, 4) => memlist_call_op16(
                    vec![Prerequisite::register(Register::F, 0x90)],
                    &(p.clone() + 1),
                    mem,
                ), //call cond, d16
                (3, 1, _, 4) => Err(analysis::Error::InvalidInstruction),          //invalid
                (3, _, 0, 5) => Ok((Register::prereqs_from_sym("sp"), true)),      //push r16
                (3, _, 1, 5) => Err(analysis::Error::InvalidInstruction),          //invalid
                (3, _, _, 6) => Ok((vec![], true)),                                //aluops a, d8
                (3, _, _, 7) => Ok((Register::prereqs_from_sym("sp"), true)),      //rst nn

                _ => Err(analysis::Error::Misinterpretation(1, false)), //invalid
            }
        }
        _ => Ok((vec![Prerequisite::memory(p.clone(), 1)], false)),
    }
}
