//! Symbolic fork analysis for SM83

use crate::analysis::Error;
use crate::arch::sm83::{
    Bus, Instruction, PtrVal, Register, RegisterPair, Requisite, Result, State,
};
use std::collections::HashSet;

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
pub fn prereq(p: PtrVal, mem: &Bus, state: &State) -> Result<(HashSet<Requisite>, bool)> {
    let (preqs, is_complete) =
        match Instruction::from_static_stream(&state.contextualize_pointer(p), mem) {
            Ok((Instruction::Nop, _)) => (vec![], true),
            Ok((Instruction::LdReg8(tgt, src), _)) => {
                let mut prereq_list = src.into_memory_requisites();
                prereq_list.append(&mut tgt.into_memory_requisites());

                (prereq_list, true)
            }
            Ok((Instruction::LdHLSP(_), _)) => (vec![], true),
            Ok((Instruction::LdSPHL, _)) => (vec![], true),
            Ok((Instruction::LdWriteStatic(_), _)) => (vec![], true),
            Ok((Instruction::LdWritePtr(ptr_regpair), _)) => (ptr_regpair.into_requisites(), true),
            Ok((Instruction::LdWriteHiStatic(_), _)) => (vec![], true),
            Ok((Instruction::LdWriteHiPtr, _)) => (vec![Register::C.into_requisite()], true),
            Ok((Instruction::LdReadStatic(_), _)) => (vec![], true),
            Ok((Instruction::LdReadPtr(ptr_regpair), _)) => (ptr_regpair.into_requisites(), true),
            Ok((Instruction::LdReadHiStatic(_), _)) => (vec![], true),
            Ok((Instruction::LdReadHiPtr, _)) => (vec![], true),
            Ok((Instruction::LdConst8(tgt, _), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::LdConst16(_regpair, _), _)) => (vec![], true),
            Ok((Instruction::LdWriteStaticSP(_), _)) => (vec![], true),
            Ok((Instruction::Inc16(_), _)) => (vec![], true),
            Ok((Instruction::Inc8(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::Dec16(_), _)) => (vec![], true),
            Ok((Instruction::Dec8(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::Jump(_, cond), _)) => {
                (cond.map(|c| c.into_requisite()).into_iter().collect(), true)
            }
            Ok((Instruction::JumpRelative(_, cond), _)) => {
                (cond.map(|c| c.into_requisite()).into_iter().collect(), true)
            }
            Ok((Instruction::JumpDynamic, _)) => (RegisterPair::HL.into_requisites(), true),
            Ok((Instruction::Call(_, cond), _)) => (
                cond.map(|c| c.into_requisite())
                    .into_iter()
                    .chain(RegisterPair::SP.into_requisites().into_iter())
                    .collect(),
                true,
            ),
            Ok((Instruction::CallRst(_), _)) => (RegisterPair::SP.into_requisites(), true),
            Ok((Instruction::Push(_), _)) => (RegisterPair::SP.into_requisites(), true),
            Ok((Instruction::Pop(_), _)) => (RegisterPair::SP.into_requisites(), true),
            Ok((Instruction::Return(cond), _)) => (
                cond.map(|c| c.into_requisite())
                    .into_iter()
                    .chain(RegisterPair::SP.into_requisites().into_iter())
                    .collect(),
                true,
            ),
            Ok((Instruction::ReturnFromInterrupt, _)) => (RegisterPair::SP.into_requisites(), true),
            Ok((Instruction::Add16(_), _)) => (vec![], true),
            Ok((Instruction::AddSpConst(_), _)) => (vec![], true),
            Ok((Instruction::Add8(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::Add8Const(_), _)) => (vec![], true),
            Ok((Instruction::AddCarry8(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::AddCarry8Const(_), _)) => (vec![], true),
            Ok((Instruction::Sub8(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::Sub8Const(_), _)) => (vec![], true),
            Ok((Instruction::SubCarry8(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::SubCarry8Const(_), _)) => (vec![], true),
            Ok((Instruction::And8(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::And8Const(_), _)) => (vec![], true),
            Ok((Instruction::Xor8(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::Xor8Const(_), _)) => (vec![], true),
            Ok((Instruction::Or8(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::Or8Const(_), _)) => (vec![], true),
            Ok((Instruction::Cp(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::CpConst(_), _)) => (vec![], true),
            Ok((Instruction::RotateLeftCarry(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::RotateLeftCarryAccum, _)) => (vec![], true),
            Ok((Instruction::RotateLeft(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::RotateLeftAccum, _)) => (vec![], true),
            Ok((Instruction::RotateRightCarry(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::RotateRightCarryAccum, _)) => (vec![], true),
            Ok((Instruction::RotateRight(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::RotateRightAccum, _)) => (vec![], true),
            Ok((Instruction::ShiftLeftArithmetic(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::ShiftRightArithmetic(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::NybbleSwap(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::ShiftRightLogical(tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::BitTest(_, tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::BitReset(_, tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::BitSet(_, tgt), _)) => (tgt.into_memory_requisites(), true),
            Ok((Instruction::Compliment, _)) => (vec![], true),
            Ok((Instruction::DecimalAdjust, _)) => (vec![], true),
            Ok((Instruction::SetCarry, _)) => (vec![], true),
            Ok((Instruction::ClearCarry, _)) => (vec![], true),
            Ok((Instruction::DisableInterrupt, _)) => (vec![], true),
            Ok((Instruction::EnableInterrupt, _)) => (vec![], true),
            Ok((Instruction::Halt, _)) => (vec![], true),
            Ok((Instruction::Stop, _)) => (vec![], true),
            Err(Error::UnconstrainedMemory(p)) => {
                (vec![Requisite::memory(*p.as_pointer(), 0xFF)], false)
            }
            Err(e) => return Err(e),
        };

    Ok((preqs.into_iter().collect(), is_complete))
}
