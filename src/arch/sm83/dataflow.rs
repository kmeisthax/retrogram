//! Symbolic fork analysis for SM83

use crate::arch::sm83;
use crate::arch::sm83::{Bus, BusAddress, Instruction, Register, RegisterPair, Requisite};
use std::collections::HashSet;
use std::iter::FromIterator;

fn static_memory_requisite(addr: u16) -> Vec<Requisite> {
    vec![Requisite::memory(addr, 1)]
}

fn static_hi_memory_requisite(addr: u8) -> Vec<Requisite> {
    vec![Requisite::memory(0xFF00 + addr as u16, 1)]
}

/// Statically determine the input and output requisites of a given SM83
/// instruction.
pub fn dataflow(
    p: &BusAddress,
    mem: &Bus,
) -> sm83::Result<(HashSet<Requisite>, HashSet<Requisite>)> {
    let (from, to) = match Instruction::from_static_stream(p, mem) {
        Ok((Instruction::Nop, _)) => (vec![], vec![]),
        Ok((Instruction::LdReg8(tgt, src), _)) => (
            src.into_register_requisites(),
            tgt.into_register_requisites(),
        ),
        Ok((Instruction::LdHlSp(_), _)) => (
            RegisterPair::Hl.into_requisites(),
            RegisterPair::Sp
                .into_requisites()
                .into_iter()
                .chain(vec![Register::F.into_requisite()].into_iter())
                .collect(),
        ),
        Ok((Instruction::LdSpHl, _)) => (
            RegisterPair::Sp.into_requisites(),
            RegisterPair::Hl.into_requisites(),
        ),
        Ok((Instruction::LdWriteStatic(tgt), _)) => (
            vec![Register::A.into_requisite()],
            static_memory_requisite(tgt),
        ),
        Ok((Instruction::LdWritePtr(ptr_regpair), _)) => (
            vec![Register::A.into_requisite()],
            ptr_regpair.into_requisites(),
        ),
        Ok((Instruction::LdWriteHiStatic(tgt), _)) => (
            vec![Register::A.into_requisite()],
            static_hi_memory_requisite(tgt),
        ),
        Ok((Instruction::LdWriteHiPtr, _)) => (
            vec![Register::A.into_requisite()],
            vec![Register::C.into_requisite()],
        ),
        Ok((Instruction::LdReadStatic(tgt), _)) => (
            static_memory_requisite(tgt),
            vec![Register::A.into_requisite()],
        ),
        Ok((Instruction::LdReadPtr(ptr_regpair), _)) => (
            ptr_regpair.into_requisites(),
            vec![Register::A.into_requisite()],
        ),
        Ok((Instruction::LdReadHiStatic(tgt), _)) => (
            static_hi_memory_requisite(tgt),
            vec![Register::A.into_requisite()],
        ),
        Ok((Instruction::LdReadHiPtr, _)) => (
            vec![Register::C.into_requisite()],
            vec![Register::A.into_requisite()],
        ),
        Ok((Instruction::LdConst8(tgt, _), _)) => (vec![], tgt.into_register_requisites()),
        Ok((Instruction::LdConst16(regpair, _), _)) => (vec![], regpair.into_requisites()),
        Ok((Instruction::LdWriteStaticSp(tgt), _)) => (
            RegisterPair::Sp.into_requisites(),
            static_memory_requisite(tgt),
        ),
        Ok((Instruction::Inc16(regpair), _)) => {
            (regpair.into_requisites(), regpair.into_requisites())
        }
        Ok((Instruction::Inc8(tgt), _)) => (
            tgt.into_register_requisites(),
            tgt.into_register_requisites(),
        ),
        Ok((Instruction::Dec16(regpair), _)) => {
            (regpair.into_requisites(), regpair.into_requisites())
        }
        Ok((Instruction::Dec8(tgt), _)) => (
            tgt.into_register_requisites(),
            tgt.into_register_requisites(),
        ),
        Ok((Instruction::Jump(_, cond), _)) if cond.is_some() => {
            (vec![Register::F.into_requisite()], vec![])
        }
        Ok((Instruction::Jump(_, _), _)) => (vec![], vec![]),
        Ok((Instruction::JumpRelative(_, cond), _)) if cond.is_some() => {
            (vec![Register::F.into_requisite()], vec![])
        }
        Ok((Instruction::JumpRelative(_, _), _)) => (vec![], vec![]),
        Ok((Instruction::JumpDynamic, _)) => (RegisterPair::Hl.into_requisites(), vec![]),
        Ok((Instruction::Call(_, cond), _)) if cond.is_some() => (
            RegisterPair::Sp
                .into_requisites()
                .into_iter()
                .chain(vec![Register::F.into_requisite()].into_iter())
                .collect(),
            RegisterPair::Sp.into_requisites(),
        ),
        Ok((Instruction::Call(_, _), _)) => (
            RegisterPair::Sp.into_requisites(),
            RegisterPair::Sp.into_requisites(),
        ),
        Ok((Instruction::CallRst(_), _)) => (
            RegisterPair::Sp.into_requisites(),
            RegisterPair::Sp.into_requisites(),
        ),
        Ok((Instruction::Push(pair), _)) => (
            RegisterPair::Sp
                .into_requisites()
                .into_iter()
                .chain(pair.into_requisites().into_iter())
                .collect(),
            RegisterPair::Sp.into_requisites(),
        ),
        Ok((Instruction::Pop(pair), _)) => (
            RegisterPair::Sp.into_requisites(),
            RegisterPair::Sp
                .into_requisites()
                .into_iter()
                .chain(pair.into_requisites().into_iter())
                .collect(),
        ),
        Ok((Instruction::Return(cond), _)) if cond.is_some() => (
            RegisterPair::Sp
                .into_requisites()
                .into_iter()
                .chain(vec![Register::F.into_requisite()].into_iter())
                .collect(),
            RegisterPair::Sp.into_requisites(),
        ),
        Ok((Instruction::Return(_), _)) => (
            RegisterPair::Sp.into_requisites(),
            RegisterPair::Sp.into_requisites(),
        ),
        Ok((Instruction::ReturnFromInterrupt, _)) => (
            RegisterPair::Sp.into_requisites(),
            RegisterPair::Sp.into_requisites(),
        ),
        Ok((Instruction::Add16(pair), _)) => (
            RegisterPair::Hl
                .into_requisites()
                .into_iter()
                .chain(pair.into_requisites().into_iter())
                .chain(vec![Register::F.into_requisite()])
                .collect(),
            RegisterPair::Hl
                .into_requisites()
                .into_iter()
                .chain(vec![Register::F.into_requisite()])
                .collect(),
        ),
        Ok((Instruction::AddSpConst(_), _)) => (
            RegisterPair::Sp.into_requisites(),
            RegisterPair::Sp
                .into_requisites()
                .into_iter()
                .chain(vec![Register::F.into_requisite()])
                .collect(),
        ),
        Ok((Instruction::Add8(tgt), _))
        | Ok((Instruction::AddCarry8(tgt), _))
        | Ok((Instruction::Sub8(tgt), _))
        | Ok((Instruction::SubCarry8(tgt), _))
        | Ok((Instruction::And8(tgt), _))
        | Ok((Instruction::Xor8(tgt), _))
        | Ok((Instruction::Or8(tgt), _))
        | Ok((Instruction::Cp(tgt), _)) => (
            tgt.into_register_requisites()
                .into_iter()
                .chain(vec![Register::A.into_requisite()])
                .collect(),
            vec![Register::A.into_requisite(), Register::F.into_requisite()],
        ),
        Ok((Instruction::Add8Const(_), _))
        | Ok((Instruction::AddCarry8Const(_), _))
        | Ok((Instruction::Sub8Const(_), _))
        | Ok((Instruction::SubCarry8Const(_), _))
        | Ok((Instruction::And8Const(_), _))
        | Ok((Instruction::Xor8Const(_), _))
        | Ok((Instruction::Or8Const(_), _))
        | Ok((Instruction::CpConst(_), _)) => (
            vec![Register::A.into_requisite()],
            vec![Register::A.into_requisite(), Register::F.into_requisite()],
        ),
        Ok((Instruction::RotateLeftCarry(tgt), _))
        | Ok((Instruction::RotateLeft(tgt), _))
        | Ok((Instruction::RotateRightCarry(tgt), _))
        | Ok((Instruction::RotateRight(tgt), _))
        | Ok((Instruction::ShiftLeftArithmetic(tgt), _))
        | Ok((Instruction::ShiftRightArithmetic(tgt), _))
        | Ok((Instruction::NybbleSwap(tgt), _))
        | Ok((Instruction::ShiftRightLogical(tgt), _)) => (
            tgt.into_register_requisites(),
            tgt.into_register_requisites()
                .into_iter()
                .chain(vec![Register::F.into_requisite()])
                .collect(),
        ),
        Ok((Instruction::RotateLeftCarryAccum, _))
        | Ok((Instruction::RotateLeftAccum, _))
        | Ok((Instruction::RotateRightCarryAccum, _))
        | Ok((Instruction::RotateRightAccum, _)) => (
            vec![Register::A.into_requisite()],
            vec![Register::A.into_requisite(), Register::F.into_requisite()],
        ),
        Ok((Instruction::BitTest(_, tgt), _)) => (
            tgt.into_register_requisites()
                .into_iter()
                .chain(vec![Register::F.into_requisite()])
                .collect(),
            vec![Register::F.into_requisite()],
        ),
        Ok((Instruction::BitReset(_, tgt), _)) | Ok((Instruction::BitSet(_, tgt), _)) => (
            tgt.into_register_requisites(),
            tgt.into_register_requisites(),
        ),
        Ok((Instruction::Compliment, _)) | Ok((Instruction::DecimalAdjust, _)) => (
            vec![Register::A.into_requisite(), Register::F.into_requisite()],
            vec![Register::A.into_requisite(), Register::F.into_requisite()],
        ),
        Ok((Instruction::SetCarry, _)) | Ok((Instruction::ClearCarry, _)) => (
            vec![Register::F.into_requisite()],
            vec![Register::F.into_requisite()],
        ),
        Ok((Instruction::DisableInterrupt, _))
        | Ok((Instruction::EnableInterrupt, _))
        | Ok((Instruction::Halt, _))
        | Ok((Instruction::Stop, _)) => (vec![], vec![]),
        Err(e) => return Err(e),
    };

    Ok((HashSet::from_iter(from), HashSet::from_iter(to)))
}
