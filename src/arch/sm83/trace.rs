//! Facilities for tracing SM83 code

use crate::arch::sm83;
use crate::reg::{Convertable, TryConvertable};
use crate::{analysis, memory, reg};
use std::cmp::PartialEq;
//use crate::arch::sm83::dis::{ALU_BITOPS, ALU_CONDCODE, ALU_OPS, ALU_TARGET_MEM, ALU_TARGET_PAIRS, ALU_TARGET_REGS, STACK_TARGET_PAIRS};
use crate::arch::sm83::{Bus, PtrVal, Register, State, Trace, Value};

/// Given a targetreg operand, produce a symbolic value of what that target reg
/// would be given the current state.
fn read_value_from_targetreg(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    state: &State,
    targetreg: u8,
) -> sm83::Result<reg::Symbolic<Value>> {
    match targetreg {
        0 => Ok(state.get_register(&Register::B)),
        1 => Ok(state.get_register(&Register::C)),
        2 => Ok(state.get_register(&Register::D)),
        3 => Ok(state.get_register(&Register::E)),
        4 => Ok(state.get_register(&Register::H)),
        5 => Ok(state.get_register(&Register::L)),
        6 => {
            let hl = reg::Symbolic::<PtrVal>::convert_from(state.get_register(&Register::H)) << 8
                | reg::Symbolic::<PtrVal>::convert_from(state.get_register(&Register::L));
            if let Some(hl) = hl.into_concrete() {
                let ptr_hl = mem.minimize_context(p.contextualize(hl)); //TODO: Attempt to pull context from state
                Ok(mem.read_unit(&ptr_hl)) //TODO: Should respond to memory state
            } else {
                Ok(reg::Symbolic::default())
            }
        }
        7 => Ok(state.get_register(&Register::A)),
        _ => Err(analysis::Error::Misinterpretation(1, false)), //TODO: Get correct instruction offset
    }
}

/// Given a targetreg operand, manipulate the given state to incorporate the
/// effect of writing a given value to that operand.
fn write_value_to_targetreg(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    state: &mut State,
    trace: &mut Trace,
    targetreg: u8,
    value: reg::Symbolic<Value>,
) -> sm83::Result<()> {
    match targetreg {
        0 => trace.register_set(Register::B, value, state),
        1 => trace.register_set(Register::C, value, state),
        2 => trace.register_set(Register::D, value, state),
        3 => trace.register_set(Register::E, value, state),
        4 => trace.register_set(Register::H, value, state),
        5 => trace.register_set(Register::L, value, state),
        6 => {
            let hl = reg::Symbolic::<PtrVal>::convert_from(state.get_register(&Register::H)) << 8
                | reg::Symbolic::<PtrVal>::convert_from(state.get_register(&Register::L));
            if let Some(hl) = hl.into_concrete() {
                let ptr_hl = mem.minimize_context(p.contextualize(hl)); //TODO: Attempt to pull context from state
                trace.memory_write(ptr_hl, &[value], state);
                return Ok(());
            }

            return Err(analysis::Error::UnconstrainedRegister);
        }
        7 => trace.register_set(Register::A, value, state),
        _ => return Err(analysis::Error::Misinterpretation(1, false)), //TODO: Get correct instruction offset
    };

    Ok(())
}

fn read_value_from_targetpair(
    state: &State,
    targetpair: u8,
    is_stackpair: bool,
) -> sm83::Result<reg::Symbolic<PtrVal>> {
    let hival: reg::Symbolic<u16> = reg::Symbolic::convert_from(match (targetpair, is_stackpair) {
        (0, _) => state.get_register(&Register::B),
        (1, _) => state.get_register(&Register::D),
        (2, _) => state.get_register(&Register::H),
        (3, false) => state.get_register(&Register::S),
        (3, true) => state.get_register(&Register::A),
        _ => return Err(analysis::Error::Misinterpretation(1, false)),
    });
    let loval: reg::Symbolic<u16> = reg::Symbolic::convert_from(match (targetpair, is_stackpair) {
        (0, _) => state.get_register(&Register::C),
        (1, _) => state.get_register(&Register::E),
        (2, _) => state.get_register(&Register::L),
        (3, false) => state.get_register(&Register::P),
        (3, true) => state.get_register(&Register::F),
        _ => return Err(analysis::Error::Misinterpretation(1, false)),
    });

    Ok(hival << 8 | loval)
}

/// Given a targetpair operand, manipulate the given state to incorporate the
/// effect of writing values to those operands.
fn write_value_to_targetpair(
    state: &mut State,
    trace: &mut Trace,
    targetpair: u8,
    is_stackpair: bool,
    value: reg::Symbolic<PtrVal>,
) -> sm83::Result<()> {
    //TODO: Some kind of "regsegment" method?
    let loval: reg::Symbolic<u8> =
        reg::Symbolic::try_convert_from(value & reg::Symbolic::from(0x00FF))
            .map_err(|_| analysis::Error::BlockSizeOverflow)?;
    let hival: reg::Symbolic<u8> = reg::Symbolic::try_convert_from(value >> 8)
        .map_err(|_| analysis::Error::BlockSizeOverflow)?;

    match (targetpair, is_stackpair) {
        (0, _) => {
            trace.register_set(Register::B, hival, state);
            trace.register_set(Register::C, loval, state);
        } //BC
        (1, _) => {
            trace.register_set(Register::D, hival, state);
            trace.register_set(Register::E, loval, state);
        } //DE
        (2, _) => {
            trace.register_set(Register::H, hival, state);
            trace.register_set(Register::L, loval, state);
        } //HL
        (3, false) => {
            trace.register_set(Register::S, hival, state);
            trace.register_set(Register::P, loval, state);
        } //SP
        (3, true) => {
            trace.register_set(Register::A, hival, state);
            trace.register_set(Register::F, loval, state);
        } //AF
        _ => return Err(analysis::Error::Misinterpretation(1, false)), //TODO: Get correct instruction offset
    }

    Ok(())
}

/// Trace a pop from the stack, yielding the new value.
///
/// This routine also writes back the new SP to the state. If your instruction
/// is conditional, make sure to test the condition before retrieving [SP].
///
/// Will fail if SP isn't set. No support currently exists for tracing relative
/// registers yet.
fn pop_value_from_sp(
    ptr: &memory::Pointer<PtrVal>,
    mem: &Bus,
    state: &mut State,
    trace: &mut Trace,
) -> sm83::Result<reg::Symbolic<u16>> {
    match read_value_from_targetpair(&state, 3, false)?.into_concrete() {
        Some(sp) => {
            let next_sp = sp + 2;
            write_value_to_targetpair(state, trace, 3, false, reg::Symbolic::from(next_sp))?;

            let loval: reg::Symbolic<u16> =
                reg::Symbolic::convert_from(state.get_memory(&ptr.contextualize(sp), mem));
            let hival: reg::Symbolic<u16> =
                reg::Symbolic::convert_from(state.get_memory(&ptr.contextualize(sp + 1), mem));
            let val = hival << 8 | loval;

            Ok(val)
        }
        _ => Err(analysis::Error::UnconstrainedRegister),
    }
}

/// Trace a push to the stack.
///
/// This routine writes both the value to the current stack and the new stack
/// pointer. If your instruction is conditional, make sure to test the condition
/// before writing [SP].
///
/// Will fail if SP isn't set. No support currently exists for tracing relative
/// registers yet.
fn push_value_to_sp(
    ptr: &memory::Pointer<PtrVal>,
    state: &mut State,
    trace: &mut Trace,
    value: reg::Symbolic<PtrVal>,
) -> sm83::Result<()> {
    match read_value_from_targetpair(&state, 3, false)?.into_concrete() {
        Some(sp) => {
            let lo_value = reg::Symbolic::try_convert_from(value & reg::Symbolic::from(0xFF))
                .map_err(|_| analysis::Error::BlockSizeOverflow)?;
            let hi_value = reg::Symbolic::try_convert_from(value >> 8)
                .map_err(|_| analysis::Error::BlockSizeOverflow)?;

            trace.memory_write(ptr.contextualize(sp), &[lo_value, hi_value], state);

            let next_sp = sp - 2;
            write_value_to_targetpair(state, trace, 3, false, reg::Symbolic::from(next_sp))?;

            Ok(())
        }
        _ => Err(analysis::Error::UnconstrainedRegister),
    }
}

/// Given a symbolic value, compute the zero flag that it would generate in the
/// F register if it were the result of a computation.
///
/// The resulting value is suitable for use as a new flags register with the
/// format of Z000. You may OR in additional bits as necessary.
fn zero_flag<T>(val: reg::Symbolic<T>) -> reg::Symbolic<T>
where
    T: Clone + From<u8> + reg::Bitwise + PartialEq,
{
    match (val.clone().into_concrete(), val.is_valid(T::from(0))) {
        (Some(ref e), _) if *e == T::from(0) => reg::Symbolic::from(T::from(0x80)),
        (Some(_), _) => reg::Symbolic::from(T::from(0)),
        (None, true) => reg::Symbolic::from_cares(T::from(0), T::from(0x7F)),
        (None, false) => reg::Symbolic::from(T::from(0)),
    }
}

/// Given a symbolic value, compute the carry flag that it would generate in the
/// F register if it were the result of a computation.
///
/// This operates under the assumption that the value was constructed by summing
/// two zero-extended u8 values to produce a u9 result (represented as u16). For
/// 16-bit math, you would need to sum u16 values into a u17 (represented as
/// u32) and then use the appropriate function to extract a carry flag for it.
///
/// The resulting value is suitable for use as a new flags register with the
/// format of Z000. You may OR in additional bits as necessary.
fn carry_flag_u9(val: reg::Symbolic<u16>) -> reg::Symbolic<u8> {
    let carry_bit = val & reg::Symbolic::from(0x100);
    match (carry_bit.into_concrete(), carry_bit.is_valid(0x100)) {
        (Some(0), _) => reg::Symbolic::from(0 as u8),
        (Some(_), _) => reg::Symbolic::from(0x10 as u8),
        (None, true) => reg::Symbolic::from_cares(0 as u8, 0xEF as u8),
        (None, false) => reg::Symbolic::from(0),
    }
}

/// Given a condition code and flags, return true if the test would pass.
///
/// You may pass None as the condition code, which indicates an unconditional
/// branch (and thus, always returns true).
fn flag_test(condcode: Option<u8>, value: reg::Symbolic<Value>) -> sm83::Result<bool> {
    match condcode {
        Some(0) => Ok((value & reg::Symbolic::from(0x80))
            .into_concrete()
            .ok_or(analysis::Error::UnconstrainedRegister)?
            != 0), //NZ
        Some(1) => Ok((value & reg::Symbolic::from(0x80))
            .into_concrete()
            .ok_or(analysis::Error::UnconstrainedRegister)?
            == 0), //Z
        Some(2) => Ok((value & reg::Symbolic::from(0x10))
            .into_concrete()
            .ok_or(analysis::Error::UnconstrainedRegister)?
            != 0), //NC
        Some(3) => Ok((value & reg::Symbolic::from(0x10))
            .into_concrete()
            .ok_or(analysis::Error::UnconstrainedRegister)?
            == 0), //C
        None => Ok(true),
        _ => Err(analysis::Error::Misinterpretation(2, false)),
    }
}

/// Given an ALU opcode, accumulator, operand, and flags, compute the new
/// accumulator and flag values.
fn aluop(
    aluop: u8,
    a: reg::Symbolic<u8>,
    operand: reg::Symbolic<u8>,
    flags: reg::Symbolic<u8>,
) -> sm83::Result<(reg::Symbolic<u8>, reg::Symbolic<u8>)> {
    let old_carry: reg::Symbolic<u8> = flags & (reg::Symbolic::from(0x10 as u8) >> 4);
    let old_halfcarry: reg::Symbolic<u8> = flags & (reg::Symbolic::from(0x20 as u8) >> 5);
    let _old_nflag: reg::Symbolic<u8> = flags & (reg::Symbolic::from(0x40 as u8) >> 6);
    let wide_carry: reg::Symbolic<u16> = reg::Symbolic::convert_from(old_carry);
    let wide_a: reg::Symbolic<u16> = reg::Symbolic::convert_from(a);
    let wide_op: reg::Symbolic<u16> = reg::Symbolic::convert_from(operand);

    let (new_halfcarry, new_nflag, wide_result) = match aluop {
        0 => (
            reg::Symbolic::from(0 as u8),
            old_halfcarry,
            wide_a + wide_op,
        ), //add
        1 => (
            reg::Symbolic::from(0 as u8),
            old_halfcarry,
            wide_a + wide_op + wide_carry,
        ), //adc
        2 => (
            reg::Symbolic::from(1 as u8),
            old_halfcarry,
            wide_a + !(wide_op + reg::Symbolic::from(1)),
        ), //sub
        3 => (
            reg::Symbolic::from(1 as u8),
            old_halfcarry,
            wide_a + !((wide_op + wide_carry) + reg::Symbolic::from(1)),
        ), //sbc
        4 => (
            reg::Symbolic::from(0 as u8),
            reg::Symbolic::from(1 as u8),
            wide_a & wide_op,
        ), //and
        5 => (
            reg::Symbolic::from(0 as u8),
            reg::Symbolic::from(0 as u8),
            wide_a ^ wide_op,
        ), //xor
        6 => (
            reg::Symbolic::from(0 as u8),
            reg::Symbolic::from(0 as u8),
            wide_a | wide_op,
        ), //or
        7 => (
            reg::Symbolic::from(1 as u8),
            old_halfcarry,
            wide_a + !(wide_op + reg::Symbolic::from(1)),
        ), //cp
        _ => return Err(analysis::Error::Misinterpretation(1, false)),
    };

    let new_a: reg::Symbolic<u8> =
        reg::Symbolic::try_convert_from(wide_result & reg::Symbolic::from(0xFF))
            .map_err(|_| analysis::Error::BlockSizeOverflow)?;
    let new_flags =
        zero_flag(new_a) | carry_flag_u9(wide_result) | new_nflag << 6 | new_halfcarry << 5;

    if aluop != 7 {
        Ok((new_a, new_flags))
    } else {
        Ok((a, new_flags))
    }
}

/// Trace a CB-prefix bit rotate operation (RLC, RRC, RL, RR, SLA, SRA, SWAP,
/// or SRL).
fn trace_bitop(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
    bitop: u8,
    targetreg: u8,
) -> sm83::Result<State> {
    let flags = state.get_register(&Register::F);
    let carry = flags & reg::Symbolic::from(0x10);
    let val = read_value_from_targetreg(p, mem, &state, targetreg)?;

    let (newval, newcarry) = match bitop {
        0 => (val << 1 | val >> 7, carry),      //RLC
        1 => (val << 1 | val >> 7, carry),      //RRC
        2 => (val << 1 | carry >> 4, val >> 7), //RL
        3 => (val >> 1 | carry << 3, val & reg::Symbolic::from(0x01)), //RR
        4 => (val << 1, val >> 7),              //SLA
        //This is a manual sign extension since we defined Value as unsigned
        5 => (
            val >> 1 | val & reg::Symbolic::from(0x80),
            val & reg::Symbolic::from(0x01),
        ), //SRA
        6 => (val >> 4 | val << 4, reg::Symbolic::from(0)), //SWAP
        7 => (val >> 1, val & reg::Symbolic::from(0x01)),   //SRL
        _ => return Err(analysis::Error::Misinterpretation(2, false)),
    };

    trace.register_set(Register::F, newcarry << 4 | zero_flag(val), &mut state); //N and H flags are always zero
    write_value_to_targetreg(p, mem, &mut state, trace, targetreg, newval)?;

    Ok(state)
}

/// Trace a CB-prefix bit test instruction (e.g. BIT n, reg).
fn trace_bittest(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
    targetbit: u8,
    targetreg: u8,
) -> sm83::Result<State> {
    let flags = state.get_register(&Register::F);
    let val = read_value_from_targetreg(p, mem, &state, targetreg)? >> targetbit
        & reg::Symbolic::from(0x01);

    trace.register_set(
        Register::F,
        flags & reg::Symbolic::from(0x10) | reg::Symbolic::from(0x20) | zero_flag(val),
        &mut state,
    );

    Ok(state)
}

/// Trace a CB-prefix bit reset instruction (e.g. RES n, reg).
fn trace_bitreset(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
    targetbit: u8,
    targetreg: u8,
) -> sm83::Result<State> {
    let mask = reg::Symbolic::from(!(1 << targetbit));
    let val = read_value_from_targetreg(p, mem, &state, targetreg)?;

    write_value_to_targetreg(p, mem, &mut state, trace, targetreg, val & mask)?;

    Ok(state)
}

/// Trace a CB-prefix bit set instruction (e.g. SET n, reg).
fn trace_bitset(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
    targetbit: u8,
    targetreg: u8,
) -> sm83::Result<State> {
    let bit = reg::Symbolic::from(1 << targetbit);
    let val = read_value_from_targetreg(p, mem, &state, targetreg)?;

    write_value_to_targetreg(p, mem, &mut state, trace, targetreg, val | bit)?;

    Ok(state)
}

/// Trace writing the stack pointer to a memory location
fn trace_sp_storage(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
) -> sm83::Result<State> {
    let op_ptr = p.contextualize(*p.as_pointer() + 1);
    let memloc = mem
        .read_leword::<PtrVal>(&op_ptr)
        .into_concrete()
        .ok_or(analysis::Error::UnconstrainedMemory(op_ptr))?;

    trace.memory_write(
        p.contextualize(memloc),
        &[
            state.get_register(&Register::P),
            state.get_register(&Register::S),
        ],
        &mut state,
    );

    Ok(state)
}

/// Trace full jumps
fn trace_jump(
    condcode: Option<u8>,
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    state: State,
) -> sm83::Result<(State, memory::Pointer<PtrVal>)> {
    if flag_test(condcode, state.get_register(&Register::F))? {
        let op_ptr = p.contextualize(*p.as_pointer() + 1);
        let target = mem
            .read_leword::<u16>(&op_ptr)
            .into_concrete()
            .ok_or(analysis::Error::UnconstrainedMemory(op_ptr))?;

        Ok((state, p.contextualize(target)))
    } else {
        Ok((state, p.clone() + 3))
    }
}

/// Trace full calls
fn trace_call(
    condcode: Option<u8>,
    ptr: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
) -> sm83::Result<(State, memory::Pointer<PtrVal>)> {
    if flag_test(condcode, state.get_register(&Register::F))? {
        let op_ptr = ptr.contextualize(*ptr.as_pointer() + 1);
        let target = mem
            .read_leword::<u16>(&op_ptr)
            .into_concrete()
            .ok_or(analysis::Error::UnconstrainedMemory(op_ptr))?;
        let ret_pc = *ptr.as_pointer() + 3;

        push_value_to_sp(ptr, &mut state, trace, reg::Symbolic::from(ret_pc))?;

        Ok((state, ptr.contextualize(target)))
    } else {
        Ok((state, ptr.clone() + 3))
    }
}

/// Trace return
fn trace_return(
    condcode: Option<u8>,
    ptr: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
) -> sm83::Result<(State, memory::Pointer<PtrVal>)> {
    if flag_test(condcode, state.get_register(&Register::F))? {
        let old_spval = read_value_from_targetpair(&state, 3, false)?
            .into_concrete()
            .ok_or(analysis::Error::UnconstrainedRegister)?;
        let new_pcval = pop_value_from_sp(ptr, mem, &mut state, trace)?
            .into_concrete()
            .ok_or_else(|| analysis::Error::UnconstrainedMemory(ptr.contextualize(old_spval)))?;
        let new_ptr = ptr.contextualize(new_pcval);

        Ok((state, new_ptr))
    } else {
        Ok((state, ptr.clone() + 1))
    }
}

/// Trace relative jumps
fn trace_jump_relative(
    condcode: Option<u8>,
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    state: State,
) -> sm83::Result<(State, memory::Pointer<PtrVal>)> {
    let op_ptr = p.contextualize((*p.as_pointer()).overflowing_add(1).0);
    let offset = state
        .get_memory(&op_ptr, mem)
        .into_concrete()
        .ok_or_else(|| analysis::Error::UnconstrainedMemory(op_ptr))? as i8 as i16
        as u16;
    let target = p.contextualize(
        (*p.as_pointer())
            .overflowing_add(2)
            .0
            .overflowing_add(offset)
            .0,
    );

    if flag_test(condcode, state.get_register(&Register::F))? {
        Ok((state, target))
    } else {
        Ok((state, p.clone() + 2))
    }
}

/// Trace dynamic jumps
fn trace_jump_dynamic(
    p: &memory::Pointer<PtrVal>,
    state: State,
) -> sm83::Result<(State, memory::Pointer<PtrVal>)> {
    match read_value_from_targetpair(&state, 2, false)?.into_concrete() {
        Some(hl) => Ok((state, p.contextualize(hl))),
        _ => Err(analysis::Error::UnconstrainedRegister),
    }
}

/// Trace SP initialization
fn trace_sp_set(mut state: State, trace: &mut Trace) -> State {
    trace.register_set(Register::S, state.get_register(&Register::H), &mut state);
    trace.register_set(Register::P, state.get_register(&Register::L), &mut state);

    state
}

/// Trace high memory store
fn trace_himem_store(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
) -> sm83::Result<State> {
    let op_ptr = p.contextualize(*p.as_pointer() + 1);
    match mem.read_unit(&op_ptr).into_concrete() {
        Some(hi) => {
            trace.memory_write(
                p.contextualize(0xFF00 | hi as u16),
                &[state.get_register(&Register::A)],
                &mut state,
            );

            Ok(state)
        }
        _ => Err(analysis::Error::UnconstrainedMemory(op_ptr)),
    }
}

/// Trace SP adjustment
fn trace_sp_adjust(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
) -> State {
    let adjust: reg::Symbolic<u16> =
        reg::Symbolic::convert_from(mem.read_unit(&p.contextualize(*p.as_pointer() + 1)));
    let hi_sp: reg::Symbolic<u16> = reg::Symbolic::convert_from(state.get_register(&Register::S));
    let lo_sp: reg::Symbolic<u16> = reg::Symbolic::convert_from(state.get_register(&Register::P));
    let new_sp = (hi_sp << 8 | lo_sp) + adjust;

    let hi_new_sp: reg::Symbolic<u8> =
        reg::Symbolic::try_convert_from(new_sp >> 8 as u8).expect("Edit:");
    let lo_new_sp: reg::Symbolic<u8> =
        reg::Symbolic::try_convert_from(new_sp & reg::Symbolic::from(0xFF as u16))
            .expect("Downvotes, really?");

    trace.register_set(Register::S, hi_new_sp, &mut state);
    trace.register_set(Register::P, lo_new_sp, &mut state);

    state
}

/// Trace high memory load
fn trace_himem_load(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
) -> sm83::Result<State> {
    let op_ptr = p.contextualize(*p.as_pointer() + 1);
    match mem.read_unit(&op_ptr).into_concrete() {
        Some(hi) => {
            let mv = state.get_memory(&p.contextualize(0xFF00 | hi as u16), mem);
            trace.register_set(Register::A, mv, &mut state);

            Ok(state)
        }
        _ => Err(analysis::Error::UnconstrainedMemory(op_ptr)),
    }
}

/// Trace SP offset calculation
fn trace_sp_offset_calc(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
) -> sm83::Result<State> {
    let op_ptr = p.clone() + 1;
    let r8: reg::Symbolic<u16> = reg::Symbolic::convert_from(mem.read_unit(&op_ptr));
    let sign = match (r8 & reg::Symbolic::from(0x0080 as u16)).into_concrete() {
        Some(0x80) => reg::Symbolic::from(0xFF80 as u16),
        Some(0x00) => reg::Symbolic::from(0x0000 as u16),
        _ => reg::Symbolic::from_cares(0 as u16, 0x007F as u16),
    };
    let offset = sign | r8;
    let s: reg::Symbolic<u16> = reg::Symbolic::convert_from(state.get_register(&Register::S));
    let p: reg::Symbolic<u16> = reg::Symbolic::convert_from(state.get_register(&Register::P));
    let sp = (s << 8) | p;
    let hl = sp + offset;
    let h: reg::Symbolic<u8> =
        reg::Symbolic::try_convert_from(hl >> 8).map_err(|_| analysis::Error::BlockSizeOverflow)?;
    let l: reg::Symbolic<u8> =
        reg::Symbolic::try_convert_from(hl & reg::Symbolic::from(0xFF as u16))
            .map_err(|_| analysis::Error::BlockSizeOverflow)?;

    trace.register_set(Register::H, h, &mut state);
    trace.register_set(Register::L, l, &mut state);

    Ok(state)
}

/// Trace himem indirect store
fn trace_himem_indir_store(
    p: &memory::Pointer<PtrVal>,
    mut state: State,
    trace: &mut Trace,
) -> sm83::Result<State> {
    match state.get_register(&Register::C).into_concrete() {
        Some(c) => {
            let op_ptr = 0xFF00 | c as u16;
            trace.memory_write(
                p.contextualize(op_ptr),
                &[state.get_register(&Register::A)],
                &mut state,
            );
            Ok(state)
        }
        None => Err(analysis::Error::UnconstrainedRegister),
    }
}

/// Trace writing the accumulator to a memory location
fn trace_mem_store(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
) -> sm83::Result<State> {
    let op_ptr = p.contextualize(*p.as_pointer() + 1);
    let memloc = mem
        .read_leword::<PtrVal>(&op_ptr)
        .into_concrete()
        .ok_or(analysis::Error::UnconstrainedMemory(op_ptr))?;

    trace.memory_write(
        p.contextualize(memloc),
        &[state.get_register(&Register::A)],
        &mut state,
    );

    Ok(state)
}

/// Trace himem indirect load
fn trace_himem_indir_load(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
) -> sm83::Result<State> {
    match state.get_register(&Register::C).into_concrete() {
        Some(c) => {
            let op_ptr = 0xFF00 | c as u16;
            trace.register_set(
                Register::A,
                state.get_memory(&p.contextualize(op_ptr), mem),
                &mut state,
            );
            Ok(state)
        }
        None => Err(analysis::Error::UnconstrainedRegister),
    }
}

/// Trace loading the accumulator from a memory location
fn trace_mem_load(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
) -> sm83::Result<State> {
    let op_ptr = p.contextualize(*p.as_pointer() + 1);
    let memloc = mem
        .read_leword::<PtrVal>(&op_ptr)
        .into_concrete()
        .ok_or(analysis::Error::UnconstrainedMemory(op_ptr))?;

    trace.register_set(
        Register::A,
        state.get_memory(&p.contextualize(memloc), mem),
        &mut state,
    );

    Ok(state)
}

fn trace_regpair_set(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
    targetpair: u8,
) -> sm83::Result<State> {
    let value: reg::Symbolic<PtrVal> = mem.read_leword(&(p.clone() + 1));

    write_value_to_targetpair(&mut state, trace, targetpair, false, value)?;

    Ok(state)
}

fn trace_wide_add(mut state: State, trace: &mut Trace, targetpair: u8) -> sm83::Result<State> {
    let hl = read_value_from_targetpair(&state, 2, false)?;
    let other_val = read_value_from_targetpair(&state, targetpair, false)?;

    let new_hl = hl + other_val;

    write_value_to_targetpair(&mut state, trace, 2, false, new_hl)?;

    Ok(state)
}

fn trace_targetmem_store(
    p: &memory::Pointer<PtrVal>,
    mut state: State,
    trace: &mut Trace,
    targetmem: u8,
) -> sm83::Result<State> {
    let ptr = match targetmem {
        2 => {
            let ptr = read_value_from_targetpair(&state, 2, false)?;
            let writeback = ptr + reg::Symbolic::from(1);

            write_value_to_targetpair(&mut state, trace, 2, false, writeback)?;
            ptr
        }
        3 => {
            let ptr = read_value_from_targetpair(&state, 2, false)?;
            let writeback = ptr - reg::Symbolic::from(1);

            write_value_to_targetpair(&mut state, trace, 2, false, writeback)?;
            ptr
        }
        _ => read_value_from_targetpair(&state, targetmem, false)?,
    };

    let a = state.get_register(&Register::A);
    let hl = match ptr.into_concrete() {
        Some(ptr) => p.contextualize(ptr),
        None => return Err(analysis::Error::UnconstrainedRegister),
    };

    trace.memory_write(hl, &[a], &mut state);

    match targetmem {
        2 => {
            let writeback = ptr + reg::Symbolic::from(1);
            write_value_to_targetpair(&mut state, trace, 2, false, writeback)?;
        }
        3 => {
            let writeback = ptr - reg::Symbolic::from(1);
            write_value_to_targetpair(&mut state, trace, 2, false, writeback)?;
        }
        _ => {}
    };

    Ok(state)
}

fn trace_targetmem_load(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
    targetmem: u8,
) -> sm83::Result<State> {
    let ptr = match targetmem {
        2 => {
            let ptr = read_value_from_targetpair(&state, 2, false)?;
            let writeback = ptr + reg::Symbolic::from(1);

            write_value_to_targetpair(&mut state, trace, 2, false, writeback)?;
            ptr
        }
        3 => {
            let ptr = read_value_from_targetpair(&state, 2, false)?;
            let writeback = ptr - reg::Symbolic::from(1);

            write_value_to_targetpair(&mut state, trace, 2, false, writeback)?;
            ptr
        }
        _ => read_value_from_targetpair(&state, targetmem, false)?,
    };

    let hl = match ptr.into_concrete() {
        Some(ptr) => p.contextualize(ptr),
        None => return Err(analysis::Error::UnconstrainedRegister),
    };
    let a = state.get_memory(&hl, mem);

    trace.register_set(Register::A, a, &mut state);

    match targetmem {
        2 => {
            let writeback = ptr + reg::Symbolic::from(1);
            write_value_to_targetpair(&mut state, trace, 2, false, writeback)?;
        }
        3 => {
            let writeback = ptr - reg::Symbolic::from(1);
            write_value_to_targetpair(&mut state, trace, 2, false, writeback)?;
        }
        _ => {}
    };

    Ok(state)
}

fn trace_targetpair_inc(
    mut state: State,
    trace: &mut Trace,
    targetpair: u8,
) -> sm83::Result<State> {
    let pairval = read_value_from_targetpair(&state, targetpair, false)?;
    let newval = pairval + reg::Symbolic::from(1);
    write_value_to_targetpair(&mut state, trace, targetpair, false, newval)?;

    Ok(state)
}

fn trace_targetpair_dec(
    mut state: State,
    trace: &mut Trace,
    targetpair: u8,
) -> sm83::Result<State> {
    let pairval = read_value_from_targetpair(&state, targetpair, false)?;
    let newval = pairval - reg::Symbolic::from(1);
    write_value_to_targetpair(&mut state, trace, targetpair, false, newval)?;

    Ok(state)
}

fn trace_targetreg_inc(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
    targetreg: u8,
) -> sm83::Result<State> {
    let pairval = read_value_from_targetreg(p, mem, &state, targetreg)?;
    let newval = pairval + reg::Symbolic::from(1);
    write_value_to_targetreg(p, mem, &mut state, trace, targetreg, newval)?;

    Ok(state)
}

fn trace_targetreg_dec(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
    targetreg: u8,
) -> sm83::Result<State> {
    let pairval = read_value_from_targetreg(p, mem, &state, targetreg)?;
    let newval = pairval - reg::Symbolic::from(1);
    write_value_to_targetreg(p, mem, &mut state, trace, targetreg, newval)?;

    Ok(state)
}

fn trace_targetreg_set(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
    targetreg: u8,
) -> sm83::Result<State> {
    let op_ptr = p.clone() + 1;
    let operand = state.get_memory(&op_ptr, mem);
    write_value_to_targetreg(p, mem, &mut state, trace, targetreg, operand)?;

    Ok(state)
}

fn trace_targetreg_copy(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
    targetreg_src: u8,
    targetreg_dst: u8,
) -> sm83::Result<State> {
    let value = read_value_from_targetreg(p, mem, &state, targetreg_src)?;
    write_value_to_targetreg(p, mem, &mut state, trace, targetreg_dst, value)?;

    Ok(state)
}

fn trace_stackpair_push(
    p: &memory::Pointer<PtrVal>,
    mut state: State,
    trace: &mut Trace,
    stackpair: u8,
) -> sm83::Result<State> {
    let value = read_value_from_targetpair(&state, stackpair, true)?;
    push_value_to_sp(p, &mut state, trace, value)?;

    Ok(state)
}

fn trace_stackpair_pop(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
    stackpair: u8,
) -> sm83::Result<State> {
    let value = pop_value_from_sp(p, mem, &mut state, trace)?;
    write_value_to_targetpair(&mut state, trace, stackpair, true, value)?;

    Ok(state)
}

fn trace_aluop_register(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
    the_aluop: u8,
    targetreg: u8,
) -> sm83::Result<State> {
    let operand = read_value_from_targetreg(p, mem, &state, targetreg)?;
    let (new_accum, new_flags) = aluop(
        the_aluop,
        state.get_register(&Register::A),
        operand,
        state.get_register(&Register::F),
    )?;

    trace.register_set(Register::A, new_accum, &mut state);
    trace.register_set(Register::F, new_flags, &mut state);

    Ok(state)
}

fn trace_aluop_immediate(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
    the_aluop: u8,
) -> sm83::Result<State> {
    let op_ptr = p.clone() + 1;
    let operand = mem.read_unit(&op_ptr);
    let (new_accum, new_flags) = aluop(
        the_aluop,
        state.get_register(&Register::A),
        operand,
        state.get_register(&Register::F),
    )?;

    trace.register_set(Register::A, new_accum, &mut state);
    trace.register_set(Register::F, new_flags, &mut state);

    Ok(state)
}

fn trace_oldbitops(mut state: State, trace: &mut Trace, op: u8) -> sm83::Result<State> {
    let old_carry = (state.get_register(&Register::F) & reg::Symbolic::from(0x10)) >> 4;
    let old_a = state.get_register(&Register::A);

    let (new_a, new_carry) = match op {
        0 => (
            (old_a << 1 | (old_a & reg::Symbolic::from(0x80)) >> 7),
            (old_a & reg::Symbolic::from(0x80)) >> 7,
        ), //RLCA
        1 => (
            (old_a >> 1 | (old_a & reg::Symbolic::from(0x01)) << 7),
            (old_a & reg::Symbolic::from(0x01)),
        ), //RRCA
        2 => (
            (old_a << 1 | old_carry),
            (old_a & reg::Symbolic::from(0x80)) >> 7,
        ), //RLA
        3 => (
            (old_a >> 1 | old_carry),
            (old_a & reg::Symbolic::from(0x01)),
        ), //RRA
        4 => return Err(analysis::Error::NotYetImplemented), //DAA
        5 => (!old_a, old_carry),                            //CPL
        6 => (old_a, reg::Symbolic::from(0x01)),             //SCF
        7 => (old_a, reg::Symbolic::from(0x00)),             //CCF
        _ => return Err(analysis::Error::Misinterpretation(1, false)),
    };

    let new_flags =
        ((state.get_register(&Register::F)) & reg::Symbolic::from(0xEF)) | new_carry << 4;

    trace.register_set(Register::F, new_flags, &mut state);
    trace.register_set(Register::A, new_a, &mut state);

    Ok(state)
}

fn trace_rst(
    ptr: &memory::Pointer<PtrVal>,
    mut state: State,
    trace: &mut Trace,
    op: u8,
) -> sm83::Result<(State, memory::Pointer<PtrVal>)> {
    let target = (op & 0x38) as u16;
    let ret_pc = *ptr.as_pointer() + 3;

    push_value_to_sp(ptr, &mut state, trace, reg::Symbolic::from(ret_pc))?;

    Ok((state, ptr.contextualize(target)))
}

/// Trace the current instruction state into a new one.
///
/// This function yields None if the current memory model and execution state
/// do not yield an unambiguous future symbolic state. In order to guarantee
/// that future execution is unambiguous, you must perform forking analysis in
/// `prereq` to determine what registers or memory must be concretely resolved.
/// If you accept the additional state load, you must then produce one state for
/// every possible combination of concrete values for those given memory and
/// register locations.
///
/// If the function was able to successfully trace execution of the next
/// instruction, the following information is returned:
///
///  * The new state after the execution has been traced
///  * The address of the next instruction to execute
pub fn trace(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    state: State,
    trace: &mut Trace,
) -> sm83::Result<(State, memory::Pointer<PtrVal>)> {
    match mem.read_unit(p).into_concrete() {
        Some(0xCB) => match mem.read_unit(&(p.clone() + 1)).into_concrete() {
            Some(subop) => {
                let targetreg = subop & 0x07;
                let new_bitop = (subop >> 3) & 0x07;

                match ((subop >> 6) & 0x03, (subop >> 3) & 0x07, subop & 0x07) {
                    (0, _, _) => Ok((
                        trace_bitop(p, mem, state, trace, new_bitop, targetreg)?,
                        (p.clone() + 2),
                    )),
                    (1, bit, _) => Ok((
                        trace_bittest(p, mem, state, trace, bit, targetreg)?,
                        (p.clone() + 2),
                    )),
                    (2, bit, _) => Ok((
                        trace_bitreset(p, mem, state, trace, bit, targetreg)?,
                        (p.clone() + 2),
                    )),
                    (3, bit, _) => Ok((
                        trace_bitset(p, mem, state, trace, bit, targetreg)?,
                        (p.clone() + 2),
                    )),
                    _ => Err(analysis::Error::Misinterpretation(2, false)),
                }
            }
            _ => Err(analysis::Error::UnconstrainedMemory(p.clone() + 1)),
        },

        //Z80 instructions that don't fit the pattern decoder below
        Some(0x00) => Ok((state, p.clone() + 1)), //nop
        Some(0x08) => Ok((trace_sp_storage(p, mem, state, trace)?, p.clone() + 3)), //ld [u16], SP
        Some(0x10) => Ok((state, p.clone() + 1)), //stop
        Some(0x18) => trace_jump_relative(None, p, mem, state), //jr u8
        Some(0x76) => Ok((state, p.clone() + 1)), //halt

        Some(0xC3) => trace_jump(None, p, mem, state), //jp u16
        Some(0xCD) => trace_call(None, p, mem, state, trace),

        Some(0xC9) => trace_return(None, p, mem, state, trace), //ret
        Some(0xD9) => trace_return(None, p, mem, state, trace), //reti
        Some(0xE9) => trace_jump_dynamic(p, state),             //jp hl
        Some(0xF9) => Ok((trace_sp_set(state, trace), p.clone() + 1)), //ld sp, hl

        Some(0xE0) => Ok((trace_himem_store(p, mem, state, trace)?, p.clone() + 2)), //ldh [u8], a
        Some(0xE8) => Ok((trace_sp_adjust(p, mem, state, trace), p.clone() + 2)),    //add sp, u8
        Some(0xF0) => Ok((trace_himem_load(p, mem, state, trace)?, p.clone() + 2)),  //ldh a, [u8]
        Some(0xF8) => Ok((trace_sp_offset_calc(p, mem, state, trace)?, p.clone() + 2)), //ld hl, sp+u8

        Some(0xE2) => Ok((trace_himem_indir_store(p, state, trace)?, p.clone() + 1)), //ld [c], a
        Some(0xEA) => Ok((trace_mem_store(p, mem, state, trace)?, p.clone() + 3)),    //ld [u16], a
        Some(0xF2) => Ok((trace_himem_indir_load(p, mem, state, trace)?, p.clone() + 1)), //ld a, [c]
        Some(0xFA) => Ok((trace_mem_load(p, mem, state, trace)?, p.clone() + 3)), //ld a, [u16]

        Some(0xF3) => Ok((state, p.clone() + 1)), //di
        Some(0xFB) => Ok((state, p.clone() + 1)), //ei

        //Z80 instructions that follow a particular pattern
        Some(op) => {
            let condcode = (op >> 3) & 0x03;
            let targetpair = (op >> 4) & 0x03;
            let targetreg = (op >> 3) & 0x07;
            let targetmem = (op >> 4) & 0x03;
            let bitop = (op >> 3) & 0x07;
            let targetreg2 = op & 0x07;
            let aluop = (op >> 3) & 0x07;
            let stackpair = (op >> 4) & 0x03;

            //decode `op` into aab?cddd. This creates a nice visual table for
            //the Z80's semiperiodic instruction encoding
            match (
                (op >> 6) & 0x03,
                (op >> 5) & 0x01,
                (op >> 3) & 0x01,
                op & 0x07,
            ) {
                (0, 0, _, 0) => Err(analysis::Error::Misinterpretation(1, false)), /* 00, 08, 10, 18 */
                (0, 1, _, 0) => trace_jump_relative(Some(condcode), p, mem, state), //jr cond, u8
                (0, _, 0, 1) => Ok((
                    trace_regpair_set(p, mem, state, trace, targetpair)?,
                    p.clone() + 3,
                )), //ld targetpair, u16
                (0, _, 1, 1) => Ok((trace_wide_add(state, trace, targetpair)?, p.clone() + 1)), //add hl, targetpair
                (0, _, 0, 2) => Ok((
                    trace_targetmem_store(p, state, trace, targetmem)?,
                    p.clone() + 1,
                )), //ld [targetmem], a
                (0, _, 1, 2) => Ok((
                    trace_targetmem_load(p, mem, state, trace, targetmem)?,
                    p.clone() + 1,
                )), //ld a, [targetmem]
                (0, _, 0, 3) => Ok((
                    trace_targetpair_inc(state, trace, targetpair)?,
                    p.clone() + 1,
                )), //inc targetpair
                (0, _, 1, 3) => Ok((
                    trace_targetpair_dec(state, trace, targetpair)?,
                    p.clone() + 1,
                )), //dec targetpair
                (0, _, _, 4) => Ok((
                    trace_targetreg_inc(p, mem, state, trace, targetreg)?,
                    p.clone() + 1,
                )), //inc targetreg
                (0, _, _, 5) => Ok((
                    trace_targetreg_dec(p, mem, state, trace, targetreg)?,
                    p.clone() + 1,
                )), //dec targetreg
                (0, _, _, 6) => Ok((
                    trace_targetreg_set(p, mem, state, trace, targetreg)?,
                    p.clone() + 2,
                )), //ld targetreg, u8
                (0, _, _, 7) => Ok((trace_oldbitops(state, trace, bitop)?, p.clone() + 1)), //old bitops
                (1, _, _, _) => Ok((
                    trace_targetreg_copy(p, mem, state, trace, targetreg, targetreg2)?,
                    p.clone() + 1,
                )), //ld targetreg2, targetreg
                (2, _, _, _) => Ok((
                    trace_aluop_register(p, mem, state, trace, aluop, targetreg2)?,
                    p.clone() + 1,
                )), //(aluop) a, targetreg2
                (3, 0, _, 0) => trace_return(Some(condcode), p, mem, state, trace), //ret cond
                (3, 1, _, 0) => Err(analysis::Error::Misinterpretation(1, false)), /* E0, E8, F0, F8 */
                (3, _, 0, 1) => Ok((
                    trace_stackpair_pop(p, mem, state, trace, stackpair)?,
                    p.clone() + 1,
                )), //pop stackpair
                (3, _, 1, 1) => Err(analysis::Error::Misinterpretation(1, false)), /* C9, D9, E9, F9 */
                (3, 0, _, 2) => trace_jump(Some(condcode), p, mem, state),         //jp cond, u16
                (3, 1, _, 2) => Err(analysis::Error::Misinterpretation(1, false)), /* E2, EA, F2, FA */
                (3, _, _, 3) => Err(analysis::Error::InvalidInstruction),
                (3, 0, _, 4) => trace_call(Some(condcode), p, mem, state, trace), //call cond, u16
                (3, 1, _, 4) => Err(analysis::Error::InvalidInstruction),
                (3, _, 0, 5) => Ok((
                    trace_stackpair_push(p, state, trace, stackpair)?,
                    p.clone() + 1,
                )), //push stackpair
                (3, _, 1, 5) => Err(analysis::Error::InvalidInstruction),
                (3, _, _, 6) => Ok((
                    trace_aluop_immediate(p, mem, state, trace, aluop)?,
                    p.clone() + 2,
                )), //(aluop) a, u8
                (3, _, _, 7) => trace_rst(p, state, trace, op), //rst op& 0x38

                _ => Err(analysis::Error::InvalidInstruction),
            }
        }
        _ => Err(analysis::Error::UnconstrainedMemory(p.clone())),
    }
}
