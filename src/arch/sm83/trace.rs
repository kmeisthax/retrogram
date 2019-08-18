//! Facilities for tracing SM83 code

use crate::{memory, reg, analysis};
use crate::reg::{New, Convertable, TryConvertable};
use crate::arch::sm83;
//use crate::arch::sm83::dis::{ALU_BITOPS, ALU_CONDCODE, ALU_OPS, ALU_TARGET_MEM, ALU_TARGET_PAIRS, ALU_TARGET_REGS, STACK_TARGET_PAIRS};
use crate::arch::sm83::{Register, Pointer, Bus, State, Value, Offset};

/// Given a targetreg operand, produce a symbolic value of what that target reg
/// would be given the current state.
fn read_value_from_targetreg(p: &memory::Pointer<Pointer>, mem: &Bus, state: &State, targetreg: u8) -> sm83::Result<reg::Symbolic<Value>> {
    match targetreg {
        0 => Ok(state.get_register(Register::B)),
        1 => Ok(state.get_register(Register::C)),
        2 => Ok(state.get_register(Register::D)),
        3 => Ok(state.get_register(Register::E)),
        4 => Ok(state.get_register(Register::H)),
        5 => Ok(state.get_register(Register::L)),
        6 => {
            let hl = reg::Symbolic::<Pointer>::convert_from(state.get_register(Register::H)) << 8 | reg::Symbolic::<Pointer>::convert_from(state.get_register(Register::L));
            if let Some(hl) = hl.into_concrete() {
                let ptr_hl = mem.minimize_context(p.contextualize(hl)); //TODO: Attempt to pull context from state
                Ok(mem.read_unit(&ptr_hl)) //TODO: Should respond to memory state
            } else {
                Ok(reg::Symbolic::default())
            }
        },
        7 => Ok(state.get_register(Register::A)),
        _ => Err(analysis::Error::Misinterpretation(1, false)) //TODO: Get correct instruction offset
    }
}

/// Given a targetreg operand, manipulate the given state to incorporate the
/// effect of writing a given value to that operand.
fn write_value_to_targetreg(p: &memory::Pointer<Pointer>, mem: &Bus, state: &mut State, targetreg: u8, value: reg::Symbolic<Value>) -> sm83::Result<()> {
    match targetreg {
        0 => Ok(state.set_register(Register::B, value)),
        1 => Ok(state.set_register(Register::C, value)),
        2 => Ok(state.set_register(Register::D, value)),
        3 => Ok(state.set_register(Register::E, value)),
        4 => Ok(state.set_register(Register::H, value)),
        5 => Ok(state.set_register(Register::L, value)),
        6 => {
            let hl = reg::Symbolic::<Pointer>::convert_from(state.get_register(Register::H)) << 8 | reg::Symbolic::<Pointer>::convert_from(state.get_register(Register::L));
            if let Some(hl) = hl.into_concrete() {
                let ptr_hl = mem.minimize_context(p.contextualize(hl)); //TODO: Attempt to pull context from state
                return Ok(state.set_memory(ptr_hl, value));
            }

            Err(analysis::Error::UnconstrainedRegister)
        },
        7 => Ok(state.set_register(Register::A, value)),
        _ => Err(analysis::Error::Misinterpretation(1, false)) //TODO: Get correct instruction offset
    }
}

fn read_value_from_targetpair(state: &State, targetpair: u8, is_stackpair: bool) -> sm83::Result<(reg::Symbolic<Pointer>)> {
    let hival : reg::Symbolic<u16> = reg::Symbolic::convert_from(match (targetpair, is_stackpair) {
        (0, _) => state.get_register(Register::B),
        (1, _) => state.get_register(Register::D),
        (2, _) => state.get_register(Register::H),
        (3, false) => state.get_register(Register::S),
        (3, true) => state.get_register(Register::A),
        _ => return Err(analysis::Error::Misinterpretation(1, false))
    });
    let loval : reg::Symbolic<u16> = reg::Symbolic::convert_from(match (targetpair, is_stackpair) {
        (0, _) => state.get_register(Register::C),
        (1, _) => state.get_register(Register::E),
        (2, _) => state.get_register(Register::L),
        (3, false) => state.get_register(Register::P),
        (3, true) => state.get_register(Register::F),
        _ => return Err(analysis::Error::Misinterpretation(1, false))
    });

    Ok(hival << 8 | loval)
}

/// Given a targetpair operand, manipulate the given state to incorporate the
/// effect of writing values to those operands.
fn write_value_to_targetpair(state: &mut State, targetpair: u8, is_stackpair: bool, value: reg::Symbolic<Pointer>) -> sm83::Result<()> {
    //TODO: Some kind of "regsegment" method?
    let loval : reg::Symbolic<u8> = reg::Symbolic::try_convert_from(value.clone() & reg::Symbolic::new(0x00FF)).map_err(|_| analysis::Error::BlockSizeOverflow)?;
    let hival : reg::Symbolic<u8> = reg::Symbolic::try_convert_from(value >> 8).map_err(|_| analysis::Error::BlockSizeOverflow)?;

    match (targetpair, is_stackpair) {
        (0, _) => {
            state.set_register(Register::B, hival);
            state.set_register(Register::C, loval);
        }, //BC
        (1, _) => {
            state.set_register(Register::D, hival);
            state.set_register(Register::E, loval);
        }, //DE
        (2, _) => {
            state.set_register(Register::H, hival);
            state.set_register(Register::L, loval);
        }, //HL
        (3, false) => {
            state.set_register(Register::S, hival);
            state.set_register(Register::P, loval);
        }, //SP
        (3, true) => {
            state.set_register(Register::A, hival);
            state.set_register(Register::F, loval);
        }, //AF
        _ => return Err(analysis::Error::Misinterpretation(1, false)) //TODO: Get correct instruction offset
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
fn pop_value_from_sp(ptr: &memory::Pointer<Pointer>, mem: &Bus, state: &mut State) -> sm83::Result<reg::Symbolic<u16>> {
    match read_value_from_targetpair(&state, 3, false)?.into_concrete() {
        Some(sp) => {
            let next_sp = sp + 2;
            write_value_to_targetpair(state, 3, false, reg::Symbolic::new(next_sp))?;

            let loval : reg::Symbolic<u16> = reg::Symbolic::convert_from(state.get_memory(ptr.contextualize(sp), mem));
            let hival : reg::Symbolic<u16> = reg::Symbolic::convert_from(state.get_memory(ptr.contextualize(sp + 1), mem));
            let val = hival << 8 | loval;
            
            Ok(val)
        },
        _ => return Err(analysis::Error::UnconstrainedRegister)
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
fn push_value_to_sp(ptr: &memory::Pointer<Pointer>, state: &mut State, value: reg::Symbolic<Pointer>) -> sm83::Result<()> {
    match read_value_from_targetpair(&state, 3, false)?.into_concrete() {
        Some(sp) => {
            let lo_value = reg::Symbolic::try_convert_from(value & reg::Symbolic::new(0xFF)).map_err(|_| analysis::Error::BlockSizeOverflow)?;
            let hi_value = reg::Symbolic::try_convert_from(value >> 8).map_err(|_| analysis::Error::BlockSizeOverflow)?;

            state.set_memory(ptr.contextualize(sp), lo_value);
            state.set_memory(ptr.contextualize(sp + 1), hi_value);

            let next_sp = sp - 2;
            write_value_to_targetpair(state, 3, false, reg::Symbolic::new(next_sp))?;

            Ok(())
        },
        _ => Err(analysis::Error::UnconstrainedRegister)
    }
}

/// Given a symbolic value, compute the zero flag that it would generate in the
/// F register if it were the result of a computation.
/// 
/// The resulting value is suitable for use as a new flags register with the
/// format of Z000. You may OR in additional bits as necessary.
fn zero_flag(val: reg::Symbolic<Value>) -> reg::Symbolic<Value> {
    match (val.into_concrete(), val.is_valid(0)) {
        (Some(0), _) => reg::Symbolic::new(0x80 as Value),
        (Some(_), _) => reg::Symbolic::new(0 as Value),
        (None, true) => reg::Symbolic::from_cares(0 as Value, 0x7F as Value),
        (None, false) => reg::Symbolic::new(0 as Value),
    }
}

/// Given a condition code and flags, return true if the test would pass.
/// 
/// You may pass None as the condition code, which indicates an unconditional
/// branch (and thus, always returns true).
fn flag_test(condcode: Option<u8>, value: reg::Symbolic<Value>) -> sm83::Result<bool> {
    match condcode {
        Some(0) => Ok((value & reg::Symbolic::new(0x80)).into_concrete().ok_or(analysis::Error::UnconstrainedRegister)? != 0), //NZ
        Some(1) => Ok((value & reg::Symbolic::new(0x80)).into_concrete().ok_or(analysis::Error::UnconstrainedRegister)? == 0), //Z
        Some(2) => Ok((value & reg::Symbolic::new(0x10)).into_concrete().ok_or(analysis::Error::UnconstrainedRegister)? != 0), //NC
        Some(3) => Ok((value & reg::Symbolic::new(0x10)).into_concrete().ok_or(analysis::Error::UnconstrainedRegister)? == 0), //C
        None => Ok(true),
        _ => Err(analysis::Error::Misinterpretation(2, false))
    }
}

/// Trace a CB-prefix bit rotate operation (RLC, RRC, RL, RR, SLA, SRA, SWAP,
/// or SRL).
fn trace_bitop(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, bitop: u8, targetreg: u8) -> sm83::Result<State> {
    let flags = state.get_register(Register::F);
    let carry = flags & reg::Symbolic::new(0x10);
    let val = read_value_from_targetreg(p, mem, &state, targetreg)?;

    let (newval, newcarry) = match bitop {
        0 => (val << 1 | val >> 7, carry), //RLC
        1 => (val << 1 | val >> 7, carry), //RRC
        2 => (val << 1 | carry >> 4, val >> 7), //RL
        3 => (val >> 1 | carry << 3, val & reg::Symbolic::new(0x01)), //RR
        4 => (val << 1, val >> 7), //SLA
        //This is a manual sign extension since we defined Value as unsigned
        5 => (val >> 1 | val & reg::Symbolic::new(0x80), val & reg::Symbolic::new(0x01)), //SRA
        6 => (val >> 4 | val << 4, reg::Symbolic::new(0)), //SWAP
        7 => (val >> 1, val & reg::Symbolic::new(0x01)), //SRL
        _ => return Err(analysis::Error::Misinterpretation(2, false))
    };
    
    state.set_register(Register::F, newcarry << 4 | zero_flag(val)); //N and H flags are always zero
    write_value_to_targetreg(p, mem, &mut state, targetreg, newval)?;

    Ok(state)
}

/// Trace a CB-prefix bit test instruction (e.g. BIT n, reg).
fn trace_bittest(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, targetbit: u8, targetreg: u8) -> sm83::Result<State> {
    let flags = state.get_register(Register::F);
    let val = read_value_from_targetreg(p, mem, &state, targetreg)? >> targetbit & reg::Symbolic::new(0x01);

    state.set_register(Register::F, flags & reg::Symbolic::new(0x10) | reg::Symbolic::new(0x20) | zero_flag(val));

    Ok(state)
}

/// Trace a CB-prefix bit reset instruction (e.g. RES n, reg).
fn trace_bitreset(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, targetbit: u8, targetreg: u8) -> sm83::Result<State> {
    let mask = reg::Symbolic::new(!(1 << targetbit));
    let val = read_value_from_targetreg(p, mem, &state, targetreg)?;

    write_value_to_targetreg(p, mem, &mut state, targetreg, val & mask)?;

    Ok(state)
}

/// Trace a CB-prefix bit set instruction (e.g. SET n, reg).
fn trace_bitset(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, targetbit: u8, targetreg: u8) -> sm83::Result<State> {
    let bit = reg::Symbolic::new(1 << targetbit);
    let val = read_value_from_targetreg(p, mem, &state, targetreg)?;

    write_value_to_targetreg(p, mem, &mut state, targetreg, val | bit)?;

    Ok(state)
}

/// Trace writing the stack pointer to a memory location
fn trace_sp_storage(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> sm83::Result<State> {
    let op_ptr = p.contextualize(p.as_pointer().clone()+1);
    let memloc = mem.read_leword::<Pointer>(&op_ptr).into_concrete().ok_or(analysis::Error::UnconstrainedMemory(op_ptr))?;

    state.set_memory(p.contextualize(memloc), state.get_register(Register::P));
    state.set_memory(p.contextualize(memloc+1), state.get_register(Register::S));

    Ok(state)
}

/// Trace full jumps
fn trace_jump(condcode: Option<u8>, p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> sm83::Result<(State, memory::Pointer<Pointer>)> {
    if flag_test(condcode, state.get_register(Register::F))? {
        let op_ptr = p.contextualize(p.as_pointer().clone()+1);
        let target = mem.read_leword::<u16>(&op_ptr).into_concrete().ok_or(analysis::Error::UnconstrainedMemory(op_ptr))?;

        Ok((state, p.contextualize(target)))
    } else {
        Ok((state, p.clone() + 3))
    }
}

/// Trace full calls
fn trace_call(condcode: Option<u8>, ptr: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> analysis::Result<(State, memory::Pointer<Pointer>), Pointer, Offset> {
    if flag_test(condcode, state.get_register(Register::F))? {
        let op_ptr = ptr.contextualize(ptr.as_pointer().clone()+1);
        let target = mem.read_leword::<u16>(&op_ptr).into_concrete().ok_or(analysis::Error::UnconstrainedMemory(op_ptr))?;
        let ret_pc = ptr.as_pointer().clone() + 3;

        push_value_to_sp(ptr, &mut state, reg::Symbolic::new(ret_pc))?;

        Ok((state, ptr.contextualize(target)))
    } else {
        Ok((state, ptr.clone() + 3))
    }
}

/// Trace return
fn trace_return(condcode: Option<u8>, ptr: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> sm83::Result<(State, memory::Pointer<Pointer>)> {
    if flag_test(condcode, state.get_register(Register::F))? {
        let old_spval = read_value_from_targetpair(&state, 3, false)?.into_concrete().ok_or(analysis::Error::UnconstrainedRegister)?;
        let new_pcval = pop_value_from_sp(ptr, mem, &mut state)?.into_concrete().ok_or_else(|| analysis::Error::UnconstrainedMemory(ptr.contextualize(old_spval)))?;
        let new_ptr = ptr.contextualize(new_pcval);

        Ok((state, new_ptr))
    } else {
        Ok((state, ptr.clone() + 1))
    }
}

/// Trace relative jumps
fn trace_jump_relative(condcode: Option<u8>, p: &memory::Pointer<Pointer>, mem: &Bus, state: State) -> sm83::Result<(State, memory::Pointer<Pointer>)> {
    let op_ptr = p.contextualize(p.as_pointer().clone() + 1);
    let offset = state.get_memory(op_ptr.clone(), mem).into_concrete().ok_or_else(|| analysis::Error::UnconstrainedMemory(op_ptr))? as i8 as i16 as u16;
    let target = p.contextualize(p.as_pointer().clone() + 2 + offset);
    
    if flag_test(condcode, state.get_register(Register::F))? {
        Ok((state, target))
    } else {
        Ok((state, p.clone()+2))
    }
}

/// Trace dynamic jumps
fn trace_jump_dynamic(p: &memory::Pointer<Pointer>, state: State) -> sm83::Result<(State, memory::Pointer<Pointer>)> {
    match read_value_from_targetpair(&state, 2, false)?.into_concrete() {
        Some(hl) => {
            Ok((state, p.contextualize(hl)))
        },
        _ => return Err(analysis::Error::UnconstrainedRegister)
    }
}

/// Trace SP initialization
fn trace_sp_set(mut state: State) -> State {
    state.set_register(Register::S, state.get_register(Register::H));
    state.set_register(Register::P, state.get_register(Register::L));

    state
}

/// Trace high memory store
fn trace_himem_store(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> sm83::Result<State> {
    let op_ptr = p.contextualize(p.as_pointer().clone()+1);
    match mem.read_unit(&op_ptr).into_concrete() {
        Some(hi) => {
            state.set_memory(p.contextualize(0xFF00 | hi as u16), state.get_register(Register::A));

            Ok(state)
        },
        _ => return Err(analysis::Error::UnconstrainedMemory(op_ptr))
    }
}

/// Trace SP adjustment
fn trace_sp_adjust(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> State {
    let adjust : reg::Symbolic<u16> = reg::Symbolic::convert_from(mem.read_unit(&p.contextualize(p.as_pointer().clone()+1)));
    let hi_sp : reg::Symbolic<u16> = reg::Symbolic::convert_from(state.get_register(Register::S));
    let lo_sp : reg::Symbolic<u16> = reg::Symbolic::convert_from(state.get_register(Register::P));
    let new_sp = (hi_sp << 8 | lo_sp) + adjust;

    let hi_new_sp : reg::Symbolic<u8> = reg::Symbolic::try_convert_from(new_sp >> 8 as u8).ok().expect("Edit:");
    let lo_new_sp : reg::Symbolic<u8> = reg::Symbolic::try_convert_from(new_sp & reg::Symbolic::new(0xFF as u16)).ok().expect("Downvotes, really?");

    state.set_register(Register::S, hi_new_sp);
    state.set_register(Register::P, lo_new_sp);

    state
}

/// Trace high memory load
fn trace_himem_load(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> sm83::Result<State> {
    let op_ptr = p.contextualize(p.as_pointer().clone()+1);
    match mem.read_unit(&op_ptr).into_concrete() {
        Some(hi) => {
            let mv = state.get_memory(p.contextualize(0xFF00 | hi as u16), mem);
            state.set_register(Register::A, mv);

            Ok(state)
        },
        _ => return Err(analysis::Error::UnconstrainedMemory(op_ptr))
    }
}

/// Trace SP offset calculation
fn trace_sp_offset_calc(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> sm83::Result<State> {
    let op_ptr = p.clone() + 1;
    let r8 : reg::Symbolic<u16> = reg::Symbolic::convert_from(mem.read_unit(&op_ptr));
    let sign = match (r8 & reg::Symbolic::new(0x0080 as u16)).into_concrete() {
        Some(0x80) => reg::Symbolic::new(0xFF80 as u16),
        Some(0x00) => reg::Symbolic::new(0x0000 as u16),
        _ => reg::Symbolic::from_cares(0 as u16, 0x007F as u16)
    };
    let offset = sign | r8;
    let s : reg::Symbolic<u16> = reg::Symbolic::convert_from(state.get_register(Register::S));
    let p : reg::Symbolic<u16> = reg::Symbolic::convert_from(state.get_register(Register::P));
    let sp = (s << 8) | p;
    let hl = sp + offset;
    let h : reg::Symbolic<u8> = reg::Symbolic::try_convert_from(hl >> 8).map_err(|_| analysis::Error::BlockSizeOverflow)?;
    let l : reg::Symbolic<u8> = reg::Symbolic::try_convert_from(hl & reg::Symbolic::new(0xFF as u16)).map_err(|_| analysis::Error::BlockSizeOverflow)?;

    state.set_register(Register::H, h);
    state.set_register(Register::L, l);

    Ok(state)
}

/// Trace himem indirect store
fn trace_himem_indir_store(p: &memory::Pointer<Pointer>, mut state: State) -> sm83::Result<State> {
    match state.get_register(Register::C).into_concrete() {
        Some(c) => {
            let op_ptr = 0xFF00 | c as u16;
            state.set_memory(p.contextualize(op_ptr), state.get_register(Register::A));
            Ok(state)
        },
        None => Err(analysis::Error::UnconstrainedRegister)
    }
}

/// Trace writing the accumulator to a memory location
fn trace_mem_store(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> sm83::Result<State> {
    let op_ptr = p.contextualize(p.as_pointer().clone()+1);
    let memloc = mem.read_leword::<Pointer>(&op_ptr).into_concrete().ok_or(analysis::Error::UnconstrainedMemory(op_ptr))?;

    state.set_memory(p.contextualize(memloc), state.get_register(Register::A));

    Ok(state)
}

/// Trace himem indirect load
fn trace_himem_indir_load(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> sm83::Result<State> {
    match state.get_register(Register::C).into_concrete() {
        Some(c) => {
            let op_ptr = 0xFF00 | c as u16;
            state.set_register(Register::A, state.get_memory(p.contextualize(op_ptr), mem));
            Ok(state)
        },
        None => Err(analysis::Error::UnconstrainedRegister)
    }
}

/// Trace loading the accumulator from a memory location
fn trace_mem_load(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> sm83::Result<State> {
    let op_ptr = p.contextualize(p.as_pointer().clone()+1);
    let memloc = mem.read_leword::<Pointer>(&op_ptr).into_concrete().ok_or(analysis::Error::UnconstrainedMemory(op_ptr))?;

    state.set_register(Register::A, state.get_memory(p.contextualize(memloc), mem));

    Ok(state)
}

fn trace_regpair_set(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, targetpair: u8) -> sm83::Result<State> {
    let value : reg::Symbolic<Pointer> = mem.read_leword(&(p.clone() + 1));

    write_value_to_targetpair(&mut state, targetpair, false, value)?;

    Ok(state)
}

fn trace_wide_add(mut state: State, targetpair: u8) -> sm83::Result<State> {
    let hl = read_value_from_targetpair(&state, 2, false)?;
    let other_val = read_value_from_targetpair(&state, targetpair, false)?;

    let new_hl = hl + other_val;

    write_value_to_targetpair(&mut state, 2, false, new_hl)?;

    Ok(state)
}

fn trace_targetmem_store(p: &memory::Pointer<Pointer>, mut state: State, targetmem: u8) -> sm83::Result<State> {
    let ptr = match targetmem {
        2 => {
            let ptr = read_value_from_targetpair(&state, 2, false)?;
            let writeback = ptr + reg::Symbolic::new(1);

            write_value_to_targetpair(&mut state, 2, false, writeback)?;
            ptr
        },
        3 => {
            let ptr = read_value_from_targetpair(&state, 2, false)?;
            let writeback = ptr - reg::Symbolic::new(1);

            write_value_to_targetpair(&mut state, 2, false, writeback)?;
            ptr
        },
        _ => read_value_from_targetpair(&state, targetmem, false)?
    };

    let a = state.get_register(Register::A);
    let hl = match ptr.into_concrete() {
        Some(ptr) => p.contextualize(ptr),
        None => return Err(analysis::Error::UnconstrainedRegister)
    };

    state.set_memory(hl, a);

    match targetmem {
        2 => {
            let writeback = ptr + reg::Symbolic::new(1);
            write_value_to_targetpair(&mut state, 2, false, writeback)?;
        },
        3 => {
            let writeback = ptr - reg::Symbolic::new(1);
            write_value_to_targetpair(&mut state, 2, false, writeback)?;
        },
        _ => {}
    };

    Ok(state)
}

fn trace_targetmem_load(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, targetmem: u8) -> sm83::Result<State> {
    let ptr = match targetmem {
        2 => {
            let ptr = read_value_from_targetpair(&state, 2, false)?;
            let writeback = ptr + reg::Symbolic::new(1);

            write_value_to_targetpair(&mut state, 2, false, writeback)?;
            ptr
        },
        3 => {
            let ptr = read_value_from_targetpair(&state, 2, false)?;
            let writeback = ptr - reg::Symbolic::new(1);

            write_value_to_targetpair(&mut state, 2, false, writeback)?;
            ptr
        },
        _ => read_value_from_targetpair(&state, targetmem, false)?
    };
    
    let hl = match ptr.into_concrete() {
        Some(ptr) => p.contextualize(ptr),
        None => return Err(analysis::Error::UnconstrainedRegister)
    };
    let a = state.get_memory(hl, mem);

    state.set_register(Register::A, a);

    match targetmem {
        2 => {
            let writeback = ptr + reg::Symbolic::new(1);
            write_value_to_targetpair(&mut state, 2, false, writeback)?;
        },
        3 => {
            let writeback = ptr - reg::Symbolic::new(1);
            write_value_to_targetpair(&mut state, 2, false, writeback)?;
        },
        _ => {}
    };

    Ok(state)
}

fn trace_targetpair_inc(mut state: State, targetpair: u8) -> sm83::Result<State> {
    let pairval = read_value_from_targetpair(&state, targetpair, false)?;
    let newval = pairval + reg::Symbolic::new(1);
    write_value_to_targetpair(&mut state, targetpair, false, newval)?;

    Ok(state)
}

fn trace_targetpair_dec(mut state: State, targetpair: u8) -> sm83::Result<State> {
    let pairval = read_value_from_targetpair(&state, targetpair, false)?;
    let newval = pairval - reg::Symbolic::new(1);
    write_value_to_targetpair(&mut state, targetpair, false, newval)?;

    Ok(state)
}

fn trace_targetreg_inc(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, targetreg: u8) -> sm83::Result<State> {
    let pairval = read_value_from_targetreg(p, mem, &state, targetreg)?;
    let newval = pairval + reg::Symbolic::new(1);
    write_value_to_targetreg(p, mem, &mut state, targetreg, newval)?;

    Ok(state)
}

fn trace_targetreg_dec(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, targetreg: u8) -> sm83::Result<State> {
    let pairval = read_value_from_targetreg(p, mem, &state, targetreg)?;
    let newval = pairval - reg::Symbolic::new(1);
    write_value_to_targetreg(p, mem, &mut state, targetreg, newval)?;

    Ok(state)
}

fn trace_targetreg_set(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, targetreg: u8) -> sm83::Result<State> {
    let op_ptr = p.clone() + 1;
    let operand = state.get_memory(op_ptr, mem);
    write_value_to_targetreg(p, mem, &mut state, targetreg, operand)?;

    Ok(state)
}

fn trace_targetreg_copy(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, targetreg_src: u8, targetreg_dst: u8) -> sm83::Result<State> {
    let value = read_value_from_targetreg(p, mem, &state, targetreg_src)?;
    write_value_to_targetreg(p, mem, &mut state, targetreg_dst, value)?;

    Ok(state)
}

fn trace_stackpair_push(p: &memory::Pointer<Pointer>, mut state: State, stackpair: u8) -> sm83::Result<State> {
    let value = read_value_from_targetpair(&state, stackpair, true)?;
    push_value_to_sp(p, &mut state, value)?;

    Ok(state)
}

fn trace_stackpair_pop(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, stackpair: u8) -> sm83::Result<State> {
    let value = pop_value_from_sp(p, mem, &mut state)?;
    write_value_to_targetpair(&mut state, stackpair, true, value)?;

    Ok(state)
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
pub fn trace(p: &memory::Pointer<Pointer>, mem: &Bus, state: State) -> sm83::Result<(State, memory::Pointer<Pointer>)> {
    match mem.read_unit(p).into_concrete() {
        Some(0xCB) => {
            match mem.read_unit(&(p.clone()+1)).into_concrete() {
                Some(subop) => {
                    let targetreg = subop & 0x07;
                    let new_bitop = (subop >> 3) & 0x07;

                    match ((subop >> 6) & 0x03, (subop >> 3) & 0x07, subop & 0x07) {
                        (0, _, _) => Ok((trace_bitop(p, mem, state, new_bitop, targetreg)?, (p.clone()+2))),
                        (1, bit, _) => Ok((trace_bittest(p, mem, state, bit, targetreg)?, (p.clone()+2))),
                        (2, bit, _) => Ok((trace_bitreset(p, mem, state, bit, targetreg)?, (p.clone()+2))),
                        (3, bit, _) => Ok((trace_bitset(p, mem, state, bit, targetreg)?, (p.clone()+2))),
                        _ => Err(analysis::Error::Misinterpretation(2, false))
                    }
                },
                _ => Err(analysis::Error::UnconstrainedMemory(p.clone()+1))
            }
        }

        //Z80 instructions that don't fit the pattern decoder below
        Some(0x00) => Ok((state, p.clone()+1)), //nop
        Some(0x08) => Ok((trace_sp_storage(p, mem, state)?, p.clone()+3)), //ld [u16], SP
        Some(0x10) => Ok((state, p.clone()+1)), //stop
        Some(0x18) => trace_jump_relative(None, p, mem, state), //jr u8
        Some(0x76) => Ok((state, p.clone()+1)), //halt

        Some(0xC3) => trace_jump(None, p, mem, state), //jp u16
        Some(0xCD) => trace_call(None, p, mem, state),

        Some(0xC9) => trace_return(None, p, mem, state), //ret
        Some(0xD9) => trace_return(None, p, mem, state), //reti
        Some(0xE9) => trace_jump_dynamic(p, state), //jp hl
        Some(0xF9) => Ok((trace_sp_set(state), p.clone()+1)), //ld sp, hl

        Some(0xE0) => Ok((trace_himem_store(p, mem, state)?, p.clone()+2)), //ldh [u8], a
        Some(0xE8) => Ok((trace_sp_adjust(p, mem, state), p.clone()+2)), //add sp, u8
        Some(0xF0) => Ok((trace_himem_load(p, mem, state)?, p.clone()+2)), //ldh a, [u8]
        Some(0xF8) => Ok((trace_sp_offset_calc(p, mem, state)?, p.clone()+2)), //ld hl, sp+u8

        Some(0xE2) => Ok((trace_himem_indir_store(p, state)?, p.clone()+1)), //ld [c], a
        Some(0xEA) => Ok((trace_mem_store(p, mem, state)?, p.clone()+3)), //ld [u16], a
        Some(0xF2) => Ok((trace_himem_indir_load(p, mem, state)?, p.clone()+1)), //ld a, [c]
        Some(0xFA) => Ok((trace_mem_load(p, mem, state)?, p.clone()+3)), //ld a, [u16]

        Some(0xF3) => Ok((state, p.clone()+1)), //di
        Some(0xFB) => Ok((state, p.clone()+1)), //ei

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
            match ((op >> 6) & 0x03, (op >> 5) & 0x01, (op >> 3) & 0x01, op & 0x07) {
                (0, 0, _, 0) => Err(analysis::Error::Misinterpretation(1, false)), /* 00, 08, 10, 18 */
                (0, 1, _, 0) => trace_jump_relative(Some(condcode), p, mem, state), //jr cond, u8
                (0, _, 0, 1) => Ok((trace_regpair_set(p, mem, state, targetpair)?, p.clone()+3)), //ld targetpair, u16
                (0, _, 1, 1) => Ok((trace_wide_add(state, targetpair)?, p.clone()+1)), //add hl, targetpair
                (0, _, 0, 2) => Ok((trace_targetmem_store(p, state, targetmem)?, p.clone()+1)), //ld [targetmem], a
                (0, _, 1, 2) => Ok((trace_targetmem_load(p, mem, state, targetmem)?, p.clone()+1)), //ld a, [targetmem]
                (0, _, 0, 3) => Ok((trace_targetpair_inc(state, targetpair)?, p.clone()+1)), //inc targetpair
                (0, _, 1, 3) => Ok((trace_targetpair_dec(state, targetpair)?, p.clone()+1)), //dec targetpair
                (0, _, _, 4) => Ok((trace_targetreg_inc(p, mem, state, targetreg)?, p.clone()+1)), //inc targetreg
                (0, _, _, 5) => Ok((trace_targetreg_dec(p, mem, state, targetreg)?, p.clone()+1)), //dec targetreg
                (0, _, _, 6) => Ok((trace_targetreg_set(p, mem, state, targetreg)?, p.clone()+2)), //ld targetreg, u8
                (0, _, _, 7) => Err(analysis::Error::NotYetImplemented), //old bitops
                (1, _, _, _) => Ok((trace_targetreg_copy(p, mem, state, targetreg, targetreg2)?, p.clone()+1)), //ld targetreg2, targetreg
                (2, _, _, _) => Err(analysis::Error::NotYetImplemented), //(aluop) a, targetreg2
                (3, 0, _, 0) => trace_return(Some(condcode), p, mem, state), //ret cond
                (3, 1, _, 0) => Err(analysis::Error::Misinterpretation(1, false)), /* E0, E8, F0, F8 */
                (3, _, 0, 1) => Ok((trace_stackpair_pop(p, mem, state, stackpair)?, p.clone()+1)), //pop stackpair
                (3, _, 1, 1) => Err(analysis::Error::Misinterpretation(1, false)), /* C9, D9, E9, F9 */
                (3, 0, _, 2) => trace_jump(Some(condcode), p, mem, state), //jp cond, u16
                (3, 1, _, 2) => Err(analysis::Error::Misinterpretation(1, false)), /* E2, EA, F2, FA */
                (3, _, _, 3) => Err(analysis::Error::InvalidInstruction),
                (3, 0, _, 4) => trace_call(Some(condcode), p, mem, state), //call cond, u16
                (3, 1, _, 4) => Err(analysis::Error::InvalidInstruction),
                (3, _, 0, 5) => Ok((trace_stackpair_push(p, state, stackpair)?, p.clone()+1)), //push stackpair
                (3, _, 1, 5) => Err(analysis::Error::InvalidInstruction),
                (3, _, _, 6) => Err(analysis::Error::NotYetImplemented), //(aluop) a, u8
                (3, _, _, 7) => Err(analysis::Error::NotYetImplemented), //rst op& 0x38

                _ => Err(analysis::Error::InvalidInstruction),
            }
        },
        _ => Err(analysis::Error::UnconstrainedMemory(p.clone()))
    }
}