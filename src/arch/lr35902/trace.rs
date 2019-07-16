//! Facilities for tracing LR35902 code

use crate::{memory, reg};
use crate::reg::Convertable;
use crate::arch::lr35902::{Register, Pointer, Bus, State, Value};

/// Given a targetreg operand, produce a symbolic value of what that target reg
/// would be given the current state.
pub fn read_value_from_targetreg(p: &memory::Pointer<Pointer>, mem: &Bus, state: &State, targetreg: u8) -> reg::Symbolic<Value> {
    match targetreg {
        0 => state.get_register(Register::B),
        1 => state.get_register(Register::C),
        2 => state.get_register(Register::D),
        3 => state.get_register(Register::E),
        4 => state.get_register(Register::H),
        5 => state.get_register(Register::L),
        6 => {
            let hl = reg::Symbolic::<Pointer>::convert_from(state.get_register(Register::H)) << 8 | reg::Symbolic::<Pointer>::convert_from(state.get_register(Register::L));
            if let Some(hl) = hl.into_concrete() {
                let ptr_hl = mem.minimize_context(p.contextualize(hl)); //TODO: Attempt to pull context from state
                mem.read_unit(&ptr_hl) //TODO: Should respond to memory state
            } else {
                reg::Symbolic::default()
            }
        },
        7 => state.get_register(Register::A),
        _ => panic!("Not a valid target register to read from")
    }
}

/// Given a targetreg operand, manipulate the given state to incorporate the
/// effect of writing a given value to that operand.
pub fn write_value_to_targetreg(p: &memory::Pointer<Pointer>, mem: &Bus, state: &mut State, targetreg: u8, value: reg::Symbolic<Value>) {
    match targetreg {
        0 => state.set_register(Register::B, value),
        1 => state.set_register(Register::C, value),
        2 => state.set_register(Register::D, value),
        3 => state.set_register(Register::E, value),
        4 => state.set_register(Register::H, value),
        5 => state.set_register(Register::L, value),
        6 => {
            let hl = reg::Symbolic::<Pointer>::convert_from(state.get_register(Register::H)) << 8 | reg::Symbolic::<Pointer>::convert_from(state.get_register(Register::L));
            if let Some(hl) = hl.into_concrete() {
                let ptr_hl = mem.minimize_context(p.contextualize(hl)); //TODO: Attempt to pull context from state
                state.set_memory(ptr_hl, value);
            }
        },
        7 => state.set_register(Register::A, value),
        _ => panic!("Not a valid target register to write to")
    }
}

/// Given a symbolic value, compute the zero flag that it would generate in the
/// F register if it were the result of a computation.
/// 
/// The resulting value is suitable for use as a new flags register with the
/// format of Z000. You may OR in additional bits as necessary.
pub fn zero_flag(val: reg::Symbolic<Value>) -> reg::Symbolic<Value> {
    match (val.into_concrete(), val.is_valid(0)) {
        (Some(0), _) => reg::Symbolic::from(0x80 as Value),
        (Some(_), _) => reg::Symbolic::from(0 as Value),
        (None, true) => reg::Symbolic::from_cares(0 as Value, 0x7F as Value),
        (None, false) => reg::Symbolic::from(0 as Value),
    }
}

/// Trace a CB-prefix bit rotate operation (RLC, RRC, RL, RR, SLA, SRA, SWAP,
/// or SRL).
pub fn trace_bitop(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, bitop: u8, targetreg: u8) -> State {
    let flags = state.get_register(Register::F);
    let carry = flags & reg::Symbolic::from(0x10);
    let val = read_value_from_targetreg(p, mem, &state, targetreg);

    let (newval, newcarry) = match bitop {
        0 => (val << 1 | val >> 7, carry), //RLC
        1 => (val << 1 | val >> 7, carry), //RRC
        2 => (val << 1 | carry >> 4, val >> 7), //RL
        3 => (val >> 1 | carry << 3, val & reg::Symbolic::from(0x01)), //RR
        4 => (val << 1, val >> 7), //SLA
        //This is a manual sign extension since we defined Value as unsigned
        5 => (val >> 1 | val & reg::Symbolic::from(0x80), val & reg::Symbolic::from(0x01)), //SRA
        6 => (val >> 4 | val << 4, reg::Symbolic::from(0)), //SWAP
        7 => (val >> 1, val & reg::Symbolic::from(0x01)), //SRL
        _ => panic!("Invalid bit operation!")
    };
    
    state.set_register(Register::F, newcarry << 4 | zero_flag(val)); //N and H flags are always zero
    write_value_to_targetreg(p, mem, &mut state, targetreg, newval);

    state
}

/// Trace a CB-prefix bit test instruction (e.g. BIT n, reg).
pub fn trace_bittest(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, targetbit: u8, targetreg: u8) -> State {
    let flags = state.get_register(Register::F);
    let val = read_value_from_targetreg(p, mem, &state, targetreg) >> targetbit & reg::Symbolic::from(0x01);

    state.set_register(Register::F, flags & reg::Symbolic::from(0x10) | reg::Symbolic::from(0x20) | zero_flag(val));

    state
}

/// Trace a CB-prefix bit reset instruction (e.g. RES n, reg).
pub fn trace_bitreset(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, targetbit: u8, targetreg: u8) -> State {
    let mask = reg::Symbolic::from(!(1 << targetbit));
    let val = read_value_from_targetreg(p, mem, &state, targetreg);

    write_value_to_targetreg(p, mem, &mut state, targetreg, val & mask);

    state
}

/// Trace a CB-prefix bit set instruction (e.g. SET n, reg).
pub fn trace_bitset(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, targetbit: u8, targetreg: u8) -> State {
    let bit = reg::Symbolic::from(1 << targetbit);
    let val = read_value_from_targetreg(p, mem, &state, targetreg);

    write_value_to_targetreg(p, mem, &mut state, targetreg, val | bit);

    state
}

pub fn trace(_p: &memory::Pointer<Pointer>, _mem: &Bus, _state: State) -> () {

}