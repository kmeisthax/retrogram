//! Facilities for tracing LR35902 code

use crate::{memory, reg};
use crate::reg::{Convertable, TryConvertable};
use crate::arch::lr35902::{Register, Pointer, Bus, State, Value};

/// Given a targetreg operand, produce a symbolic value of what that target reg
/// would be given the current state.
fn read_value_from_targetreg(p: &memory::Pointer<Pointer>, mem: &Bus, state: &State, targetreg: u8) -> reg::Symbolic<Value> {
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
fn write_value_to_targetreg(p: &memory::Pointer<Pointer>, mem: &Bus, state: &mut State, targetreg: u8, value: reg::Symbolic<Value>) {
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
fn zero_flag(val: reg::Symbolic<Value>) -> reg::Symbolic<Value> {
    match (val.into_concrete(), val.is_valid(0)) {
        (Some(0), _) => reg::Symbolic::from(0x80 as Value),
        (Some(_), _) => reg::Symbolic::from(0 as Value),
        (None, true) => reg::Symbolic::from_cares(0 as Value, 0x7F as Value),
        (None, false) => reg::Symbolic::from(0 as Value),
    }
}

/// Trace a CB-prefix bit rotate operation (RLC, RRC, RL, RR, SLA, SRA, SWAP,
/// or SRL).
fn trace_bitop(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, bitop: u8, targetreg: u8) -> State {
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
fn trace_bittest(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, targetbit: u8, targetreg: u8) -> State {
    let flags = state.get_register(Register::F);
    let val = read_value_from_targetreg(p, mem, &state, targetreg) >> targetbit & reg::Symbolic::from(0x01);

    state.set_register(Register::F, flags & reg::Symbolic::from(0x10) | reg::Symbolic::from(0x20) | zero_flag(val));

    state
}

/// Trace a CB-prefix bit reset instruction (e.g. RES n, reg).
fn trace_bitreset(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, targetbit: u8, targetreg: u8) -> State {
    let mask = reg::Symbolic::from(!(1 << targetbit));
    let val = read_value_from_targetreg(p, mem, &state, targetreg);

    write_value_to_targetreg(p, mem, &mut state, targetreg, val & mask);

    state
}

/// Trace a CB-prefix bit set instruction (e.g. SET n, reg).
fn trace_bitset(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State, targetbit: u8, targetreg: u8) -> State {
    let bit = reg::Symbolic::from(1 << targetbit);
    let val = read_value_from_targetreg(p, mem, &state, targetreg);

    write_value_to_targetreg(p, mem, &mut state, targetreg, val | bit);

    state
}

/// Trace writing the stack pointer to a memory location
fn trace_sp_storage(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> Option<State> {
    let memloc = mem.read_leword::<Pointer>(&p.contextualize(p.as_pointer().clone()+1)).into_concrete()?;

    state.set_memory(p.contextualize(memloc), state.get_register(Register::P));
    state.set_memory(p.contextualize(memloc+1), state.get_register(Register::S));

    Some(state)
}

/// Trace pc-relative jumps
fn trace_pcrel_jump(p: &memory::Pointer<Pointer>, mem: &Bus) -> Option<memory::Pointer<Pointer>> {
    let offset = mem.read_unit(&p.contextualize(p.as_pointer().clone()+1)).into_concrete()?;
    let target = ((p.as_pointer() + 1) as i16 + (offset as i8) as i16) as u16;

    Some(p.contextualize(target))
}

/// Trace full jumps
fn trace_jump(p: &memory::Pointer<Pointer>, mem: &Bus) -> Option<memory::Pointer<Pointer>> {
    let target = mem.read_leword::<u16>(&p.contextualize(p.as_pointer().clone()+1)).into_concrete()?;

    Some(p.contextualize(target))
}

/// Trace full calls
fn trace_call(ptr: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> Option<(State, memory::Pointer<Pointer>)> {
    let target = mem.read_leword::<u16>(&ptr.contextualize(ptr.as_pointer().clone()+1)).into_concrete()?;

    match (state.get_register(Register::S).into_concrete(), state.get_register(Register::P).into_concrete()) {
        (Some(s), Some(p)) => {
            let sp = (s as u16) << 8 | p as u16;
            let ret_pc = ptr.as_pointer().clone() + 3;

            state.set_memory(ptr.contextualize(sp), reg::Symbolic::from(ret_pc as u8));
            state.set_memory(ptr.contextualize(sp + 1), reg::Symbolic::from((ret_pc >> 8) as u8));

            let next_sp = sp - 2;

            state.set_register(Register::S, reg::Symbolic::from((next_sp >> 8) as u8));
            state.set_register(Register::P, reg::Symbolic::from((next_sp & 0xFF) as u8));
        },
        _ => return None
    };

    Some((state, ptr.contextualize(target)))
}

/// Trace return
fn trace_return(ptr: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> Option<(State, memory::Pointer<Pointer>)> {
    match (state.get_register(Register::S).into_concrete(), state.get_register(Register::P).into_concrete()) {
        (Some(s), Some(p)) => {
            let sp = (s as u16) << 8 | p as u16;

            match (state.get_memory(ptr.contextualize(sp)).into_concrete(), state.get_memory(ptr.contextualize(sp + 1)).into_concrete()) {
                (Some(lo), Some(hi)) => {
                    let next_sp = sp + 2;

                    state.set_register(Register::S, reg::Symbolic::from((next_sp >> 8) as u8));
                    state.set_register(Register::P, reg::Symbolic::from((next_sp & 0xFF) as u8));

                    let next_pc = (hi as u16) << 8 | (lo as u16);

                    Some((state, ptr.contextualize(next_pc)))
                },
                _ => None
            }
        },
        _ => None
    }
}

/// Trace dynamic jumps
fn trace_jump_dynamic(p: &memory::Pointer<Pointer>, mem: &Bus, state: &State) -> Option<memory::Pointer<Pointer>> {
    match (state.get_register(Register::H).into_concrete(), state.get_register(Register::L).into_concrete()) {
        (Some(h), Some(l)) => {
            let hl = (h as u16) << 8 | l as u16;
            Some(p.contextualize(hl))
        },
        _ => None
    }
}

/// Trace SP initialization
fn trace_sp_set(ptr: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> State {
    state.set_register(Register::S, state.get_register(Register::H));
    state.set_register(Register::P, state.get_register(Register::L));

    state
}

/// Trace high memory store
fn trace_himem_store(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> Option<State> {
    match mem.read_unit(&p.contextualize(p.as_pointer().clone()+1)).into_concrete() {
        Some(hi) => {
            state.set_memory(p.contextualize(0xFF00 | hi as u16), state.get_register(Register::A));

            Some(state)
        },
        _ => None
    }
}

/// Trace SP adjustment
fn trace_sp_adjust(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> State {
    let adjust : reg::Symbolic<u16> = reg::Symbolic::convert_from(mem.read_unit(&p.contextualize(p.as_pointer().clone()+1)));
    let hi_sp : reg::Symbolic<u16> = reg::Symbolic::convert_from(state.get_register(Register::S));
    let lo_sp : reg::Symbolic<u16> = reg::Symbolic::convert_from(state.get_register(Register::P));
    let new_sp = (hi_sp << 8 | lo_sp) + adjust;

    let hi_new_sp : reg::Symbolic<u8> = reg::Symbolic::try_convert_from(new_sp >> 8 as u8).ok().expect("Edit:");
    let lo_new_sp : reg::Symbolic<u8> = reg::Symbolic::try_convert_from(new_sp & reg::Symbolic::from(0xFF as u16)).ok().expect("Downvotes, really?");

    state.set_register(Register::S, hi_new_sp);
    state.set_register(Register::P, lo_new_sp);

    state
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
pub fn trace(p: &memory::Pointer<Pointer>, mem: &Bus, mut state: State) -> Option<(State, memory::Pointer<Pointer>)> {
    match mem.read_unit(p).into_concrete() {
        Some(0xCB) => {
            //TODO: CB prefix
            match mem.read_unit(&(p.clone()+1)).into_concrete() {
                Some(subop) => {
                    let targetreg = subop & 0x07;
                    let new_bitop = (subop >> 3) & 0x07;

                    match ((subop >> 6) & 0x03, (subop >> 3) & 0x07, subop & 0x07) {
                        (0, _, _) => Some((trace_bitop(p, mem, state, new_bitop, targetreg), (p.clone()+2))),
                        (1, bit, _) => Some((trace_bittest(p, mem, state, bit, targetreg), (p.clone()+2))),
                        (2, bit, _) => Some((trace_bitreset(p, mem, state, bit, targetreg), (p.clone()+2))),
                        (3, bit, _) => Some((trace_bitset(p, mem, state, bit, targetreg), (p.clone()+2))),
                        _ => None
                    }
                },
                _ => None
            }
        }

        //Z80 instructions that don't fit the pattern decoder below
        Some(0x00) => Some((state, p.clone()+1)), //nop
        Some(0x08) => Some((trace_sp_storage(p, mem, state)?, p.clone()+3)), //ld [u16], SP
        Some(0x10) => Some((state, p.clone()+1)), //stop
        Some(0x18) => Some((state, trace_pcrel_jump(p, mem)?)), //jr u8
        Some(0x76) => Some((state, p.clone()+1)), //halt

        Some(0xC3) => Some((state, trace_jump(p, mem)?)), //jp u16
        Some(0xCD) => trace_call(p, mem, state),

        Some(0xC9) => trace_return(p, mem, state), //ret
        Some(0xD9) => trace_return(p, mem, state), //reti
        Some(0xE9) => {
            let target = trace_jump_dynamic(p, mem, &state)?;
            Some((state, target))
        }, //jp hl
        Some(0xF9) => Some((trace_sp_set(p, mem, state), p.clone()+1)), //ld sp, hl

        Some(0xE0) => Some((trace_himem_store(p, mem, state)?, p.clone()+1)), //ldh [u8], a
        Some(0xE8) => Some((trace_sp_adjust(p, mem, state), p.clone()+1)), //add sp, u8
        Some(0xF0) => None, //(Some(inst::new("ldh", vec![op::sym("a"), op::indir(hram_op8(&(p.clone()+1), mem))])), 2, true, true, vec![]),
        Some(0xF8) => None, //(Some(inst::new("ld", vec![op::sym("hl"), op::add(op::sym("sp"), int_op8(&(p.clone()+1), mem))])), 2, true, true, vec![]),

        Some(0xE2) => None, //(Some(inst::new("ld", vec![op::indir(op::sym("c")), op::sym("a")])), 1, true, true, vec![]),
        Some(0xEA) => None, //(Some(inst::new("ld", vec![op::indir(dptr_op16(&(p.clone()+1), mem)), op::sym("a")])), 3, true, true, vec![]),
        Some(0xF2) => None, //(Some(inst::new("ld", vec![op::sym("a"), op::indir(op::sym("c"))])), 1, true, true, vec![]),
        Some(0xFA) => None, //(Some(inst::new("ld", vec![op::sym("a"), op::indir(dptr_op16(&(p.clone()+1), mem))])), 3, true, true, vec![]),

        Some(0xF3) => None, //(Some(inst::new("di", vec![])), 1, true, true, vec![]),
        Some(0xFB) => None, //(Some(inst::new("ei", vec![])), 1, true, true, vec![]),

        //Z80 instructions that follow a particular pattern
        Some(op) => {/*
            let condcode = ALU_CONDCODE[((op >> 3) & 0x03) as usize];
            let targetpair = ALU_TARGET_PAIRS[((op >> 4) & 0x03) as usize];
            let targetreg = ALU_TARGET_REGS[((op >> 3) & 0x07) as usize].clone();
            let targetmem = ALU_TARGET_MEM[((op >> 4) & 0x03) as usize];
            let bitop = ALU_BITOPS[((op >> 3) & 0x07) as usize];
            let targetreg2 = ALU_TARGET_REGS[(op & 0x07) as usize].clone();
            let aluop = ALU_OPS[((op >> 3) & 0x07) as usize];
            let stackpair = STACK_TARGET_PAIRS[((op >> 4) & 0x03) as usize];*/

            //decode `op` into aab?cddd. This creates a nice visual table for
            //the Z80's semiperiodic instruction encoding
            match ((op >> 6) & 0x03, (op >> 5) & 0x01, (op >> 3) & 0x01, op & 0x07) {
                (0, 0, _, 0) => panic!("Instruction shouldn't be decoded here"), /* 00, 08, 10, 18 */
                (0, 1, _, 0) => None, //(Some(inst::new("jr", vec![op::sym(condcode), pcrel_op8(&(p.clone()+1), mem)])), 2, true, false, vec![pcrel_target(&(p.clone()+1), mem, analysis::ReferenceKind::Code)]),
                (0, _, 0, 1) => None, //(Some(inst::new("ld", vec![op::sym(targetpair), int_op16(&(p.clone()+1), mem)])), 3, true, true, vec![]),
                (0, _, 1, 1) => None, //(Some(inst::new("add", vec![op::sym("hl"), op::sym(targetpair)])), 1, true, true, vec![]),
                (0, _, 0, 2) => None, //(Some(inst::new("ld", vec![op::indir(op::sym(targetmem)), op::sym("a")])), 1, true, true, vec![]),
                (0, _, 1, 2) => None, //(Some(inst::new("ld", vec![op::sym("a"), op::indir(op::sym(targetmem))])), 1, true, true, vec![]),
                (0, _, 0, 3) => None, //(Some(inst::new("inc", vec![op::sym(targetpair)])), 1, true, true, vec![]),
                (0, _, 1, 3) => None, //(Some(inst::new("dec", vec![op::sym(targetpair)])), 1, true, true, vec![]),
                (0, _, _, 4) => None, //(Some(inst::new("inc", vec![targetreg])), 1, true, true, vec![]),
                (0, _, _, 5) => None, //(Some(inst::new("dec", vec![targetreg])), 1, true, true, vec![]),
                (0, _, _, 6) => None, //(Some(inst::new("ld", vec![targetreg, int_op8(&(p.clone()+1), mem)])), 2, true, true, vec![]),
                (0, _, _, 7) => None, //(Some(inst::new(bitop, vec![])), 1, true, true, vec![]),
                (1, _, _, _) => None, //(Some(inst::new("ld", vec![targetreg2, targetreg])), 1, true, true, vec![]),
                (2, _, _, _) => None, //(Some(inst::new(aluop, vec![op::sym("a"), targetreg2])), 1, true, true, vec![]),
                (3, 0, _, 0) => None, //(Some(inst::new("ret", vec![op::sym(condcode)])), 1, true, false, vec![]),
                (3, 1, _, 0) => panic!("Instruction shouldn't be decoded here"), /* E0, E8, F0, F8 */
                (3, _, 0, 1) => None, //(Some(inst::new("pop", vec![op::sym(stackpair)])), 1, true, true, vec![]),
                (3, _, 1, 1) => panic!("Instruction shouldn't be decoded here"), /* C9, D9, E9, F9 */
                (3, 0, _, 2) => None, //(Some(inst::new("jp", vec![op::sym(condcode), cptr_op16(&(p.clone()+1), mem)])), 3, true, false, vec![cptr_target(&(p.clone()+1), mem, analysis::ReferenceKind::Code)]),
                (3, 1, _, 2) => panic!("Instruction shouldn't be decoded here"), /* E2, EA, F2, FA */
                (3, _, _, 3) => None, //(None, 0, false, true, vec![]),
                (3, 0, _, 4) => None, //(Some(inst::new("call", vec![op::sym(condcode), cptr_op16(&(p.clone()+1), mem)])), 3, true, true, vec![cptr_target(&(p.clone()+1), mem, analysis::ReferenceKind::Subroutine)]),
                (3, 1, _, 4) => None, //(None, 0, false, true, vec![]),
                (3, _, 0, 5) => None, //(Some(inst::new("push", vec![op::sym(stackpair)])), 1, true, true, vec![]),
                (3, _, 1, 5) => None, //(None, 0, false, true, vec![]),
                (3, _, _, 6) => None, //(Some(inst::new(aluop, vec![op::sym("a"), int_op8(&(p.clone()+1), mem)])), 2, true, true, vec![]),
                (3, _, _, 7) => None, //(Some(inst::new("rst", vec![op::cptr(op & 0x38)])), 1, true, true, vec![analysis::Reference::new_static_ref(p.clone(), p.contextualize((op & 0x38) as u16), analysis::ReferenceKind::Subroutine)]),

                _ => None, //(None, 0, false, true, vec![])
            }
        },
        _ => None, //(None, 0, false, true, vec![])
    }
}