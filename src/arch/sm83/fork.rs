//! Symbolic fork analysis for SM83

use crate::memory;
use crate::ast::Operand as op;
use crate::arch::sm83::{Register, Pointer, Bus, State};
use crate::arch::sm83::dis::{ALU_TARGET_REGS, ALU_TARGET_MEM};

fn memlist_op16(regs: Vec<Register>, p: &memory::Pointer<Pointer>, mem: &Bus) -> (Vec<Register>, Vec<memory::Pointer<Pointer>>, bool) {
    if let Some(val) = mem.read_leword::<u16>(p).into_concrete() {
        return (regs, vec![mem.minimize_context(p.contextualize(val))], true);
    }

    (regs, vec![p.clone(), p.clone()+1], false)
}

fn memlist_hi8(regs: Vec<Register>, p: &memory::Pointer<Pointer>, mem: &Bus) -> (Vec<Register>, Vec<memory::Pointer<Pointer>>, bool) {
    if let Some(val) = mem.read_unit(p).into_concrete() {
        return (regs, vec![mem.minimize_context(p.contextualize(0xFF00 | val as Pointer))], true);
    }

    (regs, vec![p.clone()], false)
}

fn memlist_pc8(regs: Vec<Register>, p: &memory::Pointer<Pointer>, mem: &Bus) -> (Vec<Register>, Vec<memory::Pointer<Pointer>>, bool) {
    match mem.read_unit(p).into_concrete().map(|target| ((p.as_pointer() + 1) as i16 + (target as i8) as i16) as u16) {
        Some(val) => (regs, vec![mem.minimize_context(p.contextualize(val))], true),
        None => (regs, vec![p.clone()], false)
    }
}

fn memlist_indir16(regs: Vec<Register>, p: &memory::Pointer<Pointer>, mem: &Bus, state: &State) -> (Vec<Register>, Vec<memory::Pointer<Pointer>>, bool) {
    match (regs.get(0), regs.get(1)) {
        (Some(r0), Some(r1)) => match (state.get_register(*r0).into_concrete(), state.get_register(*r1).into_concrete()) {
            (Some(h), Some(l)) => (regs, vec![mem.minimize_context(p.contextualize((h as Pointer) << 8 | l as Pointer))], true),
            _ => (regs, vec![], false),
        },
        _ => (regs, vec![], false),
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
pub fn prereq(p: &memory::Pointer<Pointer>, mem: &Bus, state: &State) -> (Vec<Register>, Vec<memory::Pointer<Pointer>>, bool) {
    match mem.read_unit(p).into_concrete() {
        Some(0xCB) => {
            match mem.read_unit(&(p.clone()+1)).into_concrete() {
                Some(subop) => {
                    match ALU_TARGET_REGS[(subop & 0x07) as usize].clone() {
                        op::Symbol(_) => (vec![], vec![], true),
                        op::Indirect(sym) => match *sym {
                            op::Symbol(ref name) if name == "hl" => (Register::reglist_from_sym("hl"), vec![], true),
                            _ => (vec![], vec![], false)
                        },
                        _ => (vec![], vec![], false)
                    }
                },
                _ => (vec![], vec![p.clone()+1], false)
            }
        }

        //Z80 instructions that don't fit the pattern decoder below
        Some(0x00) => (vec![], vec![], true), //nop
        Some(0x08) => memlist_op16(vec![], &(p.clone()+1), mem), //ld [u16], sp
        Some(0x10) => (vec![], vec![], true), //stop
        Some(0x18) => memlist_pc8(vec![], &(p.clone()+1), mem), //jr u8
        Some(0x76) => (vec![], vec![], true), //halt

        Some(0xC3) => memlist_op16(vec![], &(p.clone()+1), mem), //jp u16
        Some(0xCD) => memlist_op16(vec![Register::S, Register::P], &(p.clone()+1), mem), //call u16

        Some(0xC9) => memlist_indir16(vec![Register::S, Register::P], p, mem, state), //ret
        Some(0xD9) => memlist_indir16(vec![Register::S, Register::P], p, mem, state), //reti
        Some(0xE9) => (vec![Register::H, Register::L], vec![], true), //jp [hl]
        Some(0xF9) => (vec![], vec![], true), //ld sp, hl

        Some(0xE0) => memlist_hi8(vec![], &(p.clone()+1), mem), //ldh [u8], a
        Some(0xE8) => (vec![], vec![], true), //add sp, reg
        Some(0xF0) => memlist_hi8(vec![], &(p.clone()+1), mem), //ldh a, [u8]
        Some(0xF8) => (vec![], vec![], true), //ld hl, sp+r8

        Some(0xE2) => (vec![Register::C], vec![], true), //ldh [c], a
        Some(0xEA) => memlist_op16(vec![], &(p.clone()+1), mem), //ld [u16], a
        Some(0xF2) => (vec![Register::C], vec![], true), //ldh a, [c]
        Some(0xFA) => memlist_op16(vec![], &(p.clone()+1), mem), //ld a, [u16]
        //TODO: Should memory reads prereq on the target of the read?

        Some(0xF3) => (vec![], vec![], true),
        Some(0xFB) => (vec![], vec![], true),

        //Z80 instructions that follow a particular pattern
        Some(op) => {
            let targetmem = ALU_TARGET_MEM[((op >> 4) & 0x03) as usize];

            //decode `op` into aab?cddd. This creates a nice visual table for
            //the Z80's semiperiodic instruction encoding
            match ((op >> 6) & 0x03, (op >> 5) & 0x01, (op >> 3) & 0x01, op & 0x07) {
                (0, 0, _, 0) => panic!("Instruction shouldn't be decoded here"), /* 00, 08, 10, 18 */
                (0, 1, _, 0) => memlist_pc8(vec![Register::F], &(p.clone()+1), mem), //jr cond, pc8
                (0, _, 0, 1) => (vec![], vec![], true), //ld r16, u16
                (0, _, 1, 1) => (vec![], vec![], true), //add hl, r16
                (0, _, 0, 2) => (Register::reglist_from_sym(targetmem), vec![], true), //ld [r16], a
                (0, _, 1, 2) => (Register::reglist_from_sym(targetmem), vec![], true), //ld a, [r16]
                (0, _, 0, 3) => (vec![], vec![], true), //inc r16
                (0, _, 1, 3) => (vec![], vec![], true), //dec r16
                (0, _, _, 4) if ((op >> 3) & 0x07) == 6 => (Register::reglist_from_sym("hl"), vec![], true), //inc [hl]
                (0, _, _, 4) => (vec![], vec![], true), //inc r8
                (0, _, _, 5) if ((op >> 3) & 0x07) == 6 => (Register::reglist_from_sym("hl"), vec![], true), //dec [hl]
                (0, _, _, 5) => (vec![], vec![], true), //dec r8
                (0, _, _, 6) if ((op >> 3) & 0x07) == 6 => (Register::reglist_from_sym("hl"), vec![], true), //ld [hl], d8
                (0, _, _, 6) => (vec![], vec![], true), //ld r8, d8
                (0, _, _, 7) => (vec![], vec![], true), //8080 bitwise ops
                (1, _, _, _) if ((op >> 3) & 0x07) == 6 => (Register::reglist_from_sym("hl"), vec![], true), //ld [hl], r8
                (1, _, _, _) if (op & 0x07) == 6 => (Register::reglist_from_sym("hl"), vec![], true), //ld r8, [hl]
                (1, _, _, _) => (vec![], vec![], true), //ld r8, r8
                (2, _, _, _) if (op & 0x07) == 6 => (Register::reglist_from_sym("hl"), vec![], true), //aluops r8, [hl]
                (2, _, _, _) => (vec![], vec![], true), //aluops a, r8
                (3, 0, _, 0) => memlist_indir16(vec![Register::S, Register::P, Register::F], p, mem, state), //ret cond... TODO: Can we specify which bit of F we want?
                (3, 1, _, 0) => panic!("Instruction shouldn't be decoded here"), /* E0, E8, F0, F8 */
                (3, _, 0, 1) => memlist_indir16(vec![Register::S, Register::P], p, mem, state), //pop r16
                (3, _, 1, 1) => panic!("Instruction shouldn't be decoded here"), /* C9, D9, E9, F9 */
                (3, 0, _, 2) => memlist_op16(vec![Register::F], &(p.clone()+1), mem), //jp cond, d16
                (3, 1, _, 2) => panic!("Instruction shouldn't be decoded here"), /* E2, EA, F2, FA */
                (3, _, _, 3) => (vec![], vec![], false), //invalid
                (3, 0, _, 4) => memlist_op16(vec![Register::F], &(p.clone()+1), mem), //call cond, d16
                (3, 1, _, 4) => (vec![], vec![], false), //invalid
                (3, _, 0, 5) => memlist_indir16(vec![Register::S, Register::P], p, mem, state), //push r16
                (3, _, 1, 5) => (vec![], vec![], false), //invalid
                (3, _, _, 6) => (vec![], vec![], true), //aluops a, d8
                (3, _, _, 7) => (vec![Register::S, Register::P], vec![], true), //rst nn

                _ => (vec![], vec![], false), //invalid
            }
        },
        _ => (vec![], vec![p.clone()], false)
    }
}