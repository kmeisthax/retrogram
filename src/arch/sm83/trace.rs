//! Facilities for tracing SM83 code

use crate::analysis::Error;
use crate::arch::sm83;
use crate::arch::sm83::{
    Bus, Instruction, PtrVal, Register, RegisterPair, Result, State, Target8, Trace,
};
use crate::maths::Reinterpret;
use crate::reg::{Convertable, Symbolic, TryConvertable};

fn read_value8_from_target(target: Target8, mem: &Bus, state: &State) -> Result<Symbolic<u8>> {
    match target {
        Target8::Register(r) => Ok(state.get_register(&r)),
        Target8::IndirectHl => {
            let h: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::H));
            let l: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::L));
            let hl = h << 8 | l;
            let hl = hl.into_concrete().ok_or(Error::UnconstrainedRegister)?;

            Ok(mem.read_unit_stateful(hl, state))
        }
    }
}

fn write_value8_to_target(
    value: Symbolic<u8>,
    target: Target8,
    mem: &Bus,
    state: &mut State,
    trace: &mut Trace,
) -> Result<()> {
    match target {
        Target8::Register(r) => {
            state.set_register(r, value);
        }
        Target8::IndirectHl => {
            let h: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::H));
            let l: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::L));
            let hl = h << 8 | l;
            let hl = hl.into_concrete().ok_or(Error::UnconstrainedRegister)?;

            mem.write_memory(hl, &[value], state, Some(trace));
        }
    };

    Ok(())
}

/// Read a register pair value.
///
/// If the register pair is an autoincrement or autodecrement, then the pair
/// will be incremented or decremented, but you will get the pre-increment
/// or pre-decrement value.
fn read_value16_from_regpair(pair: RegisterPair, state: &mut State) -> Symbolic<u16> {
    let (hi, lo) = match pair {
        RegisterPair::Af => {
            let a: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::A));
            let f: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::F));
            (a, f)
        }
        RegisterPair::Bc => {
            let b: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::B));
            let c: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::C));
            (b, c)
        }
        RegisterPair::De => {
            let d: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::D));
            let e: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::E));
            (d, e)
        }
        RegisterPair::Hl => {
            let h: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::H));
            let l: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::L));
            (h, l)
        }
        RegisterPair::HlIncrement => {
            let h: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::H));
            let l: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::L));

            let hl = h << 8 | l;
            let hli = hl + Symbolic::<u16>::from(1);

            let hli_hi: Symbolic<u8> = Symbolic::try_convert_from(hli >> 8).unwrap();
            let hli_lo: Symbolic<u8> =
                Symbolic::try_convert_from(hli & Symbolic::<u16>::from(0xFF)).unwrap();

            state.set_register(Register::H, hli_hi);
            state.set_register(Register::L, hli_lo);

            (h, l)
        }
        RegisterPair::HlDecrement => {
            let h: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::H));
            let l: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::L));

            let hl = h << 8 | l;
            let hld = hl - Symbolic::<u16>::from(1);

            let hld_hi: Symbolic<u8> = Symbolic::try_convert_from(hld >> 8).unwrap();
            let hld_lo: Symbolic<u8> =
                Symbolic::try_convert_from(hld & Symbolic::<u16>::from(0xFF)).unwrap();

            state.set_register(Register::H, hld_hi);
            state.set_register(Register::L, hld_lo);

            (h, l)
        }
        RegisterPair::Sp => {
            let s: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::S));
            let p: Symbolic<u16> = Symbolic::convert_from(state.get_register(&Register::P));
            (s, p)
        }
    };

    hi << 8 | lo
}

/// Write a register pair value.
///
/// If the register pair is an autoincrement or autodecrement, this function
/// does nothing (as that only makes sense as a memory offset)
fn write_value16_to_regpair(pair: RegisterPair, value: Symbolic<u16>, state: &mut State) {
    let hi = Symbolic::try_convert_from(value >> 8).unwrap();
    let lo = Symbolic::try_convert_from(value & Symbolic::from(0xFF)).unwrap();

    match pair {
        RegisterPair::Af => {
            state.set_register(Register::A, hi);
            state.set_register(Register::F, lo);
        }
        RegisterPair::Bc => {
            state.set_register(Register::B, hi);
            state.set_register(Register::C, lo);
        }
        RegisterPair::De => {
            state.set_register(Register::D, hi);
            state.set_register(Register::E, lo);
        }
        RegisterPair::Hl => {
            state.set_register(Register::H, hi);
            state.set_register(Register::L, lo);
        }
        RegisterPair::HlIncrement => {}
        RegisterPair::HlDecrement => {}
        RegisterPair::Sp => {
            state.set_register(Register::S, hi);
            state.set_register(Register::P, lo);
        }
    }
}

/// Calculate flags with a given carry.
fn flags_with_carry(flags: Symbolic<u8>, carry: Option<bool>) -> Symbolic<u8> {
    match carry {
        None => {
            let undef_carry: Symbolic<u8> = Symbolic::<u8>::default() & Symbolic::from(0x10);
            (flags & !Symbolic::<u8>::from(0x10)) | undef_carry
        }
        Some(true) => flags | Symbolic::from(0x10),
        Some(false) => flags & !Symbolic::<u8>::from(0x10),
    }
}

/// Calculate flags with a given zero state derived from an accumulator value.
fn flags_with_zero(flags: Symbolic<u8>, a: Symbolic<u8>) -> Symbolic<u8> {
    match a.into_concrete() {
        Some(0) => flags | Symbolic::from(0x80),
        None if a.is_valid(0) => {
            let undef_zero: Symbolic<u8> = Symbolic::<u8>::default() & Symbolic::from(0x80);
            flags | undef_zero
        }
        _ => flags & !Symbolic::<u8>::from(0x80),
    }
}

/// Add a u8 to a u8 and calculate updated flags.
///
/// If `with_carry` is enabled, the carry flag in `flags` will be fed into the
/// add operation.
///
/// The first return parameter is the addition result (usually to be stored in
/// A) and the second return parameter is the new set of flags.
///
/// The flags set correspond to an `add A, r` instruction; with Z, H, and C set
/// appropriately and N cleared.
fn add_u8_u8(
    lhs: Symbolic<u8>,
    rhs: Symbolic<u8>,
    flags: Symbolic<u8>,
    with_carry: bool,
) -> (Symbolic<u8>, Symbolic<u8>) {
    let old_carry = if with_carry {
        (flags & Symbolic::from(0x10))
            .into_concrete()
            .map(|v| v != 0)
    } else {
        Some(false)
    };
    let (new_a, new_carry) = lhs.add_with_carry(rhs, old_carry);

    let flags_with_carry = flags_with_carry(flags, new_carry);
    let flags_with_zero = flags_with_zero(flags_with_carry, new_a);
    let flags_with_neg = flags_with_zero & !Symbolic::<u8>::from(0x40);

    (new_a, flags_with_neg)
}

/// Subtract a u8 from a u8 and calculate updated flags.
///
/// If `with_carry` is enabled, the carry flag in `flags` will be fed into the
/// subtract operation.
///
/// The first parameter is the addition result (usually to be stored in A) and
/// the second parameter is the new set of flags.
///
/// The flags set correspond to an `add A, r` instruction; with Z, H, and C set
/// appropriately and N set.
fn sub_u8_u8(
    lhs: Symbolic<u8>,
    rhs: Symbolic<u8>,
    flags: Symbolic<u8>,
    with_carry: bool,
) -> (Symbolic<u8>, Symbolic<u8>) {
    let old_carry = if with_carry {
        (flags & Symbolic::from(0x10))
            .into_concrete()
            .map(|v| v != 0)
    } else {
        Some(false)
    };
    let (new_a, new_carry) = lhs.sub_with_borrow(rhs, old_carry);

    let flags_with_carry = flags_with_carry(flags, new_carry);
    let flags_with_zero = flags_with_zero(flags_with_carry, new_a);
    let flags_with_neg = flags_with_zero | Symbolic::<u8>::from(0x40);

    (new_a, flags_with_neg)
}

/// Add an i8 to a u16 and calculate updated flags.
///
/// The returned value consists of the new 16 bit result (usually HL) and the
/// updated flags register.
///
/// The returned flags set Z and N to zero, which is suitable for an `SP+i8`
/// instruction.
fn add_u16_i8(
    lhs: Symbolic<u16>,
    rhs: Symbolic<i8>,
    flags: Symbolic<u8>,
) -> (Symbolic<u16>, Symbolic<u8>) {
    let rhs_i16: Symbolic<i16> = Symbolic::convert_from(rhs);
    let rhs_u16: Symbolic<u16> = rhs_i16.reinterpret();

    let (new_hls, carry) = lhs.add_with_carry(rhs_u16, Some(false));

    let flags_with_carry = flags_with_carry(flags, carry);
    let flags_without_zn = flags_with_carry & Symbolic::from(0x30);

    (new_hls, flags_without_zn)
}

/// Add two u16s together and calculate updated carry flags.
///
/// The returned value consists of the new 16 bit result (usually HL) and the
/// updated flags register.
///
/// The returned flags are suitable for `add HL, rr` instructions; other 16-bit
/// adds do not update flags and thus you should discard the flags calculated
/// here. The Z flag is unchanged, the N flag is zeroed, and the H and C flags
/// are set appropriately.
fn add_u16_u16(
    lhs: Symbolic<u16>,
    rhs: Symbolic<u16>,
    flags: Symbolic<u8>,
) -> (Symbolic<u16>, Symbolic<u8>) {
    let (new_hls, carry) = lhs.add_with_carry(rhs, Some(false));
    let flags_with_carry = flags_with_carry(flags, carry);
    let flags_without_n = flags_with_carry & Symbolic::from(0xB0);

    (new_hls, flags_without_n)
}

fn push_to_stack(
    val: Symbolic<u16>,
    mem: &Bus,
    state: &mut State,
    trace: &mut Trace,
) -> Result<()> {
    let sp = read_value16_from_regpair(RegisterPair::Sp, state)
        .into_concrete()
        .ok_or(Error::UnconstrainedRegister)?;
    let val_hi = Symbolic::try_convert_from(val >> 8).unwrap();
    let val_lo = Symbolic::try_convert_from(val & Symbolic::from(0xFF)).unwrap();

    mem.write_memory(sp, &[val_lo, val_hi], state, Some(trace));

    write_value16_to_regpair(RegisterPair::Sp, Symbolic::from(sp - 2), state);

    Ok(())
}

fn pop_from_stack(mem: &Bus, state: &mut State) -> Result<Symbolic<u16>> {
    let sp = read_value16_from_regpair(RegisterPair::Sp, state)
        .into_concrete()
        .ok_or(Error::UnconstrainedRegister)?;
    let val = mem.read_leword_stateful(sp, state);

    write_value16_to_regpair(RegisterPair::Sp, Symbolic::from(sp + 2), state);

    Ok(val)
}

fn pop_pc_from_stack(mem: &Bus, state: &mut State) -> Result<u16> {
    let sp = read_value16_from_regpair(RegisterPair::Sp, state)
        .into_concrete()
        .ok_or(Error::UnconstrainedRegister)?;
    let val = mem
        .read_leword_stateful(sp, state)
        .into_concrete()
        .ok_or_else(|| Error::UnconstrainedMemory(state.contextualize_pointer(sp)))?;

    write_value16_to_regpair(RegisterPair::Sp, Symbolic::from(sp + 2), state);

    Ok(val)
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
    p: PtrVal,
    mem: &Bus,
    mut state: State,
    trace: &mut Trace,
) -> sm83::Result<(State, PtrVal)> {
    let (instr, offset) = Instruction::from_static_stream(&state.contextualize_pointer(p), mem)?;

    match instr {
        Instruction::Nop => {}
        Instruction::LdReg8(tgt, src) => {
            let value8 = read_value8_from_target(src, mem, &state)?;
            write_value8_to_target(value8, tgt, mem, &mut state, trace)?;
        }
        Instruction::LdHlSp(offset) => {
            let sp = read_value16_from_regpair(RegisterPair::Sp, &mut state);
            let (new_sp, new_flags) =
                add_u16_i8(sp, Symbolic::from(offset), state.get_register(&Register::F));
            write_value16_to_regpair(RegisterPair::Hl, new_sp, &mut state);
            state.set_register(Register::F, new_flags & Symbolic::from(0x30));
        }
        Instruction::LdSpHl => {
            let hl = read_value16_from_regpair(RegisterPair::Hl, &mut state);
            write_value16_to_regpair(RegisterPair::Sp, hl, &mut state);
        }
        Instruction::LdWriteStatic(addr) => {
            let a = state.get_register(&Register::A);
            mem.write_memory(addr, &[a], &mut state, Some(trace));
        }
        Instruction::LdWritePtr(ptr_regpair) => {
            let addr = read_value16_from_regpair(ptr_regpair, &mut state)
                .into_concrete()
                .ok_or(Error::UnconstrainedRegister)?;
            let a = state.get_register(&Register::A);
            mem.write_memory(addr, &[a], &mut state, Some(trace));
        }
        Instruction::LdWriteHiStatic(hiaddr) => {
            let addr = 0xFF00 + hiaddr as u16;
            let a = state.get_register(&Register::A);
            mem.write_memory(addr, &[a], &mut state, Some(trace));
        }
        Instruction::LdWriteHiPtr => {
            let hiaddr = state
                .get_register(&Register::C)
                .into_concrete()
                .ok_or(Error::UnconstrainedRegister)?;
            let addr = 0xFF00 + hiaddr as u16;
            let a = state.get_register(&Register::A);
            mem.write_memory(addr, &[a], &mut state, Some(trace));
        }
        Instruction::LdReadStatic(addr) => {
            let a = mem.read_unit_stateful(addr, &state);
            state.set_register(Register::A, a);
        }
        Instruction::LdReadPtr(ptr_regpair) => {
            let addr = read_value16_from_regpair(ptr_regpair, &mut state)
                .into_concrete()
                .ok_or(Error::UnconstrainedRegister)?;
            let a = mem.read_unit_stateful(addr, &state);
            state.set_register(Register::A, a);
        }
        Instruction::LdReadHiStatic(hiaddr) => {
            let addr = 0xFF00 + hiaddr as u16;
            let a = mem.read_unit_stateful(addr, &state);
            state.set_register(Register::A, a);
        }
        Instruction::LdReadHiPtr => {
            let hiaddr = state
                .get_register(&Register::C)
                .into_concrete()
                .ok_or(Error::UnconstrainedRegister)?;
            let addr = 0xFF00 + hiaddr as u16;
            let a = mem.read_unit_stateful(addr, &state);
            state.set_register(Register::A, a);
        }
        Instruction::LdConst8(tgt, constu8) => {
            write_value8_to_target(Symbolic::from(constu8), tgt, mem, &mut state, trace)?;
        }
        Instruction::LdConst16(regpair, constu16) => {
            write_value16_to_regpair(regpair, Symbolic::from(constu16), &mut state);
        }
        Instruction::LdWriteStaticSp(addr) => {
            let s = state.get_register(&Register::S);
            let p = state.get_register(&Register::P);

            mem.write_memory(addr, &[p, s], &mut state, Some(trace));
        }
        Instruction::Inc16(regpair) => {
            let rpval = read_value16_from_regpair(regpair, &mut state);
            write_value16_to_regpair(regpair, rpval + Symbolic::from(1), &mut state);
        }
        Instruction::Inc8(tgt) => {
            let val = read_value8_from_target(tgt, mem, &state)?;
            let f = state.get_register(&Register::F);
            let (new_val, new_f) = add_u8_u8(val, Symbolic::from(1), f, false);
            let new_f_without_c = new_f & Symbolic::from(0xE0) | f & Symbolic::from(0x10);

            write_value8_to_target(new_val, tgt, mem, &mut state, trace)?;
            state.set_register(Register::F, new_f_without_c);
        }
        Instruction::Dec16(regpair) => {
            let rpval = read_value16_from_regpair(regpair, &mut state);
            write_value16_to_regpair(regpair, rpval - Symbolic::from(1), &mut state);
        }
        Instruction::Dec8(tgt) => {
            let val = read_value8_from_target(tgt, mem, &state)?;
            let f = state.get_register(&Register::F);
            let (new_val, new_f) = sub_u8_u8(val, Symbolic::from(1), f, false);
            let new_f_without_c = new_f & Symbolic::from(0xE0) | f & Symbolic::from(0x10);

            write_value8_to_target(new_val, tgt, mem, &mut state, trace)?;
            state.set_register(Register::F, new_f_without_c);
        }
        Instruction::Jump(target, cond) => {
            let f = state.get_register(&Register::F);
            let cond_passed = if let Some(cond) = cond {
                cond.test(f)?
            } else {
                true
            };

            if cond_passed {
                return Ok((state, target));
            }
        }
        Instruction::JumpRelative(offset, cond) => {
            let target = (p as i16 + offset as i16) as u16;
            let f = state.get_register(&Register::F);
            let cond_passed = if let Some(cond) = cond {
                cond.test(f)?
            } else {
                true
            };

            if cond_passed {
                return Ok((state, target));
            }
        }
        Instruction::JumpDynamic => {
            let hl = read_value16_from_regpair(RegisterPair::Hl, &mut state)
                .into_concrete()
                .ok_or(Error::UnconstrainedRegister)?;
            return Ok((state, hl));
        }
        Instruction::Call(target, cond) => {
            let f = state.get_register(&Register::F);
            let cond_passed = if let Some(cond) = cond {
                cond.test(f)?
            } else {
                true
            };

            if cond_passed {
                push_to_stack(Symbolic::from(p), mem, &mut state, trace)?;
                return Ok((state, target));
            }
        }
        Instruction::CallRst(rst) => {
            let target = rst as u16;
            push_to_stack(Symbolic::from(p), mem, &mut state, trace)?;
            return Ok((state, target));
        }
        Instruction::Push(regpair) => {
            let val = read_value16_from_regpair(regpair, &mut state);
            push_to_stack(val, mem, &mut state, trace)?;
        }
        Instruction::Pop(regpair) => {
            let val = pop_from_stack(mem, &mut state)?;
            write_value16_to_regpair(regpair, val, &mut state);
        }
        Instruction::Return(cond) => {
            let f = state.get_register(&Register::F);
            let cond_passed = if let Some(cond) = cond {
                cond.test(f)?
            } else {
                true
            };

            if cond_passed {
                let new_pc = pop_pc_from_stack(mem, &mut state)?;
                return Ok((state, new_pc));
            }
        }
        Instruction::ReturnFromInterrupt => {
            let new_pc = pop_pc_from_stack(mem, &mut state)?;
            return Ok((state, new_pc));
        }
        Instruction::Add16(regpair) => {
            let hl = read_value16_from_regpair(RegisterPair::Hl, &mut state);
            let addend = read_value16_from_regpair(regpair, &mut state);
            let f = state.get_register(&Register::F);

            let (new_hl, new_f) = add_u16_u16(hl, addend, f);

            write_value16_to_regpair(RegisterPair::Hl, new_hl, &mut state);
            state.set_register(Register::F, new_f);
        }
        Instruction::AddSpConst(offset) => {
            let sp = read_value16_from_regpair(RegisterPair::Sp, &mut state);
            let f = state.get_register(&Register::F);

            let (new_sp, new_f) = add_u16_i8(sp, Symbolic::from(offset), f);
            let new_f = new_f & Symbolic::from(0x70);

            write_value16_to_regpair(RegisterPair::Sp, new_sp, &mut state);
            state.set_register(Register::F, new_f);
        }
        Instruction::Add8(tgt) => {
            let a = state.get_register(&Register::A);
            let f = state.get_register(&Register::F);
            let addend = read_value8_from_target(tgt, mem, &state)?;

            let (new_a, new_f) = add_u8_u8(a, addend, f, false);

            state.set_register(Register::A, new_a);
            state.set_register(Register::F, new_f);
        }
        Instruction::Add8Const(constu8) => {
            let a = state.get_register(&Register::A);
            let f = state.get_register(&Register::F);

            let (new_a, new_f) = add_u8_u8(a, Symbolic::from(constu8), f, false);

            state.set_register(Register::A, new_a);
            state.set_register(Register::F, new_f);
        }
        Instruction::AddCarry8(tgt) => {
            let a = state.get_register(&Register::A);
            let f = state.get_register(&Register::F);
            let addend = read_value8_from_target(tgt, mem, &state)?;

            let (new_a, new_f) = add_u8_u8(a, addend, f, true);

            state.set_register(Register::A, new_a);
            state.set_register(Register::F, new_f);
        }
        Instruction::AddCarry8Const(constu8) => {
            let a = state.get_register(&Register::A);
            let f = state.get_register(&Register::F);

            let (new_a, new_f) = add_u8_u8(a, Symbolic::from(constu8), f, true);

            state.set_register(Register::A, new_a);
            state.set_register(Register::F, new_f);
        }
        Instruction::Sub8(tgt) => {
            let a = state.get_register(&Register::A);
            let f = state.get_register(&Register::F);
            let addend = read_value8_from_target(tgt, mem, &state)?;

            let (new_a, new_f) = sub_u8_u8(a, addend, f, false);

            state.set_register(Register::A, new_a);
            state.set_register(Register::F, new_f);
        }
        Instruction::Sub8Const(constu8) => {
            let a = state.get_register(&Register::A);
            let f = state.get_register(&Register::F);

            let (new_a, new_f) = sub_u8_u8(a, Symbolic::from(constu8), f, false);

            state.set_register(Register::A, new_a);
            state.set_register(Register::F, new_f);
        }
        Instruction::SubCarry8(tgt) => {
            let a = state.get_register(&Register::A);
            let f = state.get_register(&Register::F);
            let subtrahend = read_value8_from_target(tgt, mem, &state)?;

            let (new_a, new_f) = sub_u8_u8(a, subtrahend, f, true);

            state.set_register(Register::A, new_a);
            state.set_register(Register::F, new_f);
        }
        Instruction::SubCarry8Const(constu8) => {
            let a = state.get_register(&Register::A);
            let f = state.get_register(&Register::F);

            let (new_a, new_f) = sub_u8_u8(a, Symbolic::from(constu8), f, true);

            state.set_register(Register::A, new_a);
            state.set_register(Register::F, new_f);
        }
        Instruction::And8(tgt) => {
            let a = state.get_register(&Register::A);
            let andend = read_value8_from_target(tgt, mem, &state)?;

            let new_a = a & andend;
            let new_f = flags_with_zero(Symbolic::from(0x20), new_a);

            state.set_register(Register::A, new_a);
            state.set_register(Register::F, new_f);
        }
        Instruction::And8Const(constu8) => {
            let a = state.get_register(&Register::A);

            let new_a = a & Symbolic::from(constu8);
            let new_f = flags_with_zero(Symbolic::from(0x20), new_a);

            state.set_register(Register::A, new_a);
            state.set_register(Register::F, new_f);
        }
        Instruction::Xor8(tgt) => {
            let a = state.get_register(&Register::A);
            let xorend = read_value8_from_target(tgt, mem, &state)?;

            let new_a = a ^ xorend;
            let new_f = flags_with_zero(Symbolic::from(0x00), new_a);

            state.set_register(Register::A, new_a);
            state.set_register(Register::F, new_f);
        }
        Instruction::Xor8Const(constu8) => {
            let a = state.get_register(&Register::A);

            let new_a = a & Symbolic::from(constu8);
            let new_f = flags_with_zero(Symbolic::from(0x00), new_a);

            state.set_register(Register::A, new_a);
            state.set_register(Register::F, new_f);
        }
        Instruction::Or8(tgt) => {
            let a = state.get_register(&Register::A);
            let orend = read_value8_from_target(tgt, mem, &state)?;

            let new_a = a | orend;
            let new_f = flags_with_zero(Symbolic::from(0x00), new_a);

            state.set_register(Register::A, new_a);
            state.set_register(Register::F, new_f);
        }
        Instruction::Or8Const(constu8) => {
            let a = state.get_register(&Register::A);

            let new_a = a | Symbolic::from(constu8);
            let new_f = flags_with_zero(Symbolic::from(0x00), new_a);

            state.set_register(Register::A, new_a);
            state.set_register(Register::F, new_f);
        }
        Instruction::Cp(tgt) => {
            let a = state.get_register(&Register::A);
            let f = state.get_register(&Register::F);
            let subtrahend = read_value8_from_target(tgt, mem, &state)?;

            let (_, new_f) = sub_u8_u8(a, subtrahend, f, false);

            state.set_register(Register::F, new_f);
        }
        Instruction::CpConst(constu8) => {
            let a = state.get_register(&Register::A);
            let f = state.get_register(&Register::F);

            let (_, new_f) = sub_u8_u8(a, Symbolic::from(constu8), f, false);

            state.set_register(Register::F, new_f);
        }
        Instruction::RotateLeftCarry(tgt) => {
            let val = read_value8_from_target(tgt, mem, &state)?;
            let f = state.get_register(&Register::F);

            let old_c = (f & Symbolic::from(0x10)) >> 4;
            let new_val = val << 1 | old_c;
            let new_c = (val & Symbolic::from(0x80)) >> 3;

            let new_f = flags_with_zero(new_c, new_val);

            write_value8_to_target(new_val, tgt, mem, &mut state, trace)?;
            state.set_register(Register::F, new_f);
        }
        Instruction::RotateLeftCarryAccum => {
            let val = state.get_register(&Register::A);
            let f = state.get_register(&Register::F);

            let old_c = (f & Symbolic::from(0x10)) >> 4;
            let new_val = val << 1 | old_c;
            let new_f = (val & Symbolic::from(0x80)) >> 3;

            state.set_register(Register::A, new_val);
            state.set_register(Register::F, new_f);
        }
        Instruction::RotateLeft(tgt) => {
            let val = read_value8_from_target(tgt, mem, &state)?;

            let new_val = (val << 1) | (val >> 7);
            let new_c = (val & Symbolic::from(0x80)) >> 3;

            let new_f = flags_with_zero(new_c, new_val);

            write_value8_to_target(new_val, tgt, mem, &mut state, trace)?;
            state.set_register(Register::F, new_f);
        }
        Instruction::RotateLeftAccum => {
            let val = state.get_register(&Register::A);

            let new_val = (val << 1) | (val >> 7);
            let new_f = (val & Symbolic::from(0x80)) >> 3;

            state.set_register(Register::A, new_val);
            state.set_register(Register::F, new_f);
        }
        Instruction::RotateRightCarry(tgt) => {
            let val = read_value8_from_target(tgt, mem, &state)?;
            let f = state.get_register(&Register::F);

            let old_c = (f & Symbolic::from(0x10)) << 3;
            let new_val = old_c | (val >> 1);
            let new_c = (val & Symbolic::from(0x01)) << 4;

            let new_f = flags_with_zero(new_c, new_val);

            write_value8_to_target(new_val, tgt, mem, &mut state, trace)?;
            state.set_register(Register::F, new_f);
        }
        Instruction::RotateRightCarryAccum => {
            let val = state.get_register(&Register::A);
            let f = state.get_register(&Register::F);

            let old_c = (f & Symbolic::from(0x10)) << 3;
            let new_val = old_c | (val >> 1);
            let new_f = (val & Symbolic::from(0x01)) << 4;

            state.set_register(Register::A, new_val);
            state.set_register(Register::F, new_f);
        }
        Instruction::RotateRight(tgt) => {
            let val = read_value8_from_target(tgt, mem, &state)?;

            let new_val = (val << 7) | (val >> 1);
            let new_c = (val & Symbolic::from(0x01)) << 4;

            let new_f = flags_with_zero(new_c, new_val);

            write_value8_to_target(new_val, tgt, mem, &mut state, trace)?;
            state.set_register(Register::F, new_f);
        }
        Instruction::RotateRightAccum => {
            let val = state.get_register(&Register::A);

            let new_val = (val << 7) | (val >> 1);
            let new_f = (val & Symbolic::from(0x01)) << 4;

            state.set_register(Register::A, new_val);
            state.set_register(Register::F, new_f);
        }
        Instruction::ShiftLeftArithmetic(tgt) => {
            let val = read_value8_from_target(tgt, mem, &state)?;

            let new_val = val << 1;
            let new_c = (val & Symbolic::from(0x80)) >> 3;

            let new_f = flags_with_zero(new_c, new_val);

            write_value8_to_target(new_val, tgt, mem, &mut state, trace)?;
            state.set_register(Register::F, new_f);
        }
        Instruction::ShiftRightArithmetic(tgt) => {
            let val = read_value8_from_target(tgt, mem, &state)?;

            let new_val = val >> 1;

            let new_f = flags_with_zero(Symbolic::from(0x00), new_val);

            write_value8_to_target(new_val, tgt, mem, &mut state, trace)?;
            state.set_register(Register::F, new_f);
        }
        Instruction::NybbleSwap(tgt) => {
            let val = read_value8_from_target(tgt, mem, &state)?;

            let new_val = (val << 4) | (val >> 4);

            let new_f = flags_with_zero(Symbolic::from(0x00), new_val);

            write_value8_to_target(new_val, tgt, mem, &mut state, trace)?;
            state.set_register(Register::F, new_f);
        }
        Instruction::ShiftRightLogical(tgt) => {
            let val = read_value8_from_target(tgt, mem, &state)?;

            let sign_ext = val & Symbolic::from(0x80);
            let new_val = sign_ext | (val >> 1);
            let new_c = (val & Symbolic::from(0x01)) << 4;

            let new_f = flags_with_zero(new_c, new_val);

            write_value8_to_target(new_val, tgt, mem, &mut state, trace)?;
            state.set_register(Register::F, new_f);
        }
        Instruction::BitTest(bit, tgt) => {
            let val = read_value8_from_target(tgt, mem, &state)?;
            let f = state.get_register(&Register::F);

            let test_bit = (val >> bit) & Symbolic::from(0x01);
            let new_f = flags_with_zero(f, test_bit) & Symbolic::from(0x90) | Symbolic::from(0x20);

            state.set_register(Register::F, new_f);
        }
        Instruction::BitReset(bit, tgt) => {
            let val = read_value8_from_target(tgt, mem, &state)?;

            let reset_mask = !(Symbolic::<u8>::from(0x01) << bit);
            let new_val = val & reset_mask;

            write_value8_to_target(new_val, tgt, mem, &mut state, trace)?;
        }
        Instruction::BitSet(bit, tgt) => {
            let val = read_value8_from_target(tgt, mem, &state)?;

            let set_mask = Symbolic::<u8>::from(0x01) << bit;
            let new_val = val | set_mask;

            write_value8_to_target(new_val, tgt, mem, &mut state, trace)?;
        }
        Instruction::Compliment => {
            let val = state.get_register(&Register::A);
            let f = state.get_register(&Register::F);

            let new_val = !val;
            let new_f = f | Symbolic::from(0x60);

            state.set_register(Register::A, new_val);
            state.set_register(Register::F, new_f);
        }
        //TODO: Impl `DAA`
        Instruction::DecimalAdjust => return Err(Error::NotYetImplemented),
        Instruction::SetCarry => {
            let f = state.get_register(&Register::F);

            let new_f = f & Symbolic::from(0x60) | Symbolic::from(0x10);

            state.set_register(Register::F, new_f);
        }
        Instruction::ClearCarry => {
            let f = state.get_register(&Register::F);

            let new_f = f & Symbolic::from(0x70);

            state.set_register(Register::F, new_f);
        }
        Instruction::DisableInterrupt => {}
        Instruction::EnableInterrupt => {}
        Instruction::Halt => {}
        Instruction::Stop => {}
    };

    Ok((state, p + offset))
}
