//! Instruction enumeration

use crate::analysis::{Error, Requisite};
use crate::arch::sm83::{Bus, BusAddress, Offset, PtrVal, Register, Result, SM83};
use crate::ast::{Literal, Operand};
use crate::reg::Symbolic;

/// A target location for an 8-bit value.
#[derive(Copy, Clone, Debug)]
pub enum Target8 {
    Register(Register),
    IndirectHL,
}

impl Target8 {
    pub fn into_operand<L>(self) -> Operand<L>
    where
        L: Literal,
    {
        match self {
            Target8::Register(reg) => reg.into_operand(),
            Target8::IndirectHL => Operand::indir(Operand::sym("hl")),
        }
    }

    /// Turn the target into a list of memory requisites.
    pub fn into_memory_requisites(self) -> Vec<Requisite<SM83>> {
        match self {
            Target8::Register(_) => vec![],
            Target8::IndirectHL => vec![
                Requisite::register(Register::H, 0xFF),
                Requisite::register(Register::L, 0xFF),
            ],
        }
    }
}

/// Decoding aid for targets of 8-bit ALU instructions
pub static TARGET8_ALU_REG: [Target8; 8] = [
    Target8::Register(Register::B),
    Target8::Register(Register::C),
    Target8::Register(Register::D),
    Target8::Register(Register::E),
    Target8::Register(Register::H),
    Target8::Register(Register::L),
    Target8::IndirectHL,
    Target8::Register(Register::A),
];

/// A target location for a 16-bit value.
#[derive(Copy, Clone, Debug)]
pub enum RegisterPair {
    BC,
    DE,
    HL,
    AF,
    SP,
    HLIncrement,
    HLDecrement,
}

impl RegisterPair {
    pub fn into_operand<L>(self) -> Operand<L>
    where
        L: Literal,
    {
        match self {
            Self::BC => Operand::sym("bc"),
            Self::DE => Operand::sym("de"),
            Self::HL => Operand::sym("hl"),
            Self::AF => Operand::sym("af"),
            Self::SP => Operand::sym("sp"),
            Self::HLIncrement => Operand::sym("hli"),
            Self::HLDecrement => Operand::sym("hld"),
        }
    }

    pub fn into_requisites(self) -> Vec<Requisite<SM83>> {
        match self {
            Self::BC => vec![
                Requisite::register(Register::B, 0xFF),
                Requisite::register(Register::C, 0xFF),
            ],
            Self::DE => vec![
                Requisite::register(Register::D, 0xFF),
                Requisite::register(Register::E, 0xFF),
            ],
            Self::HL => vec![
                Requisite::register(Register::H, 0xFF),
                Requisite::register(Register::L, 0xFF),
            ],
            Self::AF => vec![
                Requisite::register(Register::A, 0xFF),
                Requisite::register(Register::F, 0xFF),
            ],
            Self::SP => vec![
                Requisite::register(Register::S, 0xFF),
                Requisite::register(Register::P, 0xFF),
            ],
            Self::HLIncrement => vec![
                Requisite::register(Register::H, 0xFF),
                Requisite::register(Register::L, 0xFF),
            ],
            Self::HLDecrement => vec![
                Requisite::register(Register::H, 0xFF),
                Requisite::register(Register::L, 0xFF),
            ],
        }
    }
}

/// Enumeration of all of the ways that ALU instructions that load constants
/// into registers name register pairs.
pub static TARGET16_ALU_REG: [RegisterPair; 4] = [
    RegisterPair::BC,
    RegisterPair::DE,
    RegisterPair::HL,
    RegisterPair::SP,
];

/// Enumeration of all the ways that ALU instructions that read or write memory
/// can reference register pairs.
///
/// The register pair `[HL]` is not referenced by these instructions; there's a
/// separate encoding scheme (see `TARGET8_ALU_REG`) for arbitrary register
/// transfer instructions that also allows writing any register to `[HL]`.
pub static TARGET16_ALU_MEM: [RegisterPair; 4] = [
    RegisterPair::BC,
    RegisterPair::DE,
    RegisterPair::HLIncrement,
    RegisterPair::HLDecrement,
];

/// Enumeration of all the ways that stack manipulation instructions can select
/// a register pair to be pushed or popped.
pub static TARGET16_STACK_REG: [RegisterPair; 4] = [
    RegisterPair::BC,
    RegisterPair::DE,
    RegisterPair::HL,
    RegisterPair::AF,
];

/// A condition code for a conditional instruction.
#[derive(Copy, Clone, Debug)]
pub enum Condition {
    Z,
    C,
    NZ,
    NC,
}

impl Condition {
    pub fn into_operand<L>(self) -> Operand<L>
    where
        L: Literal,
    {
        match self {
            Self::Z => Operand::sym("z"),
            Self::C => Operand::sym("c"),
            Self::NZ => Operand::sym("nz"),
            Self::NC => Operand::sym("nc"),
        }
    }

    pub fn into_requisite(self) -> Requisite<SM83> {
        match self {
            Self::Z | Self::NZ => Requisite::register(Register::F, 0x80),
            Self::C | Self::NC => Requisite::register(Register::F, 0x10),
        }
    }

    pub fn test(self, flags: Symbolic<u8>) -> Result<bool> {
        Ok(match self {
            Self::Z => {
                (flags & Symbolic::from(0x80))
                    .into_concrete()
                    .ok_or(Error::UnconstrainedRegister)?
                    != 0
            }
            Self::C => {
                (flags & Symbolic::from(0x10))
                    .into_concrete()
                    .ok_or(Error::UnconstrainedRegister)?
                    != 0
            }
            Self::NZ => {
                (flags & Symbolic::from(0x80))
                    .into_concrete()
                    .ok_or(Error::UnconstrainedRegister)?
                    == 0
            }
            Self::NC => {
                (flags & Symbolic::from(0x10))
                    .into_concrete()
                    .ok_or(Error::UnconstrainedRegister)?
                    == 0
            }
        })
    }
}

pub static ALU_CONDCODE: [Condition; 4] =
    [Condition::NZ, Condition::Z, Condition::NC, Condition::C];

/// A decoded SM83 instruction.
#[derive(Copy, Clone, Debug)]
pub enum Instruction {
    Nop,

    /// LD instructions of the form LD (target8), (target8)
    ///
    /// First target8 is the target of the copy, second is the source.
    LdReg8(Target8, Target8),

    /// LD HL, SP+(i8)
    LdHLSP(i8),

    /// LD SP, HL
    LdSPHL,

    /// LD instructions of the form LD [(u16)], a
    LdWriteStatic(u16),

    /// LD instructions of the form LD [(regpair)], A
    LdWritePtr(RegisterPair),

    /// LD instructions of the form LDH [(a8)], A
    LdWriteHiStatic(u8),

    /// LD [C], A
    LdWriteHiPtr,

    /// LD instructions of the form LD A, [(u16)]
    LdReadStatic(u16),

    /// LD instructions of the form LD A, [(regpair)]
    LdReadPtr(RegisterPair),

    /// LD instructions of the form LDH A, [(a8)]
    LdReadHiStatic(u8),

    /// LD A, [C]
    LdReadHiPtr,

    /// LD instructions of the form LD (target8), (u8)
    LdConst8(Target8, u8),

    /// LD instructions of the form LD (regpair), (u16)
    LdConst16(RegisterPair, u16),

    /// LD [(u16)], SP
    LdWriteStaticSP(u16),

    /// INC instructions of the form INC (regpair)
    Inc16(RegisterPair),

    /// INC instructions of the form INC (target8)
    Inc8(Target8),

    /// DEC instructions of the form DEC (regpair)
    Dec16(RegisterPair),

    /// DEC instructions of the form DEC (target8)
    Dec8(Target8),

    /// Absolute jumps of the form JP (cond), (u16)
    Jump(PtrVal, Option<Condition>),

    /// Relative jumps of the form JR (cond), (i8)
    JumpRelative(i8, Option<Condition>),

    /// JP [HL]
    JumpDynamic,

    /// CALL (cond), (u16)
    Call(PtrVal, Option<Condition>),

    /// RST (u8)
    ///
    /// u8 is constrained to [0x0, 0x8, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38].
    CallRst(u8),

    /// PUSH (regpair)
    Push(RegisterPair),

    /// POP (regpair)
    Pop(RegisterPair),

    /// RET (cond)
    Return(Option<Condition>),

    /// RETI
    ReturnFromInterrupt,

    /// Add instructions of the form ADD HL, (regpair).
    Add16(RegisterPair),

    /// ADD SP, (i8)
    AddSpConst(i8),

    /// Add instructions of the form ADD A, (target8)
    Add8(Target8),

    /// Add instructions of the form ADD A, (u8)
    Add8Const(u8),

    /// Add instructions of the form ADC A, (target8)
    AddCarry8(Target8),

    /// Add instructions of the form ADC A, (u8)
    AddCarry8Const(u8),

    /// Subtract instructions of the form SUB A, (target8)
    Sub8(Target8),

    /// Subtract instructions of the form SUB A, (u8)
    Sub8Const(u8),

    /// Subtract instructions of the form SBC A, (target8)
    SubCarry8(Target8),

    /// Subtract instructions of the form SBC A, (u8)
    SubCarry8Const(u8),

    /// Bitwise And instructions of the form AND A, (target8)
    And8(Target8),

    /// Bitwise And instructions of the form AND A, (u8)
    And8Const(u8),

    /// Exclusive or instructions of the form XOR A, (target8)
    Xor8(Target8),

    /// Exclusive or instructions of the form XOR A, (u8)
    Xor8Const(u8),

    /// Inclusive or instructions of the form OR A, (target8)
    Or8(Target8),

    /// Inclusive or instructions of the form OR A, (u8)
    Or8Const(u8),

    /// Compare instructions of the form CP (target8)
    Cp(Target8),

    /// Compare instructions of the form CP (u8)
    CpConst(u8),

    /// Rotate-left-through-carry instructions of the form RLC (target8)
    RotateLeftCarry(Target8),

    /// Legacy 8080 RLC A
    RotateLeftCarryAccum,

    /// Rotate-left instructions of the form RL (target8)
    RotateLeft(Target8),

    /// Legacy 8080 RL A
    RotateLeftAccum,

    /// Rotate-right-through-carry instructions of the form RRC (target8)
    RotateRightCarry(Target8),

    /// Legacy 8080 RRC A
    RotateRightCarryAccum,

    /// Rotate-right instructions of the form RR (target8)
    RotateRight(Target8),

    /// Legacy 8080 RR A
    RotateRightAccum,

    /// Arithmetic left-shift instructions of the form SLA (target8)
    ShiftLeftArithmetic(Target8),

    /// Arithmetic right-shift instructions of the form SRA (target8)
    ShiftRightArithmetic(Target8),

    /// Nybble swaps of the form SWAP (target8)
    NybbleSwap(Target8),

    /// Logical right-shift instructions of the form SRL (target8)
    ShiftRightLogical(Target8),

    /// Bit-test instructions of the form BIT (u8), (target8)
    BitTest(u8, Target8),

    /// Bit-reset instructions of the form RES (u8), (target8)
    BitReset(u8, Target8),

    /// Bit-set instructions of the form SET (u8), (target8)
    BitSet(u8, Target8),

    Compliment,
    DecimalAdjust,
    SetCarry,
    ClearCarry,
    DisableInterrupt,
    EnableInterrupt,
    Halt,
    Stop,
}

/// Legacy 8080 bitops
pub static LEGACY_BITOPS: [Instruction; 8] = [
    Instruction::RotateLeftCarryAccum,
    Instruction::RotateRightCarryAccum,
    Instruction::RotateLeftAccum,
    Instruction::RotateRightAccum,
    Instruction::DecimalAdjust,
    Instruction::Compliment,
    Instruction::SetCarry,
    Instruction::ClearCarry,
];

impl Instruction {
    /// Read the next instruction from a static stream.
    ///
    /// If successful, returns the decoded instruction and it's size.
    pub fn from_static_stream(p: &BusAddress, mem: &Bus) -> Result<(Self, Offset)> {
        match mem.read_unit(p).into_concrete() {
            Some(0xCB) => match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                Some(subop) => match (
                    (subop >> 6) & 0x03,
                    (subop >> 3) & 0x07,
                    TARGET8_ALU_REG[(subop & 0x07) as usize],
                ) {
                    (0, 0, t8) => Ok((Self::RotateLeftCarry(t8), 2)),
                    (0, 1, t8) => Ok((Self::RotateRightCarry(t8), 2)),
                    (0, 2, t8) => Ok((Self::RotateLeft(t8), 2)),
                    (0, 3, t8) => Ok((Self::RotateRight(t8), 2)),
                    (0, 4, t8) => Ok((Self::ShiftLeftArithmetic(t8), 2)),
                    (0, 5, t8) => Ok((Self::ShiftRightArithmetic(t8), 2)),
                    (0, 6, t8) => Ok((Self::NybbleSwap(t8), 2)),
                    (0, 7, t8) => Ok((Self::ShiftRightLogical(t8), 2)),
                    (1, b8, t8) => Ok((Self::BitTest(b8, t8), 2)),
                    (2, b8, t8) => Ok((Self::BitReset(b8, t8), 2)),
                    (3, b8, t8) => Ok((Self::BitSet(b8, t8), 2)),
                    _ => Err(Error::Misinterpretation(1, false)),
                },
                _ => Err(Error::UnconstrainedMemory(p.clone() + 1)),
            },

            //Z80 instructions that don't fit the pattern decoder below
            Some(0x00) => Ok((Self::Nop, 1)), //nop
            Some(0x08) => Ok((
                Self::LdWriteStaticSP(
                    match mem.read_leword::<u16>(&(p.clone() + 1)).into_concrete() {
                        Some(a16) => a16,
                        None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                    },
                ),
                3,
            )), //ld [u16], sp
            Some(0x10) => Ok((Self::Stop, 2)), //stop
            Some(0x18) => Ok((
                Self::JumpRelative(
                    match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                        Some(r8) => r8 as i8,
                        None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                    },
                    None,
                ),
                2,
            )), //jr u8
            Some(0x76) => Ok((Self::Halt, 1)), //halt

            Some(0xC3) => Ok((
                Self::Jump(
                    match mem.read_leword::<u16>(&(p.clone() + 1)).into_concrete() {
                        Some(a16) => a16,
                        None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                    },
                    None,
                ),
                3,
            )), //jp u16
            Some(0xCD) => Ok((
                Self::Call(
                    match mem.read_leword::<u16>(&(p.clone() + 1)).into_concrete() {
                        Some(a16) => a16,
                        None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                    },
                    None,
                ),
                3,
            )), //call u16

            Some(0xC9) => Ok((Self::Return(None), 1)), //ret
            Some(0xD9) => Ok((Self::ReturnFromInterrupt, 1)), //reti
            Some(0xE9) => Ok((Self::JumpDynamic, 1)),  //jp [hl]
            Some(0xF9) => Ok((Self::LdSPHL, 1)),       //ld sp, hl

            Some(0xE0) => Ok((
                Self::LdWriteHiStatic(match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                    Some(r8) => r8,
                    None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                }),
                2,
            )), //ldh [u8], a
            Some(0xE8) => Ok((
                Self::AddSpConst(match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                    Some(r8) => r8 as i8,
                    None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                }),
                2,
            )), //add sp, reg
            Some(0xF0) => Ok((
                Self::LdReadHiStatic(match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                    Some(r8) => r8,
                    None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                }),
                2,
            )), //ldh a, [u8]
            Some(0xF8) => Ok((
                Self::LdHLSP(match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                    Some(r8) => r8 as i8,
                    None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                }),
                2,
            )), //ld hl, sp+r8

            Some(0xE2) => Ok((Self::LdWriteHiPtr, 1)), //ldh [c], a
            Some(0xEA) => Ok((
                Self::LdWriteStatic(
                    match mem.read_leword::<u16>(&(p.clone() + 1)).into_concrete() {
                        Some(a16) => a16,
                        None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                    },
                ),
                3,
            )), //ld [u16], a
            Some(0xF2) => Ok((Self::LdReadHiPtr, 1)),  //ldh a, [c]
            Some(0xFA) => Ok((
                Self::LdReadStatic(
                    match mem.read_leword::<u16>(&(p.clone() + 1)).into_concrete() {
                        Some(a16) => a16,
                        None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                    },
                ),
                3,
            )), //ld a, [u16]

            Some(0xF3) => Ok((Self::DisableInterrupt, 1)),
            Some(0xFB) => Ok((Self::EnableInterrupt, 1)),

            //Z80 instructions that follow a particular pattern
            Some(op) => {
                let condcode = ALU_CONDCODE[((op >> 3) & 0x03) as usize];
                let targetpair = TARGET16_ALU_REG[((op >> 4) & 0x03) as usize];
                let targetreg = TARGET8_ALU_REG[((op >> 3) & 0x07) as usize];
                let targetmem = TARGET16_ALU_MEM[((op >> 4) & 0x03) as usize];
                let bitop = LEGACY_BITOPS[((op >> 3) & 0x07) as usize];
                let targetreg2 = TARGET8_ALU_REG[(op & 0x07) as usize];
                let stackpair = TARGET16_STACK_REG[((op >> 4) & 0x03) as usize];

                //decode `op` into aab?cddd. This creates a nice visual table for
                //the Z80's semiperiodic instruction encoding
                match (
                    (op >> 6) & 0x03,
                    (op >> 5) & 0x01,
                    (op >> 3) & 0x01,
                    op & 0x07,
                ) {
                    (0, 0, _, 0) => Err(Error::Misinterpretation(1, false)), /* 00, 08, 10, 18 */
                    (0, 1, _, 0) => Ok((
                        Self::JumpRelative(
                            match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                                Some(r8) => r8 as i8,
                                None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                            },
                            Some(condcode),
                        ),
                        2,
                    )), // jr (r8)
                    (0, _, 0, 1) => Ok((
                        Self::LdConst16(
                            targetpair,
                            match mem.read_leword::<u16>(&(p.clone() + 1)).into_concrete() {
                                Some(a16) => a16,
                                None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                            },
                        ),
                        3,
                    )), // ld (targetpair), u16
                    (0, _, 1, 1) => Ok((Self::Add16(targetpair), 1)),        // add hl, (targetpair)
                    (0, _, 0, 2) => Ok((Self::LdWritePtr(targetmem), 1)),    // ld (targetmem), a
                    (0, _, 1, 2) => Ok((Self::LdReadPtr(targetmem), 1)),     // ld a, (targetmem)
                    (0, _, 0, 3) => Ok((Self::Inc16(targetpair), 1)),        // inc (targetpair)
                    (0, _, 1, 3) => Ok((Self::Dec16(targetpair), 1)),        // dec (targetpair)
                    (0, _, _, 4) => Ok((Self::Inc8(targetreg), 1)),          // inc (targetreg)
                    (0, _, _, 5) => Ok((Self::Dec8(targetreg), 1)),          // dec (targetreg)
                    (0, _, _, 6) => Ok((
                        Self::LdConst8(
                            targetreg,
                            match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                                Some(r8) => r8,
                                None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                            },
                        ),
                        2,
                    )), //ld (targetreg), u8
                    (0, _, _, 7) => Ok((bitop, 1)),                          //legacy bitops
                    (1, _, _, _) => Ok((Self::LdReg8(targetreg, targetreg2), 1)), //ld (reg2), (reg)
                    (2, _, _, _) => match (op >> 3) & 0x07 {
                        0 => Ok((Self::Add8(targetreg2), 1)),
                        1 => Ok((Self::AddCarry8(targetreg2), 1)),
                        2 => Ok((Self::Sub8(targetreg2), 1)),
                        3 => Ok((Self::SubCarry8(targetreg2), 1)),
                        4 => Ok((Self::And8(targetreg2), 1)),
                        5 => Ok((Self::Xor8(targetreg2), 1)),
                        6 => Ok((Self::Or8(targetreg2), 1)),
                        7 => Ok((Self::Cp(targetreg2), 1)),
                        _ => Err(Error::Misinterpretation(1, false)),
                    },

                    (3, 0, _, 0) => Ok((Self::Return(Some(condcode)), 1)),
                    (3, 1, _, 0) => Err(Error::Misinterpretation(1, false)), /* E0, E8, F0, F8 */
                    (3, _, 0, 1) => Ok((Self::Pop(stackpair), 1)),
                    (3, _, 1, 1) => Err(Error::Misinterpretation(1, false)), /* C9, D9, E9, F9 */
                    (3, 0, _, 2) => Ok((
                        Self::Jump(
                            match mem.read_leword::<u16>(&(p.clone() + 1)).into_concrete() {
                                Some(a16) => a16,
                                None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                            },
                            Some(condcode),
                        ),
                        3,
                    )),
                    (3, 1, _, 2) => Err(Error::Misinterpretation(1, false)), /* E2, EA, F2, FA */
                    (3, _, _, 3) => Err(Error::InvalidInstruction),
                    (3, 0, _, 4) => Ok((
                        Self::Call(
                            match mem.read_leword::<u16>(&(p.clone() + 1)).into_concrete() {
                                Some(a16) => a16,
                                None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                            },
                            Some(condcode),
                        ),
                        3,
                    )),
                    (3, 1, _, 4) => Err(Error::InvalidInstruction),
                    (3, _, 0, 5) => Ok((Self::Push(stackpair), 1)),
                    (3, _, 1, 5) => Err(Error::InvalidInstruction),
                    (3, _, _, 6) => match (op >> 3) & 0x07 {
                        0 => Ok((
                            Self::Add8Const(
                                match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                                    Some(r8) => r8,
                                    None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                                },
                            ),
                            2,
                        )),
                        1 => Ok((
                            Self::AddCarry8Const(
                                match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                                    Some(r8) => r8,
                                    None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                                },
                            ),
                            2,
                        )),
                        2 => Ok((
                            Self::Sub8Const(
                                match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                                    Some(r8) => r8,
                                    None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                                },
                            ),
                            2,
                        )),
                        3 => Ok((
                            Self::SubCarry8Const(
                                match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                                    Some(r8) => r8,
                                    None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                                },
                            ),
                            2,
                        )),
                        4 => Ok((
                            Self::And8Const(
                                match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                                    Some(r8) => r8,
                                    None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                                },
                            ),
                            2,
                        )),
                        5 => Ok((
                            Self::Xor8Const(
                                match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                                    Some(r8) => r8,
                                    None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                                },
                            ),
                            2,
                        )),
                        6 => Ok((
                            Self::Or8Const(match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                                Some(r8) => r8,
                                None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                            }),
                            2,
                        )),
                        7 => Ok((
                            Self::CpConst(match mem.read_unit(&(p.clone() + 1)).into_concrete() {
                                Some(r8) => r8,
                                None => return Err(Error::UnconstrainedMemory(p.clone() + 1)),
                            }),
                            2,
                        )),
                        _ => Err(Error::Misinterpretation(1, false)),
                    },
                    (3, _, _, 7) => Ok((Self::CallRst(op & 0x38), 1)),

                    _ => Err(Error::InvalidInstruction),
                }
            }
            _ => Err(Error::UnconstrainedMemory(p.clone())),
        }
    }
}
