//! Thumb instruction repr

use crate::analysis::Error;
use crate::arch::aarch32::{
    Aarch32Register as Register, Aarch32RegisterList as RegisterList, Bus, BusAddress, Condition,
    Offset, Result,
};

/// Represents a decoded THUMB instruction.
pub enum ThumbInstruction {
    /// LSL rd, rm, immed8
    LogicalShiftLeftImmediate(Register, Register, u8),

    /// LSR rd, rm, immed8
    LogicalShiftRightImmediate(Register, Register, u8),

    /// ASR rd, rm, immed8
    ArithmeticShiftRightImmediate(Register, Register, u8),

    /// ADD rd, rn, rm
    AddRegister(Register, Register, Register),

    /// SUB rd, rn, rm
    SubRegister(Register, Register, Register),

    /// ADD rd, rn, immed3
    AddImmediate3(Register, Register, u8),

    /// SUB rd, rn, immed3
    SubImmediate3(Register, Register, u8),

    /// MOV rd, immed8
    MovImmediate8(Register, u8),

    /// CMP rd, immed8
    CmpImmediate8(Register, u8),

    /// ADD rd, rd, immed8
    AddImmediate8(Register, u8),

    /// SUB rd, rd, immed8
    SubImmediate8(Register, u8),

    /// ADD rd, PC, immed8 * 4
    AddImmediate8Pc(Register, u8),

    /// ADD rd, SP, immed8 * 4
    AddImmediate8Sp(Register, u8),

    /// ADD SP, immed7 * 4
    AddSpImmediate7(u8),

    /// SUB SP, immed7 * 4
    SubSpImmediate7(u8),

    /// AND rd, rm
    AndRegister(Register, Register),

    /// EOR rd, rm
    EorRegister(Register, Register),

    /// LSL rd, rs
    LogicalShiftLeftRegister(Register, Register),

    /// LSR rd, rs
    LogicalShiftRightRegister(Register, Register),

    /// ASR rd, rs
    ArithmeticShiftRightRegister(Register, Register),

    /// ASR rd, rm
    AddCarryRegister(Register, Register),

    /// ROR rd, rm
    RotateRightRegister(Register, Register),

    /// SBC rd, rm
    SubCarryRegister(Register, Register),

    /// TST rn, rm
    BitTestRegister(Register, Register),

    /// NEG rd, rm
    NegateRegister(Register, Register),

    /// CMP rn, rm
    CompareRegister(Register, Register),

    /// CMN rn, rm
    CompareNegativeRegister(Register, Register),

    /// ORR rd, rm
    OrRegister(Register, Register),

    /// MUL rd, rm
    MultiplyRegister(Register, Register),

    /// BIC rd, rm
    BitClearRegister(Register, Register),

    /// MVN rd, rm
    MoveNotRegister(Register, Register),

    /// ADD hrd, hrm
    AddHighRegister(Register, Register),

    /// CMP hrd, hrm
    CompareHighRegister(Register, Register),

    /// MOV hrd, hrm
    MovHighRegister(Register, Register),

    /// SWI immed_8
    SoftwareInterrupt(i8),

    /// B(cond) immed_8
    BranchConditional(Condition, i8),

    /// B immed_11
    Branch(i16),

    /// BL immed_11
    BranchLink(i32),

    /// BLX immed_11
    BranchLinkExchange(i32),

    /// BX rm
    BranchExchangeDynamic(Register),

    /// BLX rm
    BranchLinkExchangeDynamic(Register),

    /// STR rd, [SP, #immed8 * 4]
    StoreWordSpRelative(Register, u8),

    /// STR rd, [rn, rm]
    StoreWordRegisterRelative(Register, Register, Register),

    /// STR rd, [rn, #immed5 * 4]
    StoreWordRegisterImmedOffset(Register, Register, u8),

    /// STRH rd, [rn, rm]
    StoreHalfwordRegisterRelative(Register, Register, Register),

    /// STRH rd, [rn, #immed5 * 2]
    StoreHalfwordRegisterImmedOffset(Register, Register, u8),

    /// STRB rd, [rn, rm]
    StoreByteRegisterRelative(Register, Register, Register),

    /// STRB rd, [rn, #immed5]
    StoreByteRegisterImmedOffset(Register, Register, u8),

    /// LDR rd, [PC, #immed8 * 4]
    LoadWordPcRelative(Register, u8),

    /// LDR rd, [SP, #immed8 * 4]
    LoadWordSpRelative(Register, u8),

    /// LDRSB rd, [rn, rm]
    LoadSignedByteRegisterRelative(Register, Register, Register),

    /// LDR rd, [rn, rm]
    LoadWordRegisterRelative(Register, Register, Register),

    /// LDR rd, [rn, #immed5 * 4]
    LoadWordRegisterImmedOffset(Register, Register, u8),

    /// LDRH rd, [rn, rm]
    LoadHalfwordRegisterRelative(Register, Register, Register),

    /// LDRH rd, [rn, #immed5 * 2]
    LoadHalfwordRegisterImmedOffset(Register, Register, u8),

    /// LDRB rd, [rn, rm]
    LoadByteRegisterRelative(Register, Register, Register),

    /// LDRB rd, [rn, #immed5]
    LoadByteRegisterImmedOffset(Register, Register, u8),

    /// LDRSH rd, [rn, rm]
    LoadSignedHalfwordRegisterRelative(Register, Register, Register),

    /// SXTH rd, rm
    SignExtendHalfword(Register, Register),

    /// SXTB rd, rm
    SignExtendByte(Register, Register),

    /// UXTH rd, rm
    UnsignExtendHalfword(Register, Register),

    /// UXTB rd, rm
    UnsignExtendByte(Register, Register),

    /// REV rd, rn
    ByteReverseWord(Register, Register),

    /// REV16 rd, rn
    ByteReversePackedHalfword(Register, Register),

    /// REVSH rd, rn
    ByteReverseSignedHalfword(Register, Register),

    /// PUSH registerlist (r0-r7 + lr)
    Push(RegisterList),

    /// POP registerlist (r0-r7 + pc)
    Pop(RegisterList),

    /// BKPT immed_8
    Breakpoint(u8),

    /// STMIA rn!, registerlist (r0-r7)
    StoreMultipleIncrementAfter(Register, RegisterList),

    /// LDMIA rn!, registerlist (r0-r7)
    LoadMultipleIncrementAfter(Register, RegisterList),
}

impl ThumbInstruction {
    /// Read the next instruction from a static stream.
    ///
    /// If successful, returns the decoded instruction and it's size.
    #[allow(clippy::many_single_char_names)]
    pub fn from_static_stream(ptr: &BusAddress, mem: &Bus) -> Result<(Self, Offset)> {
        let instr = match mem.read_leword::<u16>(ptr).into_concrete() {
            Some(instr) => {
                let sbz = instr & 0x0007;
                let rd =
                    Register::from_instr(sbz as u32).ok_or(Error::Misinterpretation(2, false))?;
                let rn = Register::from_instr((instr as u32 & 0x0038) >> 3)
                    .ok_or(Error::Misinterpretation(2, false))?; //sometimes also rm or rs
                let shift_immed = (instr & 0x07C0) >> 6;
                let shift_opcode = (instr & 0x1800) >> 11; //also used by math imm
                let addsub_immed = (instr & 0x01C0) >> 6;
                let rm = Register::from_instr(addsub_immed as u32)
                    .ok_or(Error::Misinterpretation(2, false))?; //sometimes also rn
                let opc = (instr & 0x0200) >> 9; //add/sub bit for instructions
                let immed = (instr & 0x00FF) as u8; //sometimes also small-offset
                let math_rd_rn = Register::from_instr((instr as u32 & 0x0700) >> 8)
                    .ok_or(Error::Misinterpretation(2, false))?;
                let dp_opcode = (instr & 0x03C0) >> 6;
                let lsro_opcode = (instr & 0x0E00) >> 9;
                let cond = (instr & 0x0F00) >> 8;
                let large_offset = (instr & 0x07FF
                    | match instr & 0x0400 {
                        0 => 0,
                        _ => 0xF800,
                    }) as i16;

                match (
                    instr >> 13,
                    (instr & 0x1000) >> 12,
                    (instr & 0x0800) >> 11,
                    (instr & 0x0400) >> 10,
                ) {
                    (0, 1, 1, 0) if opc == 0 => Self::AddRegister(rd, rn, rm),
                    (0, 1, 1, 0) if opc == 1 => Self::SubRegister(rd, rn, rm),
                    (0, 1, 1, 1) if opc == 0 => Self::AddImmediate3(rd, rn, addsub_immed as u8),
                    (0, 1, 1, 1) if opc == 1 => Self::SubImmediate3(rd, rn, addsub_immed as u8),
                    (0, _, _, _) if shift_opcode == 0 => {
                        Self::LogicalShiftLeftImmediate(rd, rn, shift_immed as u8)
                    }
                    (0, _, _, _) if shift_opcode == 1 => {
                        Self::LogicalShiftRightImmediate(rd, rn, shift_immed as u8)
                    }
                    (0, _, _, _) if shift_opcode == 2 => {
                        Self::ArithmeticShiftRightImmediate(rd, rn, shift_immed as u8)
                    }
                    (0, _, _, _) => return Err(Error::Misinterpretation(2, false)),
                    (1, _, _, _) if shift_opcode == 0 => Self::MovImmediate8(math_rd_rn, immed),
                    (1, _, _, _) if shift_opcode == 1 => Self::CmpImmediate8(math_rd_rn, immed),
                    (1, _, _, _) if shift_opcode == 2 => Self::AddImmediate8(math_rd_rn, immed),
                    (1, _, _, _) if shift_opcode == 3 => Self::SubImmediate8(math_rd_rn, immed),
                    (1, _, _, _) => return Err(Error::Misinterpretation(2, false)),
                    (2, 0, 0, 0) if dp_opcode == 0 => Self::AndRegister(rd, rn),
                    (2, 0, 0, 0) if dp_opcode == 1 => Self::EorRegister(rd, rn),
                    (2, 0, 0, 0) if dp_opcode == 2 => Self::LogicalShiftLeftRegister(rd, rn),
                    (2, 0, 0, 0) if dp_opcode == 3 => Self::LogicalShiftRightRegister(rd, rn),
                    (2, 0, 0, 0) if dp_opcode == 4 => Self::ArithmeticShiftRightRegister(rd, rn),
                    (2, 0, 0, 0) if dp_opcode == 5 => Self::AddCarryRegister(rd, rn),
                    (2, 0, 0, 0) if dp_opcode == 6 => Self::SubCarryRegister(rd, rn),
                    (2, 0, 0, 0) if dp_opcode == 7 => Self::RotateRightRegister(rd, rn),
                    (2, 0, 0, 0) if dp_opcode == 8 => Self::BitTestRegister(rd, rn),
                    (2, 0, 0, 0) if dp_opcode == 9 => Self::NegateRegister(rd, rn),
                    (2, 0, 0, 0) if dp_opcode == 10 => Self::CompareRegister(rd, rn),
                    (2, 0, 0, 0) if dp_opcode == 11 => Self::CompareNegativeRegister(rd, rn),
                    (2, 0, 0, 0) if dp_opcode == 12 => Self::OrRegister(rd, rn),
                    (2, 0, 0, 0) if dp_opcode == 13 => Self::MultiplyRegister(rd, rn),
                    (2, 0, 0, 0) if dp_opcode == 14 => Self::BitClearRegister(rd, rn),
                    (2, 0, 0, 0) if dp_opcode == 15 => Self::MoveNotRegister(rd, rn),
                    (2, 0, 0, 0) => return Err(Error::Misinterpretation(2, false)),

                    (2, 0, 0, 1) => {
                        let link = (instr as u32 & 0x0080) >> 7;
                        let high_rd = Register::from_instr(sbz as u32 | (link << 3))
                            .ok_or(Error::Misinterpretation(2, false))?;
                        let high_rm = Register::from_instr((instr as u32 & 0x0078) >> 3)
                            .ok_or(Error::Misinterpretation(2, false))?;
                        let special_dp_opcode = dp_opcode >> 2;

                        match (special_dp_opcode, link != 0) {
                            (0, _) => Self::AddHighRegister(high_rd, high_rm),
                            (1, _) => Self::CompareHighRegister(high_rd, high_rm),
                            (2, _) => Self::MovHighRegister(high_rd, high_rm),
                            (3, false) if sbz == 0 => Self::BranchExchangeDynamic(high_rm),
                            (3, true) if sbz == 0 => Self::BranchLinkExchangeDynamic(high_rm),
                            (3, _) => return Err(Error::InvalidInstruction),
                            _ => return Err(Error::Misinterpretation(2, false)),
                        }
                    }
                    (2, 0, 1, _) => Self::LoadWordPcRelative(math_rd_rn, immed),
                    (2, 1, _, _) if lsro_opcode == 0 => Self::StoreWordRegisterRelative(rd, rn, rm),
                    (2, 1, _, _) if lsro_opcode == 1 => {
                        Self::StoreHalfwordRegisterRelative(rd, rn, rm)
                    }
                    (2, 1, _, _) if lsro_opcode == 2 => Self::StoreByteRegisterRelative(rd, rn, rm),
                    (2, 1, _, _) if lsro_opcode == 3 => {
                        Self::LoadSignedByteRegisterRelative(rd, rn, rm)
                    }
                    (2, 1, _, _) if lsro_opcode == 4 => Self::LoadWordRegisterRelative(rd, rn, rm),
                    (2, 1, _, _) if lsro_opcode == 5 => {
                        Self::LoadHalfwordRegisterRelative(rd, rn, rm)
                    }
                    (2, 1, _, _) if lsro_opcode == 6 => Self::LoadByteRegisterRelative(rd, rn, rm),
                    (2, 1, _, _) if lsro_opcode == 7 => {
                        Self::LoadSignedHalfwordRegisterRelative(rd, rn, rm)
                    }

                    (3, 0, 0, _) => Self::StoreWordRegisterImmedOffset(rd, rn, shift_immed as u8),
                    (3, 1, 0, _) => Self::StoreByteRegisterImmedOffset(rd, rn, shift_immed as u8),
                    (3, 0, 1, _) => Self::LoadWordRegisterImmedOffset(rd, rn, shift_immed as u8),
                    (3, 1, 1, _) => Self::LoadByteRegisterImmedOffset(rd, rn, shift_immed as u8),
                    (4, 0, 0, _) => {
                        Self::StoreHalfwordRegisterImmedOffset(rd, rn, shift_immed as u8)
                    }
                    (4, 0, 1, _) => {
                        Self::LoadHalfwordRegisterImmedOffset(rd, rn, shift_immed as u8)
                    }
                    (4, 1, 0, _) => Self::StoreWordSpRelative(math_rd_rn, immed),
                    (4, 1, 1, _) => Self::LoadWordSpRelative(math_rd_rn, immed),

                    (5, 0, 0, _) => Self::AddImmediate8Pc(math_rd_rn, immed),
                    (5, 0, 1, _) => Self::AddImmediate8Sp(math_rd_rn, immed),

                    (5, 1, a, b) => {
                        let c = (instr & 0x0200) >> 9;
                        let d = (instr & 0x0100) >> 8;
                        let e = (instr & 0x0080) >> 7;
                        let opcode = (instr & 0x00C0) >> 6;
                        match (a, b, c, d, e) {
                            (0, 0, 0, 0, 0) => Self::AddSpImmediate7(immed & 0x7F),
                            (0, 0, 0, 0, 1) => Self::SubSpImmediate7(immed & 0x7F),

                            (0, 0, 1, 0, _) if opcode == 0 => Self::SignExtendHalfword(rd, rn),
                            (0, 0, 1, 0, _) if opcode == 1 => Self::SignExtendByte(rd, rn),
                            (0, 0, 1, 0, _) if opcode == 2 => Self::UnsignExtendHalfword(rd, rn),
                            (0, 0, 1, 0, _) if opcode == 3 => Self::UnsignExtendByte(rd, rn),

                            (1, 0, 1, 0, _) if opcode == 0 => Self::ByteReverseWord(rd, rn),
                            (1, 0, 1, 0, _) if opcode == 1 => {
                                Self::ByteReversePackedHalfword(rd, rn)
                            }
                            (1, 0, 1, 0, _) if opcode == 3 => {
                                Self::ByteReverseSignedHalfword(rd, rn)
                            }

                            (0, 1, 0, r, _) => Self::Push(RegisterList::from_thumb_push(r, immed)),
                            (1, 1, 0, r, _) => Self::Pop(RegisterList::from_thumb_pop(r, immed)),

                            (1, 1, 1, 0, _) => Self::Breakpoint(immed),
                            _ => return Err(Error::InvalidInstruction), //undefined
                        }
                    }

                    (6, 0, 0, _) => Self::StoreMultipleIncrementAfter(
                        math_rd_rn,
                        RegisterList::from_thumb_stm_ldm(immed),
                    ),
                    (6, 0, 1, _) => Self::LoadMultipleIncrementAfter(
                        math_rd_rn,
                        RegisterList::from_thumb_stm_ldm(immed),
                    ),

                    (6, 1, _, _) if cond == 15 => Self::SoftwareInterrupt(immed as i8),
                    (6, 1, _, _) => Self::BranchConditional(
                        Condition::from_cond_field(cond as u8).ok_or(Error::InvalidInstruction)?,
                        immed as i8,
                    ),
                    (7, 0, 0, _) => Self::Branch(large_offset),

                    // Thumb BL/BLX is technically a psuedoinstruction,
                    // consisting of a pair of linked instructions that are
                    // ONLY allowed to occur together in a specific order.
                    // Hence, we treat it as one very long 4 byte instruction
                    // as there is no other legal interpretation of it.
                    (7, 1, 0, _) => {
                        let lower_ptr = ptr.contextualize(ptr.as_pointer() + 2);
                        match mem.read_leword::<u16>(&lower_ptr).into_concrete() {
                            Some(lower_instr) => {
                                let prelude = lower_instr & 0xF800;
                                let lower_offset = lower_instr as i32 & 0x07FF;
                                match prelude {
                                    0xF100 => {
                                        return Ok((
                                            Self::BranchLink(
                                                (large_offset as i32) << 11 | lower_offset,
                                            ),
                                            4,
                                        ))
                                    }
                                    0xF000 => {
                                        return Ok((
                                            Self::BranchLinkExchange(
                                                (large_offset as i32) << 11 | lower_offset,
                                            ),
                                            4,
                                        ))
                                    }
                                    _ => return Err(Error::InvalidInstruction),
                                }
                            }
                            _ => return Err(Error::UnconstrainedMemory(lower_ptr)),
                        }
                    }

                    _ => return Err(Error::InvalidInstruction),
                }
            }
            None => return Err(Error::UnconstrainedMemory(ptr.clone())),
        };

        Ok((instr, 2))
    }
}
