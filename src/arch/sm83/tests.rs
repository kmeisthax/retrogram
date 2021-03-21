//! SM83 test suite

use crate::analysis::Error;
use crate::arch::sm83::Target8::{IndirectHL as THL, Register as TReg};
use crate::arch::sm83::{
    Condition as Cond, Instruction as Inst, Register as Reg, RegisterPair as Pair,
};
use crate::memory::{Memory, Pointer};

macro_rules! assert_sm83 {
    ($data: expr, $instr: expr, $offset: expr) => {
        assert_eq!(
            Inst::from_static_stream(&Pointer::from(0), &Memory::test_rom(0, $data)).unwrap(),
            ($instr, $offset)
        )
    };
}

macro_rules! assert_sm83_err {
    ($data: expr, $error: pat) => {
        assert!(matches!(
            Inst::from_static_stream(&Pointer::from(0), &Memory::test_rom(0, $data)).unwrap_err(),
            $error
        ))
    };
}

/// Disassembly tests
#[test]
fn sm83_disasm() {
    assert_sm83!(vec![0x00], Inst::Nop, 1);
    assert_sm83!(vec![0x01, 0x23, 0x01], Inst::LdConst16(Pair::BC, 0x123), 3);
    assert_sm83!(vec![0x02], Inst::LdWritePtr(Pair::BC), 1);
    assert_sm83!(vec![0x03], Inst::Inc16(Pair::BC), 1);
    assert_sm83!(vec![0x04], Inst::Inc8(TReg(Reg::B)), 1);
    assert_sm83!(vec![0x05], Inst::Dec8(TReg(Reg::B)), 1);
    assert_sm83!(vec![0x06, 0x12], Inst::LdConst8(TReg(Reg::B), 0x12), 2);
    assert_sm83!(vec![0x07], Inst::RotateLeftCarryAccum, 1);
    assert_sm83!(vec![0x08, 0x34, 0x12], Inst::LdWriteStaticSP(0x1234), 3);
    assert_sm83!(vec![0x09], Inst::Add16(Pair::BC), 1);
    assert_sm83!(vec![0x0A], Inst::LdReadPtr(Pair::BC), 1);
    assert_sm83!(vec![0x0B], Inst::Dec16(Pair::BC), 1);
    assert_sm83!(vec![0x0C], Inst::Inc8(TReg(Reg::C)), 1);
    assert_sm83!(vec![0x0D], Inst::Dec8(TReg(Reg::C)), 1);
    assert_sm83!(vec![0x0E, 0x12], Inst::LdConst8(TReg(Reg::C), 0x12), 2);
    assert_sm83!(vec![0x0F], Inst::RotateRightCarryAccum, 1);

    assert_sm83!(vec![0x10, 0x00], Inst::Stop, 2);
    assert_sm83!(vec![0x11, 0x23, 0x01], Inst::LdConst16(Pair::DE, 0x123), 3);
    assert_sm83!(vec![0x12], Inst::LdWritePtr(Pair::DE), 1);
    assert_sm83!(vec![0x13], Inst::Inc16(Pair::DE), 1);
    assert_sm83!(vec![0x14], Inst::Inc8(TReg(Reg::D)), 1);
    assert_sm83!(vec![0x15], Inst::Dec8(TReg(Reg::D)), 1);
    assert_sm83!(vec![0x16, 0x12], Inst::LdConst8(TReg(Reg::D), 0x12), 2);
    assert_sm83!(vec![0x17], Inst::RotateLeftAccum, 1);
    assert_sm83!(vec![0x18, 0xFF], Inst::JumpRelative(-1, None), 2);
    assert_sm83!(vec![0x19], Inst::Add16(Pair::DE), 1);
    assert_sm83!(vec![0x1A], Inst::LdReadPtr(Pair::DE), 1);
    assert_sm83!(vec![0x1B], Inst::Dec16(Pair::DE), 1);
    assert_sm83!(vec![0x1C], Inst::Inc8(TReg(Reg::E)), 1);
    assert_sm83!(vec![0x1D], Inst::Dec8(TReg(Reg::E)), 1);
    assert_sm83!(vec![0x1E, 0x12], Inst::LdConst8(TReg(Reg::E), 0x12), 2);
    assert_sm83!(vec![0x1F], Inst::RotateRightAccum, 1);

    assert_sm83!(vec![0x20, 0xFF], Inst::JumpRelative(-1, Some(Cond::NZ)), 2);
    assert_sm83!(vec![0x21, 0x23, 0x01], Inst::LdConst16(Pair::HL, 0x123), 3);
    assert_sm83!(vec![0x22], Inst::LdWritePtr(Pair::HLIncrement), 1);
    assert_sm83!(vec![0x23], Inst::Inc16(Pair::HL), 1);
    assert_sm83!(vec![0x24], Inst::Inc8(TReg(Reg::H)), 1);
    assert_sm83!(vec![0x25], Inst::Dec8(TReg(Reg::H)), 1);
    assert_sm83!(vec![0x26, 0x12], Inst::LdConst8(TReg(Reg::H), 0x12), 2);
    assert_sm83!(vec![0x27], Inst::DecimalAdjust, 1);
    assert_sm83!(vec![0x28, 0xFF], Inst::JumpRelative(-1, Some(Cond::Z)), 2);
    assert_sm83!(vec![0x29], Inst::Add16(Pair::HL), 1);
    assert_sm83!(vec![0x2A], Inst::LdReadPtr(Pair::HLIncrement), 1);
    assert_sm83!(vec![0x2B], Inst::Dec16(Pair::HL), 1);
    assert_sm83!(vec![0x2C], Inst::Inc8(TReg(Reg::L)), 1);
    assert_sm83!(vec![0x2D], Inst::Dec8(TReg(Reg::L)), 1);
    assert_sm83!(vec![0x2E, 0x12], Inst::LdConst8(TReg(Reg::L), 0x12), 2);
    assert_sm83!(vec![0x2F], Inst::Compliment, 1);

    assert_sm83!(vec![0x30, 0xFF], Inst::JumpRelative(-1, Some(Cond::NC)), 2);
    assert_sm83!(vec![0x31, 0x23, 0x01], Inst::LdConst16(Pair::SP, 0x123), 3);
    assert_sm83!(vec![0x32], Inst::LdWritePtr(Pair::HLDecrement), 1);
    assert_sm83!(vec![0x33], Inst::Inc16(Pair::SP), 1);
    assert_sm83!(vec![0x34], Inst::Inc8(THL), 1);
    assert_sm83!(vec![0x35], Inst::Dec8(THL), 1);
    assert_sm83!(vec![0x36, 0x12], Inst::LdConst8(THL, 0x12), 2);
    assert_sm83!(vec![0x37], Inst::SetCarry, 1);
    assert_sm83!(vec![0x38, 0xFF], Inst::JumpRelative(-1, Some(Cond::C)), 2);
    assert_sm83!(vec![0x39], Inst::Add16(Pair::SP), 1);
    assert_sm83!(vec![0x3A], Inst::LdReadPtr(Pair::HLDecrement), 1);
    assert_sm83!(vec![0x3B], Inst::Dec16(Pair::SP), 1);
    assert_sm83!(vec![0x3C], Inst::Inc8(TReg(Reg::A)), 1);
    assert_sm83!(vec![0x3D], Inst::Dec8(TReg(Reg::A)), 1);
    assert_sm83!(vec![0x3E, 0x12], Inst::LdConst8(TReg(Reg::A), 0x12), 2);
    assert_sm83!(vec![0x3F], Inst::ClearCarry, 1);

    assert_sm83!(vec![0x40], Inst::LdReg8(TReg(Reg::B), TReg(Reg::B)), 1);
    assert_sm83!(vec![0x41], Inst::LdReg8(TReg(Reg::B), TReg(Reg::C)), 1);
    assert_sm83!(vec![0x42], Inst::LdReg8(TReg(Reg::B), TReg(Reg::D)), 1);
    assert_sm83!(vec![0x43], Inst::LdReg8(TReg(Reg::B), TReg(Reg::E)), 1);
    assert_sm83!(vec![0x44], Inst::LdReg8(TReg(Reg::B), TReg(Reg::H)), 1);
    assert_sm83!(vec![0x45], Inst::LdReg8(TReg(Reg::B), TReg(Reg::L)), 1);
    assert_sm83!(vec![0x46], Inst::LdReg8(TReg(Reg::B), THL), 1);
    assert_sm83!(vec![0x47], Inst::LdReg8(TReg(Reg::B), TReg(Reg::A)), 1);

    assert_sm83!(vec![0x48], Inst::LdReg8(TReg(Reg::C), TReg(Reg::B)), 1);
    assert_sm83!(vec![0x49], Inst::LdReg8(TReg(Reg::C), TReg(Reg::C)), 1);
    assert_sm83!(vec![0x4A], Inst::LdReg8(TReg(Reg::C), TReg(Reg::D)), 1);
    assert_sm83!(vec![0x4B], Inst::LdReg8(TReg(Reg::C), TReg(Reg::E)), 1);
    assert_sm83!(vec![0x4C], Inst::LdReg8(TReg(Reg::C), TReg(Reg::H)), 1);
    assert_sm83!(vec![0x4D], Inst::LdReg8(TReg(Reg::C), TReg(Reg::L)), 1);
    assert_sm83!(vec![0x4E], Inst::LdReg8(TReg(Reg::C), THL), 1);
    assert_sm83!(vec![0x4F], Inst::LdReg8(TReg(Reg::C), TReg(Reg::A)), 1);

    assert_sm83!(vec![0x50], Inst::LdReg8(TReg(Reg::D), TReg(Reg::B)), 1);
    assert_sm83!(vec![0x51], Inst::LdReg8(TReg(Reg::D), TReg(Reg::C)), 1);
    assert_sm83!(vec![0x52], Inst::LdReg8(TReg(Reg::D), TReg(Reg::D)), 1);
    assert_sm83!(vec![0x53], Inst::LdReg8(TReg(Reg::D), TReg(Reg::E)), 1);
    assert_sm83!(vec![0x54], Inst::LdReg8(TReg(Reg::D), TReg(Reg::H)), 1);
    assert_sm83!(vec![0x55], Inst::LdReg8(TReg(Reg::D), TReg(Reg::L)), 1);
    assert_sm83!(vec![0x56], Inst::LdReg8(TReg(Reg::D), THL), 1);
    assert_sm83!(vec![0x57], Inst::LdReg8(TReg(Reg::D), TReg(Reg::A)), 1);

    assert_sm83!(vec![0x58], Inst::LdReg8(TReg(Reg::E), TReg(Reg::B)), 1);
    assert_sm83!(vec![0x59], Inst::LdReg8(TReg(Reg::E), TReg(Reg::C)), 1);
    assert_sm83!(vec![0x5A], Inst::LdReg8(TReg(Reg::E), TReg(Reg::D)), 1);
    assert_sm83!(vec![0x5B], Inst::LdReg8(TReg(Reg::E), TReg(Reg::E)), 1);
    assert_sm83!(vec![0x5C], Inst::LdReg8(TReg(Reg::E), TReg(Reg::H)), 1);
    assert_sm83!(vec![0x5D], Inst::LdReg8(TReg(Reg::E), TReg(Reg::L)), 1);
    assert_sm83!(vec![0x5E], Inst::LdReg8(TReg(Reg::E), THL), 1);
    assert_sm83!(vec![0x5F], Inst::LdReg8(TReg(Reg::E), TReg(Reg::A)), 1);

    assert_sm83!(vec![0x60], Inst::LdReg8(TReg(Reg::H), TReg(Reg::B)), 1);
    assert_sm83!(vec![0x61], Inst::LdReg8(TReg(Reg::H), TReg(Reg::C)), 1);
    assert_sm83!(vec![0x62], Inst::LdReg8(TReg(Reg::H), TReg(Reg::D)), 1);
    assert_sm83!(vec![0x63], Inst::LdReg8(TReg(Reg::H), TReg(Reg::E)), 1);
    assert_sm83!(vec![0x64], Inst::LdReg8(TReg(Reg::H), TReg(Reg::H)), 1);
    assert_sm83!(vec![0x65], Inst::LdReg8(TReg(Reg::H), TReg(Reg::L)), 1);
    assert_sm83!(vec![0x66], Inst::LdReg8(TReg(Reg::H), THL), 1);
    assert_sm83!(vec![0x67], Inst::LdReg8(TReg(Reg::H), TReg(Reg::A)), 1);

    assert_sm83!(vec![0x68], Inst::LdReg8(TReg(Reg::L), TReg(Reg::B)), 1);
    assert_sm83!(vec![0x69], Inst::LdReg8(TReg(Reg::L), TReg(Reg::C)), 1);
    assert_sm83!(vec![0x6A], Inst::LdReg8(TReg(Reg::L), TReg(Reg::D)), 1);
    assert_sm83!(vec![0x6B], Inst::LdReg8(TReg(Reg::L), TReg(Reg::E)), 1);
    assert_sm83!(vec![0x6C], Inst::LdReg8(TReg(Reg::L), TReg(Reg::H)), 1);
    assert_sm83!(vec![0x6D], Inst::LdReg8(TReg(Reg::L), TReg(Reg::L)), 1);
    assert_sm83!(vec![0x6E], Inst::LdReg8(TReg(Reg::L), THL), 1);
    assert_sm83!(vec![0x6F], Inst::LdReg8(TReg(Reg::L), TReg(Reg::A)), 1);

    assert_sm83!(vec![0x70], Inst::LdReg8(THL, TReg(Reg::B)), 1);
    assert_sm83!(vec![0x71], Inst::LdReg8(THL, TReg(Reg::C)), 1);
    assert_sm83!(vec![0x72], Inst::LdReg8(THL, TReg(Reg::D)), 1);
    assert_sm83!(vec![0x73], Inst::LdReg8(THL, TReg(Reg::E)), 1);
    assert_sm83!(vec![0x74], Inst::LdReg8(THL, TReg(Reg::H)), 1);
    assert_sm83!(vec![0x75], Inst::LdReg8(THL, TReg(Reg::L)), 1);
    assert_sm83!(vec![0x76], Inst::Halt, 1);
    assert_sm83!(vec![0x77], Inst::LdReg8(THL, TReg(Reg::A)), 1);

    assert_sm83!(vec![0x78], Inst::LdReg8(TReg(Reg::A), TReg(Reg::B)), 1);
    assert_sm83!(vec![0x79], Inst::LdReg8(TReg(Reg::A), TReg(Reg::C)), 1);
    assert_sm83!(vec![0x7A], Inst::LdReg8(TReg(Reg::A), TReg(Reg::D)), 1);
    assert_sm83!(vec![0x7B], Inst::LdReg8(TReg(Reg::A), TReg(Reg::E)), 1);
    assert_sm83!(vec![0x7C], Inst::LdReg8(TReg(Reg::A), TReg(Reg::H)), 1);
    assert_sm83!(vec![0x7D], Inst::LdReg8(TReg(Reg::A), TReg(Reg::L)), 1);
    assert_sm83!(vec![0x7E], Inst::LdReg8(TReg(Reg::A), THL), 1);
    assert_sm83!(vec![0x7F], Inst::LdReg8(TReg(Reg::A), TReg(Reg::A)), 1);

    assert_sm83!(vec![0x80], Inst::Add8(TReg(Reg::B)), 1);
    assert_sm83!(vec![0x81], Inst::Add8(TReg(Reg::C)), 1);
    assert_sm83!(vec![0x82], Inst::Add8(TReg(Reg::D)), 1);
    assert_sm83!(vec![0x83], Inst::Add8(TReg(Reg::E)), 1);
    assert_sm83!(vec![0x84], Inst::Add8(TReg(Reg::H)), 1);
    assert_sm83!(vec![0x85], Inst::Add8(TReg(Reg::L)), 1);
    assert_sm83!(vec![0x86], Inst::Add8(THL), 1);
    assert_sm83!(vec![0x87], Inst::Add8(TReg(Reg::A)), 1);

    assert_sm83!(vec![0x88], Inst::AddCarry8(TReg(Reg::B)), 1);
    assert_sm83!(vec![0x89], Inst::AddCarry8(TReg(Reg::C)), 1);
    assert_sm83!(vec![0x8A], Inst::AddCarry8(TReg(Reg::D)), 1);
    assert_sm83!(vec![0x8B], Inst::AddCarry8(TReg(Reg::E)), 1);
    assert_sm83!(vec![0x8C], Inst::AddCarry8(TReg(Reg::H)), 1);
    assert_sm83!(vec![0x8D], Inst::AddCarry8(TReg(Reg::L)), 1);
    assert_sm83!(vec![0x8E], Inst::AddCarry8(THL), 1);
    assert_sm83!(vec![0x8F], Inst::AddCarry8(TReg(Reg::A)), 1);

    assert_sm83!(vec![0x90], Inst::Sub8(TReg(Reg::B)), 1);
    assert_sm83!(vec![0x91], Inst::Sub8(TReg(Reg::C)), 1);
    assert_sm83!(vec![0x92], Inst::Sub8(TReg(Reg::D)), 1);
    assert_sm83!(vec![0x93], Inst::Sub8(TReg(Reg::E)), 1);
    assert_sm83!(vec![0x94], Inst::Sub8(TReg(Reg::H)), 1);
    assert_sm83!(vec![0x95], Inst::Sub8(TReg(Reg::L)), 1);
    assert_sm83!(vec![0x96], Inst::Sub8(THL), 1);
    assert_sm83!(vec![0x97], Inst::Sub8(TReg(Reg::A)), 1);

    assert_sm83!(vec![0x98], Inst::SubCarry8(TReg(Reg::B)), 1);
    assert_sm83!(vec![0x99], Inst::SubCarry8(TReg(Reg::C)), 1);
    assert_sm83!(vec![0x9A], Inst::SubCarry8(TReg(Reg::D)), 1);
    assert_sm83!(vec![0x9B], Inst::SubCarry8(TReg(Reg::E)), 1);
    assert_sm83!(vec![0x9C], Inst::SubCarry8(TReg(Reg::H)), 1);
    assert_sm83!(vec![0x9D], Inst::SubCarry8(TReg(Reg::L)), 1);
    assert_sm83!(vec![0x9E], Inst::SubCarry8(THL), 1);
    assert_sm83!(vec![0x9F], Inst::SubCarry8(TReg(Reg::A)), 1);

    assert_sm83!(vec![0xA0], Inst::And8(TReg(Reg::B)), 1);
    assert_sm83!(vec![0xA1], Inst::And8(TReg(Reg::C)), 1);
    assert_sm83!(vec![0xA2], Inst::And8(TReg(Reg::D)), 1);
    assert_sm83!(vec![0xA3], Inst::And8(TReg(Reg::E)), 1);
    assert_sm83!(vec![0xA4], Inst::And8(TReg(Reg::H)), 1);
    assert_sm83!(vec![0xA5], Inst::And8(TReg(Reg::L)), 1);
    assert_sm83!(vec![0xA6], Inst::And8(THL), 1);
    assert_sm83!(vec![0xA7], Inst::And8(TReg(Reg::A)), 1);

    assert_sm83!(vec![0xA8], Inst::Xor8(TReg(Reg::B)), 1);
    assert_sm83!(vec![0xA9], Inst::Xor8(TReg(Reg::C)), 1);
    assert_sm83!(vec![0xAA], Inst::Xor8(TReg(Reg::D)), 1);
    assert_sm83!(vec![0xAB], Inst::Xor8(TReg(Reg::E)), 1);
    assert_sm83!(vec![0xAC], Inst::Xor8(TReg(Reg::H)), 1);
    assert_sm83!(vec![0xAD], Inst::Xor8(TReg(Reg::L)), 1);
    assert_sm83!(vec![0xAE], Inst::Xor8(THL), 1);
    assert_sm83!(vec![0xAF], Inst::Xor8(TReg(Reg::A)), 1);

    assert_sm83!(vec![0xB0], Inst::Or8(TReg(Reg::B)), 1);
    assert_sm83!(vec![0xB1], Inst::Or8(TReg(Reg::C)), 1);
    assert_sm83!(vec![0xB2], Inst::Or8(TReg(Reg::D)), 1);
    assert_sm83!(vec![0xB3], Inst::Or8(TReg(Reg::E)), 1);
    assert_sm83!(vec![0xB4], Inst::Or8(TReg(Reg::H)), 1);
    assert_sm83!(vec![0xB5], Inst::Or8(TReg(Reg::L)), 1);
    assert_sm83!(vec![0xB6], Inst::Or8(THL), 1);
    assert_sm83!(vec![0xB7], Inst::Or8(TReg(Reg::A)), 1);

    assert_sm83!(vec![0xB8], Inst::Cp(TReg(Reg::B)), 1);
    assert_sm83!(vec![0xB9], Inst::Cp(TReg(Reg::C)), 1);
    assert_sm83!(vec![0xBA], Inst::Cp(TReg(Reg::D)), 1);
    assert_sm83!(vec![0xBB], Inst::Cp(TReg(Reg::E)), 1);
    assert_sm83!(vec![0xBC], Inst::Cp(TReg(Reg::H)), 1);
    assert_sm83!(vec![0xBD], Inst::Cp(TReg(Reg::L)), 1);
    assert_sm83!(vec![0xBE], Inst::Cp(THL), 1);
    assert_sm83!(vec![0xBF], Inst::Cp(TReg(Reg::A)), 1);

    assert_sm83!(vec![0xC0], Inst::Return(Some(Cond::NZ)), 1);
    assert_sm83!(vec![0xC1], Inst::Pop(Pair::BC), 1);
    assert_sm83!(
        vec![0xC2, 0x34, 0x12],
        Inst::Jump(0x1234, Some(Cond::NZ)),
        3
    );
    assert_sm83!(vec![0xC3, 0x34, 0x12], Inst::Jump(0x1234, None), 3);
    assert_sm83!(
        vec![0xC4, 0x34, 0x12],
        Inst::Call(0x1234, Some(Cond::NZ)),
        3
    );
    assert_sm83!(vec![0xC5], Inst::Push(Pair::BC), 1);
    assert_sm83!(vec![0xC6, 0xC3], Inst::Add8Const(0xC3), 2);
    assert_sm83!(vec![0xC7], Inst::CallRst(0x00), 1);

    assert_sm83!(vec![0xC8], Inst::Return(Some(Cond::Z)), 1);
    assert_sm83!(vec![0xC9], Inst::Return(None), 1);
    assert_sm83!(vec![0xCA, 0x34, 0x12], Inst::Jump(0x1234, Some(Cond::Z)), 3);

    assert_sm83!(vec![0xCB, 0x00], Inst::RotateLeftCarry(TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x01], Inst::RotateLeftCarry(TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x02], Inst::RotateLeftCarry(TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x03], Inst::RotateLeftCarry(TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x04], Inst::RotateLeftCarry(TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x05], Inst::RotateLeftCarry(TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x06], Inst::RotateLeftCarry(THL), 2);
    assert_sm83!(vec![0xCB, 0x07], Inst::RotateLeftCarry(TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x08], Inst::RotateRightCarry(TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x09], Inst::RotateRightCarry(TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x0A], Inst::RotateRightCarry(TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x0B], Inst::RotateRightCarry(TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x0C], Inst::RotateRightCarry(TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x0D], Inst::RotateRightCarry(TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x0E], Inst::RotateRightCarry(THL), 2);
    assert_sm83!(vec![0xCB, 0x0F], Inst::RotateRightCarry(TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x10], Inst::RotateLeft(TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x11], Inst::RotateLeft(TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x12], Inst::RotateLeft(TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x13], Inst::RotateLeft(TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x14], Inst::RotateLeft(TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x15], Inst::RotateLeft(TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x16], Inst::RotateLeft(THL), 2);
    assert_sm83!(vec![0xCB, 0x17], Inst::RotateLeft(TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x18], Inst::RotateRight(TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x19], Inst::RotateRight(TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x1A], Inst::RotateRight(TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x1B], Inst::RotateRight(TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x1C], Inst::RotateRight(TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x1D], Inst::RotateRight(TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x1E], Inst::RotateRight(THL), 2);
    assert_sm83!(vec![0xCB, 0x1F], Inst::RotateRight(TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x20], Inst::ShiftLeftArithmetic(TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x21], Inst::ShiftLeftArithmetic(TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x22], Inst::ShiftLeftArithmetic(TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x23], Inst::ShiftLeftArithmetic(TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x24], Inst::ShiftLeftArithmetic(TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x25], Inst::ShiftLeftArithmetic(TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x26], Inst::ShiftLeftArithmetic(THL), 2);
    assert_sm83!(vec![0xCB, 0x27], Inst::ShiftLeftArithmetic(TReg(Reg::A)), 2);

    assert_sm83!(
        vec![0xCB, 0x28],
        Inst::ShiftRightArithmetic(TReg(Reg::B)),
        2
    );
    assert_sm83!(
        vec![0xCB, 0x29],
        Inst::ShiftRightArithmetic(TReg(Reg::C)),
        2
    );
    assert_sm83!(
        vec![0xCB, 0x2A],
        Inst::ShiftRightArithmetic(TReg(Reg::D)),
        2
    );
    assert_sm83!(
        vec![0xCB, 0x2B],
        Inst::ShiftRightArithmetic(TReg(Reg::E)),
        2
    );
    assert_sm83!(
        vec![0xCB, 0x2C],
        Inst::ShiftRightArithmetic(TReg(Reg::H)),
        2
    );
    assert_sm83!(
        vec![0xCB, 0x2D],
        Inst::ShiftRightArithmetic(TReg(Reg::L)),
        2
    );
    assert_sm83!(vec![0xCB, 0x2E], Inst::ShiftRightArithmetic(THL), 2);
    assert_sm83!(
        vec![0xCB, 0x2F],
        Inst::ShiftRightArithmetic(TReg(Reg::A)),
        2
    );

    assert_sm83!(vec![0xCB, 0x30], Inst::NybbleSwap(TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x31], Inst::NybbleSwap(TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x32], Inst::NybbleSwap(TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x33], Inst::NybbleSwap(TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x34], Inst::NybbleSwap(TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x35], Inst::NybbleSwap(TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x36], Inst::NybbleSwap(THL), 2);
    assert_sm83!(vec![0xCB, 0x37], Inst::NybbleSwap(TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x38], Inst::ShiftRightLogical(TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x39], Inst::ShiftRightLogical(TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x3A], Inst::ShiftRightLogical(TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x3B], Inst::ShiftRightLogical(TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x3C], Inst::ShiftRightLogical(TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x3D], Inst::ShiftRightLogical(TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x3E], Inst::ShiftRightLogical(THL), 2);
    assert_sm83!(vec![0xCB, 0x3F], Inst::ShiftRightLogical(TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x40], Inst::BitTest(0, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x41], Inst::BitTest(0, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x42], Inst::BitTest(0, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x43], Inst::BitTest(0, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x44], Inst::BitTest(0, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x45], Inst::BitTest(0, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x46], Inst::BitTest(0, THL), 2);
    assert_sm83!(vec![0xCB, 0x47], Inst::BitTest(0, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x48], Inst::BitTest(1, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x49], Inst::BitTest(1, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x4A], Inst::BitTest(1, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x4B], Inst::BitTest(1, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x4C], Inst::BitTest(1, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x4D], Inst::BitTest(1, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x4E], Inst::BitTest(1, THL), 2);
    assert_sm83!(vec![0xCB, 0x4F], Inst::BitTest(1, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x50], Inst::BitTest(2, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x51], Inst::BitTest(2, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x52], Inst::BitTest(2, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x53], Inst::BitTest(2, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x54], Inst::BitTest(2, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x55], Inst::BitTest(2, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x56], Inst::BitTest(2, THL), 2);
    assert_sm83!(vec![0xCB, 0x57], Inst::BitTest(2, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x58], Inst::BitTest(3, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x59], Inst::BitTest(3, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x5A], Inst::BitTest(3, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x5B], Inst::BitTest(3, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x5C], Inst::BitTest(3, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x5D], Inst::BitTest(3, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x5E], Inst::BitTest(3, THL), 2);
    assert_sm83!(vec![0xCB, 0x5F], Inst::BitTest(3, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x60], Inst::BitTest(4, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x61], Inst::BitTest(4, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x62], Inst::BitTest(4, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x63], Inst::BitTest(4, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x64], Inst::BitTest(4, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x65], Inst::BitTest(4, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x66], Inst::BitTest(4, THL), 2);
    assert_sm83!(vec![0xCB, 0x67], Inst::BitTest(4, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x68], Inst::BitTest(5, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x69], Inst::BitTest(5, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x6A], Inst::BitTest(5, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x6B], Inst::BitTest(5, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x6C], Inst::BitTest(5, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x6D], Inst::BitTest(5, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x6E], Inst::BitTest(5, THL), 2);
    assert_sm83!(vec![0xCB, 0x6F], Inst::BitTest(5, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x70], Inst::BitTest(6, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x71], Inst::BitTest(6, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x72], Inst::BitTest(6, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x73], Inst::BitTest(6, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x74], Inst::BitTest(6, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x75], Inst::BitTest(6, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x76], Inst::BitTest(6, THL), 2);
    assert_sm83!(vec![0xCB, 0x77], Inst::BitTest(6, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x78], Inst::BitTest(7, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x79], Inst::BitTest(7, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x7A], Inst::BitTest(7, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x7B], Inst::BitTest(7, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x7C], Inst::BitTest(7, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x7D], Inst::BitTest(7, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x7E], Inst::BitTest(7, THL), 2);
    assert_sm83!(vec![0xCB, 0x7F], Inst::BitTest(7, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x80], Inst::BitReset(0, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x81], Inst::BitReset(0, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x82], Inst::BitReset(0, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x83], Inst::BitReset(0, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x84], Inst::BitReset(0, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x85], Inst::BitReset(0, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x86], Inst::BitReset(0, THL), 2);
    assert_sm83!(vec![0xCB, 0x87], Inst::BitReset(0, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x88], Inst::BitReset(1, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x89], Inst::BitReset(1, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x8A], Inst::BitReset(1, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x8B], Inst::BitReset(1, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x8C], Inst::BitReset(1, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x8D], Inst::BitReset(1, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x8E], Inst::BitReset(1, THL), 2);
    assert_sm83!(vec![0xCB, 0x8F], Inst::BitReset(1, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x90], Inst::BitReset(2, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x91], Inst::BitReset(2, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x92], Inst::BitReset(2, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x93], Inst::BitReset(2, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x94], Inst::BitReset(2, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x95], Inst::BitReset(2, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x96], Inst::BitReset(2, THL), 2);
    assert_sm83!(vec![0xCB, 0x97], Inst::BitReset(2, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0x98], Inst::BitReset(3, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0x99], Inst::BitReset(3, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0x9A], Inst::BitReset(3, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0x9B], Inst::BitReset(3, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0x9C], Inst::BitReset(3, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0x9D], Inst::BitReset(3, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0x9E], Inst::BitReset(3, THL), 2);
    assert_sm83!(vec![0xCB, 0x9F], Inst::BitReset(3, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0xA0], Inst::BitReset(4, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0xA1], Inst::BitReset(4, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0xA2], Inst::BitReset(4, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0xA3], Inst::BitReset(4, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0xA4], Inst::BitReset(4, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0xA5], Inst::BitReset(4, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0xA6], Inst::BitReset(4, THL), 2);
    assert_sm83!(vec![0xCB, 0xA7], Inst::BitReset(4, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0xA8], Inst::BitReset(5, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0xA9], Inst::BitReset(5, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0xAA], Inst::BitReset(5, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0xAB], Inst::BitReset(5, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0xAC], Inst::BitReset(5, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0xAD], Inst::BitReset(5, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0xAE], Inst::BitReset(5, THL), 2);
    assert_sm83!(vec![0xCB, 0xAF], Inst::BitReset(5, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0xB0], Inst::BitReset(6, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0xB1], Inst::BitReset(6, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0xB2], Inst::BitReset(6, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0xB3], Inst::BitReset(6, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0xB4], Inst::BitReset(6, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0xB5], Inst::BitReset(6, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0xB6], Inst::BitReset(6, THL), 2);
    assert_sm83!(vec![0xCB, 0xB7], Inst::BitReset(6, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0xB8], Inst::BitReset(7, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0xB9], Inst::BitReset(7, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0xBA], Inst::BitReset(7, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0xBB], Inst::BitReset(7, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0xBC], Inst::BitReset(7, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0xBD], Inst::BitReset(7, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0xBE], Inst::BitReset(7, THL), 2);
    assert_sm83!(vec![0xCB, 0xBF], Inst::BitReset(7, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0xC0], Inst::BitSet(0, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0xC1], Inst::BitSet(0, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0xC2], Inst::BitSet(0, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0xC3], Inst::BitSet(0, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0xC4], Inst::BitSet(0, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0xC5], Inst::BitSet(0, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0xC6], Inst::BitSet(0, THL), 2);
    assert_sm83!(vec![0xCB, 0xC7], Inst::BitSet(0, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0xC8], Inst::BitSet(1, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0xC9], Inst::BitSet(1, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0xCA], Inst::BitSet(1, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0xCB], Inst::BitSet(1, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0xCC], Inst::BitSet(1, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0xCD], Inst::BitSet(1, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0xCE], Inst::BitSet(1, THL), 2);
    assert_sm83!(vec![0xCB, 0xCF], Inst::BitSet(1, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0xD0], Inst::BitSet(2, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0xD1], Inst::BitSet(2, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0xD2], Inst::BitSet(2, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0xD3], Inst::BitSet(2, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0xD4], Inst::BitSet(2, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0xD5], Inst::BitSet(2, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0xD6], Inst::BitSet(2, THL), 2);
    assert_sm83!(vec![0xCB, 0xD7], Inst::BitSet(2, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0xD8], Inst::BitSet(3, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0xD9], Inst::BitSet(3, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0xDA], Inst::BitSet(3, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0xDB], Inst::BitSet(3, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0xDC], Inst::BitSet(3, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0xDD], Inst::BitSet(3, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0xDE], Inst::BitSet(3, THL), 2);
    assert_sm83!(vec![0xCB, 0xDF], Inst::BitSet(3, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0xE0], Inst::BitSet(4, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0xE1], Inst::BitSet(4, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0xE2], Inst::BitSet(4, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0xE3], Inst::BitSet(4, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0xE4], Inst::BitSet(4, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0xE5], Inst::BitSet(4, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0xE6], Inst::BitSet(4, THL), 2);
    assert_sm83!(vec![0xCB, 0xE7], Inst::BitSet(4, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0xE8], Inst::BitSet(5, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0xE9], Inst::BitSet(5, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0xEA], Inst::BitSet(5, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0xEB], Inst::BitSet(5, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0xEC], Inst::BitSet(5, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0xED], Inst::BitSet(5, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0xEE], Inst::BitSet(5, THL), 2);
    assert_sm83!(vec![0xCB, 0xEF], Inst::BitSet(5, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0xF0], Inst::BitSet(6, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0xF1], Inst::BitSet(6, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0xF2], Inst::BitSet(6, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0xF3], Inst::BitSet(6, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0xF4], Inst::BitSet(6, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0xF5], Inst::BitSet(6, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0xF6], Inst::BitSet(6, THL), 2);
    assert_sm83!(vec![0xCB, 0xF7], Inst::BitSet(6, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCB, 0xF8], Inst::BitSet(7, TReg(Reg::B)), 2);
    assert_sm83!(vec![0xCB, 0xF9], Inst::BitSet(7, TReg(Reg::C)), 2);
    assert_sm83!(vec![0xCB, 0xFA], Inst::BitSet(7, TReg(Reg::D)), 2);
    assert_sm83!(vec![0xCB, 0xFB], Inst::BitSet(7, TReg(Reg::E)), 2);
    assert_sm83!(vec![0xCB, 0xFC], Inst::BitSet(7, TReg(Reg::H)), 2);
    assert_sm83!(vec![0xCB, 0xFD], Inst::BitSet(7, TReg(Reg::L)), 2);
    assert_sm83!(vec![0xCB, 0xFE], Inst::BitSet(7, THL), 2);
    assert_sm83!(vec![0xCB, 0xFF], Inst::BitSet(7, TReg(Reg::A)), 2);

    assert_sm83!(vec![0xCC, 0x34, 0x12], Inst::Call(0x1234, Some(Cond::Z)), 3);
    assert_sm83!(vec![0xCD, 0x34, 0x12], Inst::Call(0x1234, None), 3);
    assert_sm83!(vec![0xCE, 0xC3], Inst::AddCarry8Const(0xC3), 2);
    assert_sm83!(vec![0xCF], Inst::CallRst(0x08), 1);

    assert_sm83!(vec![0xD0], Inst::Return(Some(Cond::NC)), 1);
    assert_sm83!(vec![0xD1], Inst::Pop(Pair::DE), 1);
    assert_sm83!(
        vec![0xD2, 0x34, 0x12],
        Inst::Jump(0x1234, Some(Cond::NC)),
        3
    );
    assert_sm83_err!(vec![0xD3], Error::InvalidInstruction);
    assert_sm83!(
        vec![0xD4, 0x34, 0x12],
        Inst::Call(0x1234, Some(Cond::NC)),
        3
    );
    assert_sm83!(vec![0xD5], Inst::Push(Pair::DE), 1);
    assert_sm83!(vec![0xD6, 0xC3], Inst::Sub8Const(0xC3), 2);
    assert_sm83!(vec![0xD7], Inst::CallRst(0x10), 1);

    assert_sm83!(vec![0xD8], Inst::Return(Some(Cond::C)), 1);
    assert_sm83!(vec![0xD9], Inst::ReturnFromInterrupt, 1);
    assert_sm83!(vec![0xDA, 0x34, 0x12], Inst::Jump(0x1234, Some(Cond::C)), 3);
    assert_sm83_err!(vec![0xDB], Error::InvalidInstruction);
    assert_sm83!(vec![0xDC, 0x34, 0x12], Inst::Call(0x1234, Some(Cond::C)), 3);
    assert_sm83_err!(vec![0xDD], Error::InvalidInstruction);
    assert_sm83!(vec![0xDE, 0xC3], Inst::SubCarry8Const(0xC3), 2);
    assert_sm83!(vec![0xDF], Inst::CallRst(0x18), 1);

    assert_sm83!(vec![0xE0, 0xAC], Inst::LdWriteHiStatic(0xAC), 2);
    assert_sm83!(vec![0xE1], Inst::Pop(Pair::HL), 1);
    assert_sm83!(vec![0xE2], Inst::LdWriteHiPtr, 1);
    assert_sm83_err!(vec![0xE3], Error::InvalidInstruction);
    assert_sm83_err!(vec![0xE4], Error::InvalidInstruction);
    assert_sm83!(vec![0xE5], Inst::Push(Pair::HL), 1);
    assert_sm83!(vec![0xE6, 0xC3], Inst::And8Const(0xC3), 2);
    assert_sm83!(vec![0xE7], Inst::CallRst(0x20), 1);

    assert_sm83!(vec![0xE8, 0xFF], Inst::AddSpConst(-1), 2);
    assert_sm83!(vec![0xE9], Inst::JumpDynamic, 1);
    assert_sm83!(vec![0xEA, 0x34, 0x12], Inst::LdWriteStatic(0x1234), 3);
    assert_sm83_err!(vec![0xEB], Error::InvalidInstruction);
    assert_sm83_err!(vec![0xEC], Error::InvalidInstruction);
    assert_sm83_err!(vec![0xED], Error::InvalidInstruction);
    assert_sm83!(vec![0xEE, 0xC3], Inst::Xor8Const(0xC3), 2);
    assert_sm83!(vec![0xEF], Inst::CallRst(0x28), 1);

    assert_sm83!(vec![0xF0, 0xAC], Inst::LdReadHiStatic(0xAC), 2);
    assert_sm83!(vec![0xF1], Inst::Pop(Pair::AF), 1);
    assert_sm83!(vec![0xF2], Inst::LdReadHiPtr, 1);
    assert_sm83!(vec![0xF3], Inst::DisableInterrupt, 1);
    assert_sm83_err!(vec![0xF4], Error::InvalidInstruction);
    assert_sm83!(vec![0xF5], Inst::Push(Pair::AF), 1);
    assert_sm83!(vec![0xF6, 0xC3], Inst::Or8Const(0xC3), 2);
    assert_sm83!(vec![0xF7], Inst::CallRst(0x30), 1);

    assert_sm83!(vec![0xF8, 0xFF], Inst::LdHLSP(-1), 2);
    assert_sm83!(vec![0xF9], Inst::LdSPHL, 1);
    assert_sm83!(vec![0xFA, 0x34, 0x12], Inst::LdReadStatic(0x1234), 3);
    assert_sm83!(vec![0xFB], Inst::EnableInterrupt, 1);
    assert_sm83_err!(vec![0xFC], Error::InvalidInstruction);
    assert_sm83_err!(vec![0xFD], Error::InvalidInstruction);
    assert_sm83!(vec![0xFE, 0xC3], Inst::CpConst(0xC3), 2);
    assert_sm83!(vec![0xFF], Inst::CallRst(0x38), 1);
}
