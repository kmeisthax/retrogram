//! ARM, formerly an acronym of Acorn RISC Machine, and a quite popular ISA.
//! 
//! This only covers 32-bit ARM, now known as AArch32.

use std::string::ToString;
use crate::retrogram::{memory, ast, reg};

enum Aarch32Register {
    R0, R1, R2, R3,
    R4, R5, R6, R7,
    R8, R9, R10, R11,
    R12, R13, R14, R15,
}

impl Aarch32Register {
    pub fn from_instr(reg: u32) -> Option<Aarch32Register> {
        use Aarch32Register::*;

        match reg {
            0 => Some(R0),
            1 => Some(R1),
            2 => Some(R2),
            3 => Some(R3),
            4 => Some(R4),
            5 => Some(R5),
            6 => Some(R6),
            7 => Some(R7),
            8 => Some(R8),
            9 => Some(R9),
            10 => Some(R10),
            11 => Some(R11),
            12 => Some(R12),
            13 => Some(R13),
            14 => Some(R14),
            15 => Some(R15),
            _ => None
        }
    }
}

impl ToString for Aarch32Register {
    fn to_string(&self) -> String {
        match self {
            R0 => "R0".to_string(),
            R1 => "R1".to_string(),
            R2 => "R2".to_string(),
            R3 => "R3".to_string(),
            R4 => "R4".to_string(),
            R5 => "R5".to_string(),
            R6 => "R6".to_string(),
            R7 => "R7".to_string(),
            R8 => "R8".to_string(),
            R9 => "R9".to_string(),
            R10 => "R10".to_string(),
            R11 => "R11".to_string(),
            R12 => "R12".to_string(),
            R13 => "R13".to_string(),
            R14 => "R14".to_string(),
            R15 => "R15".to_string()
        }
    }
}

/// The type which represents a value contained in an ARM AArch32 register.
type Value = i32;

/// The type which represents an ARM AArch32 memory address.
type Pointer = u32;

/// The type which represents a positive memory offset.
type Offset = u32;

/// The type which represents data stored in memory as seen by the processor.
type Data = u8;

/// The compatible memory model type necessary to analyze AArch32 programs.
pub type Bus = memory::Memory<Pointer, Data, Offset>;

/// The AST type which represents a disassembled operand.
pub type Operand = ast::Operand<Offset, Value, f32, Pointer>;

/// The AST type which represents a disassembled instruction.
pub type Instruction = ast::Instruction<Offset, Value, f32, Pointer>;

fn shifter_operand(rn: Aarch32Register, rd: Aarch32Register, immediate_bit: u32, shifter_operand: u32) -> Vec<Operand> {
    let rotate_imm = (shifter_operand & 0x00000F00) >> 8 * 2; //Rotate immediate. Is shifted for some reason
    let shift_imm = (shifter_operand & 0x00000F80) >> 7;
    let shift = (shifter_operand & 0x00000060) >> 5; //Shift type
    let is_shift_immed = (shifter_operand & 0x00000010) >> 4; //Shift is immediate (0) or register (1)
    let rm = Aarch32Register::from_instr((shifter_operand & 0x0000000F) >> 0).expect("Could not parse RM... somehow?!");
    let immed_8 = (shifter_operand & 0x000000FF) >> 0; //Data Immediate

    match (immediate_bit, shift, shift_imm, is_shift_immed) {
        (1, _, _, _) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::int(immed_8 << rotate_imm)),
        (0, 0, 0, 0) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::sym(&rm.to_string()), Operand::sym("LSL")),
        (0, 0, _, 0) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::sym(&rm.to_string()), Operand::sym("LSL"), Operand::int(shift_imm)),
        (0, 0, _, 1) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::sym(&rm.to_string()), Operand::sym("LSL"), Operand::sym(&rm.to_string())),
        (0, 1, 0, 0) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::sym(&rm.to_string()), Operand::sym("LSR")),
        (0, 1, _, 0) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::sym(&rm.to_string()), Operand::sym("LSR"), Operand::int(shift_imm)),
        (0, 1, _, 1) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::sym(&rm.to_string()), Operand::sym("LSR"), Operand::sym(&rm.to_string())),
        (0, 2, 0, 0) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::sym(&rm.to_string()), Operand::sym("ASR")),
        (0, 2, _, 0) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::sym(&rm.to_string()), Operand::sym("ASR"), Operand::int(shift_imm)),
        (0, 2, _, 1) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::sym(&rm.to_string()), Operand::sym("ASR"), Operand::sym(&rm.to_string())),
        (0, 3, 0, 0) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::sym(&rm.to_string()), Operand::sym("RRX")),
        (0, 3, _, 0) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::sym(&rm.to_string()), Operand::sym("ROR"), Operand::int(shift_imm)),
        (0, 3, _, 1) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::sym(&rm.to_string()), Operand::sym("ROR"), Operand::sym(&rm.to_string())),
        _ => vec!(Operand::miss())
    }
}

/// Decode a 5-bit opcode field as if it was for a data processing instruction
fn dp_opcode(instr: u32) -> &'static str {
    match instr {
        0 => "AND",
        1 => "ANDS",
        2 => "EOR",
        3 => "EORS",
        4 => "SUB",
        5 => "SUBS",
        6 => "RSB",
        7 => "RSBS",
        8 => "ADD",
        9 => "ADDS",
        10 => "ADC",
        11 => "ADCS",
        12 => "SBC",
        13 => "SBCS",
        14 => "RSC",
        15 => "RSCS",
        16 => panic!("This is not a data processing instruction (TST without S)"),
        17 => "TST",
        18 => panic!("This is not a data processing instruction (TEQ without S)"),
        19 => "TEQ",
        20 => panic!("This is not a data processing instruction (CMP without S)"),
        21 => "CMP",
        22 => panic!("This is not a data processing instruction (CMN without S)"),
        23 => "CMN",
        24 => "ORR",
        25 => "ORRS",
        26 => "MOV",
        27 => "MOVS",
        28 => "BIC",
        29 => "BICS",
        30 => "MVN",
        31 => "MVNS",
        _ => panic!("Wait, why did you try to decode an AArch32 opcode longer than 5 bits?")
    }
}

fn condcode(instr: u32) -> &'static str {
    match instr {
        0 => "EQ",
        1 => "NE",
        2 => "CS",
        3 => "CC",
        4 => "MI",
        5 => "PL",
        6 => "VS",
        7 => "VC",
        8 => "HI",
        9 => "LS",
        10 => "GE",
        11 => "LT",
        12 => "GT",
        13 => "LE",
        14 => "AL",
        15 => panic!("Condition code not valid for conditional instruction"),
        _ => panic!("Not a valid condition code")
    }
}

pub fn ls_opcode(opcode: u32, immediate_bit: u32) -> &'static str {
    let is_load = opcode & 0x01 == 0x01;
    let is_byte = opcode & 0x04 == 0x04;
    let is_preindex = opcode & 0x10 == 0x10;
    let is_wbit = opcode & 0x02 == 0x02;

    match (is_load, is_byte, is_preindex, is_wbit) {
        (true, true, true, _) => "LDRB",
        (true, false, true, _) => "LDR",
        (false, true, true, _) => "STRB",
        (false, false, true, _) => "STR",
        (true, true, false, true) => "LDRBT",
        (true, false, false, true) => "LDRT",
        (false, true, false, true) => "STRBT",
        (false, false, false, true) => "STRT",
        (true, true, false, false) => "LDRB",
        (true, false, false, false) => "LDR",
        (false, true, false, false) => "STRB",
        (false, false, false, false) => "STR"
    }
}

fn address_operand(rn: Aarch32Register, rd: Aarch32Register, opcode: u32, immediate_bit: u32, address_operand: u32) -> Vec<Operand> {
    let is_preindex = opcode & 0x10 == 0x10;
    let is_offsetadd = opcode & 0x08 == 0x08;
    let is_wbit = opcode & 0x02 == 0x02;
    let offset12 = match is_offsetadd {
        true => (address_operand & 0xFFF) as i32,
        false => (address_operand & 0xFFF) as i32 * -1
    };
    
    match (immediate_bit, is_preindex, is_wbit) {
        (0, _, _) => vec![ast::Operand::sym(&rd.to_string()), ast::Operand::sym(&rn.to_string()), ast::Operand::sint(offset12)],
        (1, true, false) => panic!("unimp"),
        _ => panic!("not yet")
    }
}

/// Disassemble the instruction at `p` in `mem`.
/// 
/// This function returns:
/// 
///  * A string representation of the instruction encountered, if there is a
///    valid instruction at P; otherwise `None`
///  * The size of the current instruction
///  * True, if execution would continue at the instruction following this one,
///    or false if the instruction terminates the current basic block
pub fn disassemble(p: &memory::Pointer<Pointer>, mem: &Bus) -> (Option<Instruction>, Offset, bool) {
    let instr : reg::Symbolic<u32> = mem.read_leword(p);

    if let Some(instr) = instr.into_concrete() {
        let cond = (instr & 0xF0000000) >> 28;
        let opcat = (instr & 0x0E000000) >> 25;
        let opcode = (instr & 0x01F00000) >> 20;
        let rn = Aarch32Register::from_instr((instr & 0x000F0000) >> 16).expect("What the heck? The register definitely should have parsed");
        let rd = Aarch32Register::from_instr((instr & 0x0000F000) >> 12).expect("What the heck? The register definitely should have parsed");
        let lsimmed = (instr & 0x00000FFF) >> 0; //Load-Store Immediate (12bit)
        let is_misc = opcode & 0x19 == 0x10; //invalid S bit for opcode
        let is_multiply = lsimmed & 0x90 == 0x90; //invalid shifter operand
        let is_mediabit = lsimmed & 0x10 == 0x10; //also is for indicating a coprocessor register xfr
        let is_undefine = opcode & 0x1B == 0x10; //invalid S bit and not mov-imm
        let is_archudef = opcode == 0x1F && lsimmed & 0x0F0 == 0x0F0;
        let is_swilink_ = opcode & 0x10 == 0x10; //Upper bit of opcode indicates link and SWI

        match (cond, opcat) {
            (0xF, _) => (None, 0, false), //Unconditionally executed extension space
            (_, 0) if is_multiply => (None, 0, false), //Multiply/LS extension space
            (_, 0) if is_misc => (None, 0, false), //Misc extension space
            //TODO: Misc is supposed to be opcode 0b10xx, but the ARM ARM instruction encoding also says some bits of the shift operand should be set too.
            (_, 0) => (Some(Instruction::new(&format!("{}{}", dp_opcode(opcode), condcode(cond)), shifter_operand(rn, rd, 0, lsimmed))), 4, true), //Data processing with shift
            (_, 1) if is_misc & is_undefine => (None, 0, false), //Undefined (as of ARM DDI 0100I)
            (_, 1) if is_misc => (None, 0, false), //Move to status register
            (_, 1) => (Some(Instruction::new(&format!("{}{}", dp_opcode(opcode), condcode(cond)), shifter_operand(rn, rd, 1, lsimmed))), 4, true), //Data processing with immediate
            (_, 2) => (None, 0, false), //Load/store with immediate offset
            (_, 3) if is_archudef => (None, 0, false), //Architecturally undefined space
            (_, 3) if is_mediabit => (None, 0, false), //Media extension space
            (_, 3) => (None, 0, false), //Load/store with register offset
            (_, 4) => (None, 0, false), //Load/store multiple
            (_, 5) => (None, 0, false), //Branch with or without link
            (_, 6) => (None, 0, false), //Coprocessor load/store and doubleword xfrs
            (_, 7) if is_swilink_ => (None, 0, false), //Software interrupt
            (_, 7) if is_mediabit => (None, 0, false), //Coprocessor register transfer
            (_, 7) => (None, 0, false), //Coprocessor data processing
            _ => (None, 0, false)
        }
    } else {
        (None, 0, false)
    }
}