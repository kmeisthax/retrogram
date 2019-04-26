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
type Value = u32;

/// The type which represents an ARM AArch32 memory address.
type Pointer = u32;

/// The type which represents a positive memory offset.
type Offset = u32;

/// The type which represents data stored in memory as seen by the processor.
type Data = u8;

/// The compatible memory model type necessary to analyze AArch32 programs.
pub type Bus = memory::Memory<Pointer, Data, Offset>;

/// The AST type which represents a disassembled operand.
pub type Operand = ast::Operand<Offset, f32, Pointer>;

/// The AST type which represents a disassembled instruction.
pub type Instruction = ast::Instruction<Offset, f32, Pointer>;

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
        let linkbit = (instr & 0x01000000) >> 24;
        let opcode = (instr & 0x01E00000) >> 21; //Opcode for data-processing instructions
        let sbit = (instr & 0x00100000) >> 20; //Bit which indicates if condition codes update or no
        let rn = Aarch32Register::from_instr((instr & 0x000F0000) >> 16).expect("What the heck? The register definitely should have parsed");
        let rd = Aarch32Register::from_instr((instr & 0x0000F000) >> 12).expect("What the heck? The register definitely should have parsed");
        let rs = Aarch32Register::from_instr((instr & 0x00000F00) >> 8).expect("What the heck? The register definitely should have parsed");
        let braimmed = (instr & 0x00FFFFFF) >> 0;//Branch Immediate (24bit)
        let lsimmed = (instr & 0x00000FFF) >> 0; //Load-Store Immediate (12bit)

        match (cond, opcat) {
            (0xF, _) => (None, 0, false), //Unconditionally executed extension space
            (_, 0) => (Some(Instruction::new("blah", shifter_operand(rn, rd, 0, lsimmed))), 4, true), //Data processing with shift
            (_, 1) => (Some(Instruction::new("blah", shifter_operand(rn, rd, 1, lsimmed))), 4, true), //Data processing with immediate
            (_, 2) => (None, 0, false), //Load/store with immediate offset
            (_, 3) => (None, 0, false), //Load/store with register offset
            (_, 4) => (None, 0, false), //Load/store multiple
            (_, 5) => (None, 0, false), //Branch with or without link
            (_, 6) => (None, 0, false), //Coprocessor load/store and doubleword xfrs
            (_, 7) => (None, 0, false), //Coprocessor data processing, register xfr, and SWI
            _ => (None, 0, false)
        }
    } else {
        (None, 0, false)
    }
}