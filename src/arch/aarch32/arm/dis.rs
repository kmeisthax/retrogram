//! Static disassembler for AArch32

use crate::{memory, analysis, ast, reg};
use crate::arch::aarch32::{Aarch32Register, Pointer, Offset, Operand, Instruction, Bus};
use crate::arch::aarch32::arm::condcode;

fn shift_symbol(shift: u32, shift_imm: u32) -> &'static str {
    match (shift, shift_imm) {
        (0, _) => "LSL",
        (1, _) => "LSR",
        (2, _) => "ASR",
        (3, 0) => "RRX",
        (3, _) => "ROR",
        _ => panic!("This isn't a valid shift symbol...")
    }
}

fn shifter_operand(rn: Aarch32Register, rd: Aarch32Register, immediate_bit: u32, shifter_operand: u32) -> Vec<Operand> {
    let rotate_imm = (shifter_operand & 0x00000F00) >> 8 * 2; //Rotate immediate. Is shifted for some reason
    let shift_imm = (shifter_operand & 0x00000F80) >> 7;
    let shift = (shifter_operand & 0x00000060) >> 5; //Shift type
    let is_shift_immed = (shifter_operand & 0x00000010) >> 4; //Shift is immediate (0) or register (1)
    let rm = Aarch32Register::from_instr((shifter_operand & 0x0000000F) >> 0).expect("Could not parse RM... somehow?!");
    let immed_8 = (shifter_operand & 0x000000FF) >> 0; //Data Immediate

    match (immediate_bit, shift_imm, is_shift_immed) {
        (1, _, _) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::int(immed_8 << rotate_imm)),
        (0, 0, 0) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::sym(&rm.to_string()), Operand::sym(shift_symbol(shift, shift_imm))),
        (0, _, 0) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::sym(&rm.to_string()), Operand::sym(shift_symbol(shift, shift_imm)), Operand::int(shift_imm)),
        (0, _, 1) => vec!(Operand::sym(&rd.to_string()), Operand::sym(&rn.to_string()), Operand::sym(&rm.to_string()), Operand::sym(shift_symbol(shift, shift_imm)), Operand::sym(&rm.to_string())),
        _ => vec!(Operand::miss())
    }
}

/// Decode a 5-bit opcode field as if it was for a data processing instruction
fn decode_dpinst(p: &memory::Pointer<Pointer>, cond: u32, immediate_bit: u32, opcode: u32, rn: Aarch32Register, rd: Aarch32Register, address_operand: u32) -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    let dp_opcode = match opcode {
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
    };

    //Because ARM register 15 is the program counter, writing to it causes a
    //dynamic jump we can't predict statically.
    let target = match rd {
        Aarch32Register::R15 => vec![analysis::Reference::new_dyn_ref(p.clone(), analysis::ReferenceKind::Code)],
        _ => vec![]
    };

    let is_nonbranching = match rd {
        Aarch32Register::R15 => false,
        _ => true
    };

    let is_nonfinal = is_nonbranching || cond != 14;

    (Some(Instruction::new(&format!("{}{}", dp_opcode, condcode(cond)), shifter_operand(rn, rd, immediate_bit, address_operand))), 4, is_nonfinal, is_nonbranching, target)
}

fn decode_ldst(p: &memory::Pointer<Pointer>, cond: u32, immediate_bit: u32, opcode: u32, rn: Aarch32Register, rd: Aarch32Register, address_operand: u32) -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    let shift_imm = (address_operand & 0x00000F80) >> 7;
    let shift = (address_operand & 0x00000060) >> 5; //Shift type
    let rm = Aarch32Register::from_instr((address_operand & 0x0000000F) >> 0).expect("Could not parse RM... somehow?!");
    let is_shifted = shift_imm != 0 || shift != 0;

    let is_load = opcode & 0x01 == 0x01;
    let is_wbit = opcode & 0x02 == 0x02;
    let is_byte = opcode & 0x04 == 0x04;
    let is_offsetadd = opcode & 0x08 == 0x08;
    let is_preindex = opcode & 0x10 == 0x10;

    let offset12 = match is_offsetadd {
        true => (address_operand & 0xFFF) as i32,
        false => (address_operand & 0xFFF) as i32 * -1
    };

    let rd_operand = ast::Operand::sym(&rd.to_string());
    let rn_operand = ast::Operand::sym(&rn.to_string());
    let rm_operand = match is_offsetadd {
        true => ast::Operand::sym(&rm.to_string()),
        false => ast::Operand::pref("-", ast::Operand::sym(&rm.to_string()))
    };

    let lsw_opcode = match (is_load, is_byte, is_preindex, is_wbit) {
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
    };
    
    let address_operand = match (immediate_bit, is_preindex, is_wbit, is_shifted) {
        (0, true, true, false) => vec![rd_operand, ast::Operand::suff(ast::Operand::wrap("[", vec![rn_operand, ast::Operand::sint(offset12)], "]"), "!")],
        (1, true, true, true) => vec![rd_operand, ast::Operand::suff(ast::Operand::wrap("[", vec![rn_operand, rm_operand, Operand::sym(shift_symbol(shift, shift_imm)), Operand::int(shift_imm)], "]"), "!")],
        (1, true, true, false) => vec![rd_operand, ast::Operand::suff(ast::Operand::wrap("[", vec![rn_operand, rm_operand], "]"), "!")],
        (0, true, false, false) => vec![rd_operand, ast::Operand::wrap("[", vec![rn_operand, ast::Operand::sint(offset12)], "]")],
        (1, true, false, true) => vec![rd_operand, ast::Operand::wrap("[", vec![rn_operand, rm_operand, Operand::sym(shift_symbol(shift, shift_imm)), Operand::int(shift_imm)], "]")],
        (1, true, false, false) => vec![rd_operand, ast::Operand::wrap("[", vec![rn_operand, rm_operand], "]")],
        (0, false, _, false) => vec![rd_operand, ast::Operand::wrap("[", vec![rn_operand], "]"), ast::Operand::sint(offset12)],
        (1, false, _, true) => vec![rd_operand, ast::Operand::wrap("[", vec![rn_operand], "]"), rm_operand, Operand::sym(shift_symbol(shift, shift_imm)), Operand::int(shift_imm)],
        (1, false, _, false) => vec![rd_operand, ast::Operand::wrap("[", vec![rn_operand], "]"), rm_operand],
        _ => panic!("Invalid instruction parsing detected. Please contact your system administrator.")
    };

    //Loads into R15 constitute a dynamic jump.
    let targets = match (is_load, rd) {
        (true, Aarch32Register::R15) => vec![analysis::Reference::new_dyn_ref(p.clone(), analysis::ReferenceKind::Code)],
        _ => vec![]
    };

    let is_nonbranching = match (is_load, rd) {
        (true, Aarch32Register::R15) => false,
        _ => true
    };

    let is_nonfinal = is_nonbranching || cond != 14;

    (Some(Instruction::new(&format!("{}{}", lsw_opcode, condcode(cond)), address_operand)), 4, is_nonfinal, is_nonbranching, targets)
}

/// Decode an instruction in the LDM/STM instruction space.
fn decode_ldmstm(p: &memory::Pointer<Pointer>, cond: u32, opcode: u32, rn: Aarch32Register, reglist: u32) -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    let op = match opcode & 0x01 {
        0x00 => "STM",
        0x01 => "LDM",
        _ => return (None, 0, false, true, vec![])
    };

    let p_string = match opcode & 0x10 {
        0x00 => "A",
        0x10 => "B",
        _ => return (None, 0, false, true, vec![])
    };

    let u_string = match opcode & 0x08 {
        0x00 => "D",
        0x08 => "I",
        _ => return (None, 0, false, true, vec![])
    };

    let rn_operand = match opcode & 0x02 {
        0x00 => ast::Operand::sym(&rn.to_string()),
        0x02 => ast::Operand::suff(ast::Operand::sym(&rn.to_string()), "!"),
        _ => return (None, 0, false, true, vec![])
    };

    let mut reglist_operand = Vec::new();
    let mut targets = Vec::new();

    let mut is_nonbranching = true;

    for i in 0..15 {
        if reglist & (1 << i) != 0 {
            reglist_operand.push(ast::Operand::sym(&Aarch32Register::from_instr(i).expect("Counting from 0 to 15 does not result in something from 0 to 15. Check your universe before proceeding.").to_string()));

            //Thanks to PC being an architecturally mentionable register, we
            //have to account for overwriting PC via LDM. Normally this is the
            //moral equivalent of a ret, so it shouldn't be analyzed as a
            //dynamic jump...
            if i == 15 && op == "LDM" {
                is_nonbranching = false;
                targets.push(analysis::Reference::new_dyn_ref(p.clone(), analysis::ReferenceKind::Code))
            }
        }
    }

    let is_nonfinal = is_nonbranching || cond != 14;

    let reglist_operand = match opcode & 0x04 {
        0x00 => ast::Operand::wrap("{", reglist_operand, "}"),
        0x40 => ast::Operand::suff(ast::Operand::wrap("{", reglist_operand, "}"), "^"),
        _ => return (None, 0, false, true, vec![])
    };

    (Some(Instruction::new(&format!("{}{}{}{}", op, condcode(cond), u_string, p_string), vec![rn_operand, reglist_operand])), 0, is_nonfinal, is_nonbranching, targets)
}

fn decode_bl(pc: &memory::Pointer<Pointer>, cond: u32, offset: u32) -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    let is_link = (offset & 0x01000000) != 0;
    let signbit = if ((offset & 0x00800000) >> 23) != 0 { 0xFF800000 } else { 0 };
    let target = pc.contextualize(pc.as_pointer().wrapping_add((offset & 0x007FFFFF) | signbit));

    let is_nonbranching = is_link;
    let is_nonfinal = is_nonbranching || cond != 14;

    match is_link {
        true => (Some(ast::Instruction::new(&format!("BL{}", condcode(cond)), vec![ast::Operand::cptr(target.clone())])), 4, is_nonfinal, is_nonbranching, vec![analysis::Reference::new_static_ref(pc.clone(), target.clone(), analysis::ReferenceKind::Subroutine)]),
        false => (Some(ast::Instruction::new(&format!("B{}", condcode(cond)), vec![ast::Operand::cptr(target.clone())])), 4, is_nonfinal, is_nonbranching, vec![analysis::Reference::new_static_ref(pc.clone(), target, analysis::ReferenceKind::Code)])
    }
}

fn decode_swi(pc: &memory::Pointer<Pointer>, cond: u32, offset: u32) -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    let target = offset & 0x00FFFFFF;
    
    //TODO: The jump target can be in high RAM, how do we handle that?
    (Some(ast::Instruction::new(&format!("SWI{}", condcode(cond)), vec![ast::Operand::int(target)])),
        4, true, true,
        vec![analysis::Reference::new_static_ref(pc.clone(), pc.contextualize(0x00000008), analysis::ReferenceKind::Subroutine)])
}

/// Decode a multiply instruction.
/// 
/// Please note that the instruction space for multiplies specifies the
/// registers in a different order from most instructions. Specifically, `rn`
/// and `rd` are swapped.
fn decode_mul(p: &memory::Pointer<Pointer>, cond: u32, opcode: u32, rd: Aarch32Register, rn: Aarch32Register, mult_operand: u32) -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    let rs = Aarch32Register::from_instr((mult_operand & 0x00000F00) >> 8).expect("Could not parse RS... somehow?!");
    let rm = Aarch32Register::from_instr((mult_operand & 0x0000000F) >> 0).expect("Could not parse RM... somehow?!");

    let rd_operand = ast::Operand::sym(&rd.to_string());
    let rn_operand = ast::Operand::sym(&rn.to_string());
    let rs_operand = ast::Operand::sym(&rs.to_string());
    let rm_operand = ast::Operand::sym(&rm.to_string());

    let is_long = opcode & 0x08 != 0;
    let is_unsigned = opcode & 0x04 != 0;
    let is_fma = opcode & 0x02 != 0;
    let is_status = opcode & 0x01 != 0;

    //According to the ARM ARM, setting rn or rd to R15 is, small-caps,
    //UNPREDICTABLE. We'll represent that as a dynamic jump as usual.
    let targets = match (rd, rn) {
        (_, Aarch32Register::R15) => vec![analysis::Reference::new_dyn_ref(p.clone(), analysis::ReferenceKind::Code)],
        (Aarch32Register::R15, _) => vec![analysis::Reference::new_dyn_ref(p.clone(), analysis::ReferenceKind::Code)],
        _ => vec![]
    };

    let is_nonbranching = match (rd, rn) {
        (_, Aarch32Register::R15) => false,
        (Aarch32Register::R15, _) => false,
        _ => true
    };

    let is_nonfinal = is_nonbranching || cond != 14;

    match (is_long, is_unsigned, is_fma, is_status) {
        (true, true, true, true) => (Some(Instruction::new(&format!("SMLAL{}S", condcode(cond)), vec![rn_operand, rd_operand, rm_operand, rs_operand])), 4, is_nonfinal, is_nonbranching, targets),
        (true, true, true, false) => (Some(Instruction::new(&format!("SMLAL{}", condcode(cond)), vec![rn_operand, rd_operand, rm_operand, rs_operand])), 4, is_nonfinal, is_nonbranching, targets),
        (true, true, false, true) => (Some(Instruction::new(&format!("SMULL{}S", condcode(cond)), vec![rn_operand, rd_operand, rm_operand, rs_operand])), 4, is_nonfinal, is_nonbranching, targets),
        (true, true, false, false) => (Some(Instruction::new(&format!("SMULL{}", condcode(cond)), vec![rn_operand, rd_operand, rm_operand, rs_operand])), 4, is_nonfinal, is_nonbranching, targets),
        (true, false, true, true) => (Some(Instruction::new(&format!("UMLAL{}S", condcode(cond)), vec![rn_operand, rd_operand, rm_operand, rs_operand])), 4, is_nonfinal, is_nonbranching, targets),
        (true, false, true, false) => (Some(Instruction::new(&format!("UMLAL{}", condcode(cond)), vec![rn_operand, rd_operand, rm_operand, rs_operand])), 4, is_nonfinal, is_nonbranching, targets),
        (true, false, false, true) => (Some(Instruction::new(&format!("UMULL{}S", condcode(cond)), vec![rn_operand, rd_operand, rm_operand, rs_operand])), 4, is_nonfinal, is_nonbranching, targets),
        (true, false, false, false) => (Some(Instruction::new(&format!("UMULL{}", condcode(cond)), vec![rn_operand, rd_operand, rm_operand, rs_operand])), 4, is_nonfinal, is_nonbranching, targets),
        (false, true, false, false) => (Some(Instruction::new(&format!("UMAAL{}", condcode(cond)), vec![rn_operand, rd_operand, rm_operand, rs_operand])), 4, is_nonfinal, is_nonbranching, targets),
        (false, false, true, true) => (Some(Instruction::new(&format!("MLA{}S", condcode(cond)), vec![rd_operand, rm_operand, rs_operand, rn_operand])), 4, is_nonfinal, is_nonbranching, targets),
        (false, false, true, false) => (Some(Instruction::new(&format!("MLA{}", condcode(cond)), vec![rd_operand, rm_operand, rs_operand, rn_operand])), 4, is_nonfinal, is_nonbranching, targets),
        (false, false, false, true) => (Some(Instruction::new(&format!("MUL{}S", condcode(cond)), vec![rd_operand, rm_operand, rs_operand])), 4, is_nonfinal, is_nonbranching, targets),
        (false, false, false, false) => (Some(Instruction::new(&format!("MUL{}", condcode(cond)), vec![rd_operand, rm_operand, rs_operand])), 4, is_nonfinal, is_nonbranching, targets),
        _ => (None, 0, false, true, vec![])
    }
}

/// Disassemble the instruction at `p` in `mem`.
/// 
/// This function returns:
/// 
///  * A string representation of the instruction encountered, if there is a
///    valid instruction at P; otherwise `None`
///  * The offset to the next instruction, usually also the size of the current
///    instruction, except for architectures with polynomial program counters.
///  * is_nonfinal: If true, the next instruction is implicitly a target of the
///    current instruction.
///  * is_nonbranching: If true, the current instruction does not end the
///    current block.
///  * A list of pointers containing all statically known jump and call targets
///    from the instruction. Instructions with dynamic or unknown jump targets
///    must be expressed as None. The next instruction is implied as a target
///    if is_nonfinal is returned as True and does not need to be provided here.
pub fn disassemble(p: &memory::Pointer<Pointer>, mem: &Bus) -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    let instr : reg::Symbolic<u32> = mem.read_leword(&p);

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
            (0xF, _) => (None, 0, false, true, vec![]), //Unconditionally executed extension space
            (_, 0) if is_multiply => decode_mul(p, cond, opcode, rn, rd, lsimmed), //Multiply/LS extension space
            (_, 0) if is_misc => (None, 0, false, true, vec![]), //Misc extension space
            //TODO: Misc is supposed to be opcode 0b10xx, but the ARM ARM instruction encoding also says some bits of the shift operand should be set too.
            (_, 0) => decode_dpinst(p, cond, 0, opcode, rn, rd, lsimmed), //Data processing with shift
            (_, 1) if is_misc & is_undefine => (None, 0, false, true, vec![]), //Undefined (as of ARM DDI 0100I)
            (_, 1) if is_misc => (None, 0, false, true, vec![]), //Move to status register
            (_, 1) => decode_dpinst(p, cond, 1, opcode, rn, rd, lsimmed), //Data processing with immediate
            (_, 2) => decode_ldst(p, cond, 0, opcode, rn, rd, lsimmed), //Load/store with immediate offset
            (_, 3) if is_archudef => (None, 0, false, true, vec![]), //Architecturally undefined space
            (_, 3) if is_mediabit => (None, 0, false, true, vec![]), //Media extension space
            (_, 3) => decode_ldst(p, cond, 1, opcode, rn, rd, lsimmed), //Load/store with register offset
            (_, 4) => decode_ldmstm(p, cond, opcode, rn, instr & 0x0000FFFF), //Load/store multiple
            (_, 5) => decode_bl(&p, cond, instr), //Branch with or without link
            (_, 6) => (None, 0, false, true, vec![]), //Coprocessor load/store and doubleword xfrs
            (_, 7) if is_swilink_ => decode_swi(&p, cond, instr), //Software interrupt
            (_, 7) if is_mediabit => (None, 0, false, true, vec![]), //Coprocessor register transfer
            (_, 7) => (None, 0, false, true, vec![]), //Coprocessor data processing
            _ => (None, 0, false, true, vec![])
        }
    } else {
        (None, 0, false, true, vec![])
    }
}