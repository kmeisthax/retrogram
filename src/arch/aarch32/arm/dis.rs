//! Static disassembler for AArch32

use crate::{memory, analysis, ast, reg};
use crate::ast::Operand as op;
use crate::analysis::Reference as refr;
use crate::analysis::ReferenceKind as refkind;
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
        (1, _, _) => vec!(op::sym(&rd.to_string()), op::sym(&rn.to_string()), op::int(immed_8 << rotate_imm)),
        (0, 0, 0) => vec!(op::sym(&rd.to_string()), op::sym(&rn.to_string()), op::sym(&rm.to_string()), op::sym(shift_symbol(shift, shift_imm))),
        (0, _, 0) => vec!(op::sym(&rd.to_string()), op::sym(&rn.to_string()), op::sym(&rm.to_string()), op::sym(shift_symbol(shift, shift_imm)), op::int(shift_imm)),
        (0, _, 1) => vec!(op::sym(&rd.to_string()), op::sym(&rn.to_string()), op::sym(&rm.to_string()), op::sym(shift_symbol(shift, shift_imm)), op::sym(&rm.to_string())),
        _ => vec!(op::miss())
    }
}

/// Decode a 5-bit opcode field as if it was for a data processing instruction
fn decode_dpinst(p: &memory::Pointer<Pointer>,
        cond: u32,
        immediate_bit: u32,
        opcode: u32,
        rn: Aarch32Register,
        rd: Aarch32Register,
        address_operand: u32)
    -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
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
        Aarch32Register::R15 => vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
        _ => vec![]
    };

    let is_nonbranching = match rd {
        Aarch32Register::R15 => false,
        _ => true
    };

    let is_nonfinal = is_nonbranching || cond != 14;

    (Some(Instruction::new(&format!("{}{}", dp_opcode, condcode(cond)), shifter_operand(rn, rd, immediate_bit, address_operand))), 4, is_nonfinal, is_nonbranching, target)
}

fn decode_ldst(p: &memory::Pointer<Pointer>,
        cond: u32,
        immediate_bit: u32,
        preindex: u32,
        offsetadd: u32,
        byte: u32,
        wbit: u32,
        load: u32,
        rn: Aarch32Register,
        rd: Aarch32Register,
        address_operand: u32)
    -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let shift_imm = (address_operand & 0x00000F80) >> 7;
    let shift = (address_operand & 0x00000060) >> 5; //Shift type
    let rm = Aarch32Register::from_instr((address_operand & 0x0000000F) >> 0).expect("Could not parse RM... somehow?!");
    let is_shifted = shift_imm != 0 || shift != 0;

    let is_load = load == 0x01;
    let is_wbit = wbit == 0x02;
    let is_byte = byte == 0x04;
    let is_offsetadd = offsetadd == 0x08;
    let is_preindex = preindex == 0x10;

    let offset12 = match is_offsetadd {
        true => (address_operand & 0xFFF) as i32,
        false => (address_operand & 0xFFF) as i32 * -1
    };

    let rd_operand = op::sym(&rd.to_string());
    let rn_operand = op::sym(&rn.to_string());
    let rm_operand = match is_offsetadd {
        true => op::sym(&rm.to_string()),
        false => op::pref("-", op::sym(&rm.to_string()))
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
        (0, true, true, false) => vec![rd_operand, op::suff(op::wrap("[", vec![rn_operand, op::sint(offset12)], "]"), "!")],
        (1, true, true, true) => vec![rd_operand, op::suff(op::wrap("[", vec![rn_operand, rm_operand, op::sym(shift_symbol(shift, shift_imm)), op::int(shift_imm)], "]"), "!")],
        (1, true, true, false) => vec![rd_operand, op::suff(op::wrap("[", vec![rn_operand, rm_operand], "]"), "!")],
        (0, true, false, false) => vec![rd_operand, op::wrap("[", vec![rn_operand, op::sint(offset12)], "]")],
        (1, true, false, true) => vec![rd_operand, op::wrap("[", vec![rn_operand, rm_operand, op::sym(shift_symbol(shift, shift_imm)), op::int(shift_imm)], "]")],
        (1, true, false, false) => vec![rd_operand, op::wrap("[", vec![rn_operand, rm_operand], "]")],
        (0, false, _, false) => vec![rd_operand, op::wrap("[", vec![rn_operand], "]"), op::sint(offset12)],
        (1, false, _, true) => vec![rd_operand, op::wrap("[", vec![rn_operand], "]"), rm_operand, op::sym(shift_symbol(shift, shift_imm)), op::int(shift_imm)],
        (1, false, _, false) => vec![rd_operand, op::wrap("[", vec![rn_operand], "]"), rm_operand],
        _ => panic!("Invalid instruction parsing detected. Please contact your system administrator.")
    };

    //Loads into R15 constitute a dynamic jump.
    let targets = match (is_load, rd) {
        (true, Aarch32Register::R15) => vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
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
fn decode_ldmstm(p: &memory::Pointer<Pointer>,
        cond: u32,
        q: u32,
        u: u32,
        s: u32,
        w: u32,
        load: u32,
        rn: Aarch32Register,
        reglist: u32)
    -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    //TODO: This got refactored, ensure these are still valid
    let op = match load == 1 {
        false => "STM",
        true => "LDM"
    };

    let p_string = match q == 1 {
        false => "A",
        true => "B"
    };

    let u_string = match u == 1 {
        false => "D",
        true => "I",
    };

    let rn_operand = match w == 1 {
        false => op::sym(&rn.to_string()),
        true => op::suff(op::sym(&rn.to_string()), "!"),
    };

    let mut reglist_operand = Vec::new();
    let mut targets = Vec::new();

    let mut is_nonbranching = true;

    for i in 0..15 {
        if reglist & (1 << i) != 0 {
            reglist_operand.push(op::sym(&Aarch32Register::from_instr(i).expect("Counting from 0 to 15 does not result in something from 0 to 15. Check your universe before proceeding.").to_string()));

            //Thanks to PC being an architecturally mentionable register, we
            //have to account for overwriting PC via LDM. Normally this is the
            //moral equivalent of a ret, so it shouldn't be analyzed as a
            //dynamic jump...
            if i == 15 && op == "LDM" {
                is_nonbranching = false;
                targets.push(refr::new_dyn_ref(p.clone(), refkind::Code))
            }
        }
    }

    let is_nonfinal = is_nonbranching || cond != 14;

    let reglist_operand = match s == 1 {
        false => op::wrap("{", reglist_operand, "}"),
        true => op::suff(op::wrap("{", reglist_operand, "}"), "^")
    };

    (Some(Instruction::new(&format!("{}{}{}{}", op, condcode(cond), u_string, p_string), vec![rn_operand, reglist_operand])), 0, is_nonfinal, is_nonbranching, targets)
}

fn decode_bl(pc: &memory::Pointer<Pointer>, l: u32, cond: u32, offset: u32)
    -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let is_link = l != 0;
    let signbit = if ((offset & 0x00800000) >> 23) != 0 { 0xFF800000 } else { 0 };
    let target = pc.contextualize(pc.as_pointer().wrapping_add((offset & 0x007FFFFF) | signbit));

    let is_nonbranching = is_link;
    let is_nonfinal = is_nonbranching || cond != 14;

    match is_link {
        true => (Some(ast::Instruction::new(&format!("BL{}", condcode(cond)), vec![op::cptr(target.clone())])), 4, is_nonfinal, is_nonbranching, vec![refr::new_static_ref(pc.clone(), target.clone(), refkind::Subroutine)]),
        false => (Some(ast::Instruction::new(&format!("B{}", condcode(cond)), vec![op::cptr(target.clone())])), 4, is_nonfinal, is_nonbranching, vec![refr::new_static_ref(pc.clone(), target, refkind::Code)])
    }
}

fn decode_swi(pc: &memory::Pointer<Pointer>, cond: u32, offset: u32)
    -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let target = offset & 0x00FFFFFF;
    
    //TODO: The jump target can be in high RAM, how do we handle that?
    (Some(ast::Instruction::new(&format!("SWI{}", condcode(cond)), vec![op::int(target)])),
        4, true, true,
        vec![refr::new_static_ref(pc.clone(), pc.contextualize(0x00000008), refkind::Subroutine)])
}

/// Decode a multiply instruction.
/// 
/// Please note that the instruction space for multiplies specifies the
/// registers in a different order from most instructions. Specifically, `rn`
/// and `rd` are swapped.
fn decode_mul(p: &memory::Pointer<Pointer>,
        cond: u32,
        opcode: u32,
        rd: Aarch32Register,
        rn: Aarch32Register,
        mult_operand: u32)
    -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let rs = Aarch32Register::from_instr((mult_operand & 0x00000F00) >> 8).expect("Could not parse RS... somehow?!");
    let rm = Aarch32Register::from_instr((mult_operand & 0x0000000F) >> 0).expect("Could not parse RM... somehow?!");

    let rd_operand = op::sym(&rd.to_string());
    let rn_operand = op::sym(&rn.to_string());
    let rs_operand = op::sym(&rs.to_string());
    let rm_operand = op::sym(&rm.to_string());

    let is_long = opcode & 0x08 != 0;
    let is_unsigned = opcode & 0x04 != 0;
    let is_fma = opcode & 0x02 != 0;
    let is_status = opcode & 0x01 != 0;

    //According to the ARM ARM, setting rn or rd to R15 is, small-caps,
    //UNPREDICTABLE. We'll represent that as a dynamic jump as usual.
    let targets = match (rd, rn) {
        (_, Aarch32Register::R15) => vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
        (Aarch32Register::R15, _) => vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
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

fn msr(cond: u32, immediate_bit: u32, r: u32, rn_val: u32, shifter_operand: u32)
    -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    let c = match (rn_val & 0x1) >> 0 != 0 {
        true => "c",
        false => ""
    };
    let x = match (rn_val & 0x2) >> 1 != 0 {
        true => "x",
        false => ""
    };
    let s = match (rn_val & 0x4) >> 2 != 0 {
        true => "s",
        false => ""
    };
    let f = match (rn_val & 0x8) >> 3 != 0 {
        true => "f",
        false => ""
    };
    let xpsr = match r != 0 {
        false => "CPSR",
        true => "SPSR"
    };

    let rotate_imm = (shifter_operand & 0x00000F00) >> 8 * 2; //Rotate immediate. Is shifted for some reason
    let rm = Aarch32Register::from_instr((shifter_operand & 0x0000000F) >> 0).expect("Could not parse RM... somehow?!");
    let immed_8 = (shifter_operand & 0x000000FF) >> 0; //Data Immediate
    let xpsr = op::sym(&format!("{}_{}{}{}{}", xpsr, c, x, s, f));

    let op_list = match immediate_bit {
        1 => vec!(xpsr, op::int(immed_8 << rotate_imm)),
        0 => vec!(xpsr, op::sym(&rm.to_string())),
        _ => vec!(op::miss())
    };

    (Some(Instruction::new(&format!("MSR{}", condcode(cond)), op_list)), 4, true, true, vec![])
}

fn mrs(cond: u32, r: u32, rd: Aarch32Register)
    -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    let xpsr = match r != 0 {
        false => "CPSR",
        true => "SPSR"
    };
    
    let op_list = vec![op::sym(&rd.to_string()), op::sym(xpsr)];

    (Some(Instruction::new(&format!("MRS{}", condcode(cond)), op_list)), 4, true, true, vec![])
}

fn bx(p: &memory::Pointer<Pointer>, cond: u32, rm: Aarch32Register)
    -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    //BX PC is completely valid! And also dumb.
    let target_pc = p.contextualize(p.as_pointer().clone() + 8);
    let jumpref = match rm {
        Aarch32Register::R15 => refr::new_static_ref(p.clone(), target_pc, refkind::Code),
        _ => refr::new_dyn_ref(p.clone(), refkind::Code)
    };

    (Some(Instruction::new(&format!("BX{}", condcode(cond)), vec![op::sym(&rm.to_string())])), 4, false, false, vec![jumpref])
}

fn bxj(cond: u32, rm: Aarch32Register)
    -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    //While this instruction is technically a branch, it's designed for an
    //obsolete ARM hardware extension for directly executing Java bytecode. No
    //technical details of how Jazelle works have ever been released and
    //analysis of a Jazelle program would probably require adding a JVM disasm
    //that I don't want to write.
    (Some(Instruction::new(&format!("BXJ{}", condcode(cond)), vec![op::sym(&rm.to_string())])), 4, false, false, vec![])
}

fn clz(cond: u32, rd: Aarch32Register, rm: Aarch32Register)
    -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    (Some(Instruction::new(&format!("CLZ{}", condcode(cond)), vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())])), 4, true, true, vec![])
}

fn blx(p: &memory::Pointer<Pointer>, cond: u32, rm: Aarch32Register)
    -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    //BX PC is completely valid! And also dumb.
    let target_pc = p.contextualize(p.as_pointer().clone() + 8);
    let jumpref = match rm {
        Aarch32Register::R15 => refr::new_static_ref(p.clone(), target_pc, refkind::Code),
        _ => refr::new_dyn_ref(p.clone(), refkind::Code)
    };

    (Some(Instruction::new(&format!("BLX{}", condcode(cond)), vec![op::sym(&rm.to_string())])), 4, true, true, vec![jumpref])
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
pub fn disassemble(p: &memory::Pointer<Pointer>, mem: &Bus)
    -> (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let instr : reg::Symbolic<u32> = mem.read_leword(&p);

    if let Some(instr) = instr.into_concrete() {
        let cond = (instr & 0xF0000000) >> 28;
        let opcat = (instr & 0x0E000000) >> 25;
        let opcode = (instr & 0x01F00000) >> 20;
        let pbit = (instr & 0x01000000) >> 24;
        let u = (instr & 0x00800000) >> 23;
        let b = (instr & 0x00400000) >> 22;
        let w = (instr & 0x00200000) >> 21;
        let l = (instr & 0x00100000) >> 20;
        let rn_val = (instr & 0x000F0000) >> 16;
        let rd_val = (instr & 0x0000F000) >> 12;
        let rm_val = (instr & 0x0000000F) >> 0;
        let rn = Aarch32Register::from_instr(rn_val).expect("What the heck? The register definitely should have parsed");
        let rd = Aarch32Register::from_instr(rd_val).expect("What the heck? The register definitely should have parsed");
        let rm = Aarch32Register::from_instr(rm_val).expect("Could not parse RM... somehow?!");
        let lsimmed = (instr & 0x00000FFF) >> 0; //Load-Store Immediate (12bit)
        let miscop = (instr & 0x000000F0) >> 4; //Misc operand bits
        let is_mediabit = lsimmed & 0x10 == 0x10; //also is for indicating a coprocessor register xfr

        match (cond, opcat, pbit, u, b, w, l) {
            (0xF, _, _, _, _, _, _) => (None, 0, false, true, vec![]), //Unconditionally executed extension space
            (_, 0, 1, 0, r, o, 0) => match (r, o, (miscop & 0x8) >> 3, (miscop & 0x7)) {
                (r, 0, 0, 0) => mrs(cond, r, rd),
                (r, 1, 0, 0) => msr(cond, 0, r, rn_val, lsimmed),
                (0, 1, 0, 1) => bx(p, cond, rm), //BX to Thumb
                (0, 1, 0, 2) => bxj(cond, rm), //BX to Jazelle DBX
                (1, 1, 0, 1) => clz(cond, rd, rm), //Count Leading Zeroes
                (0, 1, 0, 3) => blx(p, cond, rm), //BLX to Thumb
                (_, _, 0, 5) => (None, 0, false, true, vec![]), //Saturation arithmetic
                (0, 1, 0, 7) => (None, 0, false, true, vec![]), //Software Breakpoint
                (_, _, 1, c) if (c & 1) == 0 => (None, 0, false, true, vec![]), //Signed multiply
                _ => (None, 0, false, true, vec![])
            },
            (_, 0, q, u, b, w, l) => match (q, (miscop & 0x08) >> 3, (miscop & 0x06) >> 1, (miscop & 1)) {
                (0, 1, 0, 1) => decode_mul(p, cond, opcode, rn, rd, lsimmed),
                (_, 1, op, 1) => (None, 0, false, true, vec![]), //Load/store extension space
                (_, _, sh, 0) => decode_dpinst(p, cond, 0, opcode, rn, rd, lsimmed),
                (_, 0, sh, 1) => decode_dpinst(p, cond, 0, opcode, rn, rd, lsimmed),
                _ => (None, 0, false, true, vec![])
            },
            (_, 1, 1, 0, _, 0, 0) => (None, 0, false, true, vec![]), //Undefined (as of ARM DDI 0100I)
            (_, 1, 1, 0, r, 1, 0) => msr(cond, 1, r, rn_val, lsimmed), //Move to status register
            (_, 1, _, _, _, _, _) => decode_dpinst(p, cond, 1, opcode, rn, rd, lsimmed), //Data processing with immediate
            (_, 2, q, u, b, w, l) => decode_ldst(p, cond, 0, q, u, b, w, l, rn, rd, lsimmed), //Load/store with immediate offset
            (_, 3, 1, 1, 1, 1, 1) if miscop == 0xF => (None, 0, false, true, vec![]), //Architecturally undefined space
            (_, 3, _, _, _, _, _) if is_mediabit => (None, 0, false, true, vec![]), //Media extension space
            (_, 3, q, u, b, w, l) => decode_ldst(p, cond, 1, q, u, b, w, l, rn, rd, lsimmed), //Load/store with register offset
            (_, 4, q, u, s, w, l) => decode_ldmstm(p, cond, q, u, s, w, l, rn, instr & 0x0000FFFF), //Load/store multiple
            (_, 5, l, _, _, _, _) => decode_bl(&p, cond, l, instr), //Branch with or without link
            (_, 6, 0, 0, n, 0, l) => (None, 0, false, true, vec![]), //Coprocessor Load/Store
            (_, 6, q, u, n, w, l) => (None, 0, false, true, vec![]), //Doubleword xfrs
            (_, 7, 1, _, _, _, _) => decode_swi(&p, cond, instr), //Software interrupt
            (_, 7, 0, _, _, _, l) => (None, 0, false, true, vec![]), //Coprocessor instruction
            _ => (None, 0, false, true, vec![])
        }
    } else {
        (None, 0, false, true, vec![])
    }
}