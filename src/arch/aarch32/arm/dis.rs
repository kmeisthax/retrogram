//! Static disassembler for AArch32

#![allow(clippy::too_many_arguments)]

use crate::analysis::Reference as refr;
use crate::analysis::ReferenceKind as refkind;
use crate::arch::aarch32;
use crate::arch::aarch32::arm::condcode;
use crate::arch::aarch32::Aarch32Register as A32Reg;
use crate::arch::aarch32::{Bus, Disasm, Instruction, Offset, Operand, Pointer, THUMB_STATE};
use crate::ast::Operand as op;
use crate::reg::New;
use crate::{analysis, ast, memory, reg};

fn shift_symbol(shift: u32, shift_imm: u32) -> analysis::Result<&'static str, Pointer, Offset> {
    match (shift, shift_imm) {
        (0, _) => Ok("LSL"),
        (1, _) => Ok("LSR"),
        (2, _) => Ok("ASR"),
        (3, 0) => Ok("RRX"),
        (3, _) => Ok("ROR"),
        _ => Err(analysis::Error::Misinterpretation(4, false)),
    }
}

fn shifter_operand(
    immediate_bit: u32,
    rn: A32Reg,
    rd: A32Reg,
    shift_imm: u32,
    regshift: u32,
    shift: u32,
    rm: A32Reg,
    rs: A32Reg,
    immed_8: u32,
) -> analysis::Result<Vec<Operand>, Pointer, Offset> {
    let rotate_imm = shift_imm & 0xFFFF_FFFE;

    Ok(match (immediate_bit, shift_imm, regshift) {
        (1, _, _) => vec![
            op::sym(&rd.to_string()),
            op::sym(&rn.to_string()),
            op::int(immed_8 << rotate_imm),
        ],
        (0, 0, 0) => vec![
            op::sym(&rd.to_string()),
            op::sym(&rn.to_string()),
            op::sym(&rm.to_string()),
            op::sym(shift_symbol(shift, shift_imm)?),
        ],
        (0, _, 0) => vec![
            op::sym(&rd.to_string()),
            op::sym(&rn.to_string()),
            op::sym(&rm.to_string()),
            op::sym(shift_symbol(shift, shift_imm)?),
            op::int(shift_imm),
        ],
        (0, _, 1) => vec![
            op::sym(&rd.to_string()),
            op::sym(&rn.to_string()),
            op::sym(&rm.to_string()),
            op::sym(shift_symbol(shift, shift_imm)?),
            op::sym(&rs.to_string()),
        ],
        _ => vec![op::miss()],
    })
}

/// Decode a 5-bit opcode field as if it was for a data processing instruction
fn dpinst(
    p: &memory::Pointer<Pointer>,
    cond: u32,
    immediate_bit: u32,
    opcode: u32,
    rn: A32Reg,
    rd: A32Reg,
    shift_imm: u32,
    regshift: u32,
    shift: u32,
    rm: A32Reg,
    rs: A32Reg,
    immed_8: u32,
) -> analysis::Result<Disasm, Pointer, Offset> {
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
        16 => return Err(analysis::Error::Misinterpretation(4, false)),
        17 => "TST",
        18 => return Err(analysis::Error::Misinterpretation(4, false)),
        19 => "TEQ",
        20 => return Err(analysis::Error::Misinterpretation(4, false)),
        21 => "CMP",
        22 => return Err(analysis::Error::Misinterpretation(4, false)),
        23 => "CMN",
        24 => "ORR",
        25 => "ORRS",
        26 => "MOV",
        27 => "MOVS",
        28 => "BIC",
        29 => "BICS",
        30 => "MVN",
        31 => "MVNS",
        _ => return Err(analysis::Error::Misinterpretation(4, false)),
    };

    let operands = shifter_operand(
        immediate_bit,
        rn,
        rd,
        shift_imm,
        regshift,
        shift,
        rm,
        rs,
        immed_8,
    )?;

    //Because ARM register 15 is the program counter, writing to it causes a
    //dynamic jump we can't predict statically.
    let target = match rd {
        A32Reg::R15 => vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
        _ => vec![],
    };

    let flow = match rd {
        A32Reg::R15 => analysis::Flow::Branching(cond != 14),
        _ => analysis::Flow::Normal,
    };

    Ok(Disasm::new(
        Instruction::new(&format!("{}{}", dp_opcode, condcode(cond)?), operands),
        4,
        flow,
        target,
    ))
}

fn ldst(
    p: &memory::Pointer<Pointer>,
    cond: u32,
    immediate_bit: u32,
    preindex: u32,
    offsetadd: u32,
    byte: u32,
    wbit: u32,
    load: u32,
    rn: A32Reg,
    rd: A32Reg,
    shift_imm: u32,
    shift: u32,
    rm: A32Reg,
    address_operand: u32,
) -> analysis::Result<Disasm, Pointer, Offset> {
    let is_shifted = shift_imm != 0 || shift != 0;

    let is_load = load != 0;
    let is_wbit = wbit != 0;
    let is_byte = byte != 0;
    let is_offsetadd = offsetadd != 0;
    let is_preindex = preindex != 0;

    let offset12 = if is_offsetadd {
        (address_operand & 0xFFF) as i32
    } else {
        -((address_operand & 0xFFF) as i32)
    };

    let rd_operand = op::sym(&rd.to_string());
    let rn_operand = op::sym(&rn.to_string());
    let rm_operand = if is_offsetadd {
        op::sym(&rm.to_string())
    } else {
        op::pref("-", op::sym(&rm.to_string()))
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
        (false, false, false, false) => "STR",
    };

    let address_operand = match (immediate_bit, is_preindex, is_wbit, is_shifted) {
        (0, true, true, _) => vec![
            rd_operand,
            op::suff(
                op::wrap("[", vec![rn_operand, op::sint(offset12)], "]"),
                "!",
            ),
        ],
        (1, true, true, true) => vec![
            rd_operand,
            op::suff(
                op::wrap(
                    "[",
                    vec![
                        rn_operand,
                        rm_operand,
                        op::sym(shift_symbol(shift, shift_imm)?),
                        op::int(shift_imm),
                    ],
                    "]",
                ),
                "!",
            ),
        ],
        (1, true, true, false) => vec![
            rd_operand,
            op::suff(op::wrap("[", vec![rn_operand, rm_operand], "]"), "!"),
        ],
        (0, true, false, _) => vec![
            rd_operand,
            op::wrap("[", vec![rn_operand, op::sint(offset12)], "]"),
        ],
        (1, true, false, true) => vec![
            rd_operand,
            op::wrap(
                "[",
                vec![
                    rn_operand,
                    rm_operand,
                    op::sym(shift_symbol(shift, shift_imm)?),
                    op::int(shift_imm),
                ],
                "]",
            ),
        ],
        (1, true, false, false) => {
            vec![rd_operand, op::wrap("[", vec![rn_operand, rm_operand], "]")]
        }
        (0, false, _, _) => vec![
            rd_operand,
            op::wrap("[", vec![rn_operand], "]"),
            op::sint(offset12),
        ],
        (1, false, _, true) => vec![
            rd_operand,
            op::wrap("[", vec![rn_operand], "]"),
            rm_operand,
            op::sym(shift_symbol(shift, shift_imm)?),
            op::int(shift_imm),
        ],
        (1, false, _, false) => vec![rd_operand, op::wrap("[", vec![rn_operand], "]"), rm_operand],
        _ => return Err(analysis::Error::Misinterpretation(4, false)),
    };

    //Loads into R15 constitute a dynamic jump.
    let targets = match (is_load, rd) {
        (true, A32Reg::R15) => vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
        _ => vec![],
    };

    let flow = match (is_load, rd) {
        (true, A32Reg::R15) => analysis::Flow::Branching(cond != 14),
        _ => analysis::Flow::Normal,
    };

    Ok(Disasm::new(
        Instruction::new(
            &format!("{}{}", lsw_opcode, condcode(cond)?),
            address_operand,
        ),
        4,
        flow,
        targets,
    ))
}

/// Decode an instruction in the LDM/STM instruction space.
#[allow(clippy::many_single_char_names)]
fn ldmstm(
    p: &memory::Pointer<Pointer>,
    cond: u32,
    q: u32,
    u: u32,
    s: u32,
    w: u32,
    load: u32,
    rn: A32Reg,
    reglist: u32,
) -> analysis::Result<Disasm, Pointer, Offset> {
    //TODO: This got refactored, ensure these are still valid
    let op = if load == 1 { "LDM" } else { "STM" };

    let p_string = if q == 1 { "B" } else { "A" };

    let u_string = if u == 1 { "I" } else { "D" };

    let rn_operand = if w == 1 {
        op::suff(op::sym(&rn.to_string()), "!")
    } else {
        op::sym(&rn.to_string())
    };

    let mut reglist_operand = Vec::new();
    let mut targets = Vec::new();

    let mut writes_pc = false;

    for i in 0..15 {
        if reglist & (1 << i) != 0 {
            reglist_operand.push(op::sym(&A32Reg::from_instr(i).expect("Counting from 0 to 15 does not result in something from 0 to 15. Check your universe before proceeding.").to_string()));

            //Thanks to PC being an architecturally mentionable register, we
            //have to account for overwriting PC via LDM. Normally this is the
            //moral equivalent of a ret, so it shouldn't be analyzed as a
            //dynamic jump...
            if i == 15 && op == "LDM" {
                writes_pc = true;
                targets.push(refr::new_dyn_ref(p.clone(), refkind::Code))
            }
        }
    }

    let flow = if writes_pc {
        analysis::Flow::Branching(cond != 14)
    } else {
        analysis::Flow::Normal
    };

    let reglist_operand = if s == 1 {
        op::suff(op::wrap("{", reglist_operand, "}"), "^")
    } else {
        op::wrap("{", reglist_operand, "}")
    };

    Ok(Disasm::new(
        Instruction::new(
            &format!("{}{}{}{}", op, condcode(cond)?, u_string, p_string),
            vec![rn_operand, reglist_operand],
        ),
        4,
        flow,
        targets,
    ))
}

fn bl(
    pc: &memory::Pointer<Pointer>,
    cond: u32,
    l: u32,
    offset: u32,
) -> analysis::Result<Disasm, Pointer, Offset> {
    let is_link = l != 0;
    let signbit = if ((offset & 0x0080_0000) >> 23) != 0 {
        0xFF80_0000
    } else {
        0
    };
    let target = pc.contextualize(
        pc.as_pointer()
            .wrapping_add(((offset & 0x007F_FFFF) | signbit) << 2),
    );
    let flow = if is_link {
        analysis::Flow::Normal
    } else {
        analysis::Flow::Branching(cond != 14)
    };

    if is_link {
        Ok(Disasm::new(
            ast::Instruction::new(
                &format!("BL{}", condcode(cond)?),
                vec![op::cptr(target.clone())],
            ),
            4,
            flow,
            vec![refr::new_static_ref(
                pc.clone(),
                target,
                refkind::Subroutine,
            )],
        ))
    } else {
        Ok(Disasm::new(
            ast::Instruction::new(
                &format!("B{}", condcode(cond)?),
                vec![op::cptr(target.clone())],
            ),
            4,
            flow,
            vec![refr::new_static_ref(pc.clone(), target, refkind::Code)],
        ))
    }
}

fn swi(
    pc: &memory::Pointer<Pointer>,
    cond: u32,
    offset: u32,
) -> analysis::Result<Disasm, Pointer, Offset> {
    let target = offset & 0x00FF_FFFF;

    //TODO: The jump target can be in high RAM, how do we handle that?
    Ok(Disasm::new(
        ast::Instruction::new(&format!("SWI{}", condcode(cond)?), vec![op::int(target)]),
        4,
        analysis::Flow::Normal,
        vec![refr::new_static_ref(
            pc.clone(),
            pc.contextualize(0x0000_0008),
            refkind::Subroutine,
        )],
    ))
}

/// Decode a multiply instruction.
///
/// Please note that the instruction space for multiplies specifies the
/// registers in a different order from most instructions. Specifically, `rn`
/// and `rd` are swapped.
fn mul(
    p: &memory::Pointer<Pointer>,
    cond: u32,
    opcode: u32,
    rd: A32Reg,
    rn: A32Reg,
    rs: A32Reg,
    rm: A32Reg,
) -> analysis::Result<Disasm, Pointer, Offset> {
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
        (_, A32Reg::R15) => vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
        (A32Reg::R15, _) => vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
        _ => vec![],
    };

    //TODO: Create a Flow type for "causes undefined/unpredictable PC behavior"
    let flow = match (rd, rn) {
        (_, A32Reg::R15) => analysis::Flow::Branching(cond != 14),
        (A32Reg::R15, _) => analysis::Flow::Branching(cond != 14),
        _ => analysis::Flow::Normal,
    };

    match (is_long, is_unsigned, is_fma, is_status) {
        (true, true, true, true) => Ok(Disasm::new(
            Instruction::new(
                &format!("SMLAL{}S", condcode(cond)?),
                vec![rn_operand, rd_operand, rm_operand, rs_operand],
            ),
            4,
            flow,
            targets,
        )),
        (true, true, true, false) => Ok(Disasm::new(
            Instruction::new(
                &format!("SMLAL{}", condcode(cond)?),
                vec![rn_operand, rd_operand, rm_operand, rs_operand],
            ),
            4,
            flow,
            targets,
        )),
        (true, true, false, true) => Ok(Disasm::new(
            Instruction::new(
                &format!("SMULL{}S", condcode(cond)?),
                vec![rn_operand, rd_operand, rm_operand, rs_operand],
            ),
            4,
            flow,
            targets,
        )),
        (true, true, false, false) => Ok(Disasm::new(
            Instruction::new(
                &format!("SMULL{}", condcode(cond)?),
                vec![rn_operand, rd_operand, rm_operand, rs_operand],
            ),
            4,
            flow,
            targets,
        )),
        (true, false, true, true) => Ok(Disasm::new(
            Instruction::new(
                &format!("UMLAL{}S", condcode(cond)?),
                vec![rn_operand, rd_operand, rm_operand, rs_operand],
            ),
            4,
            flow,
            targets,
        )),
        (true, false, true, false) => Ok(Disasm::new(
            Instruction::new(
                &format!("UMLAL{}", condcode(cond)?),
                vec![rn_operand, rd_operand, rm_operand, rs_operand],
            ),
            4,
            flow,
            targets,
        )),
        (true, false, false, true) => Ok(Disasm::new(
            Instruction::new(
                &format!("UMULL{}S", condcode(cond)?),
                vec![rn_operand, rd_operand, rm_operand, rs_operand],
            ),
            4,
            flow,
            targets,
        )),
        (true, false, false, false) => Ok(Disasm::new(
            Instruction::new(
                &format!("UMULL{}", condcode(cond)?),
                vec![rn_operand, rd_operand, rm_operand, rs_operand],
            ),
            4,
            flow,
            targets,
        )),
        (false, true, false, false) => Ok(Disasm::new(
            Instruction::new(
                &format!("UMAAL{}", condcode(cond)?),
                vec![rn_operand, rd_operand, rm_operand, rs_operand],
            ),
            4,
            flow,
            targets,
        )),
        (false, false, true, true) => Ok(Disasm::new(
            Instruction::new(
                &format!("MLA{}S", condcode(cond)?),
                vec![rd_operand, rm_operand, rs_operand, rn_operand],
            ),
            4,
            flow,
            targets,
        )),
        (false, false, true, false) => Ok(Disasm::new(
            Instruction::new(
                &format!("MLA{}", condcode(cond)?),
                vec![rd_operand, rm_operand, rs_operand, rn_operand],
            ),
            4,
            flow,
            targets,
        )),
        (false, false, false, true) => Ok(Disasm::new(
            Instruction::new(
                &format!("MUL{}S", condcode(cond)?),
                vec![rd_operand, rm_operand, rs_operand],
            ),
            4,
            flow,
            targets,
        )),
        (false, false, false, false) => Ok(Disasm::new(
            Instruction::new(
                &format!("MUL{}", condcode(cond)?),
                vec![rd_operand, rm_operand, rs_operand],
            ),
            4,
            flow,
            targets,
        )),
        _ => Err(analysis::Error::InvalidInstruction),
    }
}

#[allow(clippy::many_single_char_names)]
fn msr(
    cond: u32,
    immediate_bit: u32,
    r: u32,
    rn_val: u32,
    shift_imm: u32,
    data_immed: u32,
    rm: A32Reg,
) -> analysis::Result<Disasm, Pointer, Offset> {
    let c = if (rn_val & 0x1) != 0 { "c" } else { "" };
    let x = if (rn_val & 0x2) >> 1 != 0 { "x" } else { "" };
    let s = if (rn_val & 0x4) >> 2 != 0 { "s" } else { "" };
    let f = if (rn_val & 0x8) >> 3 != 0 { "f" } else { "" };
    let xpsr = if r != 0 { "SPSR" } else { "CPSR" };

    let xpsr = op::sym(&format!("{}_{}{}{}{}", xpsr, c, x, s, f));

    let op_list = match immediate_bit {
        1 => vec![xpsr, op::int(data_immed << (shift_imm & 0xFFFF_FFFE))],
        0 => vec![xpsr, op::sym(&rm.to_string())],
        _ => vec![op::miss()],
    };

    Ok(Disasm::new(
        Instruction::new(&format!("MSR{}", condcode(cond)?), op_list),
        4,
        analysis::Flow::Normal,
        vec![],
    ))
}

fn mrs(cond: u32, r: u32, rd: A32Reg) -> analysis::Result<Disasm, Pointer, Offset> {
    let xpsr = if r != 0 { "CPSR" } else { "SPSR" };

    let op_list = vec![op::sym(&rd.to_string()), op::sym(xpsr)];

    Ok(Disasm::new(
        Instruction::new(&format!("MRS{}", condcode(cond)?), op_list),
        4,
        analysis::Flow::Normal,
        vec![],
    ))
}

fn bx(
    p: &memory::Pointer<Pointer>,
    cond: u32,
    rm: A32Reg,
) -> analysis::Result<Disasm, Pointer, Offset> {
    //BX PC is completely valid! And also dumb.
    let target_pc = p.contextualize(*p.as_pointer() + 8);
    let jumpref = match rm {
        A32Reg::R15 => refr::new_static_ref(p.clone(), target_pc, refkind::Code),
        _ => refr::new_dyn_ref(p.clone(), refkind::Code),
    };

    Ok(Disasm::new(
        Instruction::new(
            &format!("BX{}", condcode(cond)?),
            vec![op::sym(&rm.to_string())],
        ),
        4,
        analysis::Flow::Branching(cond != 14),
        vec![jumpref],
    ))
}

fn bxj(cond: u32, rm: A32Reg) -> analysis::Result<Disasm, Pointer, Offset> {
    //TODO: Find a better status than "Returning"
    let flow = match cond {
        15 => analysis::Flow::Returning,
        _ => analysis::Flow::Branching(true),
    };

    //While this instruction is technically a branch, it's designed for an
    //obsolete ARM hardware extension for directly executing Java bytecode. No
    //technical details of how Jazelle works have ever been released and
    //analysis of a Jazelle program would probably require adding a JVM disasm
    //that I don't want to write.
    Ok(Disasm::new(
        Instruction::new(
            &format!("BXJ{}", condcode(cond)?),
            vec![op::sym(&rm.to_string())],
        ),
        4,
        flow,
        vec![],
    ))
}

fn clz(cond: u32, rd: A32Reg, rm: A32Reg) -> analysis::Result<Disasm, Pointer, Offset> {
    Ok(Disasm::new(
        Instruction::new(
            &format!("CLZ{}", condcode(cond)?),
            vec![op::sym(&rd.to_string()), op::sym(&rm.to_string())],
        ),
        4,
        analysis::Flow::Normal,
        vec![],
    ))
}

fn blx_register(
    p: &memory::Pointer<Pointer>,
    cond: u32,
    rm: A32Reg,
) -> analysis::Result<Disasm, Pointer, Offset> {
    //BX PC is completely valid! And also dumb.
    let target_pc = p.contextualize(*p.as_pointer() + 8);
    let jumpref = match rm {
        A32Reg::R15 => refr::new_static_ref(p.clone(), target_pc, refkind::Code),
        _ => refr::new_dyn_ref(p.clone(), refkind::Code),
    };

    Ok(Disasm::new(
        Instruction::new(
            &format!("BLX{}", condcode(cond)?),
            vec![op::sym(&rm.to_string())],
        ),
        4,
        analysis::Flow::Normal,
        vec![jumpref],
    ))
}

fn blx_immediate(
    p: &memory::Pointer<Pointer>,
    h: u32,
    offset: u32,
) -> analysis::Result<Disasm, Pointer, Offset> {
    let signbit = if ((offset & 0x0080_0000) >> 23) != 0 {
        0xFF80_0000
    } else {
        0
    };
    let mut target = p.contextualize(
        p.as_pointer()
            .wrapping_add(((offset & 0x007F_FFFF) | signbit) << 2 | h << 1),
    );
    target.set_arch_context(THUMB_STATE, reg::Symbolic::new(1));
    let jumpref = refr::new_static_ref(p.clone(), target.clone(), refkind::Subroutine);

    Ok(Disasm::new(
        Instruction::new("BLX", vec![op::cptr(target)]),
        4,
        analysis::Flow::Normal,
        vec![jumpref],
    ))
}

fn bkpt(instr: u32) -> analysis::Result<Disasm, Pointer, Offset> {
    let high_immed = (instr & 0x000F_FF00) >> 4;
    let low_immed = instr & 0x0000_000F;
    let immed = high_immed | low_immed;

    Ok(Disasm::new(
        Instruction::new("BKPT", vec![op::int(immed)]),
        4,
        analysis::Flow::Normal,
        vec![],
    ))
}

fn cdp(
    cond: u32,
    opcode_1: u32,
    crn: u32,
    crd: u32,
    cp_num: u32,
    opcode_2: u32,
    crm: u32,
) -> analysis::Result<Disasm, Pointer, Offset> {
    let crn_sym = op::sym(&format!("CR{}", crn));
    let crd_sym = op::sym(&format!("CR{}", crd));
    let crm_sym = op::sym(&format!("CR{}", crm));
    let cp_sym = op::sym(&format!("p{}", cp_num));
    let cop1_sym = op::int(opcode_1);
    let cop2_sym = op::int(opcode_2);

    let arm_opcode = match cond {
        16 => "CDP2".to_string(),
        cond => format!("CDP{}", condcode(cond)?),
    };

    Ok(Disasm::new(
        Instruction::new(
            &arm_opcode,
            vec![cp_sym, cop1_sym, crd_sym, crn_sym, crm_sym, cop2_sym],
        ),
        4,
        analysis::Flow::Normal,
        vec![],
    ))
}

fn crt(
    cond: u32,
    opcode_1: u32,
    d: u32,
    crn: u32,
    rd: A32Reg,
    cp_num: u32,
    opcode_2: u32,
    crm: u32,
) -> analysis::Result<Disasm, Pointer, Offset> {
    let crn_sym = op::sym(&format!("CR{}", crn));
    let crm_sym = op::sym(&format!("CR{}", crm));
    let rd_sym = op::sym(&rd.to_string());
    let cp_sym = op::sym(&format!("p{}", cp_num));
    let cop1_sym = op::int(opcode_1);
    let cop2_sym = op::int(opcode_2);

    let is_toarm = d != 0;
    let arm_opcode = match (cond, is_toarm) {
        (16, true) => "MRC2".to_string(),
        (16, false) => "MCR2".to_string(),
        (cond, true) => format!("MRC{}", condcode(cond)?),
        (cond, false) => format!("MCR{}", condcode(cond)?),
    };

    Ok(Disasm::new(
        Instruction::new(
            &arm_opcode,
            vec![cp_sym, cop1_sym, rd_sym, crn_sym, crm_sym, cop2_sym],
        ),
        4,
        analysis::Flow::Normal,
        vec![],
    ))
}

fn crt_double(
    cond: u32,
    d: u32,
    rn: A32Reg,
    rd: A32Reg,
    cp_num: u32,
    opcode: u32,
    crm: u32,
) -> analysis::Result<Disasm, Pointer, Offset> {
    let rn_sym = op::sym(&rn.to_string());
    let crm_sym = op::sym(&format!("CR{}", crm));
    let rd_sym = op::sym(&rd.to_string());
    let cp_sym = op::sym(&format!("p{}", cp_num));
    let cop_sym = op::int(opcode);

    let is_toarm = d != 0;
    let arm_opcode = match (cond, is_toarm) {
        (16, true) => "MRRC2".to_string(),
        (16, false) => "MCRR2".to_string(),
        (cond, true) => format!("MRRC{}", condcode(cond)?),
        (cond, false) => format!("MCRR{}", condcode(cond)?),
    };

    Ok(Disasm::new(
        Instruction::new(&arm_opcode, vec![cp_sym, cop_sym, rd_sym, rn_sym, crm_sym]),
        4,
        analysis::Flow::Normal,
        vec![],
    ))
}

fn cps(rn_val: u32, lsimmed: u32) -> analysis::Result<Disasm, Pointer, Offset> {
    let immod = (rn_val & 0xC) >> 2;
    //let mmod = (rn_val & 0x2) >> 1;
    let abit = (lsimmed & 0x100) >> 8;
    let ibit = (lsimmed & 0x080) >> 7;
    let fbit = (lsimmed & 0x040) >> 6;
    let mode = lsimmed & 0x01F;

    let effect = match immod {
        2 => "IE",
        3 => "ID",
        _ => "",
    };

    let aflag = match abit {
        1 => "a",
        _ => "",
    };

    let iflag = match ibit {
        1 => "i",
        _ => "",
    };

    let fflag = match fbit {
        1 => "f",
        _ => "",
    };

    let iflags = format!("{}{}{}", aflag, iflag, fflag);

    //TODO: reject bit 16 and bit 5 not being zero, reject invalid immod/mmod
    match (mode, iflags.as_ref()) {
        (0, "") => Ok(Disasm::new(
            Instruction::new(&format!("CPS{}", effect), vec![]),
            4,
            analysis::Flow::Normal,
            vec![],
        )),
        (_, "") => Ok(Disasm::new(
            Instruction::new(&format!("CPS{}", effect), vec![op::int(mode)]),
            4,
            analysis::Flow::Normal,
            vec![],
        )),
        (0, _) => Ok(Disasm::new(
            Instruction::new(&format!("CPS{}", effect), vec![op::sym(&iflags)]),
            4,
            analysis::Flow::Normal,
            vec![],
        )),
        (_, _) => Ok(Disasm::new(
            Instruction::new(
                &format!("CPS{}", effect),
                vec![op::sym(&iflags), op::int(mode)],
            ),
            4,
            analysis::Flow::Normal,
            vec![],
        )),
    }
}

fn ldstmisc(
    cond: u32,
    pbit: u32,
    ubit: u32,
    ibit: u32,
    wbit: u32,
    lbit: u32,
    rn: A32Reg,
    rd: A32Reg,
    rs_val: u32,
    shiftop: u32,
    rm: A32Reg,
) -> analysis::Result<Disasm, Pointer, Offset> {
    let sbit = (shiftop & 0x2) >> 1;
    let hbit = shiftop & 0x1;

    let is_preindex = pbit != 0;
    let is_offsetadd = ubit != 0;
    let is_immedoffset = ibit != 0;
    let is_writeback = wbit != 0;
    let is_load = lbit != 0; //not always
    let is_signed = sbit != 0;
    let is_half = hbit != 0;

    let opname = match (is_load, is_signed, is_half) {
        (false, false, true) => format!("STR{}H", condcode(cond)?),
        (false, true, false) => format!("LDR{}D", condcode(cond)?),
        (false, true, true) => format!("STR{}D", condcode(cond)?),
        (true, false, true) => format!("LDR{}H", condcode(cond)?),
        (true, true, false) => format!("LDR{}SB", condcode(cond)?),
        (true, true, true) => format!("LDR{}SH", condcode(cond)?),
        _ => return Err(analysis::Error::Misinterpretation(4, false)),
    };

    let immedoffset = (rs_val << 4) | rm.into_instr();

    //TODO: Reject misaligned registers on doublewords as invalid instructions
    //(Yes, I did say misaligned *registers*.)

    let operands = match (is_preindex, is_offsetadd, is_writeback, is_immedoffset) {
        (false, false, false, false) => vec![
            op::sym(&rd.to_string()),
            op::wrap("[", vec![op::sym(&rn.to_string())], "]"),
            op::pref("-", op::sym(&rm.to_string())),
        ],
        (false, true, false, false) => vec![
            op::sym(&rd.to_string()),
            op::wrap("[", vec![op::sym(&rn.to_string())], "]"),
            op::sym(&rm.to_string()),
        ],
        (false, false, false, true) => vec![
            op::sym(&rd.to_string()),
            op::wrap("[", vec![op::sym(&rn.to_string())], "]"),
            op::sint(-(immedoffset as i32)),
        ],
        (false, true, false, true) => vec![
            op::sym(&rd.to_string()),
            op::wrap("[", vec![op::sym(&rn.to_string())], "]"),
            op::sint(immedoffset as i32),
        ],
        (true, false, false, false) => vec![
            op::sym(&rd.to_string()),
            op::wrap(
                "[",
                vec![
                    op::sym(&rn.to_string()),
                    op::pref("-", op::sym(&rm.to_string())),
                ],
                "]",
            ),
        ],
        (true, true, false, false) => vec![
            op::sym(&rd.to_string()),
            op::wrap(
                "[",
                vec![op::sym(&rn.to_string()), op::sym(&rm.to_string())],
                "]",
            ),
        ],
        (true, false, false, true) => vec![
            op::sym(&rd.to_string()),
            op::wrap(
                "[",
                vec![op::sym(&rn.to_string()), op::sint(-(immedoffset as i32))],
                "]",
            ),
        ],
        (true, true, false, true) => vec![
            op::sym(&rd.to_string()),
            op::wrap(
                "[",
                vec![op::sym(&rn.to_string()), op::sint(immedoffset as i32)],
                "]",
            ),
        ],
        (true, false, true, false) => vec![
            op::sym(&rd.to_string()),
            op::suff(
                op::wrap(
                    "[",
                    vec![
                        op::sym(&rn.to_string()),
                        op::pref("-", op::sym(&rm.to_string())),
                    ],
                    "]",
                ),
                "!",
            ),
        ],
        (true, true, true, false) => vec![
            op::sym(&rd.to_string()),
            op::suff(
                op::wrap(
                    "[",
                    vec![op::sym(&rn.to_string()), op::sym(&rm.to_string())],
                    "]",
                ),
                "!",
            ),
        ],
        (true, false, true, true) => vec![
            op::sym(&rd.to_string()),
            op::suff(
                op::wrap(
                    "[",
                    vec![op::sym(&rn.to_string()), op::sint(-(immedoffset as i32))],
                    "]",
                ),
                "!",
            ),
        ],
        (true, true, true, true) => vec![
            op::sym(&rd.to_string()),
            op::suff(
                op::wrap(
                    "[",
                    vec![op::sym(&rn.to_string()), op::sint(immedoffset as i32)],
                    "]",
                ),
                "!",
            ),
        ],
        _ => return Err(analysis::Error::InvalidInstruction),
    };

    //TODO: Generate data references for PC-relative instructions
    //TODO: HALT DISASSEMBLY and generate dynamic refs for PC-relative loads
    //(even though that'd be stupid and useless)

    Ok(Disasm::new(
        Instruction::new(&opname, operands),
        4,
        analysis::Flow::Normal,
        vec![],
    ))
}

#[allow(clippy::too_many_arguments)]
fn ldst_coproc(
    _p: &memory::Pointer<Pointer>,
    cond: u32,
    preindex: u32,
    offsetadd: u32,
    nbit: u32,
    wbit: u32,
    load: u32,
    rn: A32Reg,
    crd: u32,
    cp_num: u32,
    uoffset: u32,
) -> analysis::Result<Disasm, Pointer, Offset> {
    let is_load = load != 0;
    let is_wbit = wbit != 0;
    let is_nbit = nbit != 0;
    let is_offsetadd = offsetadd != 0;
    let is_preindex = preindex != 0;

    let rn_operand = op::sym(&rn.to_string());
    let offset = uoffset as i32 * if is_offsetadd { 1 } else { -1 };

    let crd_op = op::sym(&format!("CR{}", crd));
    let cp_op = op::sym(&format!("p{}", cp_num));

    let operands = match (is_preindex, is_wbit) {
        (false, false) => vec![
            cp_op,
            crd_op,
            op::wrap("[", vec![rn_operand], "]"),
            op::wrap("{", vec![op::int(uoffset)], "}"),
        ],
        (false, true) => vec![
            cp_op,
            crd_op,
            op::wrap("[", vec![rn_operand], "]"),
            op::sint(offset * 4),
        ],
        (true, false) => vec![
            cp_op,
            crd_op,
            op::wrap("[", vec![rn_operand, op::sint(offset * 4)], "]"),
        ],
        (true, true) => vec![
            cp_op,
            crd_op,
            op::suff(
                op::wrap("[", vec![rn_operand, op::sint(offset * 4)], "]"),
                "!",
            ),
        ],
    };

    let opcode_str = match (is_load, is_nbit, cond) {
        (false, false, 15) => "STC2".to_string(),
        (false, false, cond) => format!("STC{}", condcode(cond)?),
        (false, true, 15) => "STC2L".to_string(),
        (false, true, cond) => format!("STC{}L", condcode(cond)?),
        (true, false, 15) => "LDC2".to_string(),
        (true, false, cond) => format!("LDC{}", condcode(cond)?),
        (true, true, 15) => "LDC2L".to_string(),
        (true, true, cond) => format!("LDC{}L", condcode(cond)?),
    };

    //TODO: PC-rel addresses should generate data refs
    Ok(Disasm::new(
        Instruction::new(&opcode_str, operands),
        4,
        analysis::Flow::Normal,
        vec![],
    ))
}

pub fn ldstrex(
    cond: u32,
    l: u32,
    rn: A32Reg,
    rd: A32Reg,
    rm: A32Reg,
) -> analysis::Result<Disasm, Pointer, Offset> {
    let is_load = l != 0;
    let opname = if is_load {
        format!("LDREX{}", condcode(cond)?)
    } else {
        format!("STREX{}", condcode(cond)?)
    };

    let operands = if is_load {
        vec![
            op::sym(&rd.to_string()),
            op::wrap("[", vec![op::sym(&rn.to_string())], "]"),
        ]
    } else {
        vec![
            op::sym(&rd.to_string()),
            op::sym(&rm.to_string()),
            op::wrap("[", vec![op::sym(&rn.to_string())], "]"),
        ]
    };

    //TODO: Data references
    Ok(Disasm::new(
        Instruction::new(&opname, operands),
        4,
        analysis::Flow::Normal,
        vec![],
    ))
}

pub fn pkh(
    cond: u32,
    rn: A32Reg,
    rd: A32Reg,
    shift_imm: u32,
    d: u32,
    rm: A32Reg,
) -> analysis::Result<Disasm, Pointer, Offset> {
    let opname = match d {
        0 => format!("PKHBT{}", condcode(cond)?),
        2 => format!("PKHTB{}", condcode(cond)?),
        _ => return Err(analysis::Error::Misinterpretation(4, false)),
    };

    let rn_sym = op::sym(&rn.to_string());
    let rd_sym = op::sym(&rd.to_string());
    let rm_sym = op::sym(&rm.to_string());

    let operands = match d {
        0 => vec![rd_sym, rn_sym, rm_sym, op::pref("LSL", op::int(shift_imm))],
        2 => vec![rd_sym, rn_sym, rm_sym, op::pref("ASR", op::int(shift_imm))],
        _ => return Err(analysis::Error::Misinterpretation(4, false)),
    };

    Ok(Disasm::new(
        Instruction::new(&opname, operands),
        4,
        analysis::Flow::Normal,
        vec![],
    ))
}

pub fn pld(
    immediate_bit: u32,
    offsetadd: u32,
    rn: A32Reg,
    shift_imm: u32,
    shift: u32,
    rm: A32Reg,
    address_operand: u32,
) -> analysis::Result<Disasm, Pointer, Offset> {
    let is_shifted = shift_imm != 0 || shift != 0;
    let is_offsetadd = offsetadd != 0;

    let offset12 = if is_offsetadd {
        (address_operand & 0xFFF) as i32
    } else {
        -((address_operand & 0xFFF) as i32)
    };

    let rn_operand = op::sym(&rn.to_string());
    let rm_operand = if is_offsetadd {
        op::sym(&rm.to_string())
    } else {
        op::pref("-", op::sym(&rm.to_string()))
    };

    let address_operand = match (immediate_bit, is_shifted) {
        (0, false) => vec![op::wrap("[", vec![rn_operand, op::sint(offset12)], "]")],
        (1, true) => vec![op::wrap(
            "[",
            vec![
                rn_operand,
                rm_operand,
                op::sym(shift_symbol(shift, shift_imm)?),
                op::int(shift_imm),
            ],
            "]",
        )],
        (1, false) => vec![op::wrap("[", vec![rn_operand, rm_operand], "]")],
        _ => return Err(analysis::Error::Misinterpretation(4, false)),
    };

    Ok(Disasm::new(
        Instruction::new("PLD", address_operand),
        4,
        analysis::Flow::Normal,
        vec![],
    ))
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
#[allow(clippy::many_single_char_names)]
pub fn disassemble(p: &memory::Pointer<Pointer>, mem: &Bus) -> aarch32::Result<Disasm> {
    let instr: reg::Symbolic<u32> = mem.read_leword(&p);

    if let Some(instr) = instr.into_concrete() {
        let cond = (instr & 0xF000_0000) >> 28;
        let opcat = (instr & 0x0C00_0000) >> 26;
        let immed_mode = (instr & 0x0200_0000) >> 25;
        let opcode = (instr & 0x01F0_0000) >> 20;
        let pbit = (instr & 0x0100_0000) >> 24;
        let u = (instr & 0x0080_0000) >> 23;
        let b = (instr & 0x0040_0000) >> 22;
        let w = (instr & 0x0020_0000) >> 21;
        let l = (instr & 0x0010_0000) >> 20;
        let copcode1 = (instr & 0x00F0_0000) >> 20;
        let rn_val = (instr & 0x000F_0000) >> 16;
        let rd_val = (instr & 0x0000_F000) >> 12;
        let rs_val = (instr & 0x0000_0F00) >> 8;
        let shift_imm = (instr & 0x0000_0F80) >> 7;
        let special = (instr & 0x0000_0080) >> 7;
        let shiftop = (instr & 0x0000_0060) >> 5;
        let copcode2 = (instr & 0x0000_00E0) >> 5;
        let regshift = (instr & 0x0000_0010) >> 4;
        let rm_val = instr & 0x0000_000F;
        let data_immed = instr & 0x0000_00FF;
        let rn = A32Reg::from_instr(rn_val)
            .expect("What the heck? The register definitely should have parsed");
        let rd = A32Reg::from_instr(rd_val)
            .expect("What the heck? The register definitely should have parsed");
        let rs = A32Reg::from_instr(rs_val).expect("Could not parse RS... somehow?!");
        let rm = A32Reg::from_instr(rm_val).expect("Could not parse RM... somehow?!");
        let lsimmed = instr & 0x0000_0FFF; //Load-Store Immediate (12bit)

        #[allow(unused_variables)]
        match (
            cond, opcat, immed_mode, pbit, u, b, w, l, special, shiftop, regshift,
        ) {
            (15, 0, 0, 1, 0, 0, 0, 0, _, _, _) => cps(rn_val, lsimmed), //Change Processor State / Set Endianness
            (15, 1, x, 1, u, 1, 0, 1, _, _, _) => pld(x, u, rn, shift_imm, regshift, rm, lsimmed), //Cache Preload
            (15, 2, 0, p, u, 1, w, 0, _, _, _) => Err(analysis::Error::NotYetImplemented), //Save Return State
            (15, 2, 0, p, u, 0, w, 1, _, _, _) => Err(analysis::Error::NotYetImplemented), //Return from Exception
            (15, 2, 1, h, _, _, _, _, _, _, _) => blx_immediate(p, h, instr),
            (15, 3, 0, q, u, n, w, l, _, _, _) => {
                ldst_coproc(p, cond, q, u, n, w, l, rn, rd_val, rs_val, data_immed)
            }
            (15, 3, 1, 0, _, _, _, _, _, _, 0) => {
                cdp(cond, copcode1, rn_val, rd_val, rs_val, copcode2, rm_val)
            }
            (15, 3, 1, 0, _, _, _, d, _, _, 1) => {
                crt(cond, copcode1, d, rn_val, rd, rs_val, copcode2, rm_val)
            }
            (15, _, _, _, _, _, _, _, _, _, _) => Err(analysis::Error::NotYetImplemented), //Unconditionally executed extension space
            (14, 0, 0, 1, 0, 0, 1, 0, 0, 3, 1) => bkpt(instr), //Software Breakpoint
            (_, 0, 0, 0, _, _, _, _, 1, 0, 1) => mul(p, cond, opcode, rn, rd, rs, rm),
            (_, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1) => bx(p, cond, rm), //BX to Thumb
            (_, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0) => bxj(cond, rm),   //BX to Jazelle DBX
            (_, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1) => blx_register(p, cond, rm), //BLX to Thumb
            (_, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1) => clz(cond, rd, rm), //Count Leading Zeroes
            (_, 0, 0, 1, 0, r, 0, 0, 0, 0, 0) => mrs(cond, r, rd),
            (_, 0, 0, 1, 0, b, 0, 0, 1, 0, 1) => Err(analysis::Error::NotYetImplemented), //Swap
            (_, 0, 0, 1, 0, r, 1, 0, 0, 0, 0) => msr(cond, 0, r, rn_val, rs_val, data_immed, rm),
            (_, 0, 0, 1, 0, _, _, 0, 0, 2, 1) => Err(analysis::Error::NotYetImplemented), //Saturation arithmetic
            (_, 0, 0, 1, 0, _, _, 0, 1, _, 0) => Err(analysis::Error::NotYetImplemented), //Signed multiply
            (_, 0, 0, 1, 1, 0, 0, l, 1, 0, 1) => ldstrex(cond, l, rn, rd, rm), //Load/store register exclusive
            (_, 0, 0, q, u, i, w, l, 1, h, 1) if h != 0 => {
                ldstmisc(cond, q, u, i, w, l, rn, rd, rs_val, h, rm)
            }
            (_, 0, 0, _, _, _, _, _, _, _, 0) => dpinst(
                p, cond, immed_mode, opcode, rn, rd, shift_imm, regshift, shiftop, rm, rs,
                data_immed,
            ),
            (_, 0, 0, _, _, _, _, _, 0, _, 1) => dpinst(
                p, cond, immed_mode, opcode, rn, rd, shift_imm, regshift, shiftop, rm, rs,
                data_immed,
            ),
            (_, 0, 1, 1, 0, _, 0, 0, _, _, _) => Err(analysis::Error::NotYetImplemented), //Undefined (as of ARM DDI 0100I)
            (_, 0, 1, 1, 0, r, 1, 0, _, _, _) => {
                msr(cond, immed_mode, r, rn_val, shift_imm, data_immed, rm)
            } //Move to status register
            (_, 0, i, _, _, _, _, _, _, _, _) => dpinst(
                p, cond, i, opcode, rn, rd, shift_imm, regshift, shiftop, rm, rs, data_immed,
            ), //Data processing with immediate
            (_, 1, 1, 0, 0, _, _, _, _, _, 1) => Err(analysis::Error::NotYetImplemented), //Parallel add-sub
            (_, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1) => Err(analysis::Error::NotYetImplemented), //Select bytes
            (_, 1, 1, 0, 1, 0, 0, 0, _, d, 1) if d & 1 == 0 => pkh(cond, rn, rd, shift_imm, d, rm), //Halfword pack
            (_, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1) => Err(analysis::Error::NotYetImplemented), //Byte reverse word
            (_, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1) => Err(analysis::Error::NotYetImplemented), //Byte reverse packed halfword
            (_, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1) => Err(analysis::Error::NotYetImplemented), //Byte reverse signed halfword
            (_, 1, 1, 0, 1, u, 1, 0, 0, 1, 1) => Err(analysis::Error::NotYetImplemented), //Parallel halfword saturate
            (_, 1, 1, 0, 1, u, 1, _, _, _, 1) => Err(analysis::Error::NotYetImplemented), //Word saturate
            (_, 1, 1, 0, 1, _, _, _, 0, 2, 1) => Err(analysis::Error::NotYetImplemented), //Sign/zero extend (add)
            (_, 1, 1, 1, 0, _, _, _, _, _, 1) => Err(analysis::Error::NotYetImplemented), //Multiplies (type 3)
            (_, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1) => Err(analysis::Error::NotYetImplemented), //Unsigned sum of absolute differences
            (_, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1) => Err(analysis::Error::NotYetImplemented), //Architecturally undefined space
            (_, 1, i, q, u, b, w, l, _, _, _) => ldst(
                p, cond, i, q, u, b, w, l, rn, rd, shift_imm, shiftop, rm, lsimmed,
            ), //Load/store
            (_, 2, 0, q, u, s, w, l, _, _, _) => {
                ldmstm(p, cond, q, u, s, w, l, rn, instr & 0x0000_FFFF)
            } //Load/store multiple
            (_, 2, 1, l, _, _, _, _, _, _, _) => bl(&p, cond, l, instr), //Branch with or without link
            (_, 3, 0, 0, 0, 1, 0, d, _, _, _) => {
                crt_double(cond, d, rn, rd, rs_val, copcode2, rm_val)
            }
            (_, 3, 0, q, u, n, w, l, _, _, _) => {
                ldst_coproc(p, cond, q, u, n, w, l, rn, rd_val, rs_val, data_immed)
            }
            (_, 3, 1, 1, _, _, _, _, _, _, _) => swi(&p, cond, instr), //Software interrupt
            (_, 3, 1, 0, _, _, _, _, _, _, 0) => {
                cdp(cond, copcode1, rn_val, rd_val, rs_val, copcode2, rm_val)
            }
            (_, 3, 1, 0, _, _, _, d, _, _, 1) => {
                crt(cond, copcode1, d, rn_val, rd, rs_val, copcode2, rm_val)
            }
            _ => Err(analysis::Error::InvalidInstruction),
        }
    } else {
        Err(analysis::Error::UnconstrainedMemory(p.clone()))
    }
}
