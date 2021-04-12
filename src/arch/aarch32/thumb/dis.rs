//! THUMB instruction set disassembly

use crate::analysis::Flow;
use crate::analysis::Reference as refr;
use crate::analysis::ReferenceKind as refkind;
use crate::arch::aarch32;
use crate::arch::aarch32::arm::condcode;
use crate::arch::aarch32::thumb::THUMB_STATE;
use crate::arch::aarch32::Aarch32Register as A32Reg;
use crate::arch::aarch32::{Bus, Disasm, Literal, PtrVal};
use crate::ast::{Instruction, Operand as op};
use crate::{analysis, memory, reg};

fn cond_branch<L>(p: &memory::Pointer<PtrVal>, cond: u16, offset: u16) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    // signed_offset = (target - base + 4) / 2
    // therefore target = signed_offset * 2 - 4 + base
    let signed_offset = (((offset as u8) as i8) as i32) << 1;
    let target = p.contextualize((signed_offset - 4 + *p.as_pointer() as i32) as PtrVal);
    let mut swi_target = p.contextualize(0x0000_0008);

    swi_target.set_arch_context(THUMB_STATE, reg::Symbolic::from(0));

    match cond {
        //Unconditional conditional branches are undefined and are thus treated as illegal
        15 => Err(analysis::Error::InvalidInstruction),

        //TODO: The jump target can be in high RAM, how do we handle that?
        16 => Ok(Disasm::new(
            Instruction::new("SWI", vec![op::lit(offset as u32)]),
            2,
            Flow::Calling,
            vec![refr::new_static_ref(
                p.clone(),
                swi_target,
                refkind::Subroutine,
            )],
        )),
        _ => Ok(Disasm::new(
            Instruction::new(
                &format!("B{}", condcode(cond as u32)?),
                vec![op::cptr(target.clone())],
            ),
            2,
            Flow::Branching(cond != 15),
            vec![refr::new_static_ref(p.clone(), target, refkind::Code)],
        )),
    }
}

fn uncond_branch<L>(p: &memory::Pointer<PtrVal>, offset: u16) -> Disasm<L>
where
    L: Literal,
{
    // signed_offset = (target - base + 4) / 2
    // therefore target = signed_offset * 2 - 4 + base
    let sign_extend = if offset & 0x0400 != 0 { 0xF800 } else { 0x0000 };
    let signed_offset = (((offset | sign_extend) as i16) as i32) << 1;
    let target = p.contextualize((signed_offset - 4 + *p.as_pointer() as i32) as PtrVal);

    Disasm::new(
        Instruction::new("B", vec![op::cptr(target.clone())]),
        2,
        Flow::Branching(false),
        vec![refr::new_static_ref(p.clone(), target, refkind::Code)],
    )
}

fn special_data<L>(
    p: &memory::Pointer<PtrVal>,
    dp_opcode: u16,
    low_rm: u16,
    low_rd: u16,
) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let opcode = (dp_opcode & 0xC) >> 2;
    let h1 = (dp_opcode & 0x2) << 2;
    let h2 = (dp_opcode & 0x1) << 3;
    let l = (dp_opcode & 0x2) != 0;
    let rm = h2 | low_rm;
    let rd = h1 | low_rd;

    let rd_reg = A32Reg::from_instr(rd as u32).expect("Invalid register");
    let rm_reg = A32Reg::from_instr(rm as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rm_operand = op::sym(&rm_reg.to_string());
    let branch_target = match rd_reg {
        A32Reg::R15 => vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
        _ => vec![],
    };
    let flow = if rd_reg == A32Reg::R15 {
        Flow::Branching(false)
    } else {
        Flow::Normal
    };

    match (opcode, l) {
        (0, _) => Ok(Disasm::new(
            Instruction::new("ADD", vec![rd_operand, rm_operand]),
            2,
            flow,
            branch_target,
        )),
        (1, _) => Ok(Disasm::new(
            Instruction::new("CMP", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        (2, _) => Ok(Disasm::new(
            Instruction::new("MOV", vec![rd_operand, rm_operand]),
            2,
            flow,
            branch_target,
        )),
        (3, false) => Ok(Disasm::new(
            Instruction::new("BX", vec![rm_operand]),
            2,
            Flow::Branching(false),
            vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
        )),
        (3, true) => Ok(Disasm::new(
            Instruction::new("BLX", vec![rm_operand]),
            2,
            Flow::Calling,
            vec![refr::new_dyn_ref(p.clone(), refkind::Subroutine)],
        )),
        _ => Err(analysis::Error::Misinterpretation(2, false)),
    }
}

fn data_processing<L>(dp_opcode: u16, low_rm: u16, low_rd: u16) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let rd_reg = A32Reg::from_instr(low_rd as u32).expect("Invalid register");
    let rm_reg = A32Reg::from_instr(low_rm as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rm_operand = op::sym(&rm_reg.to_string());

    match dp_opcode {
        0 => Ok(Disasm::new(
            Instruction::new("AND", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        1 => Ok(Disasm::new(
            Instruction::new("EOR", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        2 => Ok(Disasm::new(
            Instruction::new("LSL", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        3 => Ok(Disasm::new(
            Instruction::new("LSR", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        4 => Ok(Disasm::new(
            Instruction::new("ASR", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        5 => Ok(Disasm::new(
            Instruction::new("ADC", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        7 => Ok(Disasm::new(
            Instruction::new("ROR", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        6 => Ok(Disasm::new(
            Instruction::new("SBC", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        8 => Ok(Disasm::new(
            Instruction::new("TST", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        9 => Ok(Disasm::new(
            Instruction::new("NEG", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        0xA => Ok(Disasm::new(
            Instruction::new("CMP", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        0xB => Ok(Disasm::new(
            Instruction::new("CMN", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        0xC => Ok(Disasm::new(
            Instruction::new("ORR", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        0xD => Ok(Disasm::new(
            Instruction::new("MUL", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        0xE => Ok(Disasm::new(
            Instruction::new("BIC", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        0xF => Ok(Disasm::new(
            Instruction::new("MVN", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        _ => Err(analysis::Error::Misinterpretation(2, false)),
    }
}

fn add_sub_register<L>(
    add_sub_bit: u16,
    low_rm: u16,
    low_rn: u16,
    low_rd: u16,
) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let rd_reg = A32Reg::from_instr(low_rd as u32).expect("Invalid register");
    let rn_reg = A32Reg::from_instr(low_rn as u32).expect("Invalid register");
    let rm_reg = A32Reg::from_instr(low_rm as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rn_operand = op::sym(&rn_reg.to_string());
    let rm_operand = op::sym(&rm_reg.to_string());

    match add_sub_bit {
        0 => Ok(Disasm::new(
            Instruction::new("ADD", vec![rd_operand, rn_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        1 => Ok(Disasm::new(
            Instruction::new("SUB", vec![rd_operand, rn_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        _ => Err(analysis::Error::Misinterpretation(2, false)),
    }
}

fn add_sub_immed<L>(
    add_sub_bit: u16,
    immed: u16,
    low_rn: u16,
    low_rd: u16,
) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let rd_reg = A32Reg::from_instr(low_rd as u32).expect("Invalid register");
    let rn_reg = A32Reg::from_instr(low_rn as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rn_operand = op::sym(&rn_reg.to_string());
    let immed_operand = op::lit(immed as u32);

    match add_sub_bit {
        0 => Ok(Disasm::new(
            Instruction::new("ADD", vec![rd_operand, rn_operand, immed_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        1 => Ok(Disasm::new(
            Instruction::new("SUB", vec![rd_operand, rn_operand, immed_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        _ => Err(analysis::Error::Misinterpretation(2, false)),
    }
}

fn shifter_immed<L>(
    shift_opcode: u16,
    immed: u16,
    low_rm: u16,
    low_rd: u16,
) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let rd_reg = A32Reg::from_instr(low_rd as u32).expect("Invalid register");
    let rm_reg = A32Reg::from_instr(low_rm as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rm_operand = op::sym(&rm_reg.to_string());
    let nz_immed = match immed {
        0 => 32,
        s => s,
    };

    match shift_opcode {
        0 => Ok(Disasm::new(
            Instruction::new("LSL", vec![rd_operand, rm_operand, op::lit(immed as u32)]),
            2,
            Flow::Normal,
            vec![],
        )),
        1 => Ok(Disasm::new(
            Instruction::new(
                "LSR",
                vec![rd_operand, rm_operand, op::lit(nz_immed as u32)],
            ),
            2,
            Flow::Normal,
            vec![],
        )),
        2 => Ok(Disasm::new(
            Instruction::new(
                "ASR",
                vec![rd_operand, rm_operand, op::lit(nz_immed as u32)],
            ),
            2,
            Flow::Normal,
            vec![],
        )),
        _ => Err(analysis::Error::Misinterpretation(2, false)),
    }
}

fn math_immed<L>(math_opcode: u16, low_rd: u16, immed: u16) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let rd_reg = A32Reg::from_instr(low_rd as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let immed_operand = op::lit(immed as u32);

    match math_opcode {
        0 => Ok(Disasm::new(
            Instruction::new("MOV", vec![rd_operand, immed_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        1 => Ok(Disasm::new(
            Instruction::new("CMP", vec![rd_operand, immed_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        2 => Ok(Disasm::new(
            Instruction::new("ADD", vec![rd_operand, immed_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        3 => Ok(Disasm::new(
            Instruction::new("SUB", vec![rd_operand, immed_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        _ => Err(analysis::Error::Misinterpretation(2, false)),
    }
}

fn load_pool_constant<L>(p: &memory::Pointer<PtrVal>, low_rd: u16, immed: u16) -> Disasm<L>
where
    L: Literal,
{
    let rd_reg = A32Reg::from_instr(low_rd as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let target_ptr = p.contextualize((*p.as_pointer() & 0xFFFF_FFFC) + 4 + (immed as u32 * 4));
    let immed_operand = op::lit(immed as u32 * 4);
    let ast = Instruction::new(
        "LDR",
        vec![
            rd_operand,
            op::indir(op::wrap("", vec![op::sym("PC"), immed_operand], "")),
        ],
    );

    Disasm::new(
        ast,
        2,
        Flow::Normal,
        vec![refr::new_static_ref(p.clone(), target_ptr, refkind::Data)],
    )
}

fn load_store_register_offset<L>(
    opcode: u16,
    low_rm: u16,
    low_rn: u16,
    low_rd: u16,
) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let rd_reg = A32Reg::from_instr(low_rd as u32).expect("Invalid register");
    let rn_reg = A32Reg::from_instr(low_rn as u32).expect("Invalid register");
    let rm_reg = A32Reg::from_instr(low_rm as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rn_operand = op::sym(&rn_reg.to_string());
    let rm_operand = op::sym(&rm_reg.to_string());
    let opcode_name = match opcode {
        0 => "STR",
        1 => "STRH",
        2 => "STRB",
        3 => "LDRSB",
        4 => "LDR",
        5 => "LDRH",
        6 => "LDRB",
        7 => "LDRSH",
        _ => return Err(analysis::Error::Misinterpretation(2, false)),
    };
    let operands = vec![rd_operand, op::wrap("[", vec![rn_operand, rm_operand], "]")];

    Ok(Disasm::new(
        Instruction::new(opcode_name, operands),
        2,
        Flow::Normal,
        vec![],
    ))
}

fn load_store_immed_offset_word<L>(
    b: u16,
    l: u16,
    offset: u16,
    low_rn: u16,
    low_rd: u16,
) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let rd_reg = A32Reg::from_instr(low_rd as u32).expect("Invalid register");
    let rn_reg = A32Reg::from_instr(low_rn as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rn_operand = op::sym(&rn_reg.to_string());
    let size = match b {
        0 => 4,
        1 => 1,
        _ => return Err(analysis::Error::Misinterpretation(2, false)),
    };
    let offset_operand = op::lit((offset * size) as u32);

    let opcode_name = match (b, l) {
        (0, 0) => "STR",
        (1, 0) => "STRB",
        (0, 1) => "LDR",
        (1, 1) => "LDRB",
        _ => return Err(analysis::Error::Misinterpretation(2, false)),
    };
    let operands = vec![
        rd_operand,
        op::wrap("[", vec![rn_operand, offset_operand], "]"),
    ];

    Ok(Disasm::new(
        Instruction::new(opcode_name, operands),
        2,
        Flow::Normal,
        vec![],
    ))
}

fn load_store_immed_offset_halfword<L>(
    l: u16,
    offset: u16,
    low_rn: u16,
    low_rd: u16,
) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let rd_reg = A32Reg::from_instr(low_rd as u32).expect("Invalid register");
    let rn_reg = A32Reg::from_instr(low_rn as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rn_operand = op::sym(&rn_reg.to_string());
    let offset_operand = op::lit((offset * 2) as u32);

    let opcode_name = match l {
        0 => "STRH",
        1 => "LDRH",
        _ => return Err(analysis::Error::Misinterpretation(2, false)),
    };
    let operands = vec![
        rd_operand,
        op::wrap("[", vec![rn_operand, offset_operand], "]"),
    ];

    Ok(Disasm::new(
        Instruction::new(opcode_name, operands),
        2,
        Flow::Normal,
        vec![],
    ))
}

fn load_store_stack_offset<L>(l: u16, low_rd: u16, offset: u16) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let rd_reg = A32Reg::from_instr(low_rd as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let offset_operand = op::lit((offset * 4) as u32);

    let opcode_name = match l {
        0 => "STR",
        1 => "LDR",
        _ => return Err(analysis::Error::Misinterpretation(2, false)),
    };
    let operands = vec![
        rd_operand,
        op::wrap("[", vec![op::sym("SP"), offset_operand], "]"),
    ];

    Ok(Disasm::new(
        Instruction::new(opcode_name, operands),
        2,
        Flow::Normal,
        vec![],
    ))
}

fn compute_rel_addr<L>(s: u16, low_rd: u16, offset: u16) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let rd_reg = A32Reg::from_instr(low_rd as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let offset_operand = op::lit((offset * 4) as u32);

    let s_operand = op::sym(match s {
        0 => "PC",
        1 => "SP",
        _ => return Err(analysis::Error::Misinterpretation(2, false)),
    });

    Ok(Disasm::new(
        Instruction::new("ADD", vec![rd_operand, s_operand, offset_operand]),
        2,
        Flow::Normal,
        vec![],
    ))
}

fn load_store_multiple<L>(l: u16, low_rn: u16, register_list: u16) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let rn_reg = A32Reg::from_instr(low_rn as u32).expect("Invalid register");
    let rn_operand = op::sym(&rn_reg.to_string());
    let instr = match l {
        0 => "STMIA",
        1 => "LDMIA",
        _ => return Err(analysis::Error::Misinterpretation(2, false)),
    };
    let mut register_list_operand = vec![];

    for i in 0..7 {
        if register_list & (1 << i) != 0 {
            let reg = A32Reg::from_instr(i).expect("This should be valid");
            let reg_operand = op::sym(&reg.to_string());
            register_list_operand.push(reg_operand);
        }
    }

    let operands = vec![
        op::suff(rn_operand, "!"),
        op::wrap("{", register_list_operand, "}"),
    ];

    Ok(Disasm::new(
        Instruction::new(instr, operands),
        2,
        Flow::Normal,
        vec![],
    ))
}

fn uncond_branch_link<L>(
    p: &memory::Pointer<PtrVal>,
    mem: &Bus,
    high_offset: u16,
) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    match mem.read_leword::<u16>(&(p.clone() + 2)).into_concrete() {
        Some(low_instr) if low_instr & 0xE000 == 0xE000 => {
            let h = (low_instr & 0x1800) >> 11;
            let low_offset = low_instr & 0x07FF;
            let sign = match high_offset & 0x0400 {
                0 => 0,
                _ => 0xFF80_0000,
            };

            let offset: u32 = sign | (high_offset as u32) << 12 | (low_offset as u32) << 1;
            let target = p.contextualize((*p.as_pointer() as i32 + offset as i32) as u32);
            let mut arm_target =
                p.contextualize((*p.as_pointer() as i32 + offset as i32) as u32 & 0xFFFF_FFFC);
            arm_target.set_arch_context(THUMB_STATE, reg::Symbolic::from(0));

            match h {
                1 if low_offset & 1 == 0 => Ok(Disasm::new(
                    Instruction::new("BLX", vec![op::cptr(arm_target.clone())]),
                    4,
                    Flow::Calling,
                    vec![refr::new_static_ref(
                        p.clone(),
                        arm_target,
                        refkind::Subroutine,
                    )],
                )),
                3 => Ok(Disasm::new(
                    Instruction::new("BL", vec![op::cptr(target.clone())]),
                    4,
                    Flow::Branching(false),
                    vec![refr::new_static_ref(p.clone(), target, refkind::Subroutine)],
                )),
                _ => Err(analysis::Error::InvalidInstruction),
            }
        }
        _ => Err(analysis::Error::UnconstrainedMemory(p.clone() + 2)),
    }
}

fn sp_adjust<L>(immed: u16) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let opbit = (immed & 0x80) >> 7;
    let target = (immed & 0x7F) << 2;

    match opbit {
        0 => Ok(Disasm::new(
            Instruction::new("ADD", vec![op::sym("SP"), op::lit(target as u32)]),
            2,
            Flow::Normal,
            vec![],
        )),
        1 => Ok(Disasm::new(
            Instruction::new("SUB", vec![op::sym("SP"), op::lit(target as u32)]),
            2,
            Flow::Normal,
            vec![],
        )),
        _ => Err(analysis::Error::Misinterpretation(2, false)),
    }
}

fn sign_zero_extend<L>(immed: u16, low_rm: u16, low_rd: u16) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let opcode = (immed & 0xC0) >> 6;
    let rd_reg = A32Reg::from_instr(low_rd as u32).expect("Invalid register");
    let rm_reg = A32Reg::from_instr(low_rm as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rm_operand = op::sym(&rm_reg.to_string());

    match opcode {
        0 => Ok(Disasm::new(
            Instruction::new("SXTH", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        1 => Ok(Disasm::new(
            Instruction::new("SXTB", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        2 => Ok(Disasm::new(
            Instruction::new("UXTH", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        3 => Ok(Disasm::new(
            Instruction::new("UXTB", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        _ => Err(analysis::Error::Misinterpretation(2, false)),
    }
}

fn endian_reverse<L>(immed: u16, low_rm: u16, low_rd: u16) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let opcode = (immed & 0xC0) >> 6;
    let rd_reg = A32Reg::from_instr(low_rd as u32).expect("Invalid register");
    let rm_reg = A32Reg::from_instr(low_rm as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rm_operand = op::sym(&rm_reg.to_string());

    match opcode {
        0 => Ok(Disasm::new(
            Instruction::new("REV", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        1 => Ok(Disasm::new(
            Instruction::new("REV16", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        2 => Err(analysis::Error::InvalidInstruction),
        3 => Ok(Disasm::new(
            Instruction::new("REVSH", vec![rd_operand, rm_operand]),
            2,
            Flow::Normal,
            vec![],
        )),
        _ => Err(analysis::Error::Misinterpretation(2, false)),
    }
}

fn push_pop<L>(l: u16, r: u16, register_list: u16) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    let mut register_list_operand = vec![];

    for i in 0..7 {
        if register_list & (1 << i) != 0 {
            let reg = A32Reg::from_instr(i).expect("This should be valid");
            let reg_operand = op::sym(&reg.to_string());
            register_list_operand.push(reg_operand);
        }
    }

    let is_callret = r != 0;

    if is_callret {
        match l {
            0 => register_list_operand.push(op::sym("LR")),
            1 => register_list_operand.push(op::sym("PC")),
            _ => return Err(analysis::Error::Misinterpretation(2, false)),
        }
    }

    let flow = if is_callret {
        Flow::Returning(false)
    } else {
        Flow::Normal
    };

    match l {
        0 => Ok(Disasm::new(
            Instruction::new("PUSH", vec![op::wrap("{", register_list_operand, "}")]),
            2,
            Flow::Normal,
            vec![],
        )),
        1 => Ok(Disasm::new(
            Instruction::new("POP", vec![op::wrap("{", register_list_operand, "}")]),
            2,
            flow,
            vec![],
        )),
        _ => Err(analysis::Error::Misinterpretation(2, false)),
    }
}

fn breakpoint<L>(p: &memory::Pointer<PtrVal>, immed: u16) -> Disasm<L>
where
    L: Literal,
{
    let mut bkpt_target = p.contextualize(0x0000_000C);
    bkpt_target.set_arch_context(THUMB_STATE, reg::Symbolic::from(0));

    let targets = vec![refr::new_static_ref(
        p.clone(),
        bkpt_target,
        refkind::Subroutine,
    )];

    Disasm::new(
        Instruction::new("BKPT", vec![op::lit(immed as u32)]),
        2,
        Flow::Calling,
        targets,
    )
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
pub fn disassemble<L>(p: &memory::Pointer<PtrVal>, mem: &Bus) -> aarch32::Result<Disasm<L>>
where
    L: Literal,
{
    match mem.read_leword::<u16>(p).into_concrete() {
        Some(instr) => {
            let rd = instr & 0x0007; //sometimes also sbz
            let rn = (instr & 0x0038) >> 3; //sometimes also rm or rs
            let shift_immed = (instr & 0x07C0) >> 6;
            let shift_opcode = (instr & 0x1800) >> 11; //also used by math imm
            let rm = (instr & 0x01C0) >> 6; //sometimes also rn or add/sub immed
            let opc = (instr & 0x0200) >> 9; //add/sub bit for instructions
            let immed = instr & 0x00FF; //sometimes also small-offset
            let math_rd_rn = (instr & 0x0700) >> 8;
            let dp_opcode = (instr & 0x03C0) >> 6;
            let lsro_opcode = (instr & 0x0E00) >> 9;
            let cond = (instr & 0x0F00) >> 8;
            let large_offset = instr & 0x07FF;

            match (
                instr >> 13,
                (instr & 0x1000) >> 12,
                (instr & 0x0800) >> 11,
                (instr & 0x0400) >> 10,
            ) {
                (0, 1, 1, 0) => add_sub_register(opc, rm, rn, rd),
                (0, 1, 1, 1) => add_sub_immed(opc, rm, rn, rd),
                (0, _, _, _) => shifter_immed(shift_opcode, shift_immed, rn, rd),
                (1, _, _, _) => math_immed(shift_opcode, math_rd_rn, immed),
                (2, 0, 0, 0) => data_processing(dp_opcode, rn, rd),
                (2, 0, 0, 1) => special_data(p, dp_opcode, rn, rd),
                (2, 0, 1, _) => Ok(load_pool_constant(p, rd, immed)),
                (2, 1, _, _) => load_store_register_offset(lsro_opcode, rm, rn, rd),
                (3, b, l, _) => load_store_immed_offset_word(b, l, shift_immed, rn, rd),
                (4, 0, l, _) => load_store_immed_offset_halfword(l, shift_immed, rn, rd),
                (4, 1, l, _) => load_store_stack_offset(l, math_rd_rn, immed),
                (5, 0, s, _) => compute_rel_addr(s, math_rd_rn, immed),
                (5, 1, a, b) => match (a, b, (instr & 0x0200) >> 9, (instr & 0x0100) >> 8) {
                    (0, 0, 0, 0) => sp_adjust(immed),
                    (0, 0, 1, 0) => sign_zero_extend(immed, rn, rd),
                    (1, 0, 1, 0) => endian_reverse(immed, rn, rd),
                    (l, 1, 0, r) => push_pop(l, r, immed),
                    (1, 1, 1, 0) => Ok(breakpoint(p, immed)),
                    _ => Err(analysis::Error::InvalidInstruction), //undefined
                },
                (6, 0, l, _) => load_store_multiple(l, math_rd_rn, immed),
                (6, 1, _, _) => cond_branch(p, cond, immed),
                (7, 0, 0, _) => Ok(uncond_branch(p, large_offset)),
                (7, 0, 1, _) => Err(analysis::Error::InvalidInstruction), //BLX suffix or undefined
                (7, 1, 0, _) => uncond_branch_link(p, mem, large_offset),
                (7, 1, 1, _) => Err(analysis::Error::InvalidInstruction), //BL suffix
                _ => Err(analysis::Error::InvalidInstruction),
            }
        }
        None => Err(analysis::Error::UnconstrainedMemory(p.clone())),
    }
}
