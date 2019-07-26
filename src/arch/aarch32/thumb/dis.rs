//! THUMB instruction set disassembly

use crate::{memory, analysis, reg};
use crate::analysis::Reference as refr;
use crate::analysis::ReferenceKind as refkind;
use crate::arch::aarch32::{Aarch32Register, Pointer, Offset, Bus, Instruction};
use crate::arch::aarch32::Operand as op;
use crate::arch::aarch32::arm::condcode;
use crate::arch::aarch32::thumb::THUMB_STATE;

fn cond_branch(p: &memory::Pointer<Pointer>, cond: u16, offset: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    // signed_offset = (target - base + 4) / 2
    // therefore target = signed_offset * 2 - 4 + base
    let signed_offset = (((offset as u8) as i8) as i32) << 1;
    let target = p.contextualize((signed_offset - 4 + p.as_pointer().clone() as i32) as Pointer);
    let mut swi_target = p.contextualize(0x00000008);
    
    swi_target.set_arch_context(THUMB_STATE, reg::Symbolic::from(0));
    
    match cond {
        //Unconditional conditional branches are undefined and are thus treated as illegal
        15 => (None, 0, false, false, vec![]),

        //TODO: The jump target can be in high RAM, how do we handle that?
        //TODO: This generates a reference to the SWI handler in THUMB mode,
        //which cannot happen on ARM hardware.
        16 => (Some(Instruction::new("SWI", vec![op::int(offset as u32)])), 2, true, true,
                vec![refr::new_static_ref(p.clone(), swi_target, refkind::Subroutine)]),
        _ => (Some(Instruction::new(&format!("B{}", condcode(cond as u32)), vec![op::cptr(target.clone())])),
            2, true, false, vec![refr::new_static_ref(p.clone(), target, refkind::Code)])
    }
}

fn uncond_branch(p: &memory::Pointer<Pointer>, offset: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    // signed_offset = (target - base + 4) / 2
    // therefore target = signed_offset * 2 - 4 + base
    let sign_extend = match offset & 0x0400 != 0 {
        true => 0xF800,
        false => 0x0000
    };
    let signed_offset = (((offset | sign_extend) as i16) as i32) << 1;
    let target = p.contextualize((signed_offset - 4 + p.as_pointer().clone() as i32) as Pointer);
    
    (Some(Instruction::new("B", vec![op::cptr(target.clone())])), 2, true, false,
        vec![refr::new_static_ref(p.clone(), target, refkind::Code)])
}

fn special_data(p: &memory::Pointer<Pointer>, dp_opcode: u16, low_rm: u16, low_rd: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let opcode = dp_opcode >> 2;
    let h1 = (dp_opcode & 0x2) << 2;
    let h2 = (dp_opcode & 0x1) << 3;
    let l = (dp_opcode & 0x2) != 0;
    let rm = h2 | low_rm;
    let rd = h1 | low_rd;

    let rd_reg = Aarch32Register::from_instr(rd as u32).expect("Invalid register");
    let rm_reg = Aarch32Register::from_instr(rm as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rm_operand = op::sym(&rm_reg.to_string());
    let branch_target = match rd_reg {
        Aarch32Register::R15 => vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
        _ => vec![]
    };
    let is_nonbranching = rd_reg != Aarch32Register::R15;

    match (opcode, l) {
        (0, _) => (Some(Instruction::new("ADD", vec![rd_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        (1, _) => (Some(Instruction::new("CMP", vec![rd_operand, rm_operand])), 2, true, true, vec![]),
        (2, _) => (Some(Instruction::new("MOV", vec![rd_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        (3, false) => (Some(Instruction::new("BX", vec![rm_operand])), 2, false, false, vec![refr::new_dyn_ref(p.clone(), refkind::Code)]),
        (3, true) => (Some(Instruction::new("BLX", vec![rm_operand])), 2, true, false, vec![refr::new_dyn_ref(p.clone(), refkind::Code)]),
        _ => panic!("Invalid opcode or L flag")
    }
}

fn data_processing(p: &memory::Pointer<Pointer>, dp_opcode: u16, low_rm: u16, low_rd: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {

    let rd_reg = Aarch32Register::from_instr(low_rd as u32).expect("Invalid register");
    let rm_reg = Aarch32Register::from_instr(low_rm as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rm_operand = op::sym(&rm_reg.to_string());
    let branch_target = match rd_reg {
        Aarch32Register::R15 => vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
        _ => vec![]
    };
    let is_nonbranching = rd_reg != Aarch32Register::R15;

    match dp_opcode {
        0 => (Some(Instruction::new("AND", vec![rd_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        1 => (Some(Instruction::new("EOR", vec![rd_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        2 => (Some(Instruction::new("LSL", vec![rd_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        3 => (Some(Instruction::new("LSR", vec![rd_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        4 => (Some(Instruction::new("ASR", vec![rd_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        5 => (Some(Instruction::new("ADC", vec![rd_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        7 => (Some(Instruction::new("ROR", vec![rd_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        6 => (Some(Instruction::new("SBC", vec![rd_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        8 => (Some(Instruction::new("TST", vec![rd_operand, rm_operand])), 2, true, true, vec![]),
        9 => (Some(Instruction::new("NEG", vec![rd_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        0xA => (Some(Instruction::new("CMP", vec![rd_operand, rm_operand])), 2, true, true, vec![]),
        0xB => (Some(Instruction::new("CMM", vec![rd_operand, rm_operand])), 2, true, true, vec![]),
        0xC => (Some(Instruction::new("ORR", vec![rd_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        0xD => (Some(Instruction::new("MUL", vec![rd_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        0xE => (Some(Instruction::new("BIC", vec![rd_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        0xF => (Some(Instruction::new("MVN", vec![rd_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        _ => panic!("Not a valid THUMB data processing instruction")
    }
}

fn add_sub_register(p: &memory::Pointer<Pointer>, add_sub_bit: u16, low_rm: u16, low_rn: u16, low_rd: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let rd_reg = Aarch32Register::from_instr(low_rd as u32).expect("Invalid register");
    let rn_reg = Aarch32Register::from_instr(low_rn as u32).expect("Invalid register");
    let rm_reg = Aarch32Register::from_instr(low_rm as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rn_operand = op::sym(&rn_reg.to_string());
    let rm_operand = op::sym(&rm_reg.to_string());
    let branch_target = match rd_reg {
        Aarch32Register::R15 => vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
        _ => vec![]
    };
    let is_nonbranching = rd_reg != Aarch32Register::R15;

    match add_sub_bit {
        0 => (Some(Instruction::new("ADD", vec![rd_operand, rn_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        1 => (Some(Instruction::new("SUB", vec![rd_operand, rn_operand, rm_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        _ => panic!("Neither an add nor sub")
    }
}

fn add_sub_immed(p: &memory::Pointer<Pointer>, add_sub_bit: u16, immed: u16, low_rn: u16, low_rd: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let rd_reg = Aarch32Register::from_instr(low_rd as u32).expect("Invalid register");
    let rn_reg = Aarch32Register::from_instr(low_rn as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rn_operand = op::sym(&rn_reg.to_string());
    let immed_operand = op::int(immed);
    let branch_target = match rd_reg {
        Aarch32Register::R15 => vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
        _ => vec![]
    };
    let is_nonbranching = rd_reg != Aarch32Register::R15;

    match add_sub_bit {
        0 => (Some(Instruction::new("ADD", vec![rd_operand, rn_operand, immed_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        1 => (Some(Instruction::new("SUB", vec![rd_operand, rn_operand, immed_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        _ => panic!("Neither an add nor sub")
    }
}

fn shifter_immed(p: &memory::Pointer<Pointer>, shift_opcode: u16, immed: u16, low_rm: u16, low_rd: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let rd_reg = Aarch32Register::from_instr(low_rd as u32).expect("Invalid register");
    let rm_reg = Aarch32Register::from_instr(low_rm as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rm_operand = op::sym(&rm_reg.to_string());
    let nz_immed = match immed {
        0 => 32,
        s => s
    };
    let branch_target = match rd_reg {
        Aarch32Register::R15 => vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
        _ => vec![]
    };
    let is_nonbranching = rd_reg != Aarch32Register::R15;

    match shift_opcode {
        0 => (Some(Instruction::new("LSL", vec![rd_operand, rm_operand, op::int(immed)])), 2, is_nonbranching, is_nonbranching, branch_target),
        1 => (Some(Instruction::new("LSR", vec![rd_operand, rm_operand, op::int(nz_immed)])), 2, is_nonbranching, is_nonbranching, branch_target),
        2 => (Some(Instruction::new("ASR", vec![rd_operand, rm_operand, op::int(nz_immed)])), 2, is_nonbranching, is_nonbranching, branch_target),
        _ => panic!("Invalid opcode for shifter immediate format")
    }
}

fn math_immed(p: &memory::Pointer<Pointer>, math_opcode: u16, low_rd: u16, immed: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let rd_reg = Aarch32Register::from_instr(low_rd as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let immed_operand = op::int(immed);
    //TODO: This particular form of target could be statically analyzed.
    let branch_target = match rd_reg {
        Aarch32Register::R15 => vec![refr::new_dyn_ref(p.clone(), refkind::Code)],
        _ => vec![]
    };
    let is_nonbranching = rd_reg != Aarch32Register::R15;

    match math_opcode {
        0 => (Some(Instruction::new("MOV", vec![rd_operand, immed_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        1 => (Some(Instruction::new("CMP", vec![rd_operand, immed_operand])), 2, true, true, vec![]),
        2 => (Some(Instruction::new("ADD", vec![rd_operand, immed_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        3 => (Some(Instruction::new("SUB", vec![rd_operand, immed_operand])), 2, is_nonbranching, is_nonbranching, branch_target),
        _ => panic!("Invalid math opcode")
    }
}

fn load_pool_constant(p: &memory::Pointer<Pointer>, low_rd: u16, immed: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let rd_reg = Aarch32Register::from_instr(low_rd as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let target_ptr = p.contextualize((p.as_pointer().clone() & 0xFFFFFFFC) + 4 + (immed as u32 * 4));
    let immed_operand = op::dptr(target_ptr.clone());
    let ast = Instruction::new("LDR", vec![rd_operand, op::indir(op::wrap("", vec![op::sym("PC"), immed_operand], ""))]); 

    (Some(ast), 2, true, true, vec![refr::new_static_ref(p.clone(), target_ptr, refkind::Data)])
}

fn load_store_register_offset(p: &memory::Pointer<Pointer>, opcode: u16, low_rm: u16, low_rn: u16, low_rd: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let rd_reg = Aarch32Register::from_instr(low_rd as u32).expect("Invalid register");
    let rn_reg = Aarch32Register::from_instr(low_rn as u32).expect("Invalid register");
    let rm_reg = Aarch32Register::from_instr(low_rm as u32).expect("Invalid register");
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
        _ => panic!("Invalid opcode")
    };

    (Some(Instruction::new(opcode_name, vec![rd_operand, op::wrap("[", vec![rn_operand, rm_operand], "]")])), 2, true, true, vec![])
}

fn load_store_immed_offset_word(p: &memory::Pointer<Pointer>, b: u16, l: u16, offset: u16, low_rn: u16, low_rd: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let rd_reg = Aarch32Register::from_instr(low_rd as u32).expect("Invalid register");
    let rn_reg = Aarch32Register::from_instr(low_rn as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rn_operand = op::sym(&rn_reg.to_string());
    let size = match b {
        0 => 4,
        1 => 1,
        _ => panic!("Invalid size bit")
    };
    let offset_operand = op::int(offset * size);

    let opcode_name = match (b, l) {
        (0, 0) => "STR",
        (1, 0) => "STRB",
        (0, 1) => "LDR",
        (1, 1) => "LDRB",
        _ => panic!("Invalid size/direction bits!")
    };

    (Some(Instruction::new(opcode_name, vec![rd_operand, op::wrap("[", vec![rn_operand, offset_operand], "]")])), 2, true, true, vec![])
}

fn load_store_immed_offset_halfword(p: &memory::Pointer<Pointer>, l: u16, offset: u16, low_rn: u16, low_rd: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let rd_reg = Aarch32Register::from_instr(low_rd as u32).expect("Invalid register");
    let rn_reg = Aarch32Register::from_instr(low_rn as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let rn_operand = op::sym(&rn_reg.to_string());
    let offset_operand = op::int(offset * 2);

    let opcode_name = match l {
        0 => "STRH",
        1 => "LDRH",
        _ => panic!("Invalid direction bit!")
    };

    (Some(Instruction::new(opcode_name, vec![rd_operand, op::wrap("[", vec![rn_operand, offset_operand], "]")])), 2, true, true, vec![])
}

fn load_store_stack_offset(p: &memory::Pointer<Pointer>, l: u16, low_rd: u16, offset: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let rd_reg = Aarch32Register::from_instr(low_rd as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let offset_operand = op::int(offset * 4);

    let opcode_name = match l {
        0 => "STRH",
        1 => "LDRH",
        _ => panic!("Invalid direction bit!")
    };

    (Some(Instruction::new(opcode_name, vec![rd_operand, op::wrap("[", vec![op::sym("SP"), offset_operand], "]")])), 2, true, true, vec![])
}

fn compute_rel_addr(p: &memory::Pointer<Pointer>, s: u16, low_rd: u16, offset: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let rd_reg = Aarch32Register::from_instr(low_rd as u32).expect("Invalid register");
    let rd_operand = op::sym(&rd_reg.to_string());
    let offset_operand = op::int(offset * 4);

    let s_operand = op::sym(match s {
        0 => "PC",
        1 => "SP",
        _ => panic!("Invalid direction bit!")
    });

    (Some(Instruction::new("ADD", vec![rd_operand, op::wrap("[", vec![s_operand, offset_operand], "]")])), 2, true, true, vec![])
}

fn load_store_multiple(p: &memory::Pointer<Pointer>, l: u16, low_rn: u16, register_list: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    let rn_reg = Aarch32Register::from_instr(low_rn as u32).expect("Invalid register");
    let rn_operand = op::sym(&rn_reg.to_string());
    let instr = match l {
        0 => "STMIA",
        1 => "LDMIA",
        _ => panic!("Invalid L bit")
    };
    let mut register_list_operand = vec![];

    for i in 0..7 {
        if register_list & (1 << i) != 0 {
            let reg = Aarch32Register::from_instr(i).expect("This should be valid");
            let reg_operand = op::sym(&reg.to_string());
            register_list_operand.push(reg_operand);
        }
    }

    (Some(Instruction::new(instr, vec![op::suff(rn_operand, "!"), op::wrap("{", register_list_operand, "}")])), 2, true, true, vec![])
}

fn uncond_branch_link(p: &memory::Pointer<Pointer>, mem: &Bus, high_offset: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    match mem.read_leword::<u16>(&(p.clone() + 2)).into_concrete() {
        Some(low_instr) if low_instr & 0xE000 == 0xE000 => {
            let h = (low_instr & 0x1800) >> 11;
            let low_offset = low_instr & 0x07FF;
            let sign = match high_offset & 0x0400 {
                0 => 0,
                _ => 0xFFC00000
            };

            let offset : u32 = sign | (high_offset as u32) << 11 | (low_offset as u32);
            let target = p.contextualize((p.as_pointer().clone() as i32 + offset as i32) as u32);
            let mut arm_target = target.clone();
            arm_target.set_arch_context(THUMB_STATE, reg::Symbolic::from(0));

            match h {
                1 => (Some(Instruction::new("BLX", vec![op::cptr(arm_target.clone())])), 4, true, false, vec![refr::new_static_ref(p.clone(), arm_target, refkind::Subroutine)]),
                3 => (Some(Instruction::new("BL", vec![op::cptr(target.clone())])), 4, true, false, vec![refr::new_static_ref(p.clone(), target, refkind::Subroutine)]),
                _ => (None, 0, false, false, vec![])
            }
        },
        _ => (None, 0, false, false, vec![])
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
pub fn disassemble(p: &memory::Pointer<Pointer>, mem: &Bus) ->
    (Option<Instruction>, Offset, bool, bool, Vec<refr<Pointer>>) {
    
    match mem.read_leword::<u16>(p).into_concrete() {
        Some(instr) => {
            let rd = instr & 0x0007; //sometimes also sbz
            let rn = instr & 0x0038 >> 3; //sometimes also rm or rs
            let shift_immed = instr & 0x07C0 >> 6;
            let shift_opcode = instr & 0x1800 >> 11; //also used by math imm
            let rm = instr & 0x01C0 >> 6; //sometimes also rn or add/sub immed
            let opc = instr & 0x0200 >> 9; //add/sub bit for instructions
            let immed = instr & 0x00FF; //sometimes also small-offset
            let math_rd_rn = instr & 0x0700 >> 8;
            let dp_opcode = instr & 0x03C0 >> 6;
            let lsro_opcode = instr & 0x0E00 >> 9;
            let cond = instr & 0x0F00 >> 8;
            let large_offset = instr & 0x07FF;

            match (instr >> 13, instr & 0x1000 >> 12, instr & 0x0800 >> 11, instr & 0x0400 >> 10) {
                (0, 1, 1, 0) => add_sub_register(p, opc, rm, rn, rd),
                (0, 1, 1, 1) => add_sub_immed(p, opc, rm, rn, rd),
                (0, _, _, _) => shifter_immed(p, shift_opcode, shift_immed, rn, rd),
                (1, _, _, _) => math_immed(p, shift_opcode, math_rd_rn, immed),
                (2, 0, 0, 0) => data_processing(p, dp_opcode, rn, rd),
                (2, 0, 0, 1) => special_data(p, dp_opcode, rn, rd),
                (2, 0, 1, _) => load_pool_constant(p, rd, immed),
                (2, 1, _, _) => load_store_register_offset(p, lsro_opcode, rm, rn, rd),
                (3, b, l, _) => load_store_immed_offset_word(p, b, l, shift_immed, rn, rd),
                (4, 0, l, _) => load_store_immed_offset_halfword(p, l, shift_immed, rn, rd),
                (4, 1, l, _) => load_store_stack_offset(p, l, math_rd_rn, immed),
                (5, 0, s, _) => compute_rel_addr(p, s, math_rd_rn, immed),
                (5, 1, _, _) => (None, 0, false, false, vec![]), //misc instruction space
                (6, 0, l, _) => load_store_multiple(p, l, math_rd_rn, immed),
                (6, 1, _, _) => cond_branch(p, cond, immed),
                (7, 0, 0, _) => uncond_branch(p, large_offset),
                (7, 0, 1, _) => (None, 0, false, false, vec![]), //BLX suffix or undefined
                (7, 1, 0, _) => uncond_branch_link(p, mem, large_offset),
                (7, 1, 1, _) => (None, 0, false, false, vec![]), //BL suffix
                _ => (None, 0, false, false, vec![])
            }
        },
        None => (None, 0, false, false, vec![])
    }
}