//! THUMB instruction set disassembly

use crate::{memory, analysis};
use crate::analysis::Reference as refr;
use crate::analysis::ReferenceKind as refkind;
use crate::arch::aarch32::{Aarch32Register, Pointer, Offset, Bus, Instruction};
use crate::arch::aarch32::Operand as op;
use crate::arch::aarch32::arm::condcode;

fn cond_branch(p: &memory::Pointer<Pointer>, cond: u16, offset: u16) ->
    (Option<Instruction>, Offset, bool, bool, Vec<analysis::Reference<Pointer>>) {
    
    // signed_offset = (target - base + 4) / 2
    // therefore target = signed_offset * 2 - 4 + base
    let signed_offset = (((offset as u8) as i8) as i32) << 1;
    let target = (signed_offset - 4 + p.as_pointer().clone() as i32) as Pointer;
    let target_ptr = p.contextualize(target);
    
    match cond {
        //Unconditional conditional branches are undefined and are thus treated as illegal
        15 => (None, 0, false, false, vec![]),

        //TODO: The jump target can be in high RAM, how do we handle that?
        //TODO: This generates a reference to the SWI handler in THUMB mode,
        //which cannot happen on ARM hardware.
        16 => (Some(Instruction::new("SWI", vec![op::int(offset as u32)])), 2, true, true,
                vec![refr::new_static_ref(p.clone(), p.contextualize(0x00000008),
                    refkind::Subroutine)]),
        _ => (Some(Instruction::new(&format!("B{}", condcode(cond as u32)), vec![op::cptr(target)])),
            2, true, false, vec![refr::new_static_ref(p.clone(), target_ptr, refkind::Code)])
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
    let target = (signed_offset - 4 + p.as_pointer().clone() as i32) as Pointer;
    let target_ptr = p.contextualize(target);
    
    (Some(Instruction::new("B", vec![op::cptr(target)])), 2, true, false,
        vec![refr::new_static_ref(p.clone(), target_ptr, refkind::Code)])
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
            let add_sub_op = instr & 0x0200 >> 9;
            let immed = instr & 0x00FF; //sometimes also small-offset
            let math_rd_rn = instr & 0x0700 >> 8;
            let dp_opcode = instr & 0x03C0 >> 6;
            let lsro_opcode = instr & 0x0E00 >> 9;
            let cond = instr & 0x0F00 >> 8;
            let large_offset = instr & 0x07FF;

            match (instr >> 13, instr & 0x1000 >> 12, instr & 0x0800 >> 11, instr & 0x0400 >> 10) {
                (0, 1, 1, 0) => (None, 0, false, false, vec![]), //add/sub reg
                (0, 1, 1, 1) => (None, 0, false, false, vec![]), //add/sub imm
                (0, _, _, _) => (None, 0, false, false, vec![]), //shift imm
                (1, _, _, _) => (None, 0, false, false, vec![]), //math imm
                (2, 0, 0, 0) => (None, 0, false, false, vec![]), //data-processing reg
                (2, 0, 0, 1) => special_data(p, dp_opcode, rn, rd), //branch/exchange, special data processing
                (2, 0, 1, _) => (None, 0, false, false, vec![]), //load literal pool
                (2, 1, _, _) => (None, 0, false, false, vec![]), //load/store register offset
                (3, b, l, _) => (None, 0, false, false, vec![]), //load/store word/byte immediate offset
                (4, 0, l, _) => (None, 0, false, false, vec![]), //load/store halfword immediate offset
                (4, 1, l, _) => (None, 0, false, false, vec![]), //load/store stack offset
                (5, 0, _, _) => (None, 0, false, false, vec![]), //SP/PC increment
                (5, 1, _, _) => (None, 0, false, false, vec![]), //misc instruction space
                (6, 0, _, _) => (None, 0, false, false, vec![]), //ldm/stm
                (6, 1, _, _) => cond_branch(p, cond, immed), //cond branch, undefined, swi
                (7, 0, 0, _) => uncond_branch(p, large_offset), //uncond branch
                (7, 0, 1, _) => (None, 0, false, false, vec![]), //BLX suffix or undefined
                (7, 1, 0, _) => (None, 0, false, false, vec![]), //BL/BLX prefix
                (7, 1, 1, _) => (None, 0, false, false, vec![]), //BL suffix
                _ => (None, 0, false, false, vec![])
            }
        },
        None => (None, 0, false, false, vec![])
    }
}