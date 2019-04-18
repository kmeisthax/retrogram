//! Analysis database - Allows accumulation of program facts as disassembly
//! passes run on the program.

use std::collections::HashMap;
use std::hash::Hash;
use std::convert::TryFrom;
use std::fmt::{Display, Debug, UpperHex};
use crate::retrogram::{ast, memory};

/// A repository of information obtained from the program under analysis.
pub struct Database<P> {
    /// A list of all labels in the program.
    labels: HashMap<ast::Label, memory::Pointer<P>>,
    
    /// A list of all pointer values in the program which have a label.
    pointers: HashMap<memory::Pointer<P>, ast::Label>
}

impl<P> Database<P> where P: Clone + Eq + Hash {
    pub fn new() -> Self {
        Database {
            labels: HashMap::new(),
            pointers: HashMap::new()
        }
    }

    pub fn insert_label(&mut self, label: ast::Label, ptr: memory::Pointer<P>) {
        self.labels.insert(label.clone(), ptr.clone());
        self.pointers.insert(ptr, label);
    }

    pub fn pointer_label(&self, ptr: memory::Pointer<P>) -> Option<&ast::Label> {
        self.pointers.get(&ptr)
    }
}

/// Given an operand, replace all Pointer literals with Label operands obtained
/// from the Database.
pub fn replace_operand_with_label<I, F, P, AP>(src_operand: &ast::Operand<ast::Literal<I, F, P>>, db: &Database<AP>, start_addr: &memory::Pointer<AP>) -> ast::Operand<ast::Literal<I, F, P>> where AP: Clone + Eq + Hash + TryFrom<P>, ast::Literal<I, F, P>: Clone, <AP as TryFrom<P>>::Error : Debug {
    match src_operand.clone() {
        ast::Operand::Literal(ast::Literal::Pointer(pt)) => {
            if let Some(lbl) = db.pointer_label(start_addr.contextualize(AP::try_from(pt).expect("Label operand does not fit in architecture's target pointer size."))) {
                ast::Operand::Label(lbl.clone())
            } else {
                src_operand.clone()
            }
        },
        ast::Operand::DataReference(op) => ast::Operand::DataReference(Box::new(replace_operand_with_label(op.as_ref(), db, start_addr))),
        ast::Operand::CodeReference(op) => ast::Operand::CodeReference(Box::new(replace_operand_with_label(op.as_ref(), db, start_addr))),
        ast::Operand::Indirect(op) => ast::Operand::Indirect(Box::new(replace_operand_with_label(op.as_ref(), db, start_addr))),
        ast::Operand::Add(opl, opr) => ast::Operand::Add(Box::new(replace_operand_with_label(opl.as_ref(), db, start_addr)), Box::new(replace_operand_with_label(opr.as_ref(), db, start_addr))),
        _ => src_operand.clone()
    }
}

/// Given an Assembly, create a new Assembly with all pointers replaced with
/// their equivalent labels in the database.
pub fn replace_labels<I, F, P, AP>(src_assembly: ast::Assembly<I, F, P>, db: &Database<AP>) -> ast::Assembly<I, F, P> where P: Clone, AP: Clone + Eq + Hash + TryFrom<P>, ast::Literal<I, F, P>: Clone, ast::Line<I, F, P>: Clone, <AP as TryFrom<P>>::Error : Debug {
    let mut dst_assembly = ast::Assembly::new();

    for line in src_assembly.iter_lines() {
        if let Some(instr) = line.instr() {
            let mut new_operands = Vec::new();

            for operand in instr.iter_operands() {
                new_operands.push(replace_operand_with_label(operand, db, &line.source_address().clone().try_into_ptr().expect("Disassembly source address does not fit in architecture's target pointer size.")));
            }

            let new_instr = ast::Instruction::new(instr.opcode(), new_operands);
            let (label, old_instr, comment, src_addr) = line.clone().into_parts();
            dst_assembly.append_line(ast::Line::new(label, Some(new_instr), comment, src_addr));
        } else {
            dst_assembly.append_line(line.clone());
        }
    }

    dst_assembly
}

/// Given an Assembly, create a new Assembly with all labels inserted from the
/// database.
pub fn inject_labels<I, F, P, AP>(src_assembly: ast::Assembly<I, F, P>, db: &Database<AP>) -> ast::Assembly<I, F, P> where AP: Clone + Eq + Hash + TryFrom<P>, ast::Line<I, F, P>: Clone, memory::Pointer<P>: Clone, <AP as TryFrom<P>>::Error : Debug {
    let mut dst_assembly = ast::Assembly::new();
    
    for line in src_assembly.iter_lines() {
        if let None = line.label() {
            let (label, old_instr, comment, src_addr) = line.clone().into_parts();
            if let Some(new_label) = db.pointer_label(src_addr.clone().try_into_ptr().expect("Label operand does not fit in architecture's target pointer size.")) {
                dst_assembly.append_line(ast::Line::new(Some(new_label.clone()), old_instr, comment, src_addr));
            } else {
                dst_assembly.append_line(line.clone());
            }
        } else {
            dst_assembly.append_line(line.clone());
        }
    }

    dst_assembly
}

/// Given an operand, replace all Pointer literals with automatically generated
/// labels.
/// 
/// All automatically generated labels will be registered in the database.
pub fn replace_operand_with_templabel<I, F, P, AP>(src_operand: &ast::Operand<ast::Literal<I, F, P>>, db: &mut Database<AP>, start_addr: &memory::Pointer<AP>, in_dataref: bool, in_coderef: bool) -> ast::Operand<ast::Literal<I, F, P>> where P: UpperHex, AP: Clone + Eq + Hash + TryFrom<P>, ast::Literal<I, F, P>: Clone, <AP as TryFrom<P>>::Error : Debug {
    match src_operand.clone() {
        ast::Operand::Literal(ast::Literal::Pointer(pt)) => {
            //TODO: This preserves context, but perhaps a little too well.
            //In contrived scenarios we might accidentally carry a bunch of
            //useless contexts that clutter up the label.
            let mut name = match (in_dataref, in_coderef) {
                (true, false) => "DATA_",
                (false, true) => "CODE_",
                _ => "UNK_"
            }.to_string();

            for (_, key, cval) in start_addr.iter_contexts() {
                name = match cval.into_concrete() {
                    Some(cval) => format!("{}_{:X}", name, cval),
                    _ => format!("{}_{}??", name, key)
                };
            }

            name = format!("{}_{:X}", name, pt);

            db.insert_label(ast::Label::new(&name, None), start_addr.contextualize(AP::try_from(pt).expect("Label operand does not fit in architecture's target pointer size.")));
            ast::Operand::Label(ast::Label::new(&name, None))
        },
        ast::Operand::DataReference(op) => ast::Operand::DataReference(Box::new(replace_operand_with_templabel(op.as_ref(), db, start_addr, true, false))),
        ast::Operand::CodeReference(op) => ast::Operand::CodeReference(Box::new(replace_operand_with_templabel(op.as_ref(), db, start_addr, false, true))),
        ast::Operand::Indirect(op) => ast::Operand::Indirect(Box::new(replace_operand_with_templabel(op.as_ref(), db, start_addr, in_dataref, in_coderef))),
        ast::Operand::Add(opl, opr) => ast::Operand::Add(Box::new(replace_operand_with_templabel(opl.as_ref(), db, start_addr, in_dataref, in_coderef)), Box::new(replace_operand_with_templabel(opr.as_ref(), db, start_addr, in_dataref, in_coderef))),
        _ => src_operand.clone()
    }
}

/// Given an Assembly, create a new Assembly with all data and code references
/// replaced with temporary labels.
pub fn create_temp_labels<I, F, P, AP>(src_assembly: ast::Assembly<I, F, P>, db: &mut Database<AP>) -> ast::Assembly<I, F, P> where P: UpperHex, AP: Clone + Eq + Hash + TryFrom<P>, ast::Line<I, F, P>: Clone, ast::Literal<I, F, P>: Clone, memory::Pointer<P>: Clone, <AP as TryFrom<P>>::Error : Debug {
    let mut dst_assembly = ast::Assembly::new();

    for line in src_assembly.iter_lines() {
        if let Some(instr) = line.instr() {
            let mut new_operands = Vec::new();

            for operand in instr.iter_operands() {
                new_operands.push(replace_operand_with_templabel(operand, db, &line.source_address().clone().try_into_ptr().expect("Disassembly source address does not fit in architecture's target pointer size."), false, false));
            }

            let new_instr = ast::Instruction::new(instr.opcode(), new_operands);
            let (label, old_instr, comment, src_addr) = line.clone().into_parts();
            dst_assembly.append_line(ast::Line::new(label, Some(new_instr), comment, src_addr));
        } else {
            dst_assembly.append_line(line.clone());
        }
    }

    dst_assembly
}