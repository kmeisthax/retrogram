//! ARM instruction set

mod dis;

pub use dis::disassemble;

use crate::analysis;
use crate::arch::aarch32;

pub fn condcode(instr: u32) -> aarch32::Result<&'static str> {
    match instr {
        0 => Ok("EQ"),
        1 => Ok("NE"),
        2 => Ok("CS"),
        3 => Ok("CC"),
        4 => Ok("MI"),
        5 => Ok("PL"),
        6 => Ok("VS"),
        7 => Ok("VC"),
        8 => Ok("HI"),
        9 => Ok("LS"),
        10 => Ok("GE"),
        11 => Ok("LT"),
        12 => Ok("GT"),
        13 => Ok("LE"),
        14 => Ok(""),
        15 => Err(analysis::Error::Misinterpretation(4, false)),
        _ => Err(analysis::Error::Misinterpretation(4, false))
    }
}