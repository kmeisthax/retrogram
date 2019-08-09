//! ARM instruction set

mod dis;

pub use dis::disassemble;

pub fn condcode(instr: u32) -> &'static str {
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
        14 => "",
        15 => panic!("Condition code not valid for conditional instruction"),
        _ => panic!("Not a valid condition code")
    }
}