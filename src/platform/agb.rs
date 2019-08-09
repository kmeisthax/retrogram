//! AGB (Advanced GameBoy) platform support

use std::io;
use crate::memory::Behavior;
use crate::arch::aarch32;

pub fn construct_platform<F>(file: &mut F) -> io::Result<aarch32::Bus> where F: io::Read + io::Seek {
    let mut bus = aarch32::Bus::new();

    //TODO: The BIOS region is weird.
    //It really should have a ROM image attached, but that's copyright Nintendo.
    //Furthermore, it's execute-only; reads are open-bus and we can't model that
    //yet.
    bus.install_mem(0x00000000, 0x00004000, Behavior::Invalid, Behavior::Invalid, Behavior::Memory); //BIOS
    bus.install_ram(0x02000000, 0x00040000); //EWRAM
    bus.install_ram(0x03000000, 0x00008000); //IOWRAM
    bus.install_io(0x04000000, 0x000003FF); //IOREG
    bus.install_ram(0x05000000, 0x00000400); //CGRAM
    bus.install_ram(0x06000000, 0x00018000); //VRAM
    bus.install_ram(0x07000000, 0x00000400); //OAM
    file.seek(io::SeekFrom::Start(0))?;
    bus.install_rom(0x08000000, 0x02000000, file)?;
    file.seek(io::SeekFrom::Start(0))?;
    bus.install_rom(0x0A000000, 0x02000000, file)?;
    file.seek(io::SeekFrom::Start(0))?;
    bus.install_rom(0x0C000000, 0x02000000, file)?;
    bus.install_ram(0xE0000000, 0x00010000); //TODO: Model Flash

    Ok(bus)
}