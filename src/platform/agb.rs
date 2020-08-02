//! AGB (Advanced GameBoy) platform support

use crate::arch::aarch32;
use crate::memory::Behavior;
use std::io;

pub fn construct_platform<F>(file: &mut F) -> io::Result<aarch32::Bus>
where
    F: io::Read + io::Seek,
{
    let mut bus = aarch32::Bus::new();

    //TODO: The BIOS region is weird.
    //It really should have a ROM image attached, but that's copyright Nintendo.
    //Furthermore, it's execute-only; reads are open-bus and we can't model that
    //yet.
    bus.install_mem(
        0x0000_0000,
        0x0000_4000,
        Behavior::Invalid,
        Behavior::Invalid,
        Behavior::Memory,
    ); //BIOS
    bus.install_ram(0x0200_0000, 0x0004_0000); //EWRAM
    bus.install_ram(0x0300_0000, 0x0000_8000); //IOWRAM
    bus.install_io(0x0400_0000, 0x0000_03FF); //IOREG
    bus.install_ram(0x0500_0000, 0x0000_0400); //CGRAM
    bus.install_ram(0x0600_0000, 0x0001_8000); //VRAM
    bus.install_ram(0x0700_0000, 0x0000_0400); //OAM
    file.seek(io::SeekFrom::Start(0))?;
    bus.install_rom(0x0800_0000, 0x0200_0000, file)?;
    file.seek(io::SeekFrom::Start(0))?;
    bus.install_rom(0x0A00_0000, 0x0200_0000, file)?;
    file.seek(io::SeekFrom::Start(0))?;
    bus.install_rom(0x0C00_0000, 0x0200_0000, file)?;
    bus.install_ram(0xE000_0000, 0x0001_0000); //TODO: Model Flash

    Ok(bus)
}
