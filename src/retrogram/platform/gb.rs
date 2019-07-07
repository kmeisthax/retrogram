//! Platform implementation for Game Boy and it's attendant memory mapper chips

use std::io;
use crate::retrogram::{reg, memory};
use crate::retrogram::arch::lr35902;

/// Any type which decodes the banked memory region (0x4000) of a Game Boy ROM
/// image.
trait Mapper {

    fn decode_banked_addr(&self, ptr: &memory::Pointer<lr35902::Pointer>) -> Option<usize>;
}

/// Mapper type which does not support banking.
/// 
/// This supports Game Boy games up to a maximum of 32KB.
struct LinearMapper {

}

impl LinearMapper {
    fn new() -> Self {
        LinearMapper {}
    }
}

impl Mapper for LinearMapper {
    fn decode_banked_addr(&self, ptr: &memory::Pointer<lr35902::Pointer>) -> Option<usize> {
        Some((ptr.as_pointer() & 0x3FFF + 0x4000) as usize)
    }
}

struct MBC1Mapper {
}

impl MBC1Mapper {
    fn new() -> Self {
        MBC1Mapper {
        }
    }
}

impl Mapper for MBC1Mapper {
    fn decode_banked_addr(&self, ptr: &memory::Pointer<lr35902::Pointer>) -> Option<usize> {
        match ptr.get_platform_context("R").into_concrete() {
            Some(0x00) => None,
            Some(0x20) => None,
            Some(0x40) => None,
            Some(0x60) => None,
            Some(b) => Some(((*ptr.as_pointer() as usize) & 0x3FFF) + (b * 0x4000) as usize),
            None => None
        }
    }
}

struct MBC2Mapper {
}

impl MBC2Mapper {
    fn new() -> Self {
        MBC2Mapper {
        }
    }
}

impl Mapper for MBC2Mapper {
    fn decode_banked_addr(&self, ptr: &memory::Pointer<lr35902::Pointer>) -> Option<usize> {
        match ptr.get_platform_context("R").into_concrete() {
            Some(b) => Some(((*ptr.as_pointer() as usize) & 0x3FFF) + ((b & 0xF) * 0x4000) as usize),
            None => None
        }
    }
}

struct MBC3Mapper {
}

impl MBC3Mapper {
    fn new() -> Self {
        MBC3Mapper {
        }
    }
}

impl Mapper for MBC3Mapper {
    fn decode_banked_addr(&self, ptr: &memory::Pointer<lr35902::Pointer>) -> Option<usize> {
        match ptr.get_platform_context("R").into_concrete() {
            Some(b) => Some(((*ptr.as_pointer() as usize) & 0x3FFF) + (b * 0x4000) as usize),
            None => None
        }
    }
}

struct MBC5Mapper {
}

impl MBC5Mapper {
    fn new() -> Self {
        MBC5Mapper {
        }
    }
}

impl Mapper for MBC5Mapper {
    fn decode_banked_addr(&self, ptr: &memory::Pointer<lr35902::Pointer>) -> Option<usize> {
        match ptr.get_platform_context("R").into_concrete() {
            Some(b) => Some(((*ptr.as_pointer() as usize) & 0x3FFF) + (b * 0x4000) as usize),
            None => None
        }
    }
}

/// A program ROM image for Game Boy software.
/// 
/// With very few exceptions, all Game Boy ROM images service two memory
/// regions: a `HOME` memory region at `$0000` that is 16KB large, and a banked
/// memory region at `$4000` that is also 16KB large. As a consequence, Game Boy
/// ROM images are composed of 16KB chunks of memory that can mapped in or out
/// depending on a mapper-specific memory scheme.
struct GameBoyROMImage<M> where M: Mapper {
    data: Vec<u8>,
    mapper: M
}

impl<M> GameBoyROMImage<M> where M: Mapper {
    pub fn new<F>(file: &mut F, mapper: M) -> io::Result<Self> where F: io::Read {
        let mut data = Vec::new();

        file.read_to_end(&mut data)?;

        Ok(GameBoyROMImage {
            data: data,
            mapper: mapper
        })
    }
}

impl<M> memory::Image for GameBoyROMImage<M> where M: Mapper {
    type Pointer = lr35902::Pointer;
    type Offset = usize;
    type Data = lr35902::Data;

    fn retrieve(&self, offset: Self::Offset, count: Self::Offset) -> Option<&[Self::Data]> {
        Some(&self.data[offset as usize..(offset + count) as usize])
    }

    fn decode_addr(&self, ptr: &memory::Pointer<Self::Pointer>, base: Self::Pointer) -> Option<Self::Offset> {
        if base == 0x0000 && *ptr.as_pointer() < 0x4000 {
            Some(*ptr.as_pointer() as usize)
        } else {
            self.mapper.decode_banked_addr(ptr)
        }
    }

    fn minimize_context(&self, ptr: memory::Pointer<Self::Pointer>) -> memory::Pointer<Self::Pointer> {
        let my_ctxt = ptr.get_platform_context("R");
        let mut stripped_ptr = memory::Pointer::from(ptr.as_pointer().clone());
        
        if *stripped_ptr.as_pointer() > 0x4000 {
            stripped_ptr.set_platform_context("R", my_ctxt);
        }

        stripped_ptr
    }

    fn insert_user_context(&self, mut ptr: memory::Pointer<Self::Pointer>, ctxts: &[u64]) -> memory::Pointer<Self::Pointer> {
        if *ptr.as_pointer() > 0x4000 {
            match ctxts.get(0) {
                Some(ctxt) => ptr.set_platform_context("R", reg::Symbolic::from(*ctxt)),
                _ => {}
            }
        }

        ptr
    }
}

pub enum PlatformVariant {
    LinearMapper,
    MBC1Mapper,
    MBC2Mapper,
    MBC3Mapper,
    MBC5Mapper,
    UnknownMapper,
}

impl Default for PlatformVariant {
    fn default() -> Self {
        PlatformVariant::UnknownMapper
    }
}

pub fn create_context<V>(values: &Vec<V>) -> Option<memory::Pointer<lr35902::Pointer>>
    where V: Clone + PartialOrd + From<lr35902::Pointer>,
        lr35902::Pointer: From<V>,
        u64: From<V> {
    let mut context = memory::Pointer::from(lr35902::Pointer::from(values[values.len() - 1].clone()));

    if values.len() > 1 {
        if values[values.len() - 1] >= V::from(0xE000) {
        } else if values[values.len() - 1] >= V::from(0xC000) {
            context.set_platform_context("W", reg::Symbolic::from(u64::from(values[values.len() - 2].clone())));
        } else if values[values.len() - 1] >= V::from(0xA000) {
            context.set_platform_context("S", reg::Symbolic::from(u64::from(values[values.len() - 2].clone())));
        } else if values[values.len() - 1] >= V::from(0x8000) {
            context.set_platform_context("V", reg::Symbolic::from(u64::from(values[values.len() - 2].clone())));
        } else if values[values.len() - 1] >= V::from(0x4000) {
            context.set_platform_context("R", reg::Symbolic::from(u64::from(values[values.len() - 2].clone())));
        }
    }

    if values.len() > 0 {
        Some(context)
    } else {
        None
    }
}

/// Construct a `Memory` corresponding to the execution environment of a given
/// Game Boy ROM image.
/// 
/// You may optionally specify a `PlatformVariant` to control which MBC behavior
/// is used to analyze the image. If unspecified, the ROM header will be used to
/// determine which MBC was intended to be used alongside this program.
pub fn construct_platform<F>(file: &mut F, mut pv: PlatformVariant) -> io::Result<lr35902::Bus> where F: io::Read + io::Seek {
    let mut bus = lr35902::Bus::new();

    pv = match pv {
        PlatformVariant::UnknownMapper => {
            let orig_pos = file.seek(io::SeekFrom::Current(0))?;
            file.seek(io::SeekFrom::Start(0x147))?;

            let mut romtype : [u8; 1] = [0];
            file.read(&mut romtype)?;

            match romtype {
                [0x00] => PlatformVariant::LinearMapper, //ROM w/o RAM
                [0x01] => PlatformVariant::MBC1Mapper, //MBC1 ROM
                [0x02] => PlatformVariant::MBC1Mapper, //MBC1 ROM with RAM
                [0x03] => PlatformVariant::MBC1Mapper, //MBC1 ROM with persistent RAM
                [0x05] => PlatformVariant::MBC2Mapper, //MBC2 ROM; TODO: MBC2 has weird 4-bit SRAM and we should always return symbolic 4-bit values
                [0x06] => PlatformVariant::MBC2Mapper, //MBC2 ROM w/ persistence
                [0x08] => PlatformVariant::LinearMapper, //ROM with RAM
                [0x09] => PlatformVariant::LinearMapper, //ROM with persistent RAM
                [0x0B] => PlatformVariant::UnknownMapper, //MMM01, currently not supported
                [0x0C] => PlatformVariant::UnknownMapper, //MMM01, currently not supported, with RAM
                [0x0D] => PlatformVariant::UnknownMapper, //MMM01, currently not supported, with persistent RAM
                [0x0F] => PlatformVariant::MBC3Mapper, //MBC3 with persistent clock
                [0x10] => PlatformVariant::MBC3Mapper, //MBC3 with persistent clock and RAM
                [0x11] => PlatformVariant::MBC3Mapper, //MBC3 ROM only
                [0x12] => PlatformVariant::MBC3Mapper, //MBC3 with RAM
                [0x13] => PlatformVariant::MBC3Mapper, //MBC3 with persistent RAM, no clock
                [0x19] => PlatformVariant::MBC5Mapper, //MBC5 ROM only
                [0x1A] => PlatformVariant::MBC5Mapper, //MBC5 with RAM
                [0x1B] => PlatformVariant::MBC5Mapper, //MBC5 with persistent RAM
                [0x1C] => PlatformVariant::MBC5Mapper, //MBC5 with rumble motor
                [0x1D] => PlatformVariant::MBC5Mapper, //MBC5 with rumble motor and RAM
                [0x1E] => PlatformVariant::MBC5Mapper, //MBC5 with rumble motor and persistent RAM
                [0x20] => PlatformVariant::UnknownMapper, //MBC6 ROM only
                [0x22] => PlatformVariant::UnknownMapper, //MBC7 with tilt sensor, rumble motor, and persistent EEPROM
                [0xFC] => PlatformVariant::UnknownMapper, //Game Boy Camera with CCD video sensor
                [0xFD] => PlatformVariant::UnknownMapper, //Bandai TAMA5 (capabilities unknown)
                [0xFE] => PlatformVariant::UnknownMapper, //HuC3 (capabilities unknown)
                [0xFF] => PlatformVariant::UnknownMapper, //HuC1 with cartridge infrared port and persistent RAM
                _ => PlatformVariant::UnknownMapper
            }
        },
        e => e
    };

    match pv {
        PlatformVariant::LinearMapper => bus.install_rom_image(0x0000, 0x8000, Box::new(GameBoyROMImage::new(file, LinearMapper::new())?)),
        PlatformVariant::MBC1Mapper => bus.install_rom_image(0x0000, 0x8000, Box::new(GameBoyROMImage::new(file, MBC1Mapper::new())?)),
        PlatformVariant::MBC2Mapper => bus.install_rom_image(0x0000, 0x8000, Box::new(GameBoyROMImage::new(file, MBC2Mapper::new())?)),
        PlatformVariant::MBC3Mapper => bus.install_rom_image(0x0000, 0x8000, Box::new(GameBoyROMImage::new(file, MBC3Mapper::new())?)),
        PlatformVariant::MBC5Mapper => bus.install_rom_image(0x0000, 0x8000, Box::new(GameBoyROMImage::new(file, MBC5Mapper::new())?)),
        PlatformVariant::UnknownMapper => panic!("Platform variant detection failed! Please manually specify the platform variant.")
    }

    bus.install_ram(0x8000, 0x2000); //VRAM
    bus.install_ram(0xA000, 0x2000); //SRAM (todo: this should be modeled better...)
    bus.install_ram(0xC000, 0x2000); //WRAM (todo: bankable WRAM)
    bus.install_ram(0xFE00, 0x009F); //OAM
    bus.install_io (0xFF00, 0x007F); //IO space
    bus.install_ram(0xFF80, 0x007F); //HRAM
    bus.install_io (0xFFFF, 0x0001); //Interrupt enable
    bus.install_openbus(0xE000, 0x1000); //Echo RAM

    Ok(bus)
}