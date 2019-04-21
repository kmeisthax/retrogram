//! Platform implementation for Game Boy and it's attendant memory mapper chips

use std::io;
use std::ops::BitAnd;
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

pub fn create_context<V>(values: &Vec<V>) -> Option<memory::Pointer<lr35902::Pointer>> where V: std::fmt::Debug + Copy + BitAnd + From<lr35902::Pointer>, lr35902::Pointer: From<<V as BitAnd>::Output> + From<V>, u64: From<V> {
    let mut context = memory::Pointer::from(lr35902::Pointer::from(values[values.len() - 1]));

    if values.len() > 1 {
        match u16::from(values[values.len() - 1] & V::from(0xC000)) {
            0x0000 => {}, //HOME
            0x2000 => {},
            0x4000 => context.set_platform_context("R", reg::Symbolic::from(u64::from(values[values.len() - 2]))), //Bank
            0x6000 => context.set_platform_context("R", reg::Symbolic::from(u64::from(values[values.len() - 2]))), // ROM
            0x8000 => context.set_platform_context("V", reg::Symbolic::from(u64::from(values[values.len() - 2]))), //VRAM
            0xA000 => context.set_platform_context("S", reg::Symbolic::from(u64::from(values[values.len() - 2]))), //SRAM
            0xC000 => context.set_platform_context("W", reg::Symbolic::from(u64::from(values[values.len() - 2]))), //WRAM
            0xE000 => {}, //ECHO, IO, HRAMetc
            _ => panic!("Your AND is wrong..."),
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
pub fn construct_platform<F>(file: &mut F, pv: PlatformVariant) -> io::Result<lr35902::Bus> where F: io::Read {
    let mut bus = lr35902::Bus::new();

    match pv {
        PlatformVariant::LinearMapper => bus.install_mem_image(0x0000, 0x8000, Box::new(GameBoyROMImage::new(file, LinearMapper::new())?)),
        PlatformVariant::MBC1Mapper => bus.install_mem_image(0x0000, 0x8000, Box::new(GameBoyROMImage::new(file, MBC1Mapper::new())?)),
        PlatformVariant::MBC2Mapper => bus.install_mem_image(0x0000, 0x8000, Box::new(GameBoyROMImage::new(file, MBC2Mapper::new())?)),
        PlatformVariant::MBC3Mapper => bus.install_mem_image(0x0000, 0x8000, Box::new(GameBoyROMImage::new(file, MBC3Mapper::new())?)),
        PlatformVariant::MBC5Mapper => bus.install_mem_image(0x0000, 0x8000, Box::new(GameBoyROMImage::new(file, MBC5Mapper::new())?)),
        PlatformVariant::UnknownMapper => return Err(io::Error::new(io::ErrorKind::Other, "Not yet implemented"))
    }

    bus.install_mem(0x8000, 0x2000); //VRAM
    bus.install_mem(0xA000, 0x2000); //SRAM (todo: this should be modeled better...)
    bus.install_mem(0xC000, 0x2000); //WRAM (todo: bankable WRAM)
    bus.install_mem(0xFE00, 0x009F); //OAM
    bus.install_io (0xFF00, 0x007F); //IO space
    bus.install_mem(0xFF80, 0x007F); //HRAM
    bus.install_io (0xFFFF, 0x0001); //Interrupt enable
    bus.openbus_mem(0xE000, 0x1000); //Echo RAM

    Ok(bus)
}