//! Platform implementation for Game Boy and it's attendant memory mapper chips

use crate::analysis::Requisite;
use crate::arch::sm83;
use crate::memory::Pointer;
use crate::platform::Platform;
use crate::reg::{Convertable, State, Symbolic};
use crate::{memory, reg};
use std::collections::HashMap;
use std::io;

/// Any type which decodes the banked memory region (0x4000) of a Game Boy ROM
/// image.
trait Mapper: Send + Sync {
    fn decode_banked_addr(&self, ptr: &memory::Pointer<sm83::PtrVal>) -> Option<usize>;

    fn encode_banked_addr(&self, ioffset: usize) -> Option<memory::Pointer<sm83::PtrVal>>;

    fn context_mask(&self) -> u64;

    /// Attempt to handle an MBC register write.
    ///
    /// If this is not an MBC register write, then return false.
    fn register_write(
        &self,
        ptr: sm83::PtrVal,
        byte: Symbolic<sm83::Data>,
        state: &mut State<sm83::Sm83>,
    ) -> bool;
}

/// Mapper type which does not support banking.
///
/// This supports Game Boy games up to a maximum of 32KB.
struct LinearMapper {}

impl LinearMapper {
    fn new() -> Self {
        LinearMapper {}
    }
}

impl Mapper for LinearMapper {
    fn decode_banked_addr(&self, ptr: &memory::Pointer<sm83::PtrVal>) -> Option<usize> {
        Some(((ptr.as_pointer() & 0x3FFF) + 0x4000) as usize)
    }

    fn encode_banked_addr(&self, ioffset: usize) -> Option<Pointer<sm83::PtrVal>> {
        if ioffset < 0x8000 {
            let pval = 0x4000 + (ioffset & 0x3FFF) as sm83::PtrVal;
            let ptr = Pointer::from_ptrval_and_contexts(pval, HashMap::new());

            Some(ptr)
        } else {
            None
        }
    }

    fn context_mask(&self) -> u64 {
        0
    }

    fn register_write(
        &self,
        _ptr: sm83::PtrVal,
        _byte: Symbolic<sm83::Data>,
        _state: &mut State<sm83::Sm83>,
    ) -> bool {
        false
    }
}

struct Mbc1Mapper {}

impl Mbc1Mapper {
    fn new() -> Self {
        Mbc1Mapper {}
    }
}

impl Mapper for Mbc1Mapper {
    fn decode_banked_addr(&self, ptr: &memory::Pointer<sm83::PtrVal>) -> Option<usize> {
        match ptr.get_platform_context("R").into_concrete() {
            Some(0x00) => None,
            Some(0x20) => None,
            Some(0x40) => None,
            Some(0x60) => None,
            Some(b) => Some(((*ptr.as_pointer() as usize) & 0x3FFF) + (b * 0x4000) as usize),
            None => None,
        }
    }

    fn encode_banked_addr(&self, ioffset: usize) -> Option<Pointer<sm83::PtrVal>> {
        let pval = 0x4000 + (ioffset & 0x3FFF) as sm83::PtrVal;
        let bank = ioffset >> 14;

        if bank < 0x80 {
            let mut pointer = Pointer::from(pval);
            pointer.set_platform_context("R", Symbolic::from(bank as u64));

            Some(pointer)
        } else {
            None
        }
    }

    fn context_mask(&self) -> u64 {
        0x7F
    }

    fn register_write(
        &self,
        ptr: sm83::PtrVal,
        byte: Symbolic<sm83::Data>,
        state: &mut State<sm83::Sm83>,
    ) -> bool {
        if (0x2000..=0x3FFF).contains(&ptr) {
            let byte = match byte.into_concrete() {
                Some(0x00) => Symbolic::from(0x01),
                Some(0x20) => Symbolic::from(0x21),
                Some(0x40) => Symbolic::from(0x41),
                Some(0x60) => Symbolic::from(0x61),
                _ => byte,
            };

            state.set_platform_context(
                "R",
                Symbolic::<u64>::convert_from(byte) & self.context_mask().into(),
            );
            return true;
        }

        false
    }
}

struct Mbc2Mapper {}

impl Mbc2Mapper {
    fn new() -> Self {
        Mbc2Mapper {}
    }
}

impl Mapper for Mbc2Mapper {
    fn decode_banked_addr(&self, ptr: &memory::Pointer<sm83::PtrVal>) -> Option<usize> {
        match ptr.get_platform_context("R").into_concrete() {
            Some(b) => {
                Some(((*ptr.as_pointer() as usize) & 0x3FFF) + ((b & 0xF) * 0x4000) as usize)
            }
            None => None,
        }
    }

    fn encode_banked_addr(&self, ioffset: usize) -> Option<Pointer<sm83::PtrVal>> {
        let pval = 0x4000 + (ioffset & 0x3FFF) as sm83::PtrVal;
        let bank = ioffset >> 14;

        if bank < 0x10 {
            let mut pointer = Pointer::from(pval);
            pointer.set_platform_context("R", Symbolic::from(bank as u64));

            Some(pointer)
        } else {
            None
        }
    }

    fn context_mask(&self) -> u64 {
        0x0F
    }

    fn register_write(
        &self,
        ptr: sm83::PtrVal,
        byte: Symbolic<sm83::Data>,
        state: &mut State<sm83::Sm83>,
    ) -> bool {
        if (0x2000..=0x3FFF).contains(&ptr) && ptr & 0x0100 != 0 {
            state.set_platform_context(
                "R",
                Symbolic::<u64>::convert_from(byte) & self.context_mask().into(),
            );
            return true;
        }

        false
    }
}

struct Mbc3Mapper {}

impl Mbc3Mapper {
    fn new() -> Self {
        Mbc3Mapper {}
    }
}

impl Mapper for Mbc3Mapper {
    fn decode_banked_addr(&self, ptr: &memory::Pointer<sm83::PtrVal>) -> Option<usize> {
        match ptr.get_platform_context("R").into_concrete() {
            Some(b) => Some(((*ptr.as_pointer() as usize) & 0x3FFF) + (b * 0x4000) as usize),
            None => None,
        }
    }

    fn encode_banked_addr(&self, ioffset: usize) -> Option<Pointer<sm83::PtrVal>> {
        let pval = 0x4000 + (ioffset & 0x3FFF) as sm83::PtrVal;
        let bank = ioffset >> 14;

        if bank < 0x80 {
            let mut pointer = Pointer::from(pval);
            pointer.set_platform_context("R", Symbolic::from(bank as u64));

            Some(pointer)
        } else {
            None
        }
    }

    fn context_mask(&self) -> u64 {
        0x7F
    }

    fn register_write(
        &self,
        ptr: sm83::PtrVal,
        byte: Symbolic<sm83::Data>,
        state: &mut State<sm83::Sm83>,
    ) -> bool {
        if (0x2000..=0x3FFF).contains(&ptr) {
            let byte = match byte.into_concrete() {
                Some(0x00) => Symbolic::from(0x01),
                _ => byte,
            };

            state.set_platform_context(
                "R",
                Symbolic::<u64>::convert_from(byte) & self.context_mask().into(),
            );
            return true;
        }

        false
    }
}

struct Mbc5Mapper {}

impl Mbc5Mapper {
    fn new() -> Self {
        Mbc5Mapper {}
    }
}

impl Mapper for Mbc5Mapper {
    fn decode_banked_addr(&self, ptr: &memory::Pointer<sm83::PtrVal>) -> Option<usize> {
        match ptr.get_platform_context("R").into_concrete() {
            Some(b) => Some(((*ptr.as_pointer() as usize) & 0x3FFF) + (b * 0x4000) as usize),
            None => None,
        }
    }

    fn encode_banked_addr(&self, ioffset: usize) -> Option<Pointer<sm83::PtrVal>> {
        let pval = 0x4000 + (ioffset & 0x3FFF) as sm83::PtrVal;
        let bank = ioffset >> 14;

        if bank < 0x200 {
            let mut pointer = Pointer::from(pval);
            pointer.set_platform_context("R", Symbolic::from(bank as u64));

            Some(pointer)
        } else {
            None
        }
    }

    fn context_mask(&self) -> u64 {
        0x1FF
    }

    fn register_write(
        &self,
        ptr: sm83::PtrVal,
        byte: Symbolic<sm83::Data>,
        state: &mut State<sm83::Sm83>,
    ) -> bool {
        if (0x2000..=0x2FFF).contains(&ptr) {
            let old_data = state.get_platform_context("R") & Symbolic::from(0x100);
            state.set_platform_context(
                "R",
                old_data | Symbolic::<u64>::convert_from(byte) & self.context_mask().into(),
            );
            return true;
        } else if (0x3000..=0x3FFF).contains(&ptr) {
            let old_data = state.get_platform_context("R") & Symbolic::from(0x0FF);
            state.set_platform_context(
                "R",
                old_data | (Symbolic::<u64>::convert_from(byte) << 8) & self.context_mask().into(),
            );
            return true;
        }

        false
    }
}

/// A program ROM image for Game Boy software.
///
/// With very few exceptions, all Game Boy ROM images service two memory
/// regions: a `HOME` memory region at `$0000` that is 16KB large, and a banked
/// memory region at `$4000` that is also 16KB large. As a consequence, Game Boy
/// ROM images are composed of 16KB chunks of memory that can mapped in or out
/// depending on a mapper-specific memory scheme.
struct GameBoyRomImage<M>
where
    M: Mapper,
{
    data: Vec<u8>,
    mapper: M,
}

impl<M> GameBoyRomImage<M>
where
    M: Mapper,
{
    pub fn new<F>(file: &mut F, mapper: M) -> io::Result<Self>
    where
        F: io::Read,
    {
        let mut data = Vec::new();

        file.read_to_end(&mut data)?;

        Ok(GameBoyRomImage { data, mapper })
    }
}

impl<M> memory::Image<sm83::Sm83> for GameBoyRomImage<M>
where
    M: Mapper,
{
    fn retrieve(&self, offset: usize, count: usize) -> Option<&[sm83::Data]> {
        Some(&self.data[offset as usize..(offset + count) as usize])
    }

    fn contains(&self, ptr: &memory::Pointer<sm83::PtrVal>, _base: sm83::PtrVal) -> bool {
        *ptr.as_pointer() < 0x8000
    }

    fn decode_addr(
        &self,
        ptr: &memory::Pointer<sm83::PtrVal>,
        _base: sm83::PtrVal,
    ) -> Option<usize> {
        if *ptr.as_pointer() < 0x4000 {
            Some(*ptr.as_pointer() as usize)
        } else {
            self.mapper.decode_banked_addr(ptr)
        }
    }

    fn decode_prerequisites(
        &self,
        _ptr: sm83::PtrVal,
        _base: sm83::PtrVal,
    ) -> Vec<Requisite<sm83::Sm83>> {
        vec![Requisite::platform_context(
            "R".to_string(),
            self.mapper.context_mask(),
        )]
    }

    fn encode_addr(&self, ioffset: usize, _base: sm83::PtrVal) -> Option<Pointer<sm83::PtrVal>> {
        if ioffset >= self.data.len() {
            return None;
        }

        if ioffset < 0x4000 {
            Some(Pointer::from_ptrval_and_contexts(
                ioffset as sm83::PtrVal,
                HashMap::new(),
            ))
        } else {
            self.mapper.encode_banked_addr(ioffset)
        }
    }

    fn image_size(&self) -> usize {
        self.data.len()
    }

    fn minimize_context(
        &self,
        ptr: memory::Pointer<sm83::PtrVal>,
    ) -> memory::Pointer<sm83::PtrVal> {
        let my_ctxt = ptr.get_platform_context("R");
        let mut stripped_ptr = memory::Pointer::from(*ptr.as_pointer());

        if *stripped_ptr.as_pointer() >= 0x4000 {
            stripped_ptr.set_platform_context("R", my_ctxt);
        }

        stripped_ptr
    }

    fn insert_user_context(
        &self,
        mut ptr: memory::Pointer<sm83::PtrVal>,
        ctxts: &[&str],
    ) -> memory::Pointer<sm83::PtrVal> {
        if *ptr.as_pointer() >= 0x4000 {
            if let Some(ctxt) = ctxts.get(0) {
                if let Ok(cval) = u64::from_str_radix(ctxt, 16) {
                    ptr.set_platform_context("R", reg::Symbolic::from(cval));
                }
            }
        }

        ptr
    }

    fn write_memory(
        &self,
        ptr: sm83::PtrVal,
        _base: sm83::PtrVal,
        data: Symbolic<sm83::Data>,
        state: &mut State<sm83::Sm83>,
    ) -> bool {
        self.mapper.register_write(ptr, data, state)
    }
}

pub enum PlatformVariant {
    LinearMapper,
    Mbc1Mapper,
    Mbc2Mapper,
    Mbc3Mapper,
    Mbc5Mapper,
    UnknownMapper,
}

impl Default for PlatformVariant {
    fn default() -> Self {
        PlatformVariant::UnknownMapper
    }
}

pub fn create_context<V>(values: &[V]) -> Option<memory::Pointer<sm83::PtrVal>>
where
    V: Clone + PartialOrd + From<sm83::PtrVal>,
    sm83::PtrVal: From<V>,
    u64: From<V>,
{
    let mut context = memory::Pointer::from(sm83::PtrVal::from(values[values.len() - 1].clone()));

    if values.len() > 1 {
        if values[values.len() - 1] >= V::from(0xE000) {
        } else if values[values.len() - 1] >= V::from(0xC000) {
            context.set_platform_context(
                "W",
                reg::Symbolic::from(u64::from(values[values.len() - 2].clone())),
            );
        } else if values[values.len() - 1] >= V::from(0xA000) {
            context.set_platform_context(
                "S",
                reg::Symbolic::from(u64::from(values[values.len() - 2].clone())),
            );
        } else if values[values.len() - 1] >= V::from(0x8000) {
            context.set_platform_context(
                "V",
                reg::Symbolic::from(u64::from(values[values.len() - 2].clone())),
            );
        } else if values[values.len() - 1] >= V::from(0x4000) {
            context.set_platform_context(
                "R",
                reg::Symbolic::from(u64::from(values[values.len() - 2].clone())),
            );
        }
    }

    if !values.is_empty() {
        Some(context)
    } else {
        None
    }
}

pub struct GbPlatform();

impl Platform<sm83::Sm83> for GbPlatform {
    /// Construct a `Memory` corresponding to the execution environment of a given
    /// Game Boy ROM image.
    ///
    /// You may optionally specify a `PlatformVariant` to control which MBC behavior
    /// is used to analyze the image. If unspecified, the ROM header will be used to
    /// determine which MBC was intended to be used alongside this program.
    fn construct_platform<F>(&self, file: &mut F) -> io::Result<sm83::Bus>
    where
        F: io::Read + io::Seek,
    {
        let mut bus = sm83::Bus::new();
        let orig_pos = file.seek(io::SeekFrom::Current(0))?;
        file.seek(io::SeekFrom::Start(0x147))?;

        let mut romtype: [u8; 1] = [0];
        file.read_exact(&mut romtype)?;
        file.seek(io::SeekFrom::Start(orig_pos))?;

        let pv = match romtype {
            [0x00] => PlatformVariant::LinearMapper,  //ROM w/o RAM
            [0x01] => PlatformVariant::Mbc1Mapper,    //MBC1 ROM
            [0x02] => PlatformVariant::Mbc1Mapper,    //MBC1 ROM with RAM
            [0x03] => PlatformVariant::Mbc1Mapper,    //MBC1 ROM with persistent RAM
            [0x05] => PlatformVariant::Mbc2Mapper, //MBC2 ROM; TODO: MBC2 has weird 4-bit SRAM and we should always return symbolic 4-bit values
            [0x06] => PlatformVariant::Mbc2Mapper, //MBC2 ROM w/ persistence
            [0x08] => PlatformVariant::LinearMapper, //ROM with RAM
            [0x09] => PlatformVariant::LinearMapper, //ROM with persistent RAM
            [0x0B] => PlatformVariant::UnknownMapper, //MMM01, currently not supported
            [0x0C] => PlatformVariant::UnknownMapper, //MMM01, currently not supported, with RAM
            [0x0D] => PlatformVariant::UnknownMapper, //MMM01, currently not supported, with persistent RAM
            [0x0F] => PlatformVariant::Mbc3Mapper,    //MBC3 with persistent clock
            [0x10] => PlatformVariant::Mbc3Mapper,    //MBC3 with persistent clock and RAM
            [0x11] => PlatformVariant::Mbc3Mapper,    //MBC3 ROM only
            [0x12] => PlatformVariant::Mbc3Mapper,    //MBC3 with RAM
            [0x13] => PlatformVariant::Mbc3Mapper,    //MBC3 with persistent RAM, no clock
            [0x19] => PlatformVariant::Mbc5Mapper,    //MBC5 ROM only
            [0x1A] => PlatformVariant::Mbc5Mapper,    //MBC5 with RAM
            [0x1B] => PlatformVariant::Mbc5Mapper,    //MBC5 with persistent RAM
            [0x1C] => PlatformVariant::Mbc5Mapper,    //MBC5 with rumble motor
            [0x1D] => PlatformVariant::Mbc5Mapper,    //MBC5 with rumble motor and RAM
            [0x1E] => PlatformVariant::Mbc5Mapper,    //MBC5 with rumble motor and persistent RAM
            [0x20] => PlatformVariant::UnknownMapper, //MBC6 ROM only
            [0x22] => PlatformVariant::UnknownMapper, //MBC7 with tilt sensor, rumble motor, and persistent EEPROM
            [0xFC] => PlatformVariant::UnknownMapper, //Game Boy Camera with CCD video sensor
            [0xFD] => PlatformVariant::UnknownMapper, //Bandai TAMA5 (capabilities unknown)
            [0xFE] => PlatformVariant::UnknownMapper, //HuC3 (capabilities unknown)
            [0xFF] => PlatformVariant::UnknownMapper, //HuC1 with cartridge infrared port and persistent RAM
            _ => PlatformVariant::UnknownMapper,
        };

        match pv {
            PlatformVariant::LinearMapper => bus.install_rom_image(
                0x0000,
                0x8000,
                Box::new(GameBoyRomImage::new(file, LinearMapper::new())?),
            ),
            PlatformVariant::Mbc1Mapper => bus.install_rom_image(
                0x0000,
                0x8000,
                Box::new(GameBoyRomImage::new(file, Mbc1Mapper::new())?),
            ),
            PlatformVariant::Mbc2Mapper => bus.install_rom_image(
                0x0000,
                0x8000,
                Box::new(GameBoyRomImage::new(file, Mbc2Mapper::new())?),
            ),
            PlatformVariant::Mbc3Mapper => bus.install_rom_image(
                0x0000,
                0x8000,
                Box::new(GameBoyRomImage::new(file, Mbc3Mapper::new())?),
            ),
            PlatformVariant::Mbc5Mapper => bus.install_rom_image(
                0x0000,
                0x8000,
                Box::new(GameBoyRomImage::new(file, Mbc5Mapper::new())?),
            ),
            PlatformVariant::UnknownMapper => panic!(
                "Platform variant detection failed! Please manually specify the platform variant."
            ),
        }

        bus.install_ram(0x8000, 0x2000); //VRAM
        bus.install_ram(0xA000, 0x2000); //SRAM (todo: this should be modeled better...)
        bus.install_ram(0xC000, 0x2000); //WRAM (todo: bankable WRAM)
        bus.install_ram(0xFE00, 0x009F); //OAM
        bus.install_io(0xFF00, 0x007F); //IO space
        bus.install_ram(0xFF80, 0x007F); //HRAM
        bus.install_io(0xFFFF, 0x0001); //Interrupt enable
        bus.install_openbus(0xE000, 0x1000); //Echo RAM

        Ok(bus)
    }
}
