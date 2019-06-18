//! Platform mappers for Super Famicom program analysis.
//! 
//! Super Famicom is probably the most complicated platform to model due to the
//! vast number of different cartridge configurations, special hardware
//! registers, and other such things. Nintendo originally planned a LoROM and
//! HiROM configuration, but this had to be extended several times in weird ways
//! due to needing to make use of as much of the SNES memory space as possible.
//! 
//! However, most games still use execute-in-place ROM, so we can get away with
//! the following platform variants;
//! 
//!  1. HiROM
//!  2. LoROM
//!  3. Reading an external higan-style cartridge directory for mapping info
//!     (planned)
//! 

use std::io;
use std::convert::{TryFrom, TryInto};
use crate::retrogram::maths::u24;
use crate::retrogram::arch::w65c816;
use crate::retrogram::memory::{Memory, Image, Pointer};

/// Any type which decodes the 65C816 banking scheme into a ROM offset.
trait Decoder {
    fn decode_banked_addr(&self, ptr: &Pointer<w65c816::Pointer>) -> Option<usize>;
}

pub enum PlatformVariant {
    LoROM,
    HiROM,
    ExHiROM,
    Unknown
}

impl PlatformVariant {
    /// If the current platform variant is unknown, override it with the value
    /// in the right-hand side. Otherwise, preserve the current, known value.
    fn known_or(self, rhs: PlatformVariant) -> PlatformVariant {
        match self {
            Unknown => rhs,
            _ => self
        }
    }

    /// Wrap the platform variant in an Option, treating Unknown as None.
    fn known_or_none(self) -> Option<PlatformVariant> {
        match self {
            Unknown => None,
            _ => Some(self)
        }
    }

    /// Wrap the platform variant in an Error, treating Unknown as Err, and
    /// calling err_func to produce an error value.
    fn known_or_else<E, ET>(self, err_func: E) -> Result<PlatformVariant, ET>
        where E: FnOnce() -> ET {
        match self {
            Unknown => Err(err_func()),
            _ => Ok(self)
        }
    }
}

impl Decoder for PlatformVariant {
    fn decode_banked_addr(&self, ptr: &Pointer<w65c816::Pointer>) -> Option<usize> {
        match self {
            LoROM => {
                let pv : usize = ptr.as_pointer().clone().try_into().ok()?;
                let ba = pv & 0xFF0000 >> 16;
                let pa = pv & 0x00FFFF;

                if pa > 0x8000 && (ba < 0x7D || ba >= 0x80) {
                    Some(ba << 15 + (pa - 0x8000))
                } else {
                    None
                }
            },
            HiROM => {
                //TODO: We still don't support the LoROM mirrors in bank 0
                //required to successfully boot a HiROM or ExHiROM game.
                let pv : usize = ptr.as_pointer().clone().try_into().ok()?;
                let ba = pv & 0xFF0000 >> 16;
                let pa = pv & 0x00FFFF;

                if ba >= 0x40 && ba <= 0x7D {
                    Some((ba - 0x40) << 16 + pa)
                } else if ba >= 0xC0 && ba <= 0xFF {
                    Some((ba - 0x80) << 16 + pa)
                } else {
                    None
                }
            },
            ExHiROM => {
                //Really stupid idea on Nintendo's part: Let's store the ROM
                //data backwards if you're using fast ROM in banks 80-FF.
                //In this case, bank 80 is first in the file, and bank 0, which
                //is required to successfully boot, is about 4MB into the file.

                //TODO: We still don't support the LoROM mirrors in bank 0
                //required to successfully boot a HiROM or ExHiROM game.
                let pv : usize = ptr.as_pointer().clone().try_into().ok()?;
                let ba = pv & 0xFF0000 >> 16;
                let pa = pv & 0x00FFFF;

                if ba >= 0x40 && ba <= 0x7D {
                    Some((ba - 0x40) << 16 + pa)
                } else if ba >= 0xC0 && ba <= 0xFF {
                    Some((ba - 0xC0) << 16 + pa)
                } else {
                    None
                }
            },
            Unknown => {
                None
            }
        }
    }
}

struct SufamiPlatform<D> {
    image: Vec<u8>,
    floffset: usize,
    mapper: D
}

impl<D> SufamiPlatform<D> {
    fn new(data: Vec<u8>, floffset: usize, mapper: D) -> Self {
        SufamiPlatform {
            image: data,
            floffset: floffset,
            mapper: mapper
        }
    }
}

impl<D> Image for SufamiPlatform<D> where D: Decoder {
    type Pointer = w65c816::Pointer;
    type Offset = usize;
    type Data = w65c816::Data;

    fn retrieve(&self, offset: Self::Offset, count: Self::Offset) -> Option<&[Self::Data]> {
        let unfloffset = self.floffset + offset;

        Some(&self.image[unfloffset as usize..(unfloffset + count) as usize])
    }

    fn decode_addr(&self, ptr: &Pointer<Self::Pointer>, base: Self::Pointer) -> Option<Self::Offset> {
        self.mapper.decode_banked_addr(ptr)
    }
}

struct SuperRomHeader {
    cart_title: [u8;21], //TODO: Shift-JIS with ¥ replacing \
    rom_info: u8,
    cart_chipset: u8,
    rom_size: u8,
    ram_size: u8,
    region: u8,
    developer: u8,
    version: u8,
    checksum_inv: u16,
    checksum: u16
}

/// Parse the data contained within a SFC ROM image header.
/// 
/// Since SFC ROM images can have the ROM header in a multitude of different
/// locations depending on the way the cartridge is mapped, we must account for
/// that;
fn parse_rom_image_header(header: &[u8]) -> SuperRomHeader {
    let mut srh = SuperRomHeader {
        cart_title: Default::default(),
        rom_info: header[21],
        cart_chipset: header[22],
        rom_size: header[23],
        ram_size: header[24],
        region: header[25],
        developer: header[26],
        version: header[27],
        checksum_inv: header[28] as u16 | (header[29] as u16) << 8,
        checksum: header[30] as u16 | (header[31] as u16) << 8
    };

    srh.cart_title.copy_from_slice(&header[0..21]);

    srh
}

/// Construct a platform, as best as we can, from an SFC ROM dump.
/// 
/// This is not likely to be the most accurate source of platform model.
pub fn construct_platform_from_rom_image<F>(file: &mut F, pv: PlatformVariant) -> io::Result<w65c816::Bus> where F: io::Read + io::Seek {
    let mut m = w65c816::Bus::new();

    //Guess what? The SNES ROM header isn't a constant offset in the ROM image.
    //Kinda defeats the whole point of a ROM header. There's a whole bunch of
    //places it could be in, and each place implies a specific ROM mapping which
    //hopefully agrees with the header. It is technically possible to have a ROM
    //with a mangled header and standard mappings otherwise, but you should be
    //using higan style cartridge containers for that.

    file.seek(io::SeekFrom::Start(0))?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    //All properly dumped SFC ROM images are multiples of 1kb, any data smaller
    //than that is a piracy device header we should remove.
    let floffset = data.len() & 0xFFF;
    let clean_data = data.get(floffset..).ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "ROM header could not be stripped or ROM is shorter than 1KB"))?;

    //SFC checksumming is even weirder. Header checksums are calculated up to
    //the nearest power of two. For ROMs of a non-power-of-two size, the next
    //power of two down is repeatedly summed until the next power of two is
    //reached. USUALLY. Some games did this wrong and passed lot check anyway.
    
    //TODO: Actually calculate a checksum.

    //In order for a header to register as valid, it must claim the correct ROM
    //mapping for it's location and posses an internally consistent ROM
    //checksum. It's checksum does NOT need to match the ROM, since some valid
    //commercial dumps have weirdly calculated checksums.

    let mut rom_mapping = PlatformVariant::Unknown;

    if let Some(header_data) = clean_data.get(0x7FC0..) {
        let lorom_header = parse_rom_image_header(header_data);

        if lorom_header.checksum == !lorom_header.checksum_inv {
            rom_mapping = rom_mapping.known_or(match lorom_header.rom_info & 0x0F {
                0 => PlatformVariant::LoROM,
                2 => PlatformVariant::LoROM,
                3 => PlatformVariant::LoROM,
                _ => PlatformVariant::Unknown
            });
        }
    }

    if let Some(header_data) = clean_data.get(0xFFC0..) {
        let hirom_header = parse_rom_image_header(header_data);

        if hirom_header.checksum == !hirom_header.checksum_inv {
            rom_mapping = rom_mapping.known_or(match hirom_header.rom_info & 0x0F {
                1 => PlatformVariant::HiROM,
                5 => PlatformVariant::HiROM,
                0xA => PlatformVariant::HiROM,
                _ => PlatformVariant::Unknown
            });
        }
    }

    //Really dumb format from Nintendo where we store the later banks of the ROM
    //first for... no appreciable reason, except maybe to make it slightly
    //easier to cheap out on ROM for the slow half of a fast-ROM game?
    if let Some(header_data) = clean_data.get(0x40FFC0..) {
        let exhirom_header = parse_rom_image_header(header_data);

        if exhirom_header.checksum == !exhirom_header.checksum_inv {
            rom_mapping = rom_mapping.known_or(match exhirom_header.rom_info & 0x0F {
                1 => PlatformVariant::ExHiROM,
                5 => PlatformVariant::ExHiROM,
                0xA => PlatformVariant::ExHiROM,
                _ => PlatformVariant::Unknown
            });
        }
    }

    rom_mapping = rom_mapping.known_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "Could not automatically determine ROM mapping"))?;
    m.install_mem_image(u24::from(0 as u16),
        u24::try_from(0xFFFFFF as u32).or_else(|e| Err(io::Error::new(io::ErrorKind::InvalidData, "ROM size is too large for the SFC platform")))?,
        Box::new(SufamiPlatform::new(data, floffset, rom_mapping)));

    m.install_mem(u24::try_from(0x7E0000 as u32).expect("This should work."),
        u24::try_from(0x020000 as u32).expect("This should work."));
    //TODO: System IO area. Requires a custom image to handle banking.

    Ok(m)
}