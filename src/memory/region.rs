//! A special-purpose type for modeling the address decoding of a particular
//! platform.

use std::io;
use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use num::traits::{Zero, One};
use crate::{reg, memory};
use crate::reg::Convertable;
use crate::maths::{CheckedSub, BoundWidth};
use crate::memory::bss::UnknownImage;
use crate::memory::rombin::ROMBinaryImage;
use crate::memory::{Action, Image, Behavior, Pointer, Desegmentable, Endianness};
use crate::reg::New;

/// Models a region of memory visible to the program under analysis.
/// 
/// `P` is the architectural representation of a pointer, and `S` is any
/// compatible type which represents a size or offset of an architectural
/// pointer. Compatibility is defined as the ability to add an offset `s` to a
/// pointer `p` and obtain some output value convertable to `P` which represents
/// a pointer that many bytes forwards in the image.
/// 
/// `T` is the image type in use for the given region. The offset type of an
/// image is `IO`, which is allowed to differ from architectural pointer and
/// offset types. (For example, if your platform supports bank switching, `IO`
/// must be wide enough to represent a fully decoded offset into a ROM image.)
/// 
/// #Typical type parameter values
/// 
/// For a 32-bit CPU such as AArch32, `P` and `S` would be `u32` and `MV` would
/// be `u8`. `IO` would likely be `u32`, unless you specifically wished to model
/// platforms with more complicated addressing systems. This seems so
/// straightforward as to reveal our type system as overcomplicated, so let's
/// get weirder.
/// 
/// Certain computer architectures use uncommon architectural register types of
/// odd sizes. For example, the WDC 65816 is a CPU with a 24-bit addressing
/// space, and would need a custom `u24` type as `P` and `S` in order to be
/// properly analyzed. If we go back further in computing history, we'd see
/// systems such as the PDP-10, which uses 18-bit *word* addressing into
/// 36-bit memory. This would demand not only a `u18` `P` and `S`, but also a
/// `u36` `MV` type.
/// 
/// The most complicated situation would probably be real-mode x86, where
/// addresses are always aliased in a bizarre segment/offset scheme. If we
/// ignored that, and modeled memory as a `u20` pointer (known contemporarily as
/// "huge memory"); we still wouldn't be able to model the machine's ported I/O
/// space. Pointers on x86 would need to be an enum of port and memory space
/// addresses, with offsets being straight integers.
struct Region<P, MV, S = P, IO = usize> {
    start: P,
    length: S,
    read_memtype: Behavior,
    write_memtype: Behavior,
    exec_memtype: Behavior,
    image: Box<dyn Image<Pointer = P, Offset = IO, Data = MV>>
}

impl<P, MV, S, IO> Region<P, MV, S, IO> where P: memory::PtrNum<S>, S: memory::Offset<P> {
    pub fn is_ptr_within(&self, ptr: Pointer<P>) -> bool {
        if let Some(maybe_offset) = ptr.clone().into_pointer().checked_sub(self.start.clone()) {
            if let Ok(offset) = S::try_from(maybe_offset) {
                if let Some(_ms_offset) = self.image.decode_addr(&ptr, self.start.clone()) {
                    return self.start <= ptr.clone().into_pointer() && offset < self.length;
                }
            }
        }

        false
    }
}

//TODO: something better than just a vec pls...
pub struct Memory<P, MV, S = P, IO = usize> {
    views: Vec<Region<P, MV, S, IO>>
}

impl<P, MV, S, IO> Memory<P, MV, S, IO> {
    pub fn new() -> Self {
        Memory {
            views: Vec::new()
        }
    }

    pub fn install_mem_image(&mut self, start: P, length: S, read_memtype: Behavior, write_memtype: Behavior, exec_memtype: Behavior, image: Box<dyn Image<Pointer = P, Offset = IO, Data = MV>>) {
        self.views.push(Region {
            start: start,
            length: length,
            read_memtype: read_memtype,
            write_memtype: write_memtype,
            exec_memtype: exec_memtype,
            image: image
        });
    }

    pub fn install_rom_image(&mut self, start: P, length: S, image: Box<dyn Image<Pointer = P, Offset = IO, Data = MV>>) {
        self.views.push(Region {
            start: start,
            length: length,
            read_memtype: Behavior::Memory,
            write_memtype: Behavior::Invalid,
            exec_memtype: Behavior::Memory,
            image: image
        });
    }
}

impl<P, MV, S, IO> Memory<P, MV, S, IO>
    where P: memory::PtrNum<S> + 'static, S: memory::Offset<P>,
        IO: Clone + 'static, <P as std::ops::Sub>::Output: TryInto<IO>, MV: 'static {
    pub fn install_mem(&mut self, start: P, length: S, read_memtype: Behavior, write_memtype: Behavior, exec_memtype: Behavior) {
        self.views.push(Region {
            start: start,
            length: length,
            read_memtype: read_memtype,
            write_memtype: write_memtype,
            exec_memtype: exec_memtype,
            image: Box::new(UnknownImage::new())
        });
    }

    pub fn install_ram(&mut self, start: P, length: S) {
        self.views.push(Region {
            start: start,
            length: length,
            read_memtype: Behavior::Memory,
            write_memtype: Behavior::Memory,
            exec_memtype: Behavior::Memory,
            image: Box::new(UnknownImage::new())
        });
    }

    pub fn install_io(&mut self, start: P, length: S) {
        self.views.push(Region {
            start: start,
            length: length,
            read_memtype: Behavior::MappedIO,
            write_memtype: Behavior::MappedIO,
            exec_memtype: Behavior::MappedIO,
            image: Box::new(UnknownImage::new())
        });
    }

    pub fn install_openbus(&mut self, start: P, length: S) {
        self.views.push(Region {
            start: start,
            length: length,
            read_memtype: Behavior::Invalid,
            write_memtype: Behavior::Invalid,
            exec_memtype: Behavior::Invalid,
            image: Box::new(UnknownImage::new())
        });
    }
}

impl<P, MV, S> Memory<P, MV, S, usize>
    where P: Clone + CheckedSub + 'static, usize: memory::Offset<P>,
        MV: From<u8> + 'static {
    pub fn install_rom<F>(&mut self, start: P, length: S, file: &mut F) -> io::Result<()> where F: io::Read {
        self.views.push(Region {
            start: start,
            length: length,
            read_memtype: Behavior::Memory,
            write_memtype: Behavior::MappedIO,
            exec_memtype: Behavior::Memory,
            image: Box::new(ROMBinaryImage::read_bytes(file)?)
        });

        Ok(())
    }
}

impl<P, MV, S, IO> Memory<P, MV, S, IO>
    where P: memory::PtrNum<S>, S: memory::Offset<P>, MV: reg::Bitwise,
        IO: One, reg::Symbolic<MV>: Default {
    
    /// Determine if a given region of memory is dynamically overwritable.
    /// 
    /// In order for dynamic tracing to simulate a memory overwrite, the
    /// following events must be true:
    /// 
    /// 1. The memory view that answers reads from a given pointer must have
    ///    `Memory` semantics.
    /// 2. The memory view that answers writes to a given pointer must have
    ///    `Memory` semantics.
    /// 3. Both reads and writes must be answered by the same view.
    /// 
    /// 
    pub fn is_overwritable(&self, ptr: &Pointer<P>) -> bool {
        for view in &self.views {
            if let Some(offset) = view.image.decode_addr(ptr, view.start.clone()) {
                if view.read_memtype == Behavior::Memory && view.write_memtype == Behavior::Memory {
                    return true;
                }

                break;
            }
        }
        
        false
    }
    
    /// Read a single memory unit (e.g. byte) from memory at a given pointer.
    /// 
    /// Yields a symbolic value which is concrete if the memory model has image
    /// data for the given pointer, and is unconstrained otherwise.
    pub fn read_unit(&self, ptr: &Pointer<P>) -> reg::Symbolic<MV> {
        for view in &self.views {
            if let Some(offset) = view.image.decode_addr(ptr, view.start.clone()) {
                if let Some(imgdata) = view.image.retrieve(offset, IO::one()) {
                    if imgdata.len() > 0 {
                        return reg::Symbolic::new(imgdata[0].clone());
                    }
                }
            }
        }
        
        reg::Symbolic::default()
    }

    /// Read multiple memory units (e.g. bytes) from memory at a given pointer.
    /// 
    /// Yields an array of symbolic values whose constraints are equal to the
    /// result of calling `read_unit` on each pointer from `ptr` to
    /// `ptr + size`.
    pub fn read_memory(&self, ptr: &Pointer<P>, size: S) -> Vec<reg::Symbolic<MV>> {
        let mut count = S::zero();
        let mut out = Vec::new();

        while count < size {
            let offptr = ptr.clone() + count.clone();
            out.push(self.read_unit(&offptr));
            count = count + S::one();
        }

        out
    }
}

impl<P, MV, S, IO> Memory<P, MV, S, IO>
    where P: memory::PtrNum<S>, S: memory::Offset<P> + TryFrom<u32>,
        MV: reg::Bitwise, IO: One,
        reg::Symbolic<MV>: Default, 
        <S as TryFrom<u32>>::Error : Debug {
    
    /// Read an arbitary little-endian integer type from memory.
    /// 
    /// The underlying memory value types must implement `Desegmentable` and
    /// their symbolic versions must also implement `Desegmentable`. It must
    /// also be possible to generate a memory offset from a u32.
    pub fn read_leword<EV>(&self, ptr: &Pointer<P>) -> reg::Symbolic<EV>
        where S: TryFrom<usize>,
            reg::Symbolic<EV>: Default + memory::Desegmentable<reg::Symbolic<MV>> {
        let units_reqd = <reg::Symbolic<EV> as memory::Desegmentable<reg::Symbolic<MV>>>::units_reqd();
        let data = self.read_memory(ptr, match S::try_from(units_reqd) {
            Ok(u) => u,
            Err(_) => return reg::Symbolic::<EV>::default()
        });
        reg::Symbolic::<EV>::from_segments(&data, Endianness::LittleEndian).unwrap_or(reg::Symbolic::<EV>::default())
    }
    
    /// Read an arbitary big-endian integer type from memory.
    /// 
    /// The underlying memory value types must implement `Desegmentable` and
    /// their symbolic versions must also implement `Desegmentable`. It must
    /// also be possible to generate a memory offset from a u32.
    pub fn read_beword<EV>(&self, ptr: &Pointer<P>) -> reg::Symbolic<EV>
        where S: TryFrom<usize>,
            reg::Symbolic<EV>: Default + memory::Desegmentable<reg::Symbolic<MV>> {
        let units_reqd = <reg::Symbolic<EV> as memory::Desegmentable<reg::Symbolic<MV>>>::units_reqd();
        let data = self.read_memory(ptr, match S::try_from(units_reqd) {
            Ok(u) => u,
            Err(_) => return reg::Symbolic::<EV>::default()
        });
        reg::Symbolic::<EV>::from_segments(&data, Endianness::BigEndian).unwrap_or(reg::Symbolic::<EV>::default())
    }
}

impl<P, MV, S, IO> Memory<P, MV, S, IO>
    where P: memory::PtrNum<S>, S: memory::Offset<P> {
    pub fn minimize_context(&self, ptr: Pointer<P>) -> Pointer<P> {
        for view in &self.views {
            if view.is_ptr_within(ptr.clone()) {
                return view.image.minimize_context(ptr);
            }
        }

        ptr
    }
    
    pub fn insert_user_context(&self, ptr: Pointer<P>, ctxts: &[&str]) -> Pointer<P> {
        for view in &self.views {
            if view.is_ptr_within(ptr.clone()) {
                return view.image.insert_user_context(ptr, ctxts);
            }
        }

        ptr
    }
}