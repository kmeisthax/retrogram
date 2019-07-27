//! A special-purpose type for modeling the address decoding of a particular
//! platform.

use std::io;
use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use num::traits::One;
use crate::{reg, memory};
use crate::reg::Convertable;
use crate::maths::{CheckedSub, BoundWidth};
use crate::memory::bss::UnknownImage;
use crate::memory::rombin::ROMBinaryImage;
use crate::memory::{Image, Behavior, Pointer};

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
        if let Ok(offset) = S::try_from(ptr.clone().into_pointer() - self.start.clone()) {
            if let Some(_ms_offset) = self.image.decode_addr(&ptr, self.start.clone()) {
                self.start <= ptr.clone().into_pointer() && offset < self.length
            } else {
                false
            }
        } else {
            false
        }
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
    
    pub fn read_unit(&self, ptr: &Pointer<P>) -> reg::Symbolic<MV> {
        for view in &self.views {
            if let Some(offset) = view.image.decode_addr(ptr, view.start.clone()) {
                if let Some(imgdata) = view.image.retrieve(offset, IO::one()) {
                    if imgdata.len() > 0 {
                        return reg::Symbolic::from(imgdata[0].clone());
                    }
                }
            }
        }
        
        reg::Symbolic::default()
    }
}

impl<P, MV, S, IO> Memory<P, MV, S, IO>
    where P: memory::PtrNum<S>, S: memory::Offset<P> + TryFrom<u32>,
        MV: reg::Bitwise, IO: One,
        reg::Symbolic<MV>: Default, 
        <S as TryFrom<u32>>::Error : Debug {
    
    /// Read an arbitary little-endian integer type from memory.
    /// 
    /// All integer types involved must provide a way to reason about their
    /// widths. This is provided with the `BoundWidth` trait, which indicates how
    /// many left shifts of a given integer type are required in order to
    /// overflow it. The `BoundWidth` of the memory word type (MV) thus
    /// determines how many atomic memory units are required to be read in order
    /// to populate the expected value type.
    pub fn read_leword<EV>(&self, ptr: &Pointer<P>) -> reg::Symbolic<EV>
        where EV: memory::Desegmentable<MV> + reg::Bitwise,
            MV: reg::Bitwise,
            reg::Symbolic<EV>: reg::Bitwise {
        let ev_units = <EV as BoundWidth<u32>>::bound_width();
        let mv_units = <MV as BoundWidth<u32>>::bound_width();
        let units_reqd = (ev_units as f32 / mv_units as f32).round() as u32;
        let mut sum : reg::Symbolic<EV> = reg::Symbolic::<EV>::from(EV::zero());

        for i in 0..units_reqd {
            let ptr = ptr.contextualize(P::from(ptr.as_pointer().clone() + S::try_from(i).expect("Desired memory type is too wide for the given memory space")));
            let unit : reg::Symbolic<EV> = reg::Symbolic::convert_from(self.read_unit(&ptr));
            sum = sum | unit << (i * mv_units);
        }

        sum
    }
    
    /// Read an arbitary big-endian integer type from memory.
    /// 
    /// All integer types involved must provide a way to reason about their
    /// widths. This is provided with the `BoundWidth` trait, which indicates how
    /// many left shifts of a given integer type are required in order to
    /// overflow it. The `BoundWidth` of the memory word type (MV) thus
    /// determines how many atomic memory units are required to be read in order
    /// to populate the expected value type.
    pub fn read_beword<EV>(&self, ptr: &Pointer<P>) -> reg::Symbolic<EV>
        where EV: memory::Desegmentable<MV> + reg::Bitwise,
            MV: reg::Bitwise,
            reg::Symbolic<EV>: reg::Bitwise {
        let ev_units = <EV as BoundWidth<u32>>::bound_width();
        let mv_units = <MV as BoundWidth<u32>>::bound_width();
        let units_reqd = (ev_units as f32 / mv_units as f32).round() as u32;
        let mut sum : reg::Symbolic<EV> = reg::Symbolic::<EV>::from(EV::zero());

        for i in (0..units_reqd).rev() {
            let ptr = ptr.contextualize(P::from(ptr.as_pointer().clone() + S::try_from(i).expect("Desired memory type is too wide for the given memory space")));
            let unit : reg::Symbolic<EV> = reg::Symbolic::convert_from(self.read_unit(&ptr));
            sum = sum | unit << (i * mv_units);
        }

        sum
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