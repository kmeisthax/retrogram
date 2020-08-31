//! A special-purpose type for modeling the address decoding of a particular
//! platform.

use crate::analysis::{Prerequisite, Trace};
use crate::arch::Architecture;
use crate::maths::CheckedSub;
use crate::memory::bss::UnknownImage;
use crate::memory::rombin::ROMBinaryImage;
use crate::memory::{Behavior, Desegmentable, Endianness, Image, Offset, Pointer};
use crate::reg::{State, Symbolic};
use crate::{memory, reg};
use num::traits::{One, Zero};
use std::convert::{TryFrom, TryInto};
use std::io;

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
pub struct Region<AR>
where
    AR: Architecture,
{
    start: AR::PtrVal,
    length: AR::Offset,
    read_memtype: Behavior,
    write_memtype: Behavior,
    exec_memtype: Behavior,
    image: Box<dyn Image<AR>>,
}

impl<AR> Region<AR>
where
    AR: Architecture,
    AR::Offset: Offset<AR::PtrVal>,
{
    /// Determine if a contextual pointer is within this region.
    ///
    /// This function will call `decode_addr` on the image with the given
    /// pointer to validate that it can be decoded with the given contexts. If
    /// the contexts this image depends on aren't valid for the given image
    /// contents, then the contextual pointer will be treated as "outside" the
    /// given image.
    pub fn is_ptr_within(&self, ptr: Pointer<AR::PtrVal>) -> bool {
        if let Some(offset) = ptr
            .as_pointer()
            .clone()
            .checked_sub(self.start.clone())
            .and_then(|vo| AR::Offset::try_from(vo).ok())
        {
            if let Some(_ms_offset) = self.image.decode_addr(&ptr, self.start.clone()) {
                return self.start <= ptr.as_pointer().clone() && offset < self.length;
            }
        }

        false
    }

    /// Determine if a pointer value is within this region.
    pub fn is_bare_ptr_within(&self, ptr: AR::PtrVal) -> bool {
        if let Some(offset) = ptr
            .clone()
            .checked_sub(self.start.clone())
            .and_then(|vo| AR::Offset::try_from(vo).ok())
        {
            return self.start <= ptr && offset < self.length;
        }

        false
    }

    /// Decode an image offset into a contextual pointer.
    pub fn encode_image_offset(&self, ioffset: usize) -> Option<Pointer<AR::PtrVal>> {
        self.image.encode_addr(ioffset, self.start.clone())
    }

    pub fn image_size(&self) -> usize {
        self.image.image_size()
    }

    /// Retrieve data from the image.
    pub fn retrieve(&self, offset: usize, count: usize) -> Option<&[AR::Byte]> {
        self.image.retrieve(offset, count)
    }
}

//TODO: something better than just a vec pls...
pub struct Memory<AR>
where
    AR: Architecture,
{
    views: Vec<Region<AR>>,
}

impl<AR> Default for Memory<AR>
where
    AR: Architecture,
{
    fn default() -> Self {
        Memory { views: Vec::new() }
    }
}

impl<AR> Memory<AR>
where
    AR: Architecture,
{
    pub fn new() -> Self {
        Memory { views: Vec::new() }
    }

    pub fn install_mem_image(
        &mut self,
        start: AR::PtrVal,
        length: AR::Offset,
        read_memtype: Behavior,
        write_memtype: Behavior,
        exec_memtype: Behavior,
        image: Box<dyn Image<AR>>,
    ) {
        self.views.push(Region {
            start,
            length,
            read_memtype,
            write_memtype,
            exec_memtype,
            image,
        });
    }

    pub fn install_rom_image(
        &mut self,
        start: AR::PtrVal,
        length: AR::Offset,
        image: Box<dyn Image<AR>>,
    ) {
        self.views.push(Region {
            start,
            length,
            read_memtype: Behavior::Memory,
            write_memtype: Behavior::Invalid,
            exec_memtype: Behavior::Memory,
            image,
        });
    }

    pub fn iter_images(&self) -> impl Iterator<Item = &Region<AR>> {
        self.views.iter()
    }

    /// Get the total number of installed regions.
    pub fn region_count(&self) -> usize {
        self.views.len()
    }

    /// Get the image size for a given region.
    pub fn region_image_size(&self, region: usize) -> Option<usize> {
        self.views.get(region).map(|r| r.image_size())
    }

    /// Encode an image offset within a region as a pointer.
    pub fn region_encode_image_offset(
        &self,
        region: usize,
        ioffset: usize,
    ) -> Option<Pointer<AR::PtrVal>> {
        self.views
            .get(region)
            .and_then(|r| r.encode_image_offset(ioffset))
    }

    /// Encode an image offset within a region as a pointer.
    pub fn region_retrieve(
        &self,
        region: usize,
        ioffset: usize,
        count: usize,
    ) -> Option<&[AR::Byte]> {
        self.views
            .get(region)
            .and_then(|r| r.retrieve(ioffset, count))
    }
}

impl<AR> Memory<AR>
where
    AR: Architecture + 'static,
    AR::Offset: Offset<AR::PtrVal> + TryInto<usize>,
    <AR::Offset as TryInto<usize>>::Error: std::fmt::Debug,
{
    pub fn install_mem(
        &mut self,
        start: AR::PtrVal,
        length: AR::Offset,
        read_memtype: Behavior,
        write_memtype: Behavior,
        exec_memtype: Behavior,
    ) {
        self.views.push(Region {
            start,
            length: length.clone(),
            read_memtype,
            write_memtype,
            exec_memtype,
            image: Box::new(UnknownImage::new(length.try_into().unwrap())),
        });
    }

    pub fn install_ram(&mut self, start: AR::PtrVal, length: AR::Offset) {
        self.views.push(Region {
            start,
            length: length.clone(),
            read_memtype: Behavior::Memory,
            write_memtype: Behavior::Memory,
            exec_memtype: Behavior::Memory,
            image: Box::new(UnknownImage::new(length.try_into().unwrap())),
        });
    }

    pub fn install_io(&mut self, start: AR::PtrVal, length: AR::Offset) {
        self.views.push(Region {
            start,
            length: length.clone(),
            read_memtype: Behavior::MappedIO,
            write_memtype: Behavior::MappedIO,
            exec_memtype: Behavior::MappedIO,
            image: Box::new(UnknownImage::new(length.try_into().unwrap())),
        });
    }

    pub fn install_openbus(&mut self, start: AR::PtrVal, length: AR::Offset) {
        self.views.push(Region {
            start,
            length: length.clone(),
            read_memtype: Behavior::Invalid,
            write_memtype: Behavior::Invalid,
            exec_memtype: Behavior::Invalid,
            image: Box::new(UnknownImage::new(length.try_into().unwrap())),
        });
    }

    pub fn install_rom<F>(
        &mut self,
        start: AR::PtrVal,
        length: AR::Offset,
        file: &mut F,
    ) -> io::Result<()>
    where
        F: io::Read,
    {
        self.views.push(Region {
            start,
            length,
            read_memtype: Behavior::Memory,
            write_memtype: Behavior::MappedIO,
            exec_memtype: Behavior::Memory,
            image: Box::new(ROMBinaryImage::read_bytes(file)?),
        });

        Ok(())
    }
}

impl<AR> Memory<AR>
where
    AR: Architecture,
{
    /// Determine if a given region of memory has a mapping associated with it.
    pub fn is_mapped(&self, ptr: AR::PtrVal) -> bool {
        for view in &self.views {
            if view.is_bare_ptr_within(ptr.clone()) {
                return true;
            }
        }

        false
    }

    /// Determine if a given contextual pointer uniquely identifies a single
    /// memory location.
    pub fn is_decodable(&self, ptr: Pointer<AR::PtrVal>) -> bool {
        for view in &self.views {
            if view.is_ptr_within(ptr.clone()) {
                return true;
            }
        }

        false
    }

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
    pub fn is_overwritable(&self, ptr: &Pointer<AR::PtrVal>) -> bool {
        for view in &self.views {
            if view.image.decode_addr(ptr, view.start.clone()).is_some() {
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
    pub fn read_unit(&self, ptr: &Pointer<AR::PtrVal>) -> reg::Symbolic<AR::Byte> {
        for view in &self.views {
            if let Some(offset) = view.image.decode_addr(ptr, view.start.clone()) {
                if let Some(imgdata) = view.image.retrieve(offset, 1) {
                    if !imgdata.is_empty() {
                        return reg::Symbolic::from(imgdata[0].clone());
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
    pub fn read_memory(
        &self,
        ptr: &Pointer<AR::PtrVal>,
        size: AR::Offset,
    ) -> Vec<reg::Symbolic<AR::Byte>> {
        let mut count = AR::Offset::zero();
        let mut out = Vec::new();

        while count < size {
            let offptr = ptr.clone() + count.clone();
            out.push(self.read_unit(&offptr));
            count = count + AR::Offset::one();
        }

        out
    }

    /// Read a single memory unit (e.g. byte) from stateful memory at a given
    /// pointer.
    ///
    /// "Stateful" means that a `reg::State` is present.
    ///
    /// Yields a symbolic value which is concrete if the memory model has image
    /// data for the given pointer, and is unconstrained otherwise.
    pub fn read_unit_stateful(
        &self,
        ptr: AR::PtrVal,
        state: &State<AR>,
    ) -> reg::Symbolic<AR::Byte> {
        for view in &self.views {
            if let Some(data) = view
                .image
                .read_memory(ptr.clone(), view.start.clone(), state)
            {
                return data;
            }
        }

        reg::Symbolic::default()
    }

    /// Read multiple memory units (e.g. bytes) from memory at a given pointer.
    ///
    /// Yields an array of symbolic values whose constraints are equal to the
    /// result of calling `read_unit` on each pointer from `ptr` to
    /// `ptr + size`.
    pub fn read_memory_stateful(
        &self,
        ptr: AR::PtrVal,
        size: AR::Offset,
        state: &State<AR>,
    ) -> Vec<reg::Symbolic<AR::Byte>> {
        let mut count = AR::Offset::zero();
        let mut out = Vec::new();

        while count < size {
            let offptr = ptr.clone() + count.clone();
            out.push(self.read_unit_stateful(offptr, state));
            count = count + AR::Offset::one();
        }

        out
    }

    /// Read a single memory unit (e.g. byte) from memory at a given pointer.
    ///
    /// Yields a symbolic value which is concrete if the memory model has image
    /// data for the given pointer, and is unconstrained otherwise.
    pub fn write_unit(
        &self,
        ptr: AR::PtrVal,
        data: Symbolic<AR::Byte>,
        state: &mut State<AR>,
        trace: Option<&mut Trace<AR>>,
    ) {
        if let Some(trace) = trace {
            trace.memory_write(
                state.contextualize_pointer(ptr.clone()),
                &[data.clone()],
                state,
            );
        }

        for view in &self.views {
            if view
                .image
                .write_memory(ptr.clone(), view.start.clone(), data.clone(), state)
            {
                return;
            }
        }
    }

    /// Simulate memory writes to a particular location.
    ///
    /// Memory writes are free to change either pointer contexts or program
    /// state as necessary.
    ///
    /// Memory writes will be made in order of increasing pointer values. This
    /// means that multi-value writes that change contexts may change where
    /// writes occur mid-write.
    pub fn write_memory(
        &self,
        mut ptr: AR::PtrVal,
        data: &[Symbolic<AR::Byte>],
        state: &mut State<AR>,
        trace: Option<&mut Trace<AR>>,
    ) {
        if let Some(trace) = trace {
            trace.memory_write(state.contextualize_pointer(ptr.clone()), data, state);
        }

        for byte in data {
            self.write_unit(ptr.clone(), byte.clone(), state, None);

            ptr = ptr + AR::Offset::one();
        }
    }

    /// Read an arbitary little-endian integer type from memory.
    ///
    /// The underlying memory value types must implement `Desegmentable` and
    /// their symbolic versions must also implement `Desegmentable`. It must
    /// also be possible to generate a memory offset from a u32.
    pub fn read_leword<EV>(&self, ptr: &Pointer<AR::PtrVal>) -> reg::Symbolic<EV>
    where
        AR::Offset: TryFrom<usize>,
        reg::Symbolic<EV>: Default + memory::Desegmentable<reg::Symbolic<AR::Byte>>,
    {
        let units_reqd =
            <reg::Symbolic<EV> as memory::Desegmentable<reg::Symbolic<AR::Byte>>>::units_reqd();
        let data = self.read_memory(
            ptr,
            match AR::Offset::try_from(units_reqd) {
                Ok(u) => u,
                Err(_) => return reg::Symbolic::<EV>::default(),
            },
        );
        reg::Symbolic::<EV>::from_segments(&data, Endianness::LittleEndian)
            .unwrap_or_else(reg::Symbolic::<EV>::default)
    }

    /// Read an arbitary big-endian integer type from memory.
    ///
    /// The underlying memory value types must implement `Desegmentable` and
    /// their symbolic versions must also implement `Desegmentable`. It must
    /// also be possible to generate a memory offset from a u32.
    pub fn read_beword<EV>(&self, ptr: &Pointer<AR::PtrVal>) -> reg::Symbolic<EV>
    where
        AR::Offset: TryFrom<usize>,
        reg::Symbolic<EV>: Default + memory::Desegmentable<reg::Symbolic<AR::Byte>>,
    {
        let units_reqd =
            <reg::Symbolic<EV> as memory::Desegmentable<reg::Symbolic<AR::Byte>>>::units_reqd();
        let data = self.read_memory(
            ptr,
            match AR::Offset::try_from(units_reqd) {
                Ok(u) => u,
                Err(_) => return reg::Symbolic::<EV>::default(),
            },
        );
        reg::Symbolic::<EV>::from_segments(&data, Endianness::BigEndian)
            .unwrap_or_else(reg::Symbolic::<EV>::default)
    }

    /// Read an arbitary little-endian integer type from memory using contexts
    /// from a given state.
    ///
    /// The underlying memory value types must implement `Desegmentable` and
    /// their symbolic versions must also implement `Desegmentable`. It must
    /// also be possible to generate a memory offset from a u32.
    pub fn read_leword_stateful<EV>(&self, ptr: AR::PtrVal, state: &State<AR>) -> reg::Symbolic<EV>
    where
        AR::Offset: TryFrom<usize>,
        reg::Symbolic<EV>: Default + memory::Desegmentable<reg::Symbolic<AR::Byte>>,
    {
        let units_reqd =
            <reg::Symbolic<EV> as memory::Desegmentable<reg::Symbolic<AR::Byte>>>::units_reqd();
        let data = self.read_memory_stateful(
            ptr,
            match AR::Offset::try_from(units_reqd) {
                Ok(u) => u,
                Err(_) => return reg::Symbolic::<EV>::default(),
            },
            state,
        );
        reg::Symbolic::<EV>::from_segments(&data, Endianness::LittleEndian)
            .unwrap_or_else(reg::Symbolic::<EV>::default)
    }

    /// Read an arbitary big-endian integer type from memory using contexts
    /// from a given state.
    ///
    /// The underlying memory value types must implement `Desegmentable` and
    /// their symbolic versions must also implement `Desegmentable`. It must
    /// also be possible to generate a memory offset from a u32.
    pub fn read_beword_stateful<EV>(&self, ptr: AR::PtrVal, state: &State<AR>) -> reg::Symbolic<EV>
    where
        AR::Offset: TryFrom<usize>,
        reg::Symbolic<EV>: Default + memory::Desegmentable<reg::Symbolic<AR::Byte>>,
    {
        let units_reqd =
            <reg::Symbolic<EV> as memory::Desegmentable<reg::Symbolic<AR::Byte>>>::units_reqd();
        let data = self.read_memory_stateful(
            ptr,
            match AR::Offset::try_from(units_reqd) {
                Ok(u) => u,
                Err(_) => return reg::Symbolic::<EV>::default(),
            },
            state,
        );
        reg::Symbolic::<EV>::from_segments(&data, Endianness::BigEndian)
            .unwrap_or_else(reg::Symbolic::<EV>::default)
    }

    pub fn minimize_context(&self, ptr: Pointer<AR::PtrVal>) -> Pointer<AR::PtrVal> {
        for view in &self.views {
            if view.is_ptr_within(ptr.clone()) {
                return view.image.minimize_context(ptr);
            }
        }

        ptr
    }

    /// Determine prerequisites necessary to read or write to a bare address
    /// without context.
    pub fn prerequisites(&self, ptr: AR::PtrVal) -> Vec<Prerequisite<AR>> {
        for view in &self.views {
            if view.is_bare_ptr_within(ptr.clone()) {
                return view.image.decode_prerequisites(ptr, view.start.clone());
            }
        }

        vec![]
    }

    pub fn insert_user_context(
        &self,
        ptr: Pointer<AR::PtrVal>,
        ctxts: &[&str],
    ) -> Pointer<AR::PtrVal> {
        for view in &self.views {
            if view.is_ptr_within(ptr.clone()) {
                return view.image.insert_user_context(ptr, ctxts);
            }
        }

        ptr
    }
}
