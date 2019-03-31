//! A set of types which allow analysis to model memory correctly.

use std::ops::{Add, Sub};
use std::cmp::PartialOrd;
use std::slice::SliceIndex;
use crate::retrogram::reg;

/// Represents the various possible operating modes of a memory block.
/// 
/// The Behavior of a memory area bounds what analysises are considered
/// meaningful for a program.
pub enum Behavior {
    /// Storage behavior corresponds to memory which can be consistently read
    /// to and written from, such that reads always capture the last write,
    /// repeated reads always capture the same value, and there is no semantic
    /// value to writes aside from changing the value to be read back.
    /// Execution is allowed from storage only.
    Storage,
    
    /// MappedIO behavior corresponds to hardware devices on a memory bus.
    /// Reads are not consistent, writes have semantic value. The intent of
    /// such a memory block is communication with hardware. Execution is not
    /// analyzed within mapped I/O.
    MappedIO,
    
    /// Invalid behavior corresponds to memory not mapped to a hardware device
    /// at all. Platform makes no guarantees about behavior upon reading to or
    /// writing from an Invalid memory block. Execution, reads, and writes are
    /// not analyzed within invalid blocks.
    Invalid,
}

/// Represents the various possible actions that can be performed by a program
/// under analysis.
pub enum Action {
    DataRead,
    Write,
    ProgramExecute
}

/// Models a region of memory visible to the program under analysis.
pub struct Region<PType, SType> {
    start: PType,
    length: SType,
    memtype: Behavior,
    image: Option<Vec<u8>>
}

impl<P, S> Region<P, S> where P: Copy + PartialOrd + Add<S> + Sub + From<<P as Add<S>>::Output> + From<<P as Sub>::Output>, S: Copy {
    pub fn is_ptr_within(&self, ptr: P) -> bool {
        self.start <= ptr && P::from(self.start + self.length) > ptr
    }
    
    pub fn get_image_offset(&self, ptr: P) -> Option<P> {
        if (self.is_ptr_within(ptr)) {
            Some(P::from(ptr - self.start))
        } else {
            None
        }
    }
}

//TODO: something better than just a vec pls...
pub struct Memory<P, S> {
    views: Vec<Region<P, S>>
}

impl<P, S> Memory<P, S> where P: Copy + PartialOrd + Add<S> + Sub + From<<P as Add<S>>::Output> + From<<P as Sub>::Output> + SliceIndex<[u8]>, u8: From<<P as SliceIndex<[u8]>>::Output>, <P as SliceIndex<[u8]>>::Output : Sized + Copy, S: Copy {
    pub fn read_u8(&self, ptr: P) -> reg::Symbolic<u8> {
        for view in &self.views {
            if let Some(offset) = view.get_image_offset(ptr) {
                if let Some(ref image) = view.image {
                    return reg::Symbolic::from(u8::from(image[offset]));
                }
            }
        }
        
        reg::Symbolic::default()
    }
}