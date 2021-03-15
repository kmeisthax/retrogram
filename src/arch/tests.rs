//! Architectural tests

use crate::analysis::{Disasm, Error, RequisiteSet, Result, Trace};
use crate::arch::{ArchName, Architecture};
use crate::ast::Literal;
use crate::memory::{Memory, Pointer};
use crate::reg::State;
use serde::{Deserialize, Serialize};

/// Test architecture for use when testing things that are generic over an
/// entire architecture.
#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct TestArchitecture;

impl Architecture for TestArchitecture {
    type Register = String;
    type Word = u32;
    type SignedWord = i32;
    type Byte = u8;
    type PtrVal = u32;
    type Offset = u32;

    fn name(&self) -> ArchName {
        ArchName::SM83
    }

    fn parse_architectural_contexts(
        _contexts: &mut &[&str],
        _ptr: &mut Pointer<Self::PtrVal>,
    ) -> Option<()> {
        None
    }

    fn disassemble<L>(
        &self,
        _at: &Pointer<Self::PtrVal>,
        _bus: &Memory<Self>,
    ) -> Result<Disasm<L, Self::PtrVal, Self::Offset>, Self>
    where
        L: Literal
            + From<Self::Word>
            + From<Self::Byte>
            + From<Self::Offset>
            + From<Pointer<Self::PtrVal>>,
    {
        Err(Error::NotYetImplemented)
    }

    fn dataflow(
        &self,
        _at: &Pointer<Self::PtrVal>,
        _bus: &Memory<Self>,
    ) -> Result<(RequisiteSet<Self>, RequisiteSet<Self>), Self> {
        Err(Error::NotYetImplemented)
    }

    fn prerequisites(
        &self,
        _at: Self::PtrVal,
        _bus: &Memory<Self>,
        _state: &State<Self>,
    ) -> Result<(RequisiteSet<Self>, bool), Self> {
        Err(Error::NotYetImplemented)
    }

    fn trace(
        &self,
        _at: Self::PtrVal,
        _bus: &Memory<Self>,
        _state: State<Self>,
        _trace: &mut Trace<Self>,
    ) -> Result<(State<Self>, Self::PtrVal), Self> {
        Err(Error::NotYetImplemented)
    }
}
