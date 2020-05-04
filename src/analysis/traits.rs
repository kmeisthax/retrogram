//! Traits for analysis

use crate::{analysis, memory, reg};
use std::cmp::Ord;
use std::fmt::Display;
use std::hash::Hash;
use std::str::FromStr;

pub trait Mappable: Clone + Eq + Hash + Ord + Display + FromStr {}

impl<T> Mappable for T where T: Clone + Eq + Hash + Ord + Display + FromStr {}

pub trait Disassembler<I, SI, F, P, MV, S, IO>:
    Fn(
    &memory::Pointer<P>,
    &memory::Memory<P, MV, S, IO>,
) -> analysis::Result<analysis::Disasm<I, SI, F, P, S>, P, S>
where
    P: analysis::Mappable,
{
}

impl<T, I, SI, F, P, MV, S, IO> Disassembler<I, SI, F, P, MV, S, IO> for T
where
    P: analysis::Mappable,
    T: Fn(
        &memory::Pointer<P>,
        &memory::Memory<P, MV, S, IO>,
    ) -> analysis::Result<analysis::Disasm<I, SI, F, P, S>, P, S>,
{
}

pub trait PrerequisiteAnalysis<RK, I, P, MV, S, IO>:
    Fn(
    &memory::Pointer<P>,
    &memory::Memory<P, MV, S, IO>,
    &reg::State<RK, I, P, MV>,
) -> (Vec<RK>, Vec<memory::Pointer<P>>, bool)
{
}

impl<T, RK, I, P, MV, S, IO> PrerequisiteAnalysis<RK, I, P, MV, S, IO> for T where
    T: Fn(
        &memory::Pointer<P>,
        &memory::Memory<P, MV, S, IO>,
        &reg::State<RK, I, P, MV>,
    ) -> (Vec<RK>, Vec<memory::Pointer<P>>, bool)
{
}

pub trait Tracer<RK, I, P, MV, S, IO>:
    Fn(
    &memory::Pointer<P>,
    &memory::Memory<P, MV, S, IO>,
    reg::State<RK, I, P, MV>,
) -> analysis::Result<(reg::State<RK, I, P, MV>, memory::Pointer<P>), P, S>
where
    RK: analysis::Mappable,
    P: analysis::Mappable,
{
}

impl<T, RK, I, P, MV, S, IO> Tracer<RK, I, P, MV, S, IO> for T
where
    T: Fn(
        &memory::Pointer<P>,
        &memory::Memory<P, MV, S, IO>,
        reg::State<RK, I, P, MV>,
    ) -> analysis::Result<(reg::State<RK, I, P, MV>, memory::Pointer<P>), P, S>,
    RK: analysis::Mappable,
    P: analysis::Mappable,
{
}
