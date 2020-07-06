//! Traits for analysis

use crate::analysis::Prerequisite;
use crate::ast::Literal;
use crate::cli::Nameable;
use crate::{analysis, memory, reg};
use std::cmp::Ord;
use std::hash::Hash;

pub trait Mappable: Clone + Eq + Hash + Ord {}

impl<T> Mappable for T where T: Clone + Eq + Hash + Ord {}

pub trait Disassembler<L, P, MV, S, IO>:
    Fn(
    &memory::Pointer<P>,
    &memory::Memory<P, MV, S, IO>,
) -> analysis::Result<analysis::Disasm<L, P, S>, P, S>
where
    L: Literal,
    P: Mappable + Nameable,
{
}

impl<T, L, P, MV, S, IO> Disassembler<L, P, MV, S, IO> for T
where
    L: Literal,
    P: Mappable + Nameable,
    T: Fn(
        &memory::Pointer<P>,
        &memory::Memory<P, MV, S, IO>,
    ) -> analysis::Result<analysis::Disasm<L, P, S>, P, S>,
{
}

pub trait PrerequisiteAnalysis<RK, I, P, MV, S, IO>:
    Fn(
    &memory::Pointer<P>,
    &memory::Memory<P, MV, S, IO>,
    &reg::State<RK, I, P, MV>,
) -> analysis::Result<(Vec<Prerequisite<RK, I, P, MV, S>>, bool), P, S>
{
}

impl<T, RK, I, P, MV, S, IO> PrerequisiteAnalysis<RK, I, P, MV, S, IO> for T where
    T: Fn(
        &memory::Pointer<P>,
        &memory::Memory<P, MV, S, IO>,
        &reg::State<RK, I, P, MV>,
    ) -> analysis::Result<(Vec<Prerequisite<RK, I, P, MV, S>>, bool), P, S>
{
}

pub trait Tracer<RK, I, P, MV, S, IO>:
    Fn(
    &memory::Pointer<P>,
    &memory::Memory<P, MV, S, IO>,
    reg::State<RK, I, P, MV>,
    &mut analysis::Trace<RK, I, P, MV>,
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
        &mut analysis::Trace<RK, I, P, MV>,
    ) -> analysis::Result<(reg::State<RK, I, P, MV>, memory::Pointer<P>), P, S>,
    RK: analysis::Mappable,
    P: analysis::Mappable,
{
}
