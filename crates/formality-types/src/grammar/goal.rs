use formality_macros::term;

use crate::{cast::Upcast, cast_impl};

use super::{Parameter, Predicate, Relation, TraitRef, Wc, Wcs, PR};

#[term]
pub enum Goal {
    Equals(Parameter, Parameter),

    AllEqual(Vec<Parameter>, Vec<Parameter>),

    #[cast]
    Wc(Wc),

    #[cast]
    Wcs(Wcs),
}

impl Goal {
    pub fn eq(p1: impl Upcast<Parameter>, p2: impl Upcast<Parameter>) -> Self {
        Self::Equals(p1.upcast(), p2.upcast())
    }

    pub fn all_eq(p1: impl Upcast<Vec<Parameter>>, p2: impl Upcast<Vec<Parameter>>) -> Self {
        Self::AllEqual(p1.upcast(), p2.upcast())
    }
}

cast_impl!((Relation) <: (Wc) <: (Goal));
cast_impl!((PR) <: (Wc) <: (Goal));
cast_impl!((Predicate) <: (Wc) <: (Goal));
cast_impl!((TraitRef) <: (Wc) <: (Goal));
