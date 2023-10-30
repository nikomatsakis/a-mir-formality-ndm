use formality_macros::term;

use crate::{cast::Upcast, cast_impl};

use super::{Parameter, Predicate, Relation, TraitRef, Wc, Wcs, PR};

/// A *goal* is something that can be proven. This is a superset of where-clauses because it includes
/// some things (e.g., `=` and `||`) that cannot be put into the assumptions.
#[term]
pub enum Goal {
    /// Two parameters must be equal.
    Equals(Parameter, Parameter),

    /// All these goals must be true.
    All(Vec<Goal>),

    /// Where-clause must hold.
    #[cast]
    Wc(Wc),

    /// All where-clauses must hold.
    #[cast]
    Wcs(Wcs),
}

impl Goal {
    pub fn eq(p1: impl Upcast<Parameter>, p2: impl Upcast<Parameter>) -> Self {
        Self::Equals(p1.upcast(), p2.upcast())
    }

    pub fn all(goals: impl Upcast<Vec<Goal>>) -> Self {
        Goal::All(goals.upcast())
    }

    pub fn all_eq(p1: impl Upcast<Vec<Parameter>>, p2: impl Upcast<Vec<Parameter>>) -> Self {
        let p1: Vec<Parameter> = p1.upcast();
        let p2: Vec<Parameter> = p2.upcast();
        assert_eq!(p1.len(), p2.len());
        Self::All(
            p1.into_iter()
                .zip(p2)
                .map(|(p1, p2)| Goal::Equals(p1, p2))
                .collect(),
        )
    }
}

cast_impl!((Relation) <: (Wc) <: (Goal));
cast_impl!((PR) <: (Wc) <: (Goal));
cast_impl!((Predicate) <: (Wc) <: (Goal));
cast_impl!((TraitRef) <: (Wc) <: (Goal));
