pub mod combinators;
mod constraints;
mod env;
mod is_local;
mod minimize;
mod negation;
mod prove_after;
mod prove_after_validation;
mod prove_const_has_type;
mod prove_eq;
mod prove_fully_normalize;
mod prove_impl_wf;
pub mod prove_normalize;
mod prove_outlives;
mod prove_sub;
mod prove_validate;
mod prove_via_assumption;
mod prove_via_impl;
mod prove_wc;
mod prove_wc_list;
mod prove_wf;

use crate::grammar::Wcs;
pub use constraints::{Constrained, Constraints};
use formality_core::{ProvenSet, Upcast};

use crate::prove::prove::decls::Program;

pub use self::env::{Bias, Env, MaxUniverse, Universe};
pub(crate) use self::prove_after::prove_after;
#[cfg(test)]
pub(crate) use self::prove_after_validation::prove_after_validation;
pub(crate) use self::prove_fully_normalize::prove_fully_normalize_parameter;
pub(crate) use self::prove_via_assumption::prove_via_assumption;
pub(crate) use self::prove_via_impl::{prove_via_impl, ImplApplication};
pub use negation::{is_definitely_not_proveable, may_not_be_provable, negation_via_failure};

/// Top-level entry point for proving things.
#[track_caller]
pub fn prove(
    decls: impl Upcast<Program>,
    env: impl Upcast<Env>,
    assumptions: impl Upcast<Wcs>,
    goal: impl Upcast<Wcs>,
) -> ProvenSet<Constraints> {
    prove_after(decls, Constraints::none(env), assumptions, goal)
}
