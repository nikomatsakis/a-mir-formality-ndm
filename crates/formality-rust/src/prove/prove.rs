//! This crate contains the trait proving + type inference logic.
//! It correpsonds loosely to the `InferenceContext` and trait solving (fulfillment context, etc)
//! in the Rust compiler.
//!
//! The base operations we export are:
//!
//! * [`prove`][] -- prove a set of where-clauses to be true
//! * [`prove_normalize`][] -- normalize a type one step using the supplied assumptions

// Defines the language used by derive(term) and friends.
use crate::rust::FormalityLang;

mod db;
mod decls;
mod prove;
mod requirements;
mod trait_order;

pub use decls::*;
pub use prove::combinators;
pub use prove::prove;
pub(crate) use prove::prove_fully_normalize_parameter;
pub(crate) use prove::prove_impl_wf;
pub use prove::prove_normalize::prove_normalize;
pub use prove::{is_definitely_not_proveable, may_not_be_provable, negation_via_failure};
pub(crate) use prove::{prove_via_impl, ProvedViaImpl};
pub use prove::{Bias, Env, MaxUniverse, Universe};
pub use prove::{Constrained, Constraints};
pub use requirements::*;
pub(crate) use trait_order::{
    can_project_associated_bound, can_project_outlives, can_project_supertrait,
    validation_evidence_suffices, validation_frontier_suffices,
};

#[cfg(test)]
mod test;

pub mod test_util;
