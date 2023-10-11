use formality_types::{
    collections::Set,
    grammar::{Goal, Wcs},
};

use crate::{Constraints, Decls, Env};

use super::{
    prove_eq::{prove_all_eq, prove_eq},
    prove_wc::prove_wc,
    prove_wc_list,
};

pub fn prove_goal(decls: Decls, env: &Env, assumptions: Wcs, goal: Goal) -> Set<Constraints> {
    match goal {
        Goal::Wcs(wcs) => prove_wc_list(decls, env, assumptions, wcs),
        Goal::Wc(wc) => prove_wc(decls, env, assumptions, wc),
        Goal::Equals(a, b) => prove_eq(decls, env, assumptions, a, b),
        Goal::AllEqual(a, b) => prove_all_eq(decls, env, assumptions, a, b),
    }
}
