use formality_types::{
    cast::Upcast,
    collections::Set,
    grammar::{Goal, Wcs},
    judgment_fn,
};

use crate::{prove::prove_after::prove_after, Constraints, Decls, Env};

use super::{prove_eq::prove_eq, prove_wc::prove_wc, prove_wc_list};

pub fn prove_goal(
    decls: impl Upcast<Decls>,
    env: impl Upcast<Env>,
    assumptions: impl Upcast<Wcs>,
    goal: Goal,
) -> Set<Constraints> {
    match goal {
        Goal::Wcs(wcs) => prove_wc_list(decls, env, assumptions, wcs),
        Goal::Wc(wc) => prove_wc(decls, env, assumptions, wc),
        Goal::Equals(a, b) => prove_eq(decls, env, assumptions, a, b),
        Goal::All(goals) => prove_all(decls, env, assumptions, goals),
    }
}

judgment_fn! {
    /// Compute the constraints that make two parameters `a` and `b` equal
    /// (semantically equivalent), given the `assumptions`.
    fn prove_all(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        goals: Vec<Goal>,
    ) => Constraints {
        debug(goals, assumptions, env, decls)

        (
            ----------------------------- ("prove-all-none")
            (prove_all(_decls, env, _assumptions, ()) => Constraints::none(env))
        )

        (
            (prove_goal(&decls, &env, &assumptions, goal) => c)
            (prove_after(&decls, c, &assumptions, Goal::all(&goal_s)) => c)
            ----------------------------- ("prove-all-some")
            (prove_all(decls, env, assumptions, (goal, goal_s)) => c)
        )
    }
}
