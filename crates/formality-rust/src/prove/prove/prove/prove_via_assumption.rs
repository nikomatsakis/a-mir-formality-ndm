use crate::grammar::{ValidationState, WcData, Wcs};
use crate::prove::prove::{
    decls::Program,
    prove::{constraints::Constraints, env::Env, prove_after::prove_after},
};
use formality_core::judgment_fn;
judgment_fn! {
    /// Check whether the where-clause `via` (which is one of the `assumptions` that are in in scope)
    /// can be used to prove `goal` (the thing we are trying to prove).
    ///
    /// This is equivalent to the "elaboration" of the environment that takes place in rustc,
    /// but done lazilly. For example, if you have `where T: Eq` then you can clearly prove `T: Eq`
    /// but you can also prove `T: PartialEq` because `trait Eq: PartialEq`.
    pub fn prove_via_assumption(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        via: WcData,
        goal: WcData,
    ) => Constraints {
        debug(goal, via, assumptions, env)

        (
            (if via_state.can_prove(goal_state))
            (prove_via_validate(
                decls,
                env,
                assumptions,
                via_state,
                goal_state,
                via,
                goal,
            ) => c)
            ----------------------------- ("validate")
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                WcData::Validate(via_state, via),
                WcData::Validate(goal_state, goal),
            ) => c)
        )

        (
            // `c` = "clause", the name for something that we are assuming is true.
            (let (skel_c, parameters_c) = pred_1.debone())
            // `g` = "goal, the name for something that we are trying to prove.
            (let (skel_g, parameters_g) = pred_2.debone())
            (if skel_c == skel_g)!
            (prove_after(decls, env, assumptions, Wcs::all_eq(parameters_c, parameters_g)) => c)
            ----------------------------- ("predicate-congruence-axiom")
            (prove_via_assumption(decls, env, assumptions, WcData::Predicate(pred_1), WcData::Predicate(pred_2)) => c)
        )

        (
            (let (skel_c, parameters_c) = rel_1.debone())
            (let (skel_g, parameters_g) = rel_2.debone())
            (if skel_c == skel_g)
            (if parameters_c == parameters_g)! // for relations, we require 100% match
            ----------------------------- ("relation-axiom")
            (prove_via_assumption(_decls, env, _assumptions, WcData::Relation(rel_1), WcData::Relation(rel_2)) => Constraints::none(env))
        )

        // If you have `where for<'a> T: Trait<'a>` then you can prove `T: Trait<'b>` for any `'b`.
        (
            (let (env, subst) = env.existential_substitution(binder))
            (let via1 = binder.instantiate_with(subst).unwrap())
            // Try to prove `T: Trait<?a> == goal`.
            (prove_via_assumption(decls, env, assumptions, via1, goal) => c)
            ----------------------------- ("forall")
            (prove_via_assumption(decls, env, assumptions, WcData::ForAll(binder), goal) => c.pop_subst(subst))
        )

        // If you have `where if (T: Debug) T: Foo` (not in Rust but it should be...)...
        (
            // if the goal is `T: Foo`...
            (prove_via_assumption(decls, env, assumptions, wc_consequence, goal) => c)
            // ...and we can prove `T: Debug`... then it holds.
            (prove_after(decls, c, assumptions, wc_condition) => c)
            ----------------------------- ("implies")
            (prove_via_assumption(decls, env, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => c)
        )
    }
}

judgment_fn! {
    /// Use `via` to prove `goal` while preserving validation mode.
    fn prove_via_validate(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        via_state: ValidationState,
        goal_state: ValidationState,
        via: WcData,
        goal: WcData,
    ) => Constraints {
        debug(goal_state, goal, via_state, via, assumptions, env)

        (
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                WcData::predicate(via),
                WcData::predicate(goal),
            ) => c)
            ----------------------------- ("atomic predicate")
            (prove_via_validate(
                decls,
                env,
                assumptions,
                via_state,
                goal_state,
                WcData::Predicate(via),
                WcData::Predicate(goal),
            ) => c)
        )

        (
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                WcData::relation(via),
                WcData::relation(goal),
            ) => c)
            ----------------------------- ("atomic relation")
            (prove_via_validate(
                decls,
                env,
                assumptions,
                via_state,
                goal_state,
                WcData::Relation(via),
                WcData::Relation(goal),
            ) => c)
        )

        (
            (let (env, subst) = env.existential_substitution(binder))
            (let via = binder.instantiate_with(subst).unwrap())
            (prove_via_validate(
                decls,
                env,
                assumptions,
                via_state,
                goal_state,
                via,
                goal,
            ) => c)
            ----------------------------- ("forall")
            (prove_via_validate(
                decls,
                env,
                assumptions,
                via_state,
                goal_state,
                WcData::ForAll(binder),
                goal,
            ) => c.pop_subst(subst))
        )

        (
            (prove_via_validate(
                decls,
                env,
                assumptions,
                via_state,
                goal_state,
                consequence,
                goal,
            ) => c)
            (let validated_conditions = conditions.validated(via_state))
            (prove_after(decls, c, assumptions, validated_conditions) => c)
            ----------------------------- ("implies")
            (prove_via_validate(
                decls,
                env,
                assumptions,
                via_state,
                goal_state,
                WcData::Implies(conditions, consequence),
                goal,
            ) => c)
        )
    }
}
