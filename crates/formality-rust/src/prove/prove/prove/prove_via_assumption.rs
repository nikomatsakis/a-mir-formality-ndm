use crate::grammar::{AtomicPredicate, Predicate, TraitRef, Upto, Wc, Wcs};
use crate::prove::prove::{
    decls::Program,
    prove::{constraints::Constraints, env::Env, prove_after::prove_after},
    validation_evidence_suffices, validation_frontier_suffices,
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
        via: Wc,
        goal: Wc,
    ) => Constraints {
        debug(goal, via, assumptions, env)

        (
            (prove_via_mode(
                decls,
                env,
                assumptions,
                via_validation,
                goal_validation,
                via,
                goal,
            ) => c)
            ----------------------------- ("mode")
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                Wc::Mode(via_validation, via),
                Wc::Mode(goal_validation, goal),
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
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                Wc::Atomic(AtomicPredicate::Predicate(pred_1)),
                Wc::Atomic(AtomicPredicate::Predicate(pred_2)),
            ) => c)
        )

        (
            (if rel_1 == rel_2)! // for relations, we require 100% match
            ----------------------------- ("relation-axiom")
            (prove_via_assumption(
                _decls,
                env,
                _assumptions,
                Wc::Atomic(AtomicPredicate::Relation(rel_1)),
                Wc::Atomic(AtomicPredicate::Relation(rel_2)),
            ) => Constraints::none(env))
        )

        // If you have `where for<'a> T: Trait<'a>` then you can prove `T: Trait<'b>` for any `'b`.
        (
            (let (env, subst) = env.existential_substitution(binder))
            (let via1 = binder.instantiate_with(subst)?)
            // Try to prove `T: Trait<?a> == goal`.
            (prove_via_assumption(decls, env, assumptions, via1, goal) => c)
            ----------------------------- ("forall")
            (prove_via_assumption(decls, env, assumptions, Wc::ForAll(binder), goal) => c.pop_subst(subst))
        )

        // If you have `where if (T: Debug) T: Foo` (not in Rust but it should be...)...
        (
            // if the goal is `T: Foo`...
            (prove_via_assumption(decls, env, assumptions, wc_consequence, goal) => c)
            // ...and we can prove `T: Debug`... then it holds.
            (prove_after(decls, c, assumptions, wc_condition) => c)
            ----------------------------- ("implies")
            (prove_via_assumption(decls, env, assumptions, Wc::Implies(wc_condition, wc_consequence), goal) => c)
        )
    }
}

judgment_fn! {
    /// Use `via` to prove `goal` while preserving validation mode.
    fn prove_via_mode(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        via_validation: Upto,
        goal_validation: Upto,
        via: AtomicPredicate,
        goal: AtomicPredicate,
    ) => Constraints {
        debug(goal_validation, goal, via_validation, via, assumptions, env)

        // Validation strength is indexed by the trait inside the proposition. In particular,
        // `Valid(Zero, P)` can establish a seemingly nonzero view when that frontier exposes no
        // fields of `P` (for example, when `P` is unrelated to the frontier's root trait).
        (
            (let TraitRef {
                trait_id: via_trait_id,
                parameters: _,
            } = via_trait_ref)
            (validation_evidence_suffices(
                decls,
                via_validation,
                goal_validation,
                via_trait_id,
            ) => ())
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                via_trait_ref,
                goal_trait_ref,
            ) => c)
            ----------------------------- ("trait predicate")
            (prove_via_mode(
                decls,
                env,
                assumptions,
                via_validation,
                goal_validation,
                AtomicPredicate::Predicate(Predicate::IsImplemented(via_trait_ref)),
                AtomicPredicate::Predicate(Predicate::IsImplemented(goal_trait_ref)),
            ) => c)
        )

        (
            (validation_frontier_suffices(
                decls,
                via_validation,
                goal_validation,
            ) => ())
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                via,
                goal,
            ) => c)
            ----------------------------- ("other predicate")
            (prove_via_mode(
                decls,
                env,
                assumptions,
                via_validation,
                goal_validation,
                AtomicPredicate::Predicate(
                    via @ (
                        Predicate::NotImplemented(_)
                        | Predicate::AliasEq(_, _)
                        | Predicate::WellFormedTraitRef(_)
                        | Predicate::IsLocal(_)
                        | Predicate::ConstHasType(_, _)
                    ),
                ),
                AtomicPredicate::Predicate(goal),
            ) => c)
        )

        (
            (validation_frontier_suffices(
                decls,
                via_validation,
                goal_validation,
            ) => ())
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                via,
                goal,
            ) => c)
            ----------------------------- ("atomic relation")
            (prove_via_mode(
                decls,
                env,
                assumptions,
                via_validation,
                goal_validation,
                AtomicPredicate::Relation(via),
                AtomicPredicate::Relation(goal),
            ) => c)
        )
    }
}
