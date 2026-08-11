use crate::grammar::{
    AtomicPredicate, NegTraitImpl, NegTraitImplBoundData, Predicate, Relation, Trait,
    TraitBoundData, TraitRef, Wc, WcData, Wcs,
};
use formality_core::judgment_fn;

use crate::prove::prove::{
    decls::Program,
    prove::{
        combinators::for_all,
        env::{Bias, Env},
        is_local::{is_local_trait_ref, may_be_remote},
        prove_after::prove_after,
        prove_const_has_type::prove_const_has_type,
        prove_eq::prove_eq,
        prove_outlives::prove_outlives,
        prove_sub::prove_sub,
        prove_validate::prove_validate,
        prove_via_assumption::prove_via_assumption,
        prove_via_impl::prove_via_impl,
        prove_wf::prove_wf,
    },
    requirements::{prove_via_trait_requirement, trait_requirement},
    validation_evidence_suffices, validation_frontier_suffices,
};

use super::constraints::{Constrained, Constraints};

fn has_unconditional_proof_from_assumptions(decls: &Program, assumptions: &Wcs, goal: &Wc) -> bool {
    assumptions
        .iter()
        .any(|assumption| match (&assumption, goal) {
            (Wc::Mode(assumption_validation, assumption_goal), Wc::Mode(goal_validation, goal))
                if assumption_goal == goal =>
            {
                match assumption_goal {
                    AtomicPredicate::Predicate(Predicate::IsImplemented(trait_ref)) => {
                        validation_evidence_suffices(
                            decls,
                            assumption_validation,
                            goal_validation,
                            &trait_ref.trait_id,
                        )
                        .is_proven()
                    }

                    _ => {
                        validation_frontier_suffices(decls, assumption_validation, goal_validation)
                            .is_proven()
                    }
                }
            }

            _ => &assumption == goal,
        })
}

judgment_fn! {
    /// The "heart" of the trait system -- prove that a where-clause holds given a set of declarations, variable environment, and set of assumptions.
    /// If successful, returns the constraints under which the where-clause holds.
    pub fn prove_wc(
        decls: Program,
        env: Env,
        assumptions: Wcs,
        goal: Wc,
    ) => Constraints {
        debug(goal, assumptions, env)

        // Prefer an assumption that proves the goal directly before exploring derived proofs.
        // Validation evidence can directly prove an identical goal whose observable dictionary
        // view is no stronger. This cut is important when validating requirements of the form
        // `forall<T> conditions => goal`: opening the binder creates a fresh universal and adds
        // the conditions to the assumptions. Even when one of those assumptions proves the goal,
        // exhaustive search would otherwise also explore the impl rule, which can recursively
        // validate the same associated type requirement. Each recursion opens `forall<T>` again,
        // so the assumptions grow with distinct universals (`!T_1`, `!T_2`, ...); the fixed-point
        // machinery therefore sees distinct calls instead of recognizing a cycle.
        //
        // `trivial` acts as a logical cut here. The direct assumption proves the goal without
        // introducing constraints, which is the most general possible result, so no alternative
        // derivation can improve it. This cut would not be valid if the result were more
        // restrictive.
        trivial(
            has_unconditional_proof_from_assumptions(&decls, &assumptions, &goal)
            => Constraints::none(env)
        )
        cut(Constraints::unconditionally_true)

        (
            (let (env, subst) = env.universal_substitution(binder))
            (let p1 = binder.instantiate_with(subst).unwrap())
            (prove_wc(decls, env, assumptions, p1) => c)
            --- ("forall")
            (prove_wc(decls, env, assumptions, WcData::ForAll(binder)) => c.pop_subst(subst))
        )

        (
            (prove_wc(decls, env, (assumptions, p1), p2) => c)
            --- ("implies")
            (prove_wc(decls, env, assumptions, WcData::Implies(p1, p2)) => c)
        )

        (
            (prove_validate(decls, env, assumptions, validation, validate_goal) => c)
            --- ("mode")
            (prove_wc(
                decls,
                env,
                assumptions,
                WcData::Mode(validation, validate_goal),
            ) => c)
        )

        (
            (a in assumptions)
            (prove_via_assumption(decls, env, assumptions, a, goal) => c)
            ----------------------------- ("assumption")
            (prove_wc(decls, env, assumptions, Wc::Atomic(goal)) => c)
        )

        (
            (a in assumptions)
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                a,
                validation.apply_goal(goal),
            ) => c)!
            ----------------------------- ("validation assumption")
            (prove_wc(
                decls,
                env,
                assumptions,
                Wc::Mode(validation, goal),
            ) => c)
        )

        // Prove an ordinary trait goal with a concrete impl. Validation goals enter the ordinary
        // solver through `prove_validate`'s `verify_x(G) :- G` rule.
        (
            (candidate in decls.raw_trait_impls_for(trait_id))!
            (prove_via_impl(
                decls,
                env,
                assumptions,
                trait_ref,
                candidate,
            ) => Constrained(_, c))
            ----------------------------- ("positive impl")
            (prove_wc(
                decls,
                env,
                assumptions,
                trait_ref @ TraitRef {
                    trait_id,
                    parameters: _,
                },
            ) => c)
        )

        (
            (if env.bias() == Bias::Completeness)!
            (may_be_remote(decls, env, assumptions, trait_ref) => c)
            ----------------------------- ("coherence / remote impl")
            (prove_wc(decls, env, assumptions, trait_ref @ TraitRef { .. }) => c)
        )

        (
            (NegTraitImpl {
                binder,
                safety: _,
            } in decls.neg_trait_impls_for(trait_id))
            (let (env, subst) = env.existential_substitution(binder))
            (let NegTraitImplBoundData {
                trait_id: impl_trait_id,
                self_ty,
                trait_parameters,
                where_clauses,
            } = binder.instantiate_with(subst).unwrap())
            (let impl_trait_ref = impl_trait_id.with(self_ty, trait_parameters))
            (let TraitRef {
                trait_id: _,
                parameters: impl_parameters,
            } = impl_trait_ref)
            (prove_after(
                decls,
                env,
                assumptions,
                Wcs::all_eq(goal_parameters, impl_parameters),
            ) => c)
            (prove_after(decls, c, assumptions, where_clauses) => c)
            ----------------------------- ("negative impl")
            (prove_wc(
                decls,
                env,
                assumptions,
                Predicate::NotImplemented(TraitRef {
                    trait_id,
                    parameters: goal_parameters,
                }),
            ) => c.pop_subst(subst))
        )

        (
            (prove_eq(decls, env, assumptions, alias_ty, ty) => c)
            ----------------------------- ("alias eq")
            (prove_wc(decls, env, assumptions, Predicate::AliasEq(alias_ty, ty)) => c)
        )

        // The Rust declaration `trait Eq: PartialEq` gives rise to the requirement template
        // `forall<T> T: Eq => T: PartialEq`. Apply that requirement by backward chaining: for
        // the goal `U: PartialEq`, instantiate `T` with an inference variable, match the
        // requirement's `required` clause against the goal, and then prove its `source`
        // (`U: Eq`). This lazily elaborates implied bounds rather than adding all of their
        // consequences to the assumptions eagerly.
        (
            (trait_def in decls.traits())
            (trait_requirement(trait_def) => requirements)
            (requirement in requirements)
            (prove_via_trait_requirement(
                decls,
                env,
                assumptions,
                trait_def,
                requirement,
                trait_ref,
            ) => c)!
            ----------------------------- ("trait requirement")
            (prove_wc(decls, env, assumptions, trait_ref @ TraitRef { .. }) => c)
        )

        (
            (prove_eq(decls, env, assumptions, a, b) => c)
            ----------------------------- ("eq")
            (prove_wc(decls, env, assumptions, Relation::Equals(a, b)) => c)
        )

        (
            (prove_sub(decls, env, assumptions, a, b) => c)
            ----------------------------- ("subtype")
            (prove_wc(decls, env, assumptions, Relation::Sub(a, b)) => c)
        )

        // For example, `trait Foo<T> where T: Debug` means that the trait-ref `S: Foo<U>` is
        // well formed only if `S` and `U` are well formed and `U: Debug`. In general, substitute
        // the trait-ref's parameters into the Rust trait declaration and prove every resulting
        // where-clause. This checks every trait-header condition; it is distinct from the
        // trait-requirement rule above, which exposes only the clauses classified as implied
        // requirements.
        (
            (for_all(decls, env, assumptions, parameters, &prove_wf) => c)
            (let Trait { binder, .. } = decls.trait_def(trait_id))
            (let TraitBoundData {
                where_clauses,
                trait_items: _,
            } = binder.instantiate_with(parameters).unwrap())
            (prove_after(decls, c, assumptions, where_clauses) => c)
            ----------------------------- ("trait well formed")
            (prove_wc(
                decls,
                env,
                assumptions,
                Predicate::WellFormedTraitRef(TraitRef {
                    trait_id,
                    parameters,
                }),
            ) => c)
        )

        (
            (is_local_trait_ref(decls, env, assumptions, trait_ref) => c)
            ----------------------------- ("trait ref is local")
            (prove_wc(decls, env, assumptions, Predicate::IsLocal(trait_ref)) => c)
        )

        (
            (prove_outlives(decls, env, assumptions, a, b) => c)
            ----------------------------- ("outlives")
            (prove_wc(decls, env, assumptions, Relation::Outlives(a, b)) => c)
        )


        (
            (prove_wf(decls, env, assumptions, p) => c)
            ----------------------------- ("parameter well formed")
            (prove_wc(decls, env, assumptions, Relation::WellFormed(p)) => c)
        )

        (
            (prove_const_has_type(decls, env, assumptions, constant) => (ty_constant, c))
            (prove_after(decls, c, assumptions, Relation::equals(ty_constant, ty)) => c)
            ----------------------------- ("const has ty")
            (prove_wc(decls, env, assumptions, Predicate::ConstHasType(constant, ty)) => c)
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::Crates;
    use crate::rust::term;

    fn supertrait_program() -> Program {
        let crates: Crates = term(
            "[
                crate test {
                    trait Super {}
                    trait Mid where Self: Super {}
                    trait Sub where Self: Mid {}
                    trait ValidationRoot where Self: Sub {}
                }
            ]",
        );
        crates.to_prove_decls()
    }

    #[test]
    fn exact_assumption_uses_trivial_proof() {
        let goal: Wc = term("u32: Exact");
        let (_, proof) = prove_wc(Program::empty(), Env::default(), &goal, &goal)
            .into_singleton()
            .unwrap();

        assert_eq!(proof.total_nodes(), 1, "{proof}");
    }

    #[test]
    fn ordinary_supertrait_assumption_is_proven() {
        let result = prove_wc(
            supertrait_program(),
            Env::default(),
            term::<Wc>("u32: Sub"),
            term::<Wc>("u32: Super"),
        );

        assert!(result.is_proven(), "{result}");
    }
}
