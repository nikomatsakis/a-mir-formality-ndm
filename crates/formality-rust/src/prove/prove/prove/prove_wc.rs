use crate::grammar::{Predicate, Relation, ValidationState, Wc, WcData, Wcs};
use crate::prove::ToWcs;
use formality_core::{judgment_fn, Upcast};

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
        prove_via_assumption::prove_via_assumption,
        prove_via_impl::prove_via_impl,
        prove_wf::{prove_wf, wf_requirements},
    },
    requirements::{
        prove_validate_via_supertrait_requirement, prove_via_trait_requirement, trait_requirement,
    },
};

use super::constraints::{Constrained, Constraints};

judgment_fn! {
    /// The "heart" of the trait system -- prove that a where-clause holds given a set of declarations, variable environment, and set of assumptions.
    /// If successful, returns the constraints under which the where-clause holds.
    pub fn prove_wc(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        goal: Wc,
    ) => Constraints {
        debug(goal, assumptions, env)

        // Prefer an exactly equal assumption before exploring derived proofs. This cut is
        // important when validating associated type requirements of the form
        // `forall<T> conditions => goal`: opening the binder creates a fresh universal and adds
        // the conditions to the assumptions. Even when one of those assumptions proves the goal,
        // exhaustive search would otherwise also explore the impl rule, which can recursively
        // validate the same associated type requirement. Each recursion opens `forall<T>` again,
        // so the assumptions grow with distinct universals (`!T_1`, `!T_2`, ...); the fixed-point
        // machinery therefore sees distinct calls instead of recognizing a cycle.
        //
        // `trivial` acts as a logical cut here. The exact assumption proves the goal without
        // introducing constraints, which is the most general possible result, so no alternative
        // derivation can improve it. This cut would not be valid if the result were more
        // restrictive.
        trivial(
            assumptions.iter().any(|assumption| assumption == goal)
            => Constraints::none(env)
        )

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
            (prove_validate(decls, env, assumptions, validation_state, validate_goal) => c)
            --- ("validate")
            (prove_wc(
                decls,
                env,
                assumptions,
                WcData::Validate(validation_state, validate_goal),
            ) => c)
        )

        (
            (a in assumptions)
            (prove_via_assumption(decls, env, assumptions, a, goal) => c)
            ----------------------------- ("assumption - predicate")
            (prove_wc(decls, env, assumptions, WcData::Predicate(goal)) => c)
        )
        (
            (a in assumptions)
            (prove_via_assumption(decls, env, assumptions, a, goal) => c)
            ----------------------------- ("assumption - relation")
            (prove_wc(decls, env, assumptions, WcData::Relation(goal)) => c)
        )


        // This rule is: prove `T: Foo<U>` holds on the basis of an `impl<A,B> Foo<B> for A where WC` impl somewhere.
        (
            (candidate in decls.raw_trait_impls_for(&trait_ref.trait_id))!
            (prove_via_impl(
                decls,
                env,
                assumptions,
                trait_ref,
                candidate,
            ) => Constrained(application, c))
            (let c = application.proof_constraints(c))
            ----------------------------- ("positive impl")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c)
        )

        (
            (if env.bias() == Bias::Completeness)!
            (may_be_remote(decls, env, assumptions, trait_ref) => c)
            ----------------------------- ("coherence / remote impl")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c)
        )

        (
            (i in decls.neg_trait_impls_for(&trait_ref.trait_id))
            (let (env, subst) = env.existential_substitution(&i.binder))
            (let i = i.binder.instantiate_with(subst).unwrap())
            (let impl_trait_ref = i.trait_ref())
            (let impl_where_clauses = i.where_clauses.to_wcs())
            (prove_after(decls, env, assumptions, Wcs::all_eq(&trait_ref.parameters, &impl_trait_ref.parameters)) => c)
            (prove_after(decls, c, assumptions, impl_where_clauses) => c)
            ----------------------------- ("negative impl")
            (prove_wc(decls, env, assumptions, Predicate::NotImplemented(trait_ref)) => c.pop_subst(subst))
        )

        (
            (prove_eq(decls, env, assumptions, alias_ty, ty) => c)
            ----------------------------- ("alias eq")
            (prove_wc(decls, env, assumptions, Predicate::AliasEq(alias_ty, ty)) => c)
        )

        // The Rust declaration `trait Eq: PartialEq` gives rise to the requirement template
        // `forall<T> Eq(T) => PartialEq(T)`. Apply that requirement by backward chaining: for
        // the goal `PartialEq(U)`, instantiate `T` with an inference variable, match the
        // requirement's `required` clause against the goal, and then prove its `source`
        // (`Eq(U)`). This lazily elaborates implied bounds rather than adding all of their
        // consequences to the assumptions eagerly.
        (
            (trait_def in decls.traits())
            (trait_requirement(trait_def) => requirements)
            (requirement in requirements)
            (let goal: Wc = Predicate::is_implemented(trait_ref).upcast())
            (prove_via_trait_requirement(
                decls,
                env,
                assumptions,
                trait_def,
                requirement,
                goal,
            ) => c)!
            ----------------------------- ("trait requirement")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c)
        )

        (
            (prove_eq(decls, env, assumptions, a, b) => c)
            ----------------------------- ("eq")
            (prove_wc(decls, env, assumptions, Relation::Equals(a, b)) => c)
        )

        (
            (prove_sub(decls, env, assumptions, a, b) => c)
            ----------------------------- ("subtype")
            (prove_wc(decls, env, assumptions, WcData::Relation(Relation::Sub(a, b))) => c)
        )

        // For example, `trait Foo<T> where T: Debug` means that the trait-ref `S: Foo<U>` is
        // well formed only if `S` and `U` are well formed and `U: Debug`. In general, substitute
        // the trait-ref's parameters into the Rust trait declaration and prove every resulting
        // where-clause. This checks every trait-header condition; it is distinct from the
        // trait-requirement rule above, which exposes only the clauses classified as implied
        // requirements.
        (
            (for_all(decls, env, assumptions, &trait_ref.parameters, &prove_wf) => c)
            (let trait_def = decls.trait_def(&trait_ref.trait_id))
            (let trait_data = trait_def.binder.instantiate_with(&trait_ref.parameters).unwrap())
            (let trait_where_clauses = trait_data.where_clauses.to_wcs())
            (prove_after(decls, c, assumptions, trait_where_clauses) => c)
            ----------------------------- ("trait well formed")
            (prove_wc(decls, env, assumptions, Predicate::WellFormedTraitRef(trait_ref)) => c)
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
    use crate::rust::term;

    #[test]
    fn exact_assumption_uses_trivial_proof() {
        let goal: Wc = term("Exact(u32)");
        let (_, proof) = prove_wc(Program::empty(), Env::default(), &goal, &goal)
            .into_singleton()
            .unwrap();

        assert_eq!(proof.total_nodes(), 1, "{proof}");
    }

    #[test]
    fn stronger_validation_assumption_uses_trivial_validation_proof() {
        let inner: Wc = term("Exact(u32)");
        let assumption = Wc::validate(ValidationState::B, &inner);
        let goal = Wc::validate(ValidationState::A, inner);
        let (_, proof) = prove_wc(Program::empty(), Env::default(), assumption, goal)
            .into_singleton()
            .unwrap();

        assert_eq!(proof.total_nodes(), 2, "{proof}");
    }
}

judgment_fn! {
    /// Prove that `validate_goal` holds as an impl-validation requirement.
    fn prove_validate(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        validation_state: ValidationState,
        validate_goal: Wc,
    ) => Constraints {
        debug(validation_state, validate_goal, assumptions, env)

        // An exact validation assumption at the same or a stronger stage is the most general
        // possible proof, so no other rule can contribute a distinct result.
        trivial(
            assumptions.iter().any(|assumption| match assumption {
                Wc::Validate(assumption_state, assumption_goal) => {
                    assumption_state.can_prove(&validation_state)
                        && assumption_goal.as_ref() == &validate_goal
                }
                _ => false,
            })
            => Constraints::none(env)
        )

        (
            (let (env, subst) = env.universal_substitution(binder))
            (let validate_goal = binder.instantiate_with(subst).unwrap())
            (prove_validate(
                decls,
                env,
                assumptions,
                validation_state,
                validate_goal,
            ) => c)
            --- ("forall")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation_state,
                WcData::ForAll(binder),
            ) => c.pop_subst(subst))
        )

        (
            (let validated_conditions = conditions.validated(validation_state))
            (prove_validate(
                decls,
                env,
                (assumptions, validated_conditions),
                validation_state,
                consequence,
            ) => c)
            --- ("implies")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation_state,
                WcData::Implies(conditions, consequence),
            ) => c)
        )

        (
            (wf_requirements(decls, parameter) => requirements)
            (let requirements = requirements.validated(validation_state))
            (prove_after(decls, env, assumptions, requirements) => c)
            --- ("well formed")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation_state,
                WcData::Relation(Relation::WellFormed(parameter)),
            ) => c)
        )

        (
            (a in assumptions)
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                a,
                Wc::validate(validation_state, validate_goal),
            ) => c)!
            ----------------------------- ("assumption")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation_state,
                validate_goal,
            ) => c)
        )

        // A stage-B validation hypothesis may expose declaration-side supertraits. Keep the
        // originating trait at stage B while walking the requirement chain, even when the target
        // only needs stage A.
        (
            (trait_def in decls.traits())
            (trait_requirement(trait_def) => requirements)
            (requirement in requirements)
            (let goal: Wc = Predicate::is_implemented(trait_ref).upcast())
            (prove_validate_via_supertrait_requirement(
                decls,
                env,
                assumptions,
                trait_def,
                requirement,
                goal,
            ) => c)!
            ----------------------------- ("trait requirement")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation_state,
                WcData::Predicate(Predicate::IsImplemented(trait_ref)),
            ) => c)
        )

        (
            (prove_wc(decls, env, assumptions, WcData::predicate(validate_goal)) => c)
            --- ("atomic predicate")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation_state,
                WcData::Predicate(validate_goal),
            ) => c)
        )

        (
            (prove_wc(decls, env, assumptions, WcData::relation(validate_goal)) => c)
            --- ("atomic relation")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation_state,
                WcData::Relation(validate_goal),
            ) => c)
        )
    }
}
