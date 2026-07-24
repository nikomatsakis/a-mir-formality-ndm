use crate::grammar::{Predicate, Relation, TraitRef, ValidationState, Wc, WcData, Wcs};
use crate::prove::prove::prove;
use crate::prove::ToWcs;
use formality_core::{judgment_fn, Downcast, Upcast};

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
        has_unconditional_validation_supertrait_assumption,
        prove_validate_via_supertrait_requirement, prove_via_trait_requirement, trait_requirement,
    },
};

use super::constraints::{Constrained, Constraints};

fn has_unconditional_proof_from_assumptions(decls: &Program, assumptions: &Wcs, goal: &Wc) -> bool {
    if assumptions
        .iter()
        .any(|assumption| match (&assumption, goal) {
            (Wc::Validate(assumption_state, assumption_goal), Wc::Validate(goal_state, goal)) => {
                assumption_state.can_prove(goal_state) && assumption_goal == goal
            }

            _ => &assumption == goal,
        })
    {
        return true;
    }

    let Wc::Validate(_, goal) = goal else {
        return false;
    };

    has_unconditional_validation_supertrait_assumption(decls, assumptions, goal)
}

fn positive_impl_trait_ref(goal: &Wc) -> Option<TraitRef> {
    match goal {
        // Stage A records provisional evidence for a concrete impl under construction, so it can
        // be established by selecting that impl. Stage B represents caller evidence and cannot
        // be manufactured by impl selection.
        Wc::Validate(ValidationState::A, goal) => goal.as_ref().downcast(),
        Wc::Validate(ValidationState::B, _) => None,
        goal => goal.downcast(),
    }
}

fn is_ordinary_assumption_goal(goal: &Wc) -> bool {
    matches!(goal, Wc::Predicate(_) | Wc::Relation(_))
}

fn is_validation_assumption_goal(goal: &Wc) -> bool {
    matches!(
        goal,
        Wc::Validate(_, goal)
            if matches!(goal.as_ref(), Wc::Predicate(_) | Wc::Relation(_))
    )
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
        // Validation evidence can directly prove an identical goal at the same or a weaker
        // validation stage. This cut is important when validating requirements of the form
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
            (if is_ordinary_assumption_goal(&goal))!
            (a in assumptions)
            (prove_via_assumption(decls, env, assumptions, a, goal) => c)
            ----------------------------- ("assumption")
            (prove_wc(decls, env, assumptions, goal) => c)
        )

        (
            (if is_validation_assumption_goal(&goal))
            (a in assumptions)
            (prove_via_assumption(decls, env, assumptions, a, goal) => c)!
            ----------------------------- ("validation assumption")
            (prove_wc(decls, env, assumptions, goal) => c)
        )

        // Prove an ordinary trait goal, or a stage-A validation goal, with a concrete impl.
        // `positive_impl_trait_ref` deliberately leaves stage B ineligible.
        (
            (if let Some(trait_ref) = positive_impl_trait_ref(goal))
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
            (prove_wc(decls, env, assumptions, goal) => c)
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
            (prove_via_trait_requirement(
                decls,
                env,
                assumptions,
                trait_def,
                requirement,
                Predicate::is_implemented(trait_ref),
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
    use crate::grammar::Crates;
    use crate::rust::term;

    fn supertrait_program() -> Program {
        let crates: Crates = term(
            "[
                crate test {
                    trait Super {}
                    trait Mid where Self: Super {}
                    trait Sub where Self: Mid {}
                }
            ]",
        );
        crates.to_prove_decls()
    }

    #[test]
    fn exact_assumption_uses_trivial_proof() {
        let goal: Wc = term("Exact(u32)");
        let (_, proof) = prove_wc(Program::empty(), Env::default(), &goal, &goal)
            .into_singleton()
            .unwrap();

        assert_eq!(proof.total_nodes(), 1, "{proof}");
    }

    #[test]
    fn stronger_validation_assumption_uses_trivial_proof() {
        let inner: Wc = term("u32 = bool");
        let assumption = Wc::validate(ValidationState::B, &inner);
        let goal = Wc::validate(ValidationState::A, inner);
        let (_, proof) = prove_wc(Program::empty(), Env::default(), assumption, goal)
            .into_singleton()
            .unwrap();

        assert_eq!(proof.total_nodes(), 1, "{proof}");
    }

    #[test]
    fn ordinary_supertrait_assumption_is_proven() {
        let result = prove_wc(
            supertrait_program(),
            Env::default(),
            term::<Wc>("Sub(u32)"),
            term::<Wc>("Super(u32)"),
        );

        assert!(result.is_proven(), "{result}");
    }

    #[test]
    fn stage_b_supertrait_assumption_uses_trivial_proof() {
        let assumption = Wc::validate(ValidationState::B, term::<Wc>("Sub(u32)"));
        let goal = Wc::validate(ValidationState::A, term::<Wc>("Super(u32)"));
        let (_, proof) = prove_wc(supertrait_program(), Env::default(), assumption, goal)
            .into_singleton()
            .unwrap();

        assert_eq!(proof.total_nodes(), 1, "{proof}");
    }
}

judgment_fn! {
    /// Prove that `validate_goal` holds as an impl-validation requirement.
    fn prove_validate(
        decls: Program,
        env: Env,
        assumptions: Wcs,
        validation_state: ValidationState,
        validate_goal: Wc,
    ) => Constraints {
        debug(validation_state, validate_goal, assumptions, env)

        (
            (let goal = Wc::for_all(binder.map(
                |goal| Wc::validate(validation_state, goal),
            )))
            (prove(
                decls,
                env,
                assumptions,
                goal,
            ) => c)
            --- ("forall")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation_state,
                WcData::ForAll(binder),
            ) => c)
        )

        (
            (let validated_conditions = conditions.validated(validation_state))
            (let goal = Wc::implies(
                validated_conditions,
                Wc::validate(validation_state, consequence),
            ))
            (prove(
                decls,
                env,
                assumptions,
                goal,
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
            (if let None = validate_goal.downcast::<TraitRef>())!
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
