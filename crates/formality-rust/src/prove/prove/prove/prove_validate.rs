use crate::grammar::{
    AliasTy, AssociatedTyBoundData, Predicate, Relation, Trait, TraitItem, TraitRef,
    ValidationContext, Wc, WcData, Wcs,
};
use crate::prove::prove::{
    prove, trait_less_than, trait_requirement, TraitRequirement, TraitRequirementBoundData,
};
use crate::prove::ToWcs;
use formality_core::judgment_fn;

use super::{
    constraints::{Constrained, Constraints},
    env::Env,
    prove_after::prove_after,
    prove_match_impl::{match_impl_candidate, ImplMatchMode},
    prove_via_assumption::prove_via_assumption,
    prove_wc::prove_wc,
    prove_wf::wf_requirements,
};
use crate::prove::prove::Program;

judgment_fn! {
    /// Prove that `validate_goal` holds in the validation context for one impl.
    pub(super) fn prove_validate(
        decls: Program,
        env: Env,
        assumptions: Wcs,
        validation: ValidationContext,
        validate_goal: Wc,
    ) => Constraints {
        debug(validation, validate_goal, assumptions, env)

        (
            (prove(
                decls,
                env,
                assumptions,
                Wc::for_all(binder.map(|goal| Wc::validate(validation, goal))),
            ) => c)
            --- ("forall")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation,
                WcData::ForAll(binder),
            ) => c)
        )

        (
            (prove(
                decls,
                env,
                assumptions,
                Wc::implies(
                    conditions.validated(validation),
                    Wc::validate(validation, consequence),
                ),
            ) => c)
            --- ("implies")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation,
                WcData::Implies(conditions, consequence),
            ) => c)
        )

        (
            (wf_requirements(decls, parameter) => requirements)
            (let requirements = requirements.validated(validation))
            (prove_after(decls, env, assumptions, requirements) => c)
            --- ("well formed")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation,
                WcData::Relation(Relation::WellFormed(parameter)),
            ) => c)
        )

        // Provisional evidence may expose a declared requirement only when both the source and
        // the result are strictly below the trait whose impl is being validated. Exact evidence
        // is handled by the assumption rule before reaching this rule, and a complete ordinary
        // proof is handled by the fallback rules below.
        (
            (trait_def in decls.traits())
            (trait_requirement(trait_def) => requirements)
            (requirement in requirements)
            (prove_validate_via_trait_requirement(
                decls,
                env,
                assumptions,
                validation,
                trait_def,
                requirement,
                trait_ref,
            ) => c)!
            ----------------------------- ("trait requirement")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation,
                WcData::Predicate(Predicate::IsImplemented(trait_ref)),
            ) => c)
        )

        // The same declared-requirement judgment handles outlives conclusions. Unlike a
        // supertrait conclusion this is a relation, so it needs its own dispatch rule.
        (
            (trait_def in decls.traits())
            (trait_requirement(trait_def) => requirements)
            (requirement in requirements)
            (prove_validate_via_trait_requirement(
                decls,
                env,
                assumptions,
                validation,
                trait_def,
                requirement,
                Relation::Outlives(a.clone(), b.clone()),
            ) => c)!
            ----------------------------- ("outlives requirement")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation,
                WcData::Relation(Relation::Outlives(a, b)),
            ) => c)
        )

        // Apply a concrete impl within the validation judgment. Selecting a concrete dictionary
        // constructor is productive, so it does not require a rank check. Its inputs may consume
        // coinductive evidence already present in the enclosing validation assumptions; this rule
        // does not add the candidate goal as a new hypothesis. Rank is required only by the
        // trait-requirement rule above, which projects evidence without constructing a dictionary.
        //
        // This is deliberately distinct from `prove_via_impl`: header equality and residual
        // where-clauses stay wrapped in the caller's validation context here. Ordinary impl
        // application instead discards ambient validation assumptions and establishes its own
        // root context for the selected candidate.
        (
            (candidate in decls.raw_trait_impls_for(&trait_ref.trait_id))!
            (match_impl_candidate(
                decls,
                env,
                assumptions,
                trait_ref,
                candidate,
                ImplMatchMode::Validated(validation.clone()),
            ) => Constrained(matched, c))
            (let trait_impl = matched.trait_impl(c))
            (let impl_where_clauses = trait_impl
                .where_clauses
                .to_wcs()
                .validated(validation))
            (prove_after(decls, c, assumptions, impl_where_clauses) => c)
            (let c = matched.pop_constraints(c))
            ----------------------------- ("impl")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation,
                WcData::Predicate(Predicate::IsImplemented(trait_ref)),
            ) => c)
        )

        // A complete ordinary proof is valid in every validation context. Validation evidence
        // itself never becomes ordinary evidence, so this fallback cannot bypass the rank check
        // above.
        (
            (prove_wc(decls, env, assumptions, validate_goal) => c)
            --- ("atomic predicate")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation,
                WcData::Predicate(validate_goal),
            ) => c)
        )

        (
            (prove_wc(decls, env, assumptions, validate_goal) => c)
            --- ("atomic relation")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation,
                WcData::Relation(validate_goal),
            ) => c)
        )
    }
}

judgment_fn! {
    /// Use one declared trait requirement without turning provisional evidence into ordinary
    /// evidence.
    fn prove_validate_via_trait_requirement(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        validation: ValidationContext,
        trait_def: Trait,
        requirement: TraitRequirement,
        goal: Wc,
    ) => Constraints {
        debug(validation, assumptions, trait_def, requirement, goal, env)

        // Given `trait Stronger: Super`, this rule derives
        //
        //     verify(S, Impl, T: Stronger)
        //     --------------------------------
        //     verify(S, Impl, T: Super)
        //
        // only when both `Stronger < Impl` and `Super < Impl`.
        (
            (if let WcData::Predicate(Predicate::IsImplemented(goal_trait_ref)) = goal)
            (trait_less_than(decls, &trait_def.id, &validation.impl_trait_id) => ())
            (trait_less_than(
                decls,
                &goal_trait_ref.trait_id,
                &validation.impl_trait_id,
            ) => ())

            (let (env, trait_subst) =
                env.existential_substitution(&requirement.binder))
            (let requirement = requirement.binder.instantiate_with(trait_subst)?)
            (if let TraitRequirementBoundData::Supertrait(supertrait) = requirement)!
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                Wc::for_all(supertrait),
                goal,
            ) => c)
            (prove_after(
                decls,
                c,
                assumptions,
                Wc::validate(
                    validation,
                    TraitRef::new(&trait_def.id, trait_subst),
                ),
            ) => c)
            ----------------------------- ("supertrait")
            (prove_validate_via_trait_requirement(
                decls,
                env,
                assumptions,
                validation,
                trait_def,
                requirement,
                goal,
            ) => c.pop_subst(trait_subst))
        )

        // Associated type bounds are implied requirements too. For example, from verified
        // `T: Family` and the verified GAT conditions this rule can derive a verified
        // `<T as Family>::Item<U>: Bound`, subject to the same strict ordering on the owner and
        // result traits.
        (
            (if let WcData::Predicate(Predicate::IsImplemented(goal_trait_ref)) = goal)
            (trait_less_than(decls, &trait_def.id, &validation.impl_trait_id) => ())
            (trait_less_than(
                decls,
                &goal_trait_ref.trait_id,
                &validation.impl_trait_id,
            ) => ())

            (let (env, trait_subst) =
                env.existential_substitution(&requirement.binder))
            (let requirement = requirement.binder.instantiate_with(trait_subst)?)
            (if let TraitRequirementBoundData::AssociatedTyRequirement(associated) = requirement)
            (let (env, associated_subst) =
                env.existential_substitution(&associated.binder))
            (let value_template = associated.binder.instantiate_with(associated_subst)?)
            (let alias = AliasTy::associated_ty(
                &trait_def.id,
                &associated.id,
                associated_subst.len(),
                (trait_subst, associated_subst),
            ))
            (let value_bounds =
                value_template.instantiate_with(std::slice::from_ref(&alias))?)
            (required in value_bounds)!
            (prove_via_assumption(decls, env, assumptions, required, goal) => c)

            (let trait_data = trait_def.binder.instantiate_with(trait_subst)?)
            (let trait_associated_ty = trait_data
                .trait_items
                .iter()
                .find_map(|item| match item {
                    TraitItem::AssociatedTy(associated_ty)
                        if associated_ty.id == associated.id =>
                    {
                        Some(associated_ty)
                    }
                    _ => None,
                })
                .ok_or_else(|| anyhow::anyhow!(
                    "trait has no associated type {:?}",
                    associated.id,
                ))?)
            (let AssociatedTyBoundData {
                ensures: _,
                where_clauses,
            } = trait_associated_ty.binder.instantiate_with(associated_subst)?)
            (prove_after(
                decls,
                c,
                assumptions,
                where_clauses.to_wcs().validated(validation),
            ) => c)
            (prove_after(
                decls,
                c,
                assumptions,
                Wc::validate(
                    validation,
                    TraitRef::new(&trait_def.id, trait_subst),
                ),
            ) => c)
            (let c = c.pop_subst(associated_subst))
            ----------------------------- ("associated type")
            (prove_validate_via_trait_requirement(
                decls,
                env,
                assumptions,
                validation,
                trait_def,
                requirement,
                goal,
            ) => c.pop_subst(trait_subst))
        )

        // Outlives requirements have no target trait to rank. The trait supplying the
        // requirement must nevertheless be strictly below the impl under validation.
        (
            (trait_less_than(decls, &trait_def.id, &validation.impl_trait_id) => ())
            (if let WcData::Relation(goal_relation) = goal)
            (let (env, trait_subst) =
                env.existential_substitution(&requirement.binder))
            (let requirement = requirement.binder.instantiate_with(trait_subst)?)
            (if let TraitRequirementBoundData::Outlives(outlives) = requirement)!
            (let (env, outlives_subst) = env.existential_substitution(outlives))
            (let required_relation = outlives.instantiate_with(outlives_subst)?)
            (let (required_skeleton, required_parameters) = required_relation.debone())
            (let (goal_skeleton, goal_parameters) = goal_relation.debone())
            (if required_skeleton == goal_skeleton)
            (prove_after(
                decls,
                env,
                assumptions,
                Wcs::all_eq(required_parameters, goal_parameters),
            ) => c)
            (prove_after(
                decls,
                c,
                assumptions,
                Wc::validate(
                    validation,
                    TraitRef::new(&trait_def.id, trait_subst),
                ),
            ) => c)
            (let c = c.pop_subst(outlives_subst))
            ----------------------------- ("outlives")
            (prove_validate_via_trait_requirement(
                decls,
                env,
                assumptions,
                validation,
                trait_def,
                requirement,
                goal,
            ) => c.pop_subst(trait_subst))
        )
    }
}
