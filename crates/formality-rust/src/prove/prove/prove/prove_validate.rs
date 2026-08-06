use crate::grammar::{
    AliasTy, AssociatedTyBoundData, Predicate, Relation, Trait, TraitItem, TraitRef, Upto, Wc,
    WcData, Wcs,
};
use crate::prove::prove::{
    can_project_associated_bound, can_project_outlives, can_project_supertrait, prove,
    trait_requirement, TraitRequirement, TraitRequirementBoundData,
};
use crate::prove::ToWcs;
use formality_core::judgment_fn;

use super::{
    constraints::Constraints, env::Env, prove_after::prove_after,
    prove_via_assumption::prove_via_assumption, prove_wc::prove_wc, prove_wf::wf_requirements,
};
use crate::prove::prove::Program;

judgment_fn! {
    /// Prove that `validate_goal` is available at one dictionary-construction frontier.
    pub(super) fn prove_validate(
        decls: Program,
        env: Env,
        assumptions: Wcs,
        validation: Upto,
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

        // Provisional evidence may expose only fields already constructed at `validation`.
        // Exact evidence is handled by the assumption rule before reaching this rule, and a
        // complete ordinary proof is handled by the fallback rules below.
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
        validation: Upto,
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
        // only when that supertrait field is available at `S`. At the supertrait frontier this
        // requires both `Stronger < Impl` and `Super < Impl`; at the GAT-bound frontier the root
        // trait's own supertrait fields are available too.
        (
            (if let WcData::Predicate(Predicate::IsImplemented(goal_trait_ref)) = goal)
            (can_project_supertrait(
                decls,
                validation,
                &trait_def.id,
                &goal_trait_ref.trait_id,
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
        // `<T as Family>::Item<U>: Bound`, provided that associated-bound field has already been
        // constructed at the current frontier.
        (
            (if let WcData::Predicate(Predicate::IsImplemented(goal_trait_ref)) = goal)
            (can_project_associated_bound(
                decls,
                validation,
                &trait_def.id,
                &goal_trait_ref.trait_id,
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

        // Outlives requirements occupy the supertrait portion of a dictionary and follow that
        // portion's construction frontier.
        (
            (can_project_outlives(decls, validation, &trait_def.id) => ())
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
