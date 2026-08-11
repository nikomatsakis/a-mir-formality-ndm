use crate::grammar::{
    AliasTy, AtomicPredicate, Mode, Parameter, Predicate, Relation, Trait, TraitRef, Wc, Wcs,
};
use crate::prove::prove::{
    associated_ty_bound, can_project_associated_bound, can_project_outlives,
    can_project_supertrait, trait_requirement, AssociatedTyRequirement, TraitRequirement,
    TraitRequirementBoundData,
};
use formality_core::{judgment_fn, Downcast};

use super::{
    constraints::{Constrained, Constraints},
    env::Env,
    prove_after::prove_after,
    prove_normalize::prove_normalize_for_validation,
    prove_via_assumption::prove_via_assumption,
    prove_wc::prove_wc,
    prove_wf::wf_requirements,
};
use crate::prove::prove::Program;

fn associated_type_parameters(
    parameters: &[Parameter],
) -> impl Iterator<Item = (usize, AliasTy)> + '_ {
    parameters
        .iter()
        .enumerate()
        .filter_map(|(index, parameter)| parameter.downcast().map(|alias| (index, alias)))
}

fn replace_trait_parameter(trait_ref: &TraitRef, index: usize, parameter: &Parameter) -> TraitRef {
    let mut parameters = trait_ref.parameters.to_owned();
    parameters[index] = parameter.to_owned();
    TraitRef::new(&trait_ref.trait_id, parameters)
}

judgment_fn! {
    /// Extract the parameters of an outlives relation.
    fn as_outlives(
        relation: Relation,
    ) => (Parameter, Parameter) {
        debug(relation)

        (
            ----------------------------- ("outlives")
            (as_outlives(Relation::Outlives(source, target)) => (source, target))
        )
    }
}

judgment_fn! {
    /// Prove that `validate_goal` is available at one dictionary-construction frontier.
    pub(super) fn prove_validate(
        decls: Program,
        env: Env,
        assumptions: Wcs,
        validation: Mode,
        validate_goal: AtomicPredicate,
    ) => Constraints {
        debug(validation, validate_goal, assumptions, env)
        cut(Constraints::unconditionally_true)

        (
            (wf_requirements(decls, parameter) => requirements)
            (let requirements = validation.apply_goals(requirements))
            (prove_after(decls, env, assumptions, requirements) => c)
            --- ("well formed")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation,
                Relation::WellFormed(parameter),
            ) => c)
        )

        // Associated values are fixed as soon as an impl is selected, before the dictionaries
        // proving their well-formedness and declared bounds have been constructed. Rewrite a
        // validated trait predicate through that value without promoting the equation or the
        // normalized type into ordinary evidence.
        (
            ((index, alias) in associated_type_parameters(parameters))!
            (prove_normalize_for_validation(
                decls,
                env,
                assumptions,
                alias,
            ) => Constrained(normalized, c))
            (let normalized_trait_ref =
                replace_trait_parameter(trait_ref, *index, normalized))
            (prove_after(
                decls,
                c,
                assumptions,
                validation.apply_goal(normalized_trait_ref),
            ) => c)
            ----------------------------- ("normalize associated value")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation,
                trait_ref @ TraitRef {
                    trait_id: _,
                    parameters,
                },
            ) => c)
        )

        // Provisional evidence may expose only fields already constructed at `validation`. Exact
        // evidence is handled by the assumption rule before reaching this rule, and a complete
        // ordinary proof is handled by the fallback rule below. Trait requirements can establish
        // either trait predicates or outlives relations, so restrict this dispatcher to those
        // constructors before enumerating every trait requirement.
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
                validate_goal,
            ) => c)!
            ----------------------------- ("trait requirement")
            (prove_validate(
                decls,
                env,
                assumptions,
                validation,
                validate_goal @ (
                    AtomicPredicate::Predicate(Predicate::IsImplemented(_))
                    | AtomicPredicate::Relation(Relation::Outlives(_, _))
                ),
            ) => c)
        )

        // A complete ordinary proof is valid in every validation context. Validation evidence
        // itself never becomes ordinary evidence, so this fallback cannot bypass the rank check
        // above.
        (
            (prove_wc(decls, env, assumptions, validate_goal) => c)
            --- ("ordinary proof")
            (prove_validate(
                decls,
                env,
                assumptions,
                _validation,
                validate_goal,
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
        validation: Mode,
        trait_def: Trait,
        requirement: TraitRequirement,
        goal: AtomicPredicate,
    ) => Constraints {
        debug(validation, assumptions, trait_def, requirement, goal, env)

        (
            (let (env, trait_subst) = env.existential_substitution(requirement_binder))
            (let requirement = requirement_binder.instantiate_with(trait_subst)?)
            (let source_trait_ref = TraitRef::new(source_trait_id, trait_subst))
            (prove_validate_via_instantiated_trait_requirement(
                decls,
                env,
                assumptions,
                validation,
                source_trait_ref,
                requirement,
                goal,
            ) => c)
            ----------------------------- ("instantiate")
            (prove_validate_via_trait_requirement(
                decls,
                env,
                assumptions,
                validation,
                Trait {
                    safety: _,
                    id: source_trait_id,
                    binder: _,
                },
                TraitRequirement {
                    binder: requirement_binder,
                },
                goal,
            ) => c.pop_subst(trait_subst))
        )
    }
}

judgment_fn! {
    /// Use one instantiated trait requirement without turning provisional evidence into ordinary
    /// evidence.
    fn prove_validate_via_instantiated_trait_requirement(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        validation: Mode,
        source_trait_ref: TraitRef,
        requirement: TraitRequirementBoundData,
        goal: AtomicPredicate,
    ) => Constraints {
        debug(validation, assumptions, source_trait_ref, requirement, goal, env)

        // Given `trait Stronger: Super`, this rule derives
        //
        //     verify(S, Impl, T: Stronger)
        //     --------------------------------
        //     verify(S, Impl, T: Super)
        //
        // only when that supertrait field is available at `S`. This requires both
        // `Stronger < Impl` and `Super < Impl`.
        (
            (can_project_supertrait(
                decls,
                validation,
                source_trait_id,
                goal_trait_id,
            ) => ())
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                Wc::for_all(supertrait),
                goal_trait_ref,
            ) => c)
            (prove_after(
                decls,
                c,
                assumptions,
                validation.apply_goal(source_trait_ref),
            ) => c)
            ----------------------------- ("supertrait")
            (prove_validate_via_instantiated_trait_requirement(
                decls,
                env,
                assumptions,
                validation,
                source_trait_ref @ TraitRef {
                    trait_id: source_trait_id,
                    parameters: _,
                },
                TraitRequirementBoundData::Supertrait(supertrait),
                goal_trait_ref @ TraitRef {
                    trait_id: goal_trait_id,
                    parameters: _,
                },
            ) => c)
        )

        // Associated type bounds are implied requirements too. For example, from verified
        // `T: Family` and the verified GAT conditions this rule can derive a verified
        // `<T as Family>::Item<U>: Bound`, provided that associated-bound field has already been
        // constructed at the current frontier.
        (
            (can_project_associated_bound(
                decls,
                validation,
                source_trait_id,
                goal_trait_id,
            ) => ())
            (associated_ty_bound(source_trait_ref, associated_requirement) => clause)
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                validation.apply_assumption(clause),
                validation.apply_goal(goal_trait_ref),
            ) => c)
            ----------------------------- ("associated type")
            (prove_validate_via_instantiated_trait_requirement(
                decls,
                env,
                assumptions,
                validation,
                source_trait_ref @ TraitRef {
                    trait_id: source_trait_id,
                    parameters: _,
                },
                associated_requirement @ AssociatedTyRequirement { .. },
                goal_trait_ref @ TraitRef {
                    trait_id: goal_trait_id,
                    parameters: _,
                },
            ) => c)
        )

        // Outlives requirements occupy the supertrait portion of a dictionary and follow that
        // portion's construction frontier.
        (
            (can_project_outlives(decls, validation, source_trait_id) => ())
            (let (env, outlives_subst) = env.existential_substitution(outlives))
            (let required_relation = outlives.instantiate_with(outlives_subst)?)
            (as_outlives(required_relation) => (required_source, required_target))
            (prove_after(
                decls,
                env,
                assumptions,
                (
                    Relation::equals(required_source, goal_source),
                    Relation::equals(required_target, goal_target),
                ),
            ) => c)
            (prove_after(
                decls,
                c,
                assumptions,
                validation.apply_goal(source_trait_ref),
            ) => c)
            (let c = c.pop_subst(outlives_subst))
            ----------------------------- ("outlives")
            (prove_validate_via_instantiated_trait_requirement(
                decls,
                env,
                assumptions,
                validation,
                source_trait_ref @ TraitRef {
                    trait_id: source_trait_id,
                    parameters: _,
                },
                TraitRequirementBoundData::Outlives(outlives),
                Relation::Outlives(goal_source, goal_target),
            ) => c)
        )
    }
}
