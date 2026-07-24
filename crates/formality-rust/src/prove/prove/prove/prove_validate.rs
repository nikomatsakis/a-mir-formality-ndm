use crate::grammar::{Predicate, Relation, Trait, TraitRef, ValidationState, Wc, WcData, Wcs};
use crate::prove::prove::{
    decls::Program,
    prove,
    requirements::{
        quantified_goal, trait_requirement, TraitRequirement, TraitRequirementBoundData,
    },
};
use formality_core::{judgment_fn, Downcast, Upcast};

use super::{
    constraints::Constraints, env::Env, prove_after::prove_after,
    prove_via_assumption::prove_via_assumption, prove_wc::prove_wc, prove_wf::wf_requirements,
};

judgment_fn! {
    /// Prove that `validate_goal` holds as an impl-validation requirement.
    pub(super) fn prove_validate(
        decls: Program,
        env: Env,
        assumptions: Wcs,
        validation_state: ValidationState,
        validate_goal: Wc,
    ) => Constraints {
        debug(validation_state, validate_goal, assumptions, env)

        (
            (prove(
                decls,
                env,
                assumptions,
                Wc::for_all(binder.map(|goal| Wc::validate(validation_state, goal))),
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
            (prove(
                decls,
                env,
                assumptions,
                Wc::implies(
                    conditions.validated(validation_state),
                    Wc::validate(validation_state, consequence),
                ),
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
            (prove_validate_via_supertrait_requirement(
                decls,
                env,
                assumptions,
                trait_def,
                requirement,
                trait_ref,
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

judgment_fn! {
    /// Use a declared supertrait requirement while preserving validation stages.
    ///
    /// For example, given `trait Ord: PartialOrd`, ordinary requirement backchaining proves
    /// `PartialOrd(T)` by proving `Ord(T)`. During impl validation, this rule lifts that same
    /// requirement as follows:
    ///
    /// ```text
    /// Validate(S, PartialOrd(T)) <- Validate(B, Ord(T))
    /// ```
    ///
    /// `S` may be either A or B, but the source evidence must be stage B: it must be
    /// caller-supplied evidence for a completed dictionary. Accepting stage-A `Ord(T)` here would
    /// let an `Ord` dictionary under construction expose `PartialOrd(T)` and use that provisional
    /// evidence to validate its own requirements.
    fn prove_validate_via_supertrait_requirement(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        trait_def: Trait,
        requirement: TraitRequirement,
        goal: Wc,
    ) => Constraints {
        debug(assumptions, trait_def, requirement, goal, env)

        (
            (let (env, trait_subst) =
                env.existential_substitution(&requirement.binder))
            (let requirement =
                requirement.binder.instantiate_with(trait_subst)?)
            (if let TraitRequirementBoundData::Supertrait(supertrait) = requirement)!
            (let required = quantified_goal(
                supertrait,
                |trait_ref| Predicate::is_implemented(trait_ref).upcast(),
            ))
            (prove_via_assumption(decls, env, assumptions, required, goal) => c)
            (prove_after(
                decls,
                c,
                assumptions,
                Wc::validate(ValidationState::B, TraitRef::new(&trait_def.id, trait_subst))
            ) => c)
            ----------------------------- ("supertrait")
            (prove_validate_via_supertrait_requirement(
                decls,
                env,
                assumptions,
                trait_def,
                requirement,
                goal,
            ) => c.pop_subst(trait_subst))
        )
    }
}
