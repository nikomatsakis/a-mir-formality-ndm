use crate::grammar::{
    AliasTy, Predicate, Relation, Trait, TraitRef, ValidationState, Wc, WcData, Wcs,
};
use crate::prove::prove::{
    decls::Program,
    prove,
    requirements::{trait_requirement, TraitRequirement, TraitRequirementBoundData},
};
use formality_core::{judgment_fn, Downcast};

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
            (prove_validate_via_trait_requirement(
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
    /// Use a declared trait requirement while preserving validation stages.
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
    fn prove_validate_via_trait_requirement(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        trait_def: Trait,
        requirement: TraitRequirement,
        goal: Wc,
    ) => Constraints {
        debug(assumptions, trait_def, requirement, goal, env)

        (
            // Running example: suppose we are validating `PartialOrd(u32)` and considering the
            // declaration `trait Ord: PartialOrd`. Here `trait_def` is `Ord`, `requirement` is
            // `for<Self> Self: PartialOrd`, and `goal` is `PartialOrd(u32)`.
            //
            // Instantiate the requirement's `Self` with a fresh existential `?T`. The selected
            // supertrait requirement is therefore `PartialOrd(?T)`.
            (let (env, trait_subst) =
                env.existential_substitution(&requirement.binder))
            (let requirement =
                requirement.binder.instantiate_with(trait_subst)?)
            (if let TraitRequirementBoundData::Supertrait(supertrait) = requirement)!

            // Check the conclusion (`PartialOrd(T)`) can be used to prove our goal.
            //
            // Note that the `Validate` from the goal and the validate from the conclusion
            // have both been stripped here.
            (prove_via_assumption(decls, env, assumptions, Wc::for_all(supertrait), goal) => c)

            // In that case, we have to prove that the condition `Validate(B, Ord(T))`
            // fulfills our goal. This rule always requires stage B evidence.
            (prove_after(
                decls,
                c,
                assumptions,
                Wc::validate(ValidationState::B, TraitRef::new(&trait_def.id, trait_subst))
            ) => c)
            ----------------------------- ("supertrait")
            (prove_validate_via_trait_requirement(
                decls,
                env,
                assumptions,
                trait_def,
                requirement,
                goal,
            ) => c.pop_subst(trait_subst))
        )

        (
            // Instantiate the declaration's trait parameters and associated
            // type parameters, then match one declared value bound against the
            // validation goal.
            (let (env, trait_subst) =
                env.existential_substitution(&requirement.binder))
            (let requirement =
                requirement.binder.instantiate_with(trait_subst)?)
            (if let TraitRequirementBoundData::AssociatedTyRequirement(associated) =
                requirement)
            (let (env, associated_subst) =
                env.existential_substitution(&associated.binder))
            (let value_template =
                associated.binder.instantiate_with(&associated_subst)?)
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

            // A completed source dictionary and completed GAT conditions may
            // expose the associated type's declaration-side bounds during
            // verification. Provisional stage-A evidence may not.
            (let (source, conditions) =
                decls.associated_ty_requirements(&alias)?)
            (prove_after(
                decls,
                c,
                assumptions,
                conditions.validated(ValidationState::B),
            ) => c)
            (prove_after(
                decls,
                c,
                assumptions,
                Wc::validate(ValidationState::B, source),
            ) => c)
            ----------------------------- ("associated type")
            (prove_validate_via_trait_requirement(
                decls,
                env,
                assumptions,
                trait_def,
                requirement,
                goal,
            ) => c.pop_subst(associated_subst).pop_subst(trait_subst))
        )
    }
}
