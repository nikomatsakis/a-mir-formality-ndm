//! Establish the output fields promised by one trait dictionary.

use crate::grammar::{
    AliasTy, AtomicPredicate, Mode, Parameter, Predicate, Relation, TraitId, TraitRef, Wc, Wcs,
};
use formality_core::{judgment_fn, Cons, Downcast};

use super::{
    constraints::{Constrained, Constraints},
    env::Env,
    prove_after::prove_after,
    prove_normalize::{prove_definition_value, prove_normalize_for_validation},
    prove_via_assumption::prove_via_assumption,
    prove_via_impl::prove_via_impl,
    prove_wf::wf_requirements,
};
use crate::prove::prove::decls::Program;
use crate::prove::prove::{
    trait_less_than, AssociatedTyRequirement, AssociatedTyRequirementData, TraitRequirement,
    TraitRequirementBoundData,
};

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

fn contains_later(wc: &Wc) -> bool {
    match wc {
        Wc::Atomic(_) => false,
        Wc::ForAll(binder) => contains_later(binder.peek()),
        Wc::Implies(conditions, consequence) => {
            conditions
                .iter()
                .any(|condition| contains_later(&condition))
                || contains_later(consequence)
        }
        Wc::Mode(Mode::Later, _) => true,
    }
}

fn completed_assumptions(assumptions: &Wcs) -> Wcs {
    assumptions
        .iter()
        .filter(|assumption| !contains_later(assumption))
        .collect()
}

fn is_completed_below_root(program: &Program, root: &TraitId, wc: &Wc) -> bool {
    match wc {
        Wc::Atomic(AtomicPredicate::Predicate(Predicate::IsImplemented(trait_ref))) => {
            trait_less_than(program, &trait_ref.trait_id, root).is_proven()
        }
        Wc::Atomic(_) => true,
        Wc::ForAll(binder) => is_completed_below_root(program, root, binder.peek()),
        Wc::Implies(conditions, consequence) => {
            conditions
                .iter()
                .all(|condition| is_completed_below_root(program, root, &condition))
                && is_completed_below_root(program, root, consequence)
        }
        Wc::Mode(Mode::Later, _) => false,
    }
}

fn completed_below_root_assumptions(program: &Program, root: &TraitId, assumptions: &Wcs) -> Wcs {
    assumptions
        .iter()
        .filter(|assumption| is_completed_below_root(program, root, assumption))
        .collect()
}

judgment_fn! {
    /// Establish a later-phase dictionary without projecting another dictionary requirement.
    ///
    /// An exact guarded handle is already the dictionary promised by this field. Otherwise a
    /// checked impl constructor must supply it. General trait-requirement backchaining is excluded:
    /// it could project the same associated-bound field through an implication antecedent and
    /// create evidence with no concrete constructor to monomorphize.
    fn prove_guarded_trait_ref(
        decls: Program,
        env: Env,
        root: TraitId,
        assumptions: Wcs,
        goal: TraitRef,
    ) => Constraints {
        debug(goal, assumptions, env)
        cut(Constraints::unconditionally_true)

        (
            (assumption in assumptions)
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                assumption,
                Wc::later(goal),
            ) => c)!
            ----------------------------- ("guarded assumption")
            (prove_guarded_trait_ref(decls, env, _root, assumptions, goal) => c)
        )

        (
            (candidate in decls.raw_trait_impls_for(trait_id))!
            (prove_via_impl(
                decls,
                env,
                assumptions,
                goal,
                candidate,
            ) => Constrained(_, c))
            ----------------------------- ("concrete impl")
            (prove_guarded_trait_ref(
                decls,
                env,
                root,
                assumptions,
                goal @ TraitRef {
                    trait_id,
                    parameters: _,
                },
            ) => c)
        )

        // Complete inputs strictly below the construction root may expose their requirements.
        // The trait-order closure guarantees that everything projected from those dictionaries is
        // below `root` as well. An implication input for `root` itself is deliberately excluded.
        (
            (let completed = completed_below_root_assumptions(decls, root, assumptions))
            (prove_after(decls, env, completed, goal) => c)
            ----------------------------- ("completed lower inputs")
            (prove_guarded_trait_ref(decls, env, root, assumptions, goal) => c)
        )
    }
}

judgment_fn! {
    /// Establish one goal belonging to an associated-type dictionary field.
    ///
    /// This judgment is reached only from a structured `AssociatedTyRequirement`. Keeping this
    /// phase local to that requirement prevents ordinary ranked rules from treating a
    /// projection-shaped associated bound as a supertrait field.
    fn prove_establish_associated_goal(
        decls: Program,
        env: Env,
        root: TraitId,
        assumptions: Wcs,
        requirement: Wc,
    ) => Constraints {
        debug(root, requirement, assumptions, env)
        cut(Constraints::unconditionally_true)

        (
            (let (env, subst) = env.universal_substitution(binder))
            (let requirement = binder.instantiate_with(subst)?)
            (prove_establish_associated_goal(
                decls,
                env,
                root,
                assumptions,
                requirement,
            ) => c)
            ----------------------------- ("forall")
            (prove_establish_associated_goal(
                decls,
                env,
                root,
                assumptions,
                Wc::ForAll(binder),
            ) => c.pop_subst(subst))
        )

        // The field promises that the impl's selected value is well formed, not merely that the
        // projection expression itself is legal. Its provisional `Later(AliasEq)` definition
        // fixes that value without exposing any of its bounds.
        (
            (let alias = parameter.downcast_err::<AliasTy>()?)
            (definition in assumptions)!
            (prove_definition_value(
                decls,
                env,
                assumptions,
                definition,
                alias,
            ) => Constrained(value, c))
            (let (assumptions, value) = c.substitution().apply((assumptions, value)))
            (wf_requirements(decls, value) => requirements)
            (prove_establish_goals(
                decls,
                c.env(),
                root,
                assumptions,
                requirements,
            ) => c2)
            ----------------------------- ("associated value well formed")
            (prove_establish_associated_goal(
                decls,
                env,
                root,
                assumptions,
                Relation::WellFormed(parameter),
            ) => c.seq(c2))
        )

        // Establish the obligation about the value selected by an associated type, not about the
        // projection syntax. This is computational unfolding through the current impl's
        // `Later(AliasEq)` definition; it does not expose the projection's declared bounds. The
        // resulting field can use an exact guarded dictionary or invoke a concrete checked impl
        // constructor, but it cannot project another trait requirement to manufacture evidence.
        (
            ((index, alias) in associated_type_parameters(parameters))!
            (prove_normalize_for_validation(
                decls,
                env,
                assumptions,
                alias,
            ) => Constrained(value, c))
            (let normalized_trait_ref = replace_trait_parameter(trait_ref, *index, value))
            (let (assumptions, normalized_trait_ref) =
                c.substitution().apply((assumptions, normalized_trait_ref)))
            (prove_guarded_trait_ref(
                decls,
                c.env(),
                root,
                assumptions,
                normalized_trait_ref,
            ) => c2)
            ----------------------------- ("normalize associated value in trait requirement")
            (prove_establish_associated_goal(
                decls,
                env,
                root,
                assumptions,
                Predicate::IsImplemented(
                    trait_ref @ TraitRef {
                        trait_id: _,
                        parameters,
                    },
                ),
            ) => c.seq(c2))
        )

        // Outlives bounds contain no dictionary data.
        (
            (prove_after(decls, env, assumptions, Relation::outlives(a, b)) => c)
            ----------------------------- ("outlives")
            (prove_establish_associated_goal(
                decls,
                env,
                _root,
                assumptions,
                Relation::Outlives(a, b),
            ) => c)
        )
    }
}

judgment_fn! {
    /// Establish every goal belonging to one associated-type dictionary field.
    fn prove_establish_associated_goals(
        decls: Program,
        env: Env,
        root: TraitId,
        assumptions: Wcs,
        requirements: Wcs,
    ) => Constraints {
        debug(root, requirements, assumptions, env)
        cut(Constraints::unconditionally_true)

        (
            ----------------------------- ("none")
            (prove_establish_associated_goals(
                _decls,
                env,
                _root,
                _assumptions,
                (),
            ) => Constraints::none(env))
        )

        (
            (prove_establish_associated_goal(
                decls,
                env,
                root,
                assumptions,
                requirement,
            ) => c)
            (let (assumptions, rest) = c.substitution().apply((assumptions, rest)))
            (prove_establish_associated_goals(
                decls,
                c.env(),
                root,
                assumptions,
                rest,
            ) => c2)
            ----------------------------- ("some")
            (prove_establish_associated_goals(
                decls,
                env,
                root,
                assumptions,
                Cons(requirement, rest),
            ) => c.seq(c2))
        )
    }
}

judgment_fn! {
    /// Establish one structured trait requirement promised by the dictionary under construction.
    ///
    /// A dictionary-valued field can use an exact guarded handle, an independently completed
    /// proof, or the partial input contract when its trait is strictly below `root`. Associated
    /// type bounds form the later phase of dictionary construction: after unfolding the current
    /// impl's definition, their selected values may be established through guarded impl
    /// constructors. Outlives facts contain no dictionary data and are established immediately.
    /// The requirement kind remains explicit through this dispatch; only formulas nested inside a
    /// requirement are handled as generic logical goals.
    pub(crate) fn prove_establish(
        decls: Program,
        env: Env,
        source: TraitRef,
        assumptions: Wcs,
        requirement: TraitRequirement,
    ) => Constraints {
        debug(source, requirement, assumptions, env)
        cut(Constraints::unconditionally_true)

        (
            (let requirement = binder.instantiate_with(parameters)?)
            (prove_establish_requirement(
                decls,
                env,
                source,
                assumptions,
                requirement,
            ) => c)
            ----------------------------- ("requirement")
            (prove_establish(
                decls,
                env,
                TraitRef {
                    trait_id: _,
                    parameters,
                },
                assumptions,
                TraitRequirement { binder },
            ) => c)
        )
    }
}

judgment_fn! {
    /// Establish one instantiated structured requirement.
    fn prove_establish_requirement(
        decls: Program,
        env: Env,
        source: TraitRef,
        assumptions: Wcs,
        requirement: TraitRequirementBoundData,
    ) => Constraints {
        debug(source, requirement, assumptions, env)
        cut(Constraints::unconditionally_true)

        (
            (let (env, subst) = env.universal_substitution(supertrait))
            (let supertrait = supertrait.instantiate_with(subst)?)
            (prove_establish_goal(
                decls,
                env,
                trait_id,
                assumptions,
                supertrait,
            ) => c)
            ----------------------------- ("supertrait")
            (prove_establish_requirement(
                decls,
                env,
                TraitRef {
                    trait_id,
                    parameters: _,
                },
                assumptions,
                TraitRequirementBoundData::Supertrait(supertrait),
            ) => c.pop_subst(subst))
        )

        (
            (let (env, subst) = env.universal_substitution(outlives))
            (let outlives = outlives.instantiate_with(subst)?)
            (prove_establish_goal(
                decls,
                env,
                trait_id,
                assumptions,
                outlives,
            ) => c)
            ----------------------------- ("outlives")
            (prove_establish_requirement(
                decls,
                env,
                TraitRef {
                    trait_id,
                    parameters: _,
                },
                assumptions,
                TraitRequirementBoundData::Outlives(outlives),
            ) => c.pop_subst(subst))
        )

        (
            (let (env, subst) = env.universal_substitution(associated_binder))
            (let AssociatedTyRequirementData {
                where_clauses,
                value_bounds,
            } = associated_binder.instantiate_with(subst)?)
            (let alias = AliasTy::associated_ty(
                trait_id,
                associated_id,
                subst.len(),
                (trait_parameters, subst),
            ))
            (let value_bounds = value_bounds.instantiate_with((alias,))?)
            (prove_establish_associated_goals(
                decls,
                env,
                trait_id,
                (assumptions, where_clauses),
                (Relation::well_formed(alias), value_bounds),
            ) => c)
            ----------------------------- ("associated type")
            (prove_establish_requirement(
                decls,
                env,
                TraitRef {
                    trait_id,
                    parameters: trait_parameters,
                },
                assumptions,
                TraitRequirementBoundData::AssociatedTyRequirement(
                    AssociatedTyRequirement {
                        id: associated_id,
                        binder: associated_binder,
                    },
                ),
            ) => c.pop_subst(subst))
        )
    }
}

judgment_fn! {
    /// Establish derived logical goals used while constructing a dictionary field.
    pub(super) fn prove_establish_goals(
        decls: Program,
        env: Env,
        root: TraitId,
        assumptions: Wcs,
        requirements: Wcs,
    ) => Constraints {
        debug(root, requirements, assumptions, env)
        cut(Constraints::unconditionally_true)

        (
            ----------------------------- ("none")
            (prove_establish_goals(
                _decls,
                env,
                _root,
                _assumptions,
                (),
            ) => Constraints::none(env))
        )

        (
            (prove_establish_goal(decls, env, root, assumptions, requirement) => c)
            (let (assumptions, rest) = c.substitution().apply((assumptions, rest)))
            (prove_establish_goals(decls, c.env(), root, assumptions, rest) => c2)
            ----------------------------- ("some")
            (prove_establish_goals(
                decls,
                env,
                root,
                assumptions,
                Cons(requirement, rest),
            ) => c.seq(c2))
        )
    }
}

judgment_fn! {
    /// Establish one derived logical goal.
    fn prove_establish_goal(
        decls: Program,
        env: Env,
        root: TraitId,
        assumptions: Wcs,
        requirement: Wc,
    ) => Constraints {
        debug(requirement, assumptions, env)
        cut(Constraints::unconditionally_true)

        (
            (let (env, subst) = env.universal_substitution(binder))
            (let requirement = binder.instantiate_with(subst)?)
            (prove_establish_goal(decls, env, root, assumptions, requirement) => c)
            ----------------------------- ("forall")
            (prove_establish_goal(
                decls,
                env,
                root,
                assumptions,
                Wc::ForAll(binder),
            ) => c.pop_subst(subst))
        )

        (
            (prove_establish_goal(
                decls,
                env,
                root,
                (assumptions, conditions),
                consequence,
            ) => c)
            ----------------------------- ("implies")
            (prove_establish_goal(
                decls,
                env,
                root,
                assumptions,
                Wc::Implies(conditions, consequence),
            ) => c)
        )

        (
            (prove_after(decls, env, assumptions, Relation::outlives(a, b)) => c)
            ----------------------------- ("outlives")
            (prove_establish_goal(
                decls,
                env,
                root,
                assumptions,
                Relation::Outlives(a, b),
            ) => c)
        )

        // A guarded output may use an exact recursive handle. Do not invoke ordinary proof search
        // with provisional assumptions: that could pass a `Later` input through another impl and
        // project a dictionary that no completed impl supplies.
        (
            (assumption in assumptions)
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                assumption,
                Wc::later(predicate),
            ) => c)!
            ----------------------------- ("guarded predicate")
            (prove_establish_goal(
                decls,
                env,
                _root,
                assumptions,
                AtomicPredicate::Predicate(predicate),
            ) => c)
        )

        // A completed dictionary strictly below the construction root can be assembled using
        // the whole partial input contract. By transitive closure of the trait order, every field
        // projectable from this dictionary is below `root` as well, so this cannot expose a field
        // that is still under construction.
        (
            (trait_less_than(decls, trait_id, root) => ())!
            (prove_after(decls, env, assumptions, trait_ref) => c)
            ----------------------------- ("completed lower trait")
            (prove_establish_goal(
                decls,
                env,
                root,
                assumptions,
                trait_ref @ TraitRef {
                    trait_id,
                    parameters: _,
                },
            ) => c)
        )

        // A completed proof is also sufficient, but it must be independent of every provisional
        // handle in this impl-WF check.
        (
            (let completed = completed_assumptions(assumptions))
            (prove_after(decls, env, completed, predicate) => c)
            ----------------------------- ("completed predicate")
            (prove_establish_goal(
                decls,
                env,
                _root,
                assumptions,
                AtomicPredicate::Predicate(predicate),
            ) => c)
        )

        (
            (assumption in assumptions)
            (prove_via_assumption(
                decls,
                env,
                assumptions,
                assumption,
                Wc::later(relation),
            ) => c)!
            ----------------------------- ("guarded relation")
            (prove_establish_goal(
                decls,
                env,
                _root,
                assumptions,
                AtomicPredicate::Relation(
                    relation @ (Relation::Equals(_, _) | Relation::Sub(_, _)),
                ),
            ) => c)
        )

        (
            (let completed = completed_assumptions(assumptions))
            (prove_after(decls, env, completed, relation) => c)
            ----------------------------- ("completed relation")
            (prove_establish_goal(
                decls,
                env,
                _root,
                assumptions,
                AtomicPredicate::Relation(
                    relation @ (Relation::Equals(_, _) | Relation::Sub(_, _)),
                ),
            ) => c)
        )

        (
            (if parameter.downcast::<AliasTy>().is_none())!
            (wf_requirements(decls, parameter) => requirements)
            (prove_establish_goals(decls, env, root, assumptions, requirements) => c)
            ----------------------------- ("well formed")
            (prove_establish_goal(
                decls,
                env,
                root,
                assumptions,
                Relation::WellFormed(parameter),
            ) => c)
        )
    }
}
