use crate::grammar::{
    AssociatedTyValue, AssociatedTyValueBoundData, Mode, Relation, TraitImpl, TraitImplBoundData,
    TraitRef, Wc, Wcs,
};
use crate::prove::prove::decls::Program;
use crate::prove::prove::prove::prove;
use crate::prove::prove::{
    trait_requirement, AssociatedTyRequirement, AssociatedTyRequirementData, TraitRequirement,
    TraitRequirementBoundData,
};
use formality_core::judgment_fn;

use super::{constraints::Constraints, env::Env, impl_contract};

judgment_fn! {
    /// Prove that an impl declaration satisfies every requirement imposed by its trait.
    ///
    /// This is a closed judgment: its caller supplies neither an environment nor assumptions.
    /// The impl binder is instantiated universally. While checking the dictionary's supertrait
    /// fields, the impl header is available at `IfBelow[ImplTrait]`. While checking an
    /// associated value and its promised dictionaries, it is available at
    /// `IfBelowG[ImplTrait]`. It never becomes an ordinary trait assumption. Program checking
    /// establishes this judgment for every impl. Selection and projection normalization repeat it
    /// defensively because lower-level solver entry points can be invoked on an unchecked
    /// `Program`.
    pub(crate) fn prove_impl_wf(
        program: Program,
        trait_impl: TraitImpl,
    ) => () {
        debug(trait_impl, program)

        (
            (let (env, impl_data @ TraitImplBoundData { trait_id, .. }) =
                Env::default().instantiate_universally(binder))
            (let TraitRef { parameters, .. } = impl_data.trait_ref())
            (let trait_def = program.trait_def(trait_id))
            (trait_requirement(trait_def) => requirements)
            (for_all(requirement in requirements)
                (let TraitRequirement { binder: requirement_binder } = requirement)
                (let requirement =
                    requirement_binder.instantiate_with(parameters)?)
                (validate_impl_requirement(
                    program,
                    env,
                    impl_data,
                    requirement,
                ) => c)
                (if c.unconditionally_true()))
            ----------------------------- ("requirements")
            (prove_impl_wf(program, TraitImpl { binder, safety: _ }) => ())
        )
    }
}

judgment_fn! {
    /// Validate one instantiated requirement of an impl declaration.
    fn validate_impl_requirement(
        program: Program,
        env: Env,
        trait_impl: TraitImplBoundData,
        requirement: TraitRequirementBoundData,
    ) => Constraints {
        debug(trait_impl, requirement, env, program)

        (
            (impl_contract(trait_impl) => (impl_header, conditions))
            (prove(
                program,
                env,
                (
                    Mode::HasImpl.apply_assumption(impl_header),
                    Mode::if_below(&impl_header.trait_id).apply_assumptions(conditions),
                ),
                Wc::for_all(supertrait),
            ) => c)
            ----------------------------- ("supertrait")
            (validate_impl_requirement(
                program,
                env,
                trait_impl,
                TraitRequirementBoundData::Supertrait(supertrait),
            ) => c)
        )

        (
            (impl_contract(trait_impl) => (impl_header, conditions))
            (prove(
                program,
                env,
                (
                    Mode::HasImpl.apply_assumption(impl_header),
                    Mode::if_below(&impl_header.trait_id).apply_assumptions(conditions),
                ),
                Wc::for_all(outlives),
            ) => c)
            ----------------------------- ("outlives")
            (validate_impl_requirement(
                program,
                env,
                trait_impl,
                TraitRequirementBoundData::Outlives(outlives),
            ) => c)
        )

        (
            // Universally instantiate the associated type's parameters. These variables are
            // independent of the universally instantiated impl parameters already in `env`.
            (let (env, gat_subst) = env.universal_substitution(associated_binder))

            // Instantiate this impl's associated value with the same GAT arguments.
            (AssociatedTyValue { binder: value_binder, .. } in
                trait_impl.assoc_ty_value(associated_id))
            (if value_binder.kinds() == associated_binder.kinds())!
            (let AssociatedTyValueBoundData {
                where_clauses: _,
                ty: impl_ty,
            } = value_binder.instantiate_with(gat_subst)?)

            // Substitute that value into the bounds promised by the trait and instantiate the
            // declaration-side GAT conditions with the same arguments.
            (let AssociatedTyRequirementData {
                where_clauses: trait_gat_wc,
                value_bounds,
            } = associated_binder.instantiate_with(gat_subst)?)
            (let gat_goals = value_bounds.instantiate_with((impl_ty,))?)

            (impl_contract(trait_impl) => (impl_header, conditions))
            (let TraitRef { trait_id, parameters: _ } = impl_header)

            // Associated values and supertrait fields are already available while constructing
            // the dictionaries promised by an associated type. The impl header and the impl/GAT
            // conditions are therefore viewed at `IfBelowG[ImplTrait]` while checking both the
            // concrete value's WF and each promised bound.
            (let gat_bounds = Mode::if_below_g(trait_id))
            (let conditions =
                gat_bounds.apply_assumptions((conditions, trait_gat_wc)))
            (let goals =
                gat_bounds.apply_goals((Relation::well_formed(impl_ty), gat_goals)))
            (let goals = Wcs::from_iter(
                goals.iter().map(|goal| Wc::implies(conditions, goal))))
            (prove(
                program,
                env,
                gat_bounds.apply_assumption(impl_header),
                goals,
            ) => c)
            ----------------------------- ("associated type")
            (validate_impl_requirement(
                program,
                env,
                trait_impl,
                AssociatedTyRequirement {
                    id: associated_id,
                    binder: associated_binder,
                },
            ) => c.pop_subst(gat_subst))
        )
    }
}

#[cfg(test)]
mod tests {
    use super::prove_impl_wf;
    use crate::grammar::{Crates, TraitId};
    use crate::prove::prove::Program;
    use crate::rust::term;

    fn program(source: &str) -> Program {
        term::<Crates>(source).to_prove_decls()
    }

    fn impl_wf(program: &Program, trait_id: &str) -> bool {
        let candidate = program
            .raw_trait_impls_for(&term::<TraitId>(trait_id))
            .into_iter()
            .next()
            .unwrap();
        prove_impl_wf(program, candidate.trait_impl).is_proven()
    }

    #[test]
    fn exact_where_clause_verifies_a_supertrait() {
        let program = program(
            "[
                crate test {
                    trait Super {}
                    trait Sub where Self: Super {}
                    impl<T> Sub for T where T: Super {}
                }
            ]",
        );

        assert!(impl_wf(&program, "Sub"));
    }

    #[test]
    fn missing_supertrait_requirement_is_not_wf() {
        let program = program(
            "[
                crate test {
                    trait Super {}
                    trait Sub where Self: Super {}
                    struct Ground {}
                    impl Sub for Ground {}
                }
            ]",
        );

        assert!(!impl_wf(&program, "Sub"));
    }

    #[test]
    fn lower_ranked_where_clause_can_expose_its_supertrait() {
        let program = program(
            "[
                crate test {
                    trait Super {}
                    trait Stronger where Self: Super {}
                    trait Sub where Self: Super {}
                    impl<T> Sub for T where T: Stronger {}
                }
            ]",
        );

        assert!(impl_wf(&program, "Sub"));
    }

    #[test]
    fn mutually_recursive_sources_are_not_ordered() {
        let program = program(
            "[
                crate test {
                    trait Base {}
                    trait A where Self: Base {}
                    trait B where Self: Base {}
                    impl<T> A for T where T: B {}
                    impl<T> B for T where T: A {}
                }
            ]",
        );

        assert!(!impl_wf(&program, "A"));
        assert!(!impl_wf(&program, "B"));
    }

    #[test]
    fn grounded_blanket_chain_is_wf() {
        let program = program(
            "[
                crate test {
                    trait Base {}
                    trait Debug {}
                    trait A where Self: Base {}
                    trait B where Self: Base {}
                    impl<T> A for T where T: B {}
                    impl<T> B for T where T: Debug {}
                    impl<T> Base for T where T: Debug {}
                }
            ]",
        );

        assert!(impl_wf(&program, "A"));
        assert!(impl_wf(&program, "B"));
        assert!(impl_wf(&program, "Base"));
    }

    #[test]
    fn dependency_trait_can_validate_a_local_impl() {
        let program = program(
            "[
                crate dependency {
                    trait Base {}
                    trait Upstream where Self: Base {}
                },

                crate current {
                    struct LocalType {}
                    trait Local where Self: Base {}

                    impl Local for LocalType
                    where
                        LocalType: Upstream,
                    {}
                }
            ]",
        );

        // Every dependency trait is strictly below every local trait. The provisional
        // `LocalType: Upstream` input may therefore expose its `Base` supertrait while checking
        // the local `Local` impl.
        assert!(impl_wf(&program, "Local"));
    }

    #[test]
    fn local_trait_cannot_validate_a_dependency_impl() {
        let program = program(
            "[
                crate dependency {
                    trait Base {}
                    trait Upstream where Self: Base {}
                },

                crate current {
                    struct LocalType {}
                    trait Local where Self: Base {}

                    impl Upstream for LocalType
                    where
                        LocalType: Local,
                    {}
                }
            ]",
        );

        // Crate ordering gives `Upstream < Local`, never the reverse. Provisional local evidence
        // must not flow upward to validate an impl of the dependency trait.
        assert!(!impl_wf(&program, "Upstream"));
    }

    #[test]
    fn exact_supertrait_clause_is_usable_inside_an_scc() {
        let program = program(
            "[
                crate test {
                    trait Base {}
                    trait A where Self: Base {}
                    trait B where Self: Base {}

                    impl<T> A for T
                    where
                        T: B,
                        T: Base,
                    {}

                    impl<T> B for T
                    where
                        T: A,
                    {}
                }
            ]",
        );

        // `A` and `B` are incomparable because their blanket impls put them in one SCC. This
        // prevents projecting `Base` from either trait, but does not invalidate exact evidence.
        assert!(impl_wf(&program, "A"));
        assert!(!impl_wf(&program, "B"));
    }

    #[test]
    fn associated_value_can_be_selected_without_projecting_its_bound() {
        let program = program(
            "[
                crate test {
                    trait Bound {}

                    trait Family {
                        type Item: [Bound];
                    }

                    trait Root {
                        type Value: [Bound];
                    }

                    struct Good {}
                    impl Bound for Good {}

                    impl<T> Root for T
                    where
                        T: Family,
                    {
                        type Value = <T as Family>::Item;
                    }

                    impl<T> Family for T
                    where
                        T: Root,
                    {
                        type Item = Good;
                    }
                }
            ]",
        );

        // `Family` and `Root` are incomparable because their blanket conditions put them in one
        // SCC. The `Root` impl therefore cannot project `Bound` from provisional `Family`
        // evidence. It can nevertheless select `Family::Item = Good`: associated values are
        // visible before their promised dictionaries, and the independent `Good: Bound` impl
        // finishes the proof.
        assert!(impl_wf(&program, "Family"));
        assert!(impl_wf(&program, "Root"));
    }

    #[test]
    fn gat_bound_frontier_verifies_an_associated_bound() {
        let program = program(
            "[
                crate test {
                    trait Foo {
                        type Bar: [Foo];
                    }

                    impl Foo for u32 {
                        type Bar = u32;
                    }
                }
            ]",
        );

        // The header is local `IfBelowG[Foo]` evidence while checking this associated-type
        // guarantee, so it can satisfy the exact `u32: Foo` bound without becoming an ordinary
        // trait assumption.
        assert!(impl_wf(&program, "Foo"));
    }

    #[test]
    fn duplicate_associated_values_are_not_wf() {
        let program = program(
            "[
                crate test {
                    trait Bound {}
                    trait Family {
                        type Item: [Bound];
                    }

                    struct Good {}
                    struct Bad {}
                    impl Bound for Good {}

                    impl Family for () {
                        type Item = Good;
                        type Item = Bad;
                    }
                }
            ]",
        );

        assert!(!impl_wf(&program, "Family"));
    }

    #[test]
    fn coinductive_scc_without_requirements_is_wf() {
        let program = program(
            "[
                crate test {
                    trait A {}
                    trait B {}
                    impl<T> A for T where T: B {}
                    impl<T> B for T where T: A {}
                }
            ]",
        );

        assert!(impl_wf(&program, "A"));
        assert!(impl_wf(&program, "B"));
    }
}
