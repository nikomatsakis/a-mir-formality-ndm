use crate::grammar::{
    AssociatedTyBoundData, AssociatedTyValueBoundData, Relation, TraitImpl, TraitImplBoundData,
    TraitItem, ValidationContext, ValidationState, Wc, Wcs,
};
use crate::prove::prove::decls::Program;
use crate::prove::prove::prove::prove;
use crate::prove::prove::{trait_requirement, TraitRequirementBoundData};
use crate::prove::ToWcs;
use formality_core::{judgment_fn, Upcast};

use super::{constraints::Constraints, env::Env};

judgment_fn! {
    /// Prove that an impl declaration satisfies every requirement imposed by its trait.
    ///
    /// This is a closed judgment: its caller supplies neither an environment nor assumptions.
    /// The impl binder is instantiated universally. The impl header is available inside the
    /// derivation as stage-A evidence for constructor requirements and stage-B evidence for
    /// associated-type guarantees; it never becomes an ordinary trait assumption. Program
    /// checking establishes this judgment for every impl. Selection and projection normalization
    /// repeat it defensively because lower-level solver entry points can be invoked on an unchecked
    /// `Program`.
    pub(crate) fn prove_impl_wf(
        program: Program,
        trait_impl: TraitImpl,
    ) => () {
        debug(trait_impl, program)

        (
            (let (env, impl_data) =
                Env::default().instantiate_universally(&trait_impl.binder))
            (let trait_ref = impl_data.trait_ref())
            (let validation = ValidationContext::new(
                ValidationState::A,
                &trait_ref.trait_id,
            ))
            (let provisional_impl_header = Wc::validate(validation, trait_ref).to_wcs())
            (let trait_def = program.trait_def(&trait_ref.trait_id))
            (trait_requirement(trait_def) => requirements)
            (for_all(requirement in requirements)
                (let requirement =
                    requirement.binder.instantiate_with(&trait_ref.parameters)?)
                (validate_impl_requirement(
                    program,
                    env,
                    provisional_impl_header,
                    impl_data,
                    requirement,
                ) => c)
                (if c.unconditionally_true()))
            ----------------------------- ("requirements")
            (prove_impl_wf(program, trait_impl) => ())
        )
    }
}

judgment_fn! {
    /// Validate one instantiated requirement of an impl declaration.
    fn validate_impl_requirement(
        program: Program,
        env: Env,
        provisional_impl_header: Wcs,
        trait_impl: TraitImplBoundData,
        requirement: TraitRequirementBoundData,
    ) => Constraints {
        debug(provisional_impl_header, trait_impl, requirement, env, program)

        (
            (let validation = ValidationContext::new(
                ValidationState::A,
                &trait_impl.trait_id,
            ))
            (let assumptions = (
                provisional_impl_header,
                trait_impl.where_clauses.to_wcs().validated(validation),
            ).to_wcs())
            (let goal = Wc::validate(validation, Wc::for_all(supertrait)))
            (prove(program, env, assumptions, goal) => c)
            ----------------------------- ("supertrait")
            (validate_impl_requirement(
                program,
                env,
                provisional_impl_header,
                trait_impl,
                TraitRequirementBoundData::Supertrait(supertrait),
            ) => c)
        )

        (
            (let validation = ValidationContext::new(
                ValidationState::A,
                &trait_impl.trait_id,
            ))
            (let assumptions = (
                provisional_impl_header,
                trait_impl.where_clauses.to_wcs().validated(validation),
            ).to_wcs())
            (let goal = Wc::validate(validation, Wc::for_all(outlives)))
            (prove(program, env, assumptions, goal) => c)
            ----------------------------- ("outlives")
            (validate_impl_requirement(
                program,
                env,
                provisional_impl_header,
                trait_impl,
                TraitRequirementBoundData::Outlives(outlives),
            ) => c)
        )

        (
            // Universally instantiate the associated type's parameters. These variables are
            // independent of the universally instantiated impl parameters already in `env`.
            (let (env, gat_subst) = env.universal_substitution(&associated.binder))

            // Instantiate this impl's associated value with the same GAT arguments.
            (if let Some(assoc_ty_value) = trait_impl.assoc_ty_value(&associated.id))
            (if assoc_ty_value.binder.kinds() == associated.binder.kinds())!
            (let AssociatedTyValueBoundData {
                where_clauses: _,
                ty: impl_ty,
            } = assoc_ty_value.binder.instantiate_with(gat_subst)?)

            // Substitute that value into the bounds promised by the trait.
            (let gat_goals = associated
                .binder
                .instantiate_with(gat_subst)?
                .instantiate_with(std::slice::from_ref(&impl_ty))?)

            // Instantiate the declaration-side GAT conditions with the same arguments.
            (let trait_def = program.trait_def(&trait_impl.trait_id))
            (let trait_data =
                trait_def.binder.instantiate_with(&trait_impl.trait_ref().parameters)?)
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
                where_clauses: trait_gat_wc,
            } = trait_associated_ty.binder.instantiate_with(gat_subst)?)

            // The impl header and the impl/GAT conditions are completed inputs at the point
            // callers rely on an associated type guarantee. The concrete value's WF and every
            // promised bound must therefore be established as completed conclusions too.
            (let completed = ValidationContext::new(
                ValidationState::B,
                &trait_impl.trait_id,
            ))
            (let completed_impl_header =
                Wc::validate(completed, trait_impl.trait_ref()).to_wcs())
            (let validation_conditions =
                (trait_impl.where_clauses.to_wcs(), trait_gat_wc.to_wcs())
                    .to_wcs()
                    .validated(completed))
            (let value_wf: Wc = Relation::well_formed(impl_ty).upcast())
            (let validation_goals: Wcs = std::iter::once(value_wf.clone())
                .chain(gat_goals.iter())
                .map(|goal| Wc::implies(
                    validation_conditions,
                    Wc::validate(completed, goal),
                ))
                .collect())
            (prove(program, env, completed_impl_header, validation_goals) => c)
            ----------------------------- ("associated type")
            (validate_impl_requirement(
                program,
                env,
                provisional_impl_header,
                trait_impl,
                TraitRequirementBoundData::AssociatedTyRequirement(associated),
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
    fn associated_bound_cannot_be_projected_within_an_scc() {
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

        // The concrete `Good: Bound` proof makes the `Family` impl independently WF. The two
        // blanket conditions nevertheless put `Family` and `Root` in one SCC, so the `Root` impl
        // cannot project `Bound` from its provisional `Family` input.
        assert!(impl_wf(&program, "Family"));
        assert!(!impl_wf(&program, "Root"));
    }

    #[test]
    fn completed_impl_header_verifies_an_associated_bound() {
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

        // The completed header is local stage-B evidence while checking this associated-type
        // guarantee, so it can satisfy the exact completed `u32: Foo` bound without becoming an
        // ordinary trait assumption.
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
