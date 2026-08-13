use crate::grammar::{TraitImpl, TraitImplBoundData, TraitRef, Wc};
use crate::prove::prove::decls::Program;
use crate::prove::prove::{partial, trait_requirements};
use formality_core::judgment_fn;

use super::{env::Env, impl_contract, prove_establish};

judgment_fn! {
    /// Prove that an impl declaration satisfies every requirement imposed by its trait.
    ///
    /// This is a closed judgment: its caller supplies neither an environment nor assumptions.
    /// The impl binder is instantiated universally. The impl header is available as `Later`
    /// evidence while checking the dictionary body: it is the guarded recursive handle to the
    /// dictionary currently being constructed. Completing this closed judgment discharges that
    /// handle and certifies a constructor from `Partial[ImplTrait](ImplConditions)` to ordinary,
    /// completed evidence for the impl header. `Partial` is a translation, not a proposition: it
    /// exposes completed dictionary fields strictly below the impl trait and otherwise supplies
    /// only guarded `Later` handles. Impl application proves exactly this same translated input
    /// contract.
    ///
    /// Program checking establishes this judgment for every impl. Selection and projection
    /// normalization repeat it defensively because lower-level solver entry points can be invoked
    /// on an unchecked `Program`.
    pub(crate) fn prove_impl_wf(
        program: Program,
        trait_impl: TraitImpl,
    ) => () {
        debug(trait_impl, program)

        (
            (let (env, impl_data @ TraitImplBoundData { .. }) =
                Env::default().instantiate_universally(binder))
            (impl_contract(impl_data) => (
                impl_header @ TraitRef {
                    trait_id: impl_trait_id,
                    parameters: _,
                },
                conditions,
                definitions,
            ))
            (partial(
                program,
                impl_trait_id,
                conditions,
            ) => partial_conditions)
            (let trait_def = program.trait_def(impl_trait_id))
            (trait_requirements(trait_def) => requirements)
            (for_all(requirement in requirements)
                (prove_establish(
                    program,
                    env,
                    impl_header,
                    (
                        Wc::later(impl_header),
                        definitions,
                        partial_conditions,
                    ),
                    requirement,
                ) => c)
                (if c.unconditionally_true()))
            ----------------------------- ("requirements")
            (prove_impl_wf(program, TraitImpl { binder, safety: _ }) => ())
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
    fn associated_bound_cannot_use_its_own_implication_input() {
        let crates = term::<Crates>(
            "[
                crate test {
                    trait Ord {}
                    trait MyTrait {
                        type Gat: [Ord] where Self: MyTrait;
                    }
                    struct X {}
                    struct Bad {}
                    impl MyTrait for X {
                        type Gat = Bad where X: MyTrait;
                    }
                    fn require_ord<T>(value: T) -> () where T: Ord { trusted }
                    fn main() -> () {
                        let bad: Bad = Bad {};
                        require_ord::<Bad>(bad);
                    }
                }
            ]",
        );
        let program = crates.to_prove_decls();

        assert!(!impl_wf(&program, "MyTrait"));
        assert!(!crate::check::check_all_crates(crates).is_proven());
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
    fn impl_header_can_verify_its_associated_bound() {
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

        // This ought to be valid: the dictionary under construction will satisfy the exact
        // recursive `u32: Foo` occurrence. A replacement for the old GAT frontier must express
        // that without making the impl header an arbitrary ordinary trait assumption.
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
