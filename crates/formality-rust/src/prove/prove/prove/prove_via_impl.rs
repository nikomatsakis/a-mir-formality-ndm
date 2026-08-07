use crate::grammar::{ExistentialVar, Parameter, TraitImplBoundData, TraitRef, Upto, Wcs};
use crate::prove::prove::decls::{ImplCandidate, ImplId, Program};
use crate::prove::prove::prove::{impl_contract, match_impl_candidate, prove_after};
use crate::prove::prove::{Constrained, Constraints, Env};
use formality_core::{judgment_fn, To};

use super::prove_match_impl::MatchedImpl;

/// A successful application of one particular impl declaration.
///
/// The impl has been opened, matched against the requested trait-ref, and had its where-clauses
/// proven. Its fields have the final substitution learned during that proof applied to them. They
/// can still mention caller variables, or unconstrained impl variables when the application is
/// ambiguous; consumers decide whether those are acceptable before removing `impl_variables` from
/// the returned constraints.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct ProvedImpl {
    pub(crate) impl_id: ImplId,

    pub(crate) impl_variables: Vec<ExistentialVar>,

    /// Impl binder arguments after applying all constraints learned while proving this impl.
    pub(crate) impl_substitution: Vec<Parameter>,

    /// The opened impl after applying all constraints learned while proving this impl.
    pub(crate) trait_impl: TraitImplBoundData,
}

formality_core::cast_impl!(ProvedImpl);

// Operations on an opened, candidate-local impl.

impl ProvedImpl {
    pub(crate) fn new(
        impl_id: &ImplId,
        constraints: &Constraints,
        impl_variables: &[ExistentialVar],
        trait_impl: &TraitImplBoundData,
    ) -> Self {
        Self {
            impl_id: impl_id.clone(),
            impl_variables: impl_variables.to_vec(),
            impl_substitution: constraints
                .substitution()
                .apply(impl_variables.to::<Vec<Parameter>>()),
            trait_impl: constraints.substitution().apply(trait_impl),
        }
    }
}

judgment_fn! {
    /// Prove that the explicitly supplied impl candidate applies to
    /// `requested_trait_ref`. This judgment never searches another impl.
    pub(crate) fn prove_via_impl(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        requested_trait_ref: TraitRef,
        candidate: ImplCandidate,
    ) => Constrained<ProvedImpl> {
        debug(requested_trait_ref, candidate, assumptions, env)

        (
            (match_impl_candidate(
                decls,
                env,
                assumptions,
                requested_trait_ref,
                candidate,
            ) => Constrained(
                MatchedImpl { impl_variables, trait_impl, .. },
                c,
            ))!
            (impl_contract(trait_impl) => (impl_header, conditions))

            // A well-formed impl is a dictionary constructor from validated inputs to ordinary,
            // completed `Implemented` evidence. Selecting the impl fixes its associated-type
            // values, so its header is available at the supertrait frontier within this branch.
            // That view still cannot expose the root dictionary's own supertrait or associated-
            // bound fields.
            //
            (let validation = Upto::supertraits(&impl_header.trait_id))
            (let provisional_impl_header =
                // There's something very subtle going on here!
                //
                // Adding `Supertraits[Trait](T: Trait)` as an assumption
                // does not allow upcasting to `Supertraits[Trait](T: Supertrait)`.
                validation.apply_assumption(impl_header))
            (prove_after(
                decls,
                c,
                (assumptions, provisional_impl_header),
                validation.apply_goals(conditions),
            ) => c)
            ---------------------------------------------------- ("candidate")
            (prove_via_impl(
                decls,
                env,
                assumptions,
                requested_trait_ref,
                candidate,
            ) => Constrained(
                ProvedImpl::new(&candidate.id, c, impl_variables, trait_impl),
                c,
            ))
        )
    }
}

#[cfg(test)]
mod tests {
    use super::{prove_via_impl, ProvedImpl};
    use crate::grammar::{Crates, Parameter, ParameterKind, TraitId, TraitRef, Variable, Wcs};
    use crate::prove::prove::decls::ImplCandidate;
    use crate::prove::prove::{Constrained, Constraints, Env, Program};
    use crate::rust::term;

    fn program(source: &str) -> Program {
        term::<Crates>(source).to_prove_decls()
    }

    fn basic_program() -> Program {
        program(
            "[
                crate test {
                    trait Foo {}
                    impl Foo for u32 {}
                    impl Foo for i32 {}
                }
            ]",
        )
    }

    fn apply_candidate(
        program: &Program,
        env: Env,
        requested_trait_ref: TraitRef,
        candidate: &ImplCandidate,
    ) -> Option<(ProvedImpl, Constraints)> {
        prove_via_impl(program, env, Wcs::t(), requested_trait_ref, candidate)
            .into_singleton()
            .ok()
            .map(|(Constrained(application, constraints), _)| (application, constraints))
    }

    #[test]
    fn matching_ground_impl_succeeds() {
        let program = basic_program();
        let candidates = program.raw_trait_impls_for(&term("Foo"));

        assert!(apply_candidate(
            &program,
            Env::default(),
            term::<TraitRef>("u32: Foo"),
            &candidates[0],
        )
        .is_some());
    }

    #[test]
    fn wrong_candidate_does_not_search_for_a_replacement() {
        let program = basic_program();
        let candidates = program.raw_trait_impls_for(&term("Foo"));

        assert!(apply_candidate(
            &program,
            Env::default(),
            term::<TraitRef>("u32: Foo"),
            &candidates[1],
        )
        .is_none());
    }

    #[test]
    fn candidate_for_a_different_trait_is_rejected() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    trait Bar {}
                    impl Bar for u32 {}
                }
            ]",
        );
        let candidates = program.raw_trait_impls_for(&term("Bar"));

        assert!(
            apply_candidate(&program, Env::default(), term("u32: Foo"), &candidates[0],).is_none()
        );
    }

    #[test]
    fn structurally_identical_impls_retain_distinct_source_ids() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    impl Foo for u32 {}
                    impl Foo for u32 {}
                }
            ]",
        );
        let candidates = program.raw_trait_impls_for(&term("Foo"));
        assert_eq!(candidates.len(), 2);
        assert_eq!(candidates[0].trait_impl, candidates[1].trait_impl);
        assert_ne!(candidates[0].id, candidates[1].id);

        let (first, _) =
            apply_candidate(&program, Env::default(), term("u32: Foo"), &candidates[0]).unwrap();
        let (second, _) =
            apply_candidate(&program, Env::default(), term("u32: Foo"), &candidates[1]).unwrap();
        assert_ne!(first.impl_id, second.impl_id);
    }

    #[test]
    fn matching_header_with_unsatisfied_where_clause_fails() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    trait Marker {}
                    impl<T> Foo for T where T: Marker {}
                }
            ]",
        );
        let candidates = program.raw_trait_impls_for(&term("Foo"));

        assert!(
            apply_candidate(&program, Env::default(), term("u32: Foo"), &candidates[0],).is_none()
        );
    }

    #[test]
    fn generic_impl_arguments_are_inferred_in_binder_order() {
        let program = program(
            "[
                crate test {
                    trait Pair<A> {}
                    struct Wrapper<T, U> {}
                    impl<T, U> Pair<U> for Wrapper<T, U> {}
                }
            ]",
        );
        let candidates = program.raw_trait_impls_for(&term("Pair"));
        let (application, _) = apply_candidate(
            &program,
            Env::default(),
            term("Wrapper<u32, i32>: Pair<i32>"),
            &candidates[0],
        )
        .unwrap();

        assert_eq!(
            application.impl_substitution,
            vec![term::<Parameter>("u32"), term::<Parameter>("i32")],
        );
    }

    #[test]
    fn trait_parameters_are_not_reported_as_impl_arguments() {
        let program = program(
            "[
                crate test {
                    trait Project<A> {}
                    impl<T> Project<i32> for T {}
                }
            ]",
        );
        let candidates = program.raw_trait_impls_for(&term("Project"));
        let (application, _) = apply_candidate(
            &program,
            Env::default(),
            term("u32: Project<i32>"),
            &candidates[0],
        )
        .unwrap();

        assert_eq!(
            application.impl_substitution,
            vec![term::<Parameter>("u32")],
        );
    }

    #[test]
    fn where_clause_constraints_specialize_impl_arguments() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    trait Witness<T> {}
                    impl Witness<i32> for () {}
                    impl<T> Foo for u32 where (): Witness<T> {}
                }
            ]",
        );
        let candidates = program.raw_trait_impls_for(&term("Foo"));
        let (application, _) =
            apply_candidate(&program, Env::default(), term("u32: Foo"), &candidates[0]).unwrap();

        assert_eq!(
            application.impl_substitution,
            vec![term::<Parameter>("i32")],
        );
    }

    #[test]
    fn proof_constraints_pop_impl_variables() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    impl<T> Foo for T {}
                }
            ]",
        );
        let candidates = program.raw_trait_impls_for(&term("Foo"));
        let (application, constraints) =
            apply_candidate(&program, Env::default(), term("u32: Foo"), &candidates[0]).unwrap();

        assert_ne!(constraints.env(), &Env::default());
        assert_eq!(
            constraints.pop_subst(&application.impl_variables).env(),
            &Env::default()
        );
    }

    #[test]
    fn caller_existential_constraint_survives_impl_pop() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    trait Witness<T> {}
                    struct Wrapper<T> {}
                    impl Witness<i32> for () {}
                    impl<T> Foo for Wrapper<T> where (): Witness<T> {}
                }
            ]",
        );
        let candidates = program.raw_trait_impls_for(&term("Foo"));
        let mut env = Env::default();
        let caller_variable = env.fresh_existential(ParameterKind::Ty);
        let requested = term::<TraitId>("Foo").with(&caller_variable, ());
        let (application, constraints) =
            apply_candidate(&program, env, requested, &candidates[0]).unwrap();
        let proof_constraints = constraints.pop_subst(&application.impl_variables);

        assert_eq!(
            proof_constraints
                .substitution()
                .get(Variable::ExistentialVar(caller_variable)),
            Some(term::<Parameter>("Wrapper<i32>")),
        );
    }

    #[test]
    fn unconstrained_impl_argument_remains_visible() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    impl<T> Foo for u32 {}
                }
            ]",
        );
        let candidates = program.raw_trait_impls_for(&term("Foo"));
        let (application, _) =
            apply_candidate(&program, Env::default(), term("u32: Foo"), &candidates[0]).unwrap();
        let arguments = application.impl_substitution;

        assert_eq!(arguments.len(), 1);
        assert!(arguments[0].is_variable());
    }

    #[test]
    fn direct_coinductive_impl_retains_existing_outcome() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    impl<T> Foo for T where T: Foo {}
                }
            ]",
        );
        let candidates = program.raw_trait_impls_for(&term("Foo"));

        assert!(
            apply_candidate(&program, Env::default(), term("u32: Foo"), &candidates[0],).is_some()
        );
    }

    #[test]
    fn mutual_coinductive_impls_retain_existing_outcome() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    trait Bar {}
                    impl<T> Foo for T where T: Bar {}
                    impl<T> Bar for T where T: Foo {}
                }
            ]",
        );
        let candidates = program.raw_trait_impls_for(&term("Foo"));

        assert!(
            apply_candidate(&program, Env::default(), term("u32: Foo"), &candidates[0],).is_some()
        );
    }
}
