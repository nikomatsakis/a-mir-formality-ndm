use crate::grammar::{ExistentialVar, Parameter, TraitImplBoundData, TraitRef, Wc, Wcs};
use crate::prove::prove::decls::{ImplCandidate, ImplId, Program};
use crate::prove::prove::prove::{impl_contract, match_impl_candidate, prove_after};
use crate::prove::prove::{partial, Constrained, Constraints, Env};
use formality_core::{judgment_fn, To};

use super::prove_match_impl::MatchedImpl;

/// A successful application of one particular impl declaration.
///
/// The impl has been opened, matched against the requested trait-ref, and had its where-clauses
/// proven. Its fields have the final substitution learned during that proof applied to them. They
/// can still mention caller variables. A definite result cannot mention the candidate's fresh
/// variables; an ambiguous result may retain them solely to identify the unresolved application.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct ProvedImpl {
    pub(crate) impl_id: ImplId,

    /// Impl binder arguments after applying all constraints learned while proving this impl.
    pub(crate) impl_substitution: Vec<Parameter>,

    /// The opened impl after applying all constraints learned while proving this impl.
    pub(crate) trait_impl: TraitImplBoundData,
}

formality_core::cast_impl!(ProvedImpl);

impl ProvedImpl {
    fn new(
        impl_id: &ImplId,
        constraints: &Constraints,
        impl_variables: &[ExistentialVar],
        trait_impl: &TraitImplBoundData,
    ) -> Self {
        Self {
            impl_id: *impl_id,
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
                (assumptions, Wc::later(requested_trait_ref)),
                requested_trait_ref,
                candidate,
            ) => Constrained(
                MatchedImpl { impl_variables, trait_impl, .. },
                c,
            ))!
            (impl_contract(trait_impl) => (
                TraitRef {
                    trait_id: impl_trait_id,
                    parameters: _,
                },
                conditions,
                definitions,
            ))
            (partial(
                decls,
                impl_trait_id,
                conditions,
            ) => partial_conditions)

            // `prove_impl_wf` certifies a constructor
            //
            //     Partial[ImplTrait](conditions) -> Implemented(impl_header).
            //
            // `Partial` is the same translation here as in impl well-formedness. If the
            // application-scoped `Later(requested_trait_ref)` handle establishes that translated
            // input contract, the checked constructor produces the completed dictionary.
            (prove_after(
                decls,
                c,
                (
                    assumptions,
                    Wc::later(requested_trait_ref),
                    definitions,
                ),
                partial_conditions,
            ) => c)

            // Snapshot everything learned about the impl before removing its fresh variables.
            // Rust's constrained-impl-parameter rules guarantee that none of those variables can
            // escape through the completed application. a-mir-formality does not yet enforce
            // those rules; see https://github.com/rust-lang/a-mir-formality/issues/57.
            (let proved_impl @ ProvedImpl {
                impl_substitution: final_impl_substitution,
                trait_impl: final_trait_impl,
                ..
            } = ProvedImpl::new(&candidate.id, c, impl_variables, trait_impl))
            (let c = c.pop_subst(impl_variables))
            (assert c.env().encloses(c.substitution()))
            // An ambiguous recursive answer may not determine every impl argument. Its application
            // is retained only so consumers can report the ambiguity; it cannot be selected as
            // evidence. Every answer claimed to be true must be entirely caller-scoped.
            (assert !c.known_true ||
                c.env().encloses((final_impl_substitution, final_trait_impl)))
            ---------------------------------------------------- ("candidate")
            (prove_via_impl(
                decls,
                env,
                assumptions,
                requested_trait_ref,
                candidate,
            ) => Constrained(proved_impl, c))
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
    fn returned_constraints_exclude_impl_variables() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    impl<T> Foo for T {}
                }
            ]",
        );
        let candidates = program.raw_trait_impls_for(&term("Foo"));
        let (_, constraints) =
            apply_candidate(&program, Env::default(), term("u32: Foo"), &candidates[0]).unwrap();

        assert_eq!(constraints.env(), &Env::default());
    }

    #[test]
    fn caller_existential_constraint_survives_candidate_pop() {
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
        let (_, constraints) = apply_candidate(&program, env, requested, &candidates[0]).unwrap();

        assert_eq!(
            constraints
                .substitution()
                .get(Variable::ExistentialVar(caller_variable)),
            Some(term::<Parameter>("Wrapper<i32>")),
        );
    }

    #[test]
    #[should_panic]
    fn unconstrained_impl_argument_cannot_escape() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    impl<T> Foo for u32 {}
                }
            ]",
        );
        let candidates = program.raw_trait_impls_for(&term("Foo"));
        apply_candidate(&program, Env::default(), term("u32: Foo"), &candidates[0]);
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
