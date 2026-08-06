use crate::grammar::{ExistentialVar, Parameter, TraitImpl, TraitRef, Upto, Wc, Wcs};
use crate::prove::prove::decls::{ImplCandidate, ImplId, Program};
use crate::prove::prove::prove::{match_impl_candidate, prove_after};
use crate::prove::prove::{Constrained, Constraints, Env};
use crate::prove::ToWcs;
use formality_core::{judgment_fn, Upcast};

/// A successful application of one particular impl declaration.
/// It retains the source impl identity and its inferred binder variables.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct ImplApplication {
    pub(crate) impl_id: ImplId,
    pub(crate) trait_impl: TraitImpl,
    pub(crate) impl_variables: Vec<ExistentialVar>,
}

formality_core::cast_impl!(ImplApplication);

impl ImplApplication {
    fn new(candidate: &ImplCandidate, impl_variables: &[ExistentialVar]) -> Self {
        Self {
            impl_id: candidate.id,
            trait_impl: (&candidate.trait_impl).upcast(),
            impl_variables: impl_variables.upcast(),
        }
    }

    /// Infer the impl binder arguments while the impl variables are still in
    /// scope and represented in `constraints`.
    pub(crate) fn inferred_impl_arguments(&self, constraints: &Constraints) -> Vec<Parameter> {
        self.impl_variables
            .iter()
            .map(|variable| {
                let parameter: Parameter = variable.upcast();
                constraints.substitution().apply(parameter)
            })
            .collect()
    }

    /// Restore the caller's proof environment after applying this impl.
    pub(crate) fn proof_constraints(&self, constraints: &Constraints) -> Constraints {
        constraints.pop_subst(&self.impl_variables)
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
    ) => Constrained<ImplApplication> {
        debug(requested_trait_ref, candidate, assumptions, env)

        (
            // Löb induction begins with an opaque handle to the dictionary being constructed.
            // Header matching may close an exact recursive occurrence with this handle, but it
            // cannot project supertraits or associated-type bounds from it. After matching, the
            // residual obligations below receive the candidate's independently validated
            // `Supertraits` view instead.
            (let recursive_assumption =
                Wc::validate(Upto::Zero, requested_trait_ref))
            (match_impl_candidate(
                decls,
                env,
                (assumptions, &recursive_assumption),
                requested_trait_ref,
                candidate,
            ) => Constrained(matched, c))!
            (let trait_impl = matched.trait_impl(c))
            (let impl_where_clauses = trait_impl.where_clauses.to_wcs())

            // A well-formed impl is a dictionary constructor from validated inputs to ordinary,
            // completed `Implemented` evidence. Selecting the impl fixes its associated-type
            // values, so its header is available at the supertrait frontier within this branch.
            // That view still cannot expose the root dictionary's own supertrait or associated-
            // bound fields.
            //
            // FIXME(ndm): It seems to me that this logic can be "extracted and shared" somehow
            // between impl WF checking and this code here. We are basically constructing the
            // "inputs" to the impl's implication.
            (let validation = Upto::supertraits(&trait_impl.trait_id))
            (let provisional_impl_header =
                Wc::validate(validation.clone(), trait_impl.trait_ref()))
            (prove_after(
                decls,
                c,
                (assumptions, provisional_impl_header),
                Wcs::validate(validation, impl_where_clauses),
            ) => c)
            ---------------------------------------------------- ("candidate")
            (prove_via_impl(
                decls,
                env,
                assumptions,
                requested_trait_ref,
                candidate,
            ) => Constrained(
                ImplApplication::new(
                    candidate,
                    &matched.impl_variables,
                ),
                c,
            ))
        )
    }
}

#[cfg(test)]
mod tests {
    use super::{prove_via_impl, ImplApplication};
    use crate::grammar::{Crates, Parameter, ParameterKind, TraitId, TraitRef, Wcs};
    use crate::prove::prove::decls::ImplCandidate;
    use crate::prove::prove::{Constrained, Constraints, Env, Program};
    use crate::rust::term;
    use formality_core::Upcast;

    fn program(source: &str) -> Program {
        let crates: Crates = term(source);
        crates.to_prove_decls()
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
    ) -> Option<(ImplApplication, Constraints)> {
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
            term::<TraitRef>("Foo(u32)"),
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
            term::<TraitRef>("Foo(u32)"),
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
            apply_candidate(&program, Env::default(), term("Foo(u32)"), &candidates[0],).is_none()
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
            apply_candidate(&program, Env::default(), term("Foo(u32)"), &candidates[0]).unwrap();
        let (second, _) =
            apply_candidate(&program, Env::default(), term("Foo(u32)"), &candidates[1]).unwrap();
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
            apply_candidate(&program, Env::default(), term("Foo(u32)"), &candidates[0],).is_none()
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
        let (application, constraints) = apply_candidate(
            &program,
            Env::default(),
            term("Pair(Wrapper<u32, i32>, i32)"),
            &candidates[0],
        )
        .unwrap();

        assert_eq!(
            application.inferred_impl_arguments(&constraints),
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
        let (application, constraints) = apply_candidate(
            &program,
            Env::default(),
            term("Project(u32, i32)"),
            &candidates[0],
        )
        .unwrap();

        assert_eq!(
            application.inferred_impl_arguments(&constraints),
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
        let (application, constraints) =
            apply_candidate(&program, Env::default(), term("Foo(u32)"), &candidates[0]).unwrap();

        assert_eq!(
            application.inferred_impl_arguments(&constraints),
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
            apply_candidate(&program, Env::default(), term("Foo(u32)"), &candidates[0]).unwrap();

        assert_ne!(constraints.env(), &Env::default());
        assert_eq!(
            application.proof_constraints(&constraints).env(),
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
        let proof_constraints = application.proof_constraints(&constraints);

        assert_eq!(
            proof_constraints
                .substitution()
                .get(caller_variable.upcast()),
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
        let (application, constraints) =
            apply_candidate(&program, Env::default(), term("Foo(u32)"), &candidates[0]).unwrap();
        let arguments = application.inferred_impl_arguments(&constraints);

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
            apply_candidate(&program, Env::default(), term("Foo(u32)"), &candidates[0],).is_some()
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
            apply_candidate(&program, Env::default(), term("Foo(u32)"), &candidates[0],).is_some()
        );
    }
}
