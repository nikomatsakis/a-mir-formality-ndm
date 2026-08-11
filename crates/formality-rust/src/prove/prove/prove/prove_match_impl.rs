use crate::grammar::{ExistentialVar, Parameter, TraitImpl, TraitImplBoundData, TraitRef, Wcs};
use crate::prove::prove::decls::{ImplCandidate, Program};
use crate::prove::prove::prove::{prove, prove_impl_wf};
use crate::prove::prove::{Constrained, Constraints, Env};
use formality_core::{judgment_fn, To};

/// An impl whose binder has been opened and whose header matches a requested trait-ref.
///
/// This is not yet an applicable impl: the caller must still prove the where-clauses in
/// [`trait_impl`](Self::trait_impl). The stored fields have the substitution derived by header
/// matching applied to them, but they may still contain fresh existential variables. Those
/// variables remain in the returned constraints and are expected to become fully constrained only
/// after the where-clauses have been solved.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct MatchedImpl {
    pub(crate) impl_variables: Vec<ExistentialVar>,
    /// Impl binder arguments after applying the substitution derived by header matching thus far.
    ///
    /// These arguments may still contain fresh existential variables that residual where-clauses
    /// will constrain.
    pub(crate) impl_substitution: Vec<Parameter>,
    /// The opened impl after applying the substitution derived by header matching thus far.
    ///
    /// Its fields may still contain fresh existential variables that residual where-clauses will
    /// constrain.
    pub(crate) trait_impl: TraitImplBoundData,
}

formality_core::cast_impl!(MatchedImpl);

// Operations on an opened, candidate-local impl.

impl MatchedImpl {
    pub(crate) fn new(
        constraints: &Constraints,
        impl_variables: &[ExistentialVar],
        trait_impl: &TraitImplBoundData,
    ) -> Self {
        Self {
            impl_variables: impl_variables.to_vec(),
            impl_substitution: constraints
                .substitution()
                .apply(impl_variables.to::<Vec<Parameter>>()),
            trait_impl: constraints.substitution().apply(trait_impl),
        }
    }
}

judgment_fn! {
    /// Open `candidate` with fresh existential variables and match its header against
    /// `requested_trait_ref`.
    ///
    /// Matching uses only the caller's assumptions; it does not introduce either a recursive
    /// `Later` handle or completed evidence for the requested trait-ref. The caller remains
    /// responsible for proving the returned impl's where-clauses in the appropriate context. The
    /// returned [`Constrained`] value carries both the environment extended with the impl variables
    /// and the substitution learned by matching; [`MatchedImpl`] carries the opened impl body those
    /// constraints apply to.
    pub(crate) fn match_impl_candidate(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        requested_trait_ref: TraitRef,
        candidate: ImplCandidate,
    ) => Constrained<MatchedImpl> {
        debug(requested_trait_ref, candidate, assumptions, env)

        (
            (if candidate_impl.trait_id() == requested_trait_id)
            (prove_impl_wf(decls, candidate_impl) => ())
            (let (env, impl_variables) =
                env.existential_substitution(candidate_binder))
            (let trait_impl = candidate_binder.instantiate_with(impl_variables)?)
            (let TraitRef { parameters: impl_parameters, .. } = trait_impl.trait_ref())
            (prove(
                decls,
                env,
                assumptions,
                Wcs::all_eq(requested_parameters, impl_parameters),
            ) => c)
            ----------------------------- ("match impl candidate")
            (match_impl_candidate(
                decls,
                env,
                assumptions,
                TraitRef {
                    trait_id: requested_trait_id,
                    parameters: requested_parameters,
                },
                ImplCandidate {
                    id: _,
                    trait_impl: candidate_impl @ TraitImpl {
                        binder: candidate_binder,
                        safety: _,
                    },
                },
            ) => Constrained(MatchedImpl::new(c, impl_variables, trait_impl), c))
        )
    }
}
