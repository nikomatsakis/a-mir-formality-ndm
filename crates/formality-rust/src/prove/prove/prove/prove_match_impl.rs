use crate::grammar::{ExistentialVar, TraitImplBoundData, TraitRef, ValidationContext, Wc, Wcs};
use crate::prove::prove::decls::{ImplCandidate, Program};
use crate::prove::prove::prove::{prove, prove_impl_wf};
use crate::prove::prove::{Constrained, Constraints, Env};
use formality_core::judgment_fn;

/// An impl whose binder has been opened and whose header matches a requested trait-ref.
///
/// This is not yet an applicable impl: the caller must still prove the where-clauses in
/// [`trait_impl`](Self::trait_impl). The fresh impl variables remain in the returned constraints
/// until that proof succeeds, so later obligations can infer impl arguments that do not occur in
/// the header.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct MatchedImpl {
    pub(crate) impl_variables: Vec<ExistentialVar>,
    trait_impl: TraitImplBoundData,
}

formality_core::cast_impl!(MatchedImpl);

/// How the caller wants the impl header equality to be established.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) enum ImplMatchMode {
    Ordinary,
    Validated(ValidationContext),
}

formality_core::cast_impl!(ImplMatchMode);

/// Remove provisional validation evidence from an ordinary proof context.
pub(crate) fn ordinary_assumptions(assumptions: &Wcs) -> Wcs {
    assumptions
        .iter()
        .filter(|assumption| !matches!(assumption, Wc::Validate(_, _)))
        .collect()
}

impl MatchedImpl {
    fn new(impl_variables: &[ExistentialVar], trait_impl: &TraitImplBoundData) -> Self {
        Self {
            impl_variables: impl_variables.to_vec(),
            trait_impl: trait_impl.clone(),
        }
    }

    /// Return the opened impl after applying everything inferred so far.
    ///
    /// Call this again after proving the impl's where-clauses: those clauses may constrain impl
    /// parameters that did not occur in the header.
    pub(crate) fn trait_impl(&self, constraints: &Constraints) -> TraitImplBoundData {
        constraints.substitution().apply(self.trait_impl.clone())
    }

    /// Remove the fresh impl variables after all caller-specific obligations have succeeded.
    pub(crate) fn pop_constraints(&self, constraints: &Constraints) -> Constraints {
        constraints.pop_subst(&self.impl_variables)
    }
}

fn header_equality(mode: &ImplMatchMode, requested: &TraitRef, impl_trait_ref: &TraitRef) -> Wcs {
    let equality = Wcs::all_eq(&requested.parameters, &impl_trait_ref.parameters);
    match mode {
        ImplMatchMode::Ordinary => equality,
        ImplMatchMode::Validated(validation) => equality.validated(validation),
    }
}

judgment_fn! {
    /// Open `candidate` with fresh existential variables and match its header against
    /// `requested_trait_ref`.
    ///
    /// `mode` controls only how header equality is proven. The caller supplies the exact
    /// assumptions for matching and remains responsible for proving the returned impl's
    /// where-clauses in the appropriate context. The returned [`Constrained`] value carries both
    /// the environment extended with the impl variables and the substitution learned by matching;
    /// [`MatchedImpl`] carries the opened impl body those constraints apply to.
    pub(crate) fn match_impl_candidate(
        _decls: Program,
        env: Env,
        match_assumptions: Wcs,
        requested_trait_ref: TraitRef,
        candidate: ImplCandidate,
        mode: ImplMatchMode,
    ) => Constrained<MatchedImpl> {
        debug(requested_trait_ref, candidate, mode, match_assumptions, env)

        (
            (if candidate.trait_impl.trait_id() == &requested_trait_ref.trait_id)
            (prove_impl_wf(decls, &candidate.trait_impl) => ())
            (let (env, impl_variables) =
                env.existential_substitution(&candidate.trait_impl.binder))
            (let trait_impl = candidate
                .trait_impl
                .binder
                .instantiate_with(impl_variables)?)
            (let equality = header_equality(
                mode,
                requested_trait_ref,
                &trait_impl.trait_ref(),
            ))
            (prove(decls, env, match_assumptions, equality) => c)
            (let trait_impl = c.substitution().apply(trait_impl.clone()))
            (let matched = MatchedImpl::new(impl_variables, trait_impl))
            ----------------------------- ("match impl candidate")
            (match_impl_candidate(
                decls,
                env,
                match_assumptions,
                requested_trait_ref,
                candidate,
                mode,
            ) => Constrained(matched, c))
        )
    }
}
