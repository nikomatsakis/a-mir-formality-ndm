use anyhow::bail;
use fn_error_context::context;
use formality_macros::term;
use formality_prove::{prove, Constraints, Decls, Env};
use formality_rust::{
    grammar::{Crate, NegTraitImpl, TraitImpl},
    prove::ToWcs,
};
use formality_types::{
    cast::{Downcasted, Upcast},
    collections::Set,
    grammar::{Binder, Fallible, Goal, Parameter, TraitRef, Variable, Wc, Wcs},
    set,
};
use itertools::Itertools;

use crate::Check;

impl Check<'_> {
    pub(crate) fn check_coherence(&self, current_crate: &Crate) -> Fallible<()> {
        let all_crate_impls: Vec<TraitImpl> =
            self.program.items_from_all_crates().downcasted().collect();
        let current_crate_impls: Vec<TraitImpl> = current_crate.items.iter().downcasted().collect();
        let current_crate_neg_impls: Vec<NegTraitImpl> =
            current_crate.items.iter().downcasted().collect();

        for impl_a in &current_crate_impls {
            self.orphan_check(impl_a)?;
        }

        for impl_a in &current_crate_neg_impls {
            self.orphan_check_neg(impl_a)?;
        }

        // check for duplicate impls in the current crate
        for (impl_a, i) in current_crate_impls.iter().zip(0..) {
            if current_crate_impls[i + 1..].contains(impl_a) {
                bail!("duplicate impl in current crate: {:?}", impl_a)
            }
        }

        // check each impl in current crate against impls in all other crates
        for (impl_a, impl_b) in current_crate_impls
            .iter()
            .cartesian_product(&all_crate_impls)
            .filter(|(impl_a, impl_b)| impl_a != impl_b)
            .filter(|(impl_a, impl_b)| impl_a.trait_id() == impl_b.trait_id())
        {
            self.overlap_check(impl_a, impl_b)?;
        }

        Ok(())
    }

    #[context("orphan_check({impl_a:?})")]
    fn orphan_check(&self, impl_a: &TraitImpl) -> Fallible<()> {
        let mut env = Env::default();

        let a = env.instantiate_universally(&impl_a.binder);
        let trait_ref = a.trait_ref();

        self.prove_goal(
            &env.with_coherence_mode(true),
            &a.where_clauses,
            trait_ref.is_local(),
        )
    }

    #[context("orphan_check_neg({impl_a:?})")]
    fn orphan_check_neg(&self, impl_a: &NegTraitImpl) -> Fallible<()> {
        let mut env = Env::default();

        let a = env.instantiate_universally(&impl_a.binder);
        let trait_ref = a.trait_ref();

        self.prove_goal(
            &env.with_coherence_mode(true),
            &a.where_clauses,
            trait_ref.is_local(),
        )
    }

    #[tracing::instrument(level = "Debug", skip(self))]
    fn overlap_check(&self, impl_a: &TraitImpl, impl_b: &TraitImpl) -> Fallible<()> {
        // "Classic" coherence check. Determine whether the impl headers can be equated
        // and, if so, whether any of the where-clauses are provably not implemented.
        if let Ok(()) = self.prove_not_equatable(impl_a, impl_b) {
            return Ok(());
        }

        // "Enhanced" coherence check. If, for all possible intersections of the impls,
        // we find that there is a contradiction, then we're good.
        if impl_intersections(&self.decls, impl_a, impl_b)
            .iter()
            .all(|intersection| {
                self.prove_contradiction(impl_a, impl_b, intersection)
                    .is_ok()
            })
        {
            return Ok(());
        }

        bail!("impls may overlap:\n{impl_a:?}\n{impl_b:?}")
    }

    /// Classic coherence check: Proves that two impls cannot be equated, either because their input types cannot be
    /// equated or because -- once they *are* equated -- one of their where-clauses is provably false.
    /// This proof is done in "coherence mode", which means that we assume that impls may be added in the future for types
    /// from upstream crates and that there may be downstream (or cousin) crates we don't know about.
    #[tracing::instrument(level = "Debug", skip(self))]
    fn prove_not_equatable(&self, impl_a: &TraitImpl, impl_b: &TraitImpl) -> Fallible<()> {
        let mut env = Env::default();

        // Example:
        //
        // Given two impls...
        //
        //   impl<P_a..> SomeTrait<T_a...> for T_a0 where Wc_a { }
        //   impl<P_b..> SomeTrait<T_b...> for T_b0 where Wc_b { }

        // ∀P_a, ∀P_b....
        let a = env.instantiate_universally(&impl_a.binder);
        let b = env.instantiate_universally(&impl_b.binder);

        // ...get the trait refs from each impl...
        let trait_ref_a = a.trait_ref();
        let trait_ref_b = b.trait_ref();

        assert_eq!(trait_ref_a.trait_id, trait_ref_b.trait_id);

        // If we can prove that the parameters cannot be equated *or* the where-clauses don't hold,
        // in coherence mode, then they do not overlap.
        //
        // ∀P_a, ∀P_b. ⌐ (coherence_mode => (Ts_a = Ts_b && WC_a && WC_b))
        let () = self.prove_not_goal(
            &env.with_coherence_mode(true),
            (),
            (
                Wcs::all_eq(&trait_ref_a.parameters, &trait_ref_b.parameters),
                &a.where_clauses,
                &b.where_clauses,
            ),
        )?;

        tracing::debug!(
            "proved not {:?}",
            (
                Wcs::all_eq(&trait_ref_a.parameters, &trait_ref_b.parameters),
                &a.where_clauses,
                &b.where_clauses,
            )
        );

        Ok(())
    }

    /// Given two impls and a potential intersection for them, see if we can prove that there is a contradiction.
    /// A contradiction exists when one of the impls has a where clause `T: Trait` but we can prove `T: !Trait` (i.e.,
    /// there exists a negative impl for one of the where-clauses that would have to be true).
    #[tracing::instrument(level = "Debug", skip(self))]
    fn prove_contradiction(
        &self,
        impl_a: &TraitImpl,
        impl_b: &TraitImpl,
        intersection: &Binder<ImplIntersection>,
    ) -> Fallible<()> {
        let mut env = Env::default();

        // We want to prove that the contradiction exists for ALL instantiations of the intersection.
        let ImplIntersection {
            substitution_a,
            substitution_b,
            assumptions,
            known_true,
        } = env.instantiate_universally(&intersection);

        // If we don't know that the intersection is true,
        // then we are not going to be able to prove the inverted
        // goals definitively, so that's not good enough.
        if !known_true {
            bail!("intersection not known to be true");
        }

        // Extract the impl headers.
        let (trait_ref_a, wc_a) = impl_a
            .binder
            .instantiate_with(&substitution_a)?
            .into_header();
        let (trait_ref_b, wc_b) = impl_b
            .binder
            .instantiate_with(&substitution_b)?
            .into_header();

        // Invert the where-clauses -- so `T: Trait` becomes `T: !Trait`.
        let inverted: Vec<Wc> = wc_a
            .iter()
            .chain(&wc_b)
            .flat_map(|wc| wc.invert())
            .collect();

        // Search for a single inverted goal that holds in the intersection.
        // This is a "positive check" -- we are searching for a path provable today,
        // so we don't have to do it in coherence mode, as it only has to be sound (not complete).
        let provable_inverted_goal = inverted
            .iter()
            .find(|inverted_wc| self.prove_goal(&env, &assumptions, inverted_wc).is_ok());

        if let Some(inverted_wc) = provable_inverted_goal {
            tracing::debug!(
                "proved {:?} assuming {:?}",
                &inverted_wc,
                (
                    Wcs::all_eq(&trait_ref_a.parameters, &trait_ref_b.parameters),
                    &wc_a,
                    &wc_b,
                )
            );

            return Ok(());
        }

        bail!("no contradiction found")
    }
}

/// Result of [`assume_eq`][].
#[term]
pub struct ImplIntersection {
    pub substitution_a: Vec<Parameter>,
    pub substitution_b: Vec<Parameter>,
    pub assumptions: Wcs,
    pub known_true: bool,
}

/// Computes the *intersections* of two impls -- meaning the sets of types
/// for which both impls hold (and the assumptions we know about those types).
/// To represent each intersection, we return a `Binder<ImplIntersection>`:
/// * The binder represents fresh universally quantified variables (e.g., the variable `A` in the previous example).
/// * The `ImplIntersection` stores the substitutions for each impl (e.g., `T = Vec<A>` and `U = A`) along with any where-clauses that can be added to the environment.
///
/// # Example 0: no intersection
///
/// Given...
///
/// ```rust
/// # trait Foo { }
/// impl Foo for u32 {}
/// impl Foo for i32 {}
/// ```
///
/// ...this function would return an empty set, as those two impls cannot intersect.
///
/// # Example 1: one intersection
///
/// The following impls...
///
/// ```rust,compile_fail
/// # trait Foo { }
/// # struct Set<T> { t: Vec<T> }
/// impl<T> Foo for Vec<T> {}
/// impl<U> Foo for Vec<Set<U>> {}
/// ```
///
/// ...intersect when, for some type `A: Sized`, `T = Set<A>` and `U = A`.
///
/// We would therefore return a single `Binder<ImplItersection>` that binds
/// a single type variable to represent `A` and....
///
/// * `substitution_a` equal to `T = Set<^0>` (`^0` is the bound type variable);
/// * `substitution_b` equal to `T = ^0` (`^0` is the bound type variable);
/// * `assumptions` equal to `Set<^0>: Sized` and `^0: Sized` (these are the where-clauses from the impls, with substitutions applied).
pub fn impl_intersections(
    decls: &Decls,
    impl_a: &TraitImpl,
    impl_b: &TraitImpl,
) -> Set<Binder<ImplIntersection>> {
    // We are searching for any *possible* intersection, so be sure to do this search in coherence mode.
    let mut env = Env::default().with_coherence_mode(true);

    // The procedure here is inspired by "let generalization" in ML or Haskell.
    // Let's walk through how it works, using Example 1 above.
    //
    // First, we start by instantiating each impl with existential variables
    // for its generics, giving us...
    //
    // ```
    // impl Foo for Vec<?T> where ?T: Sized { }
    // impl Foo for Vec<Set<?U>> where ?U: Sized { }
    // ```
    let (a_substitution, b_substitution);
    (env, a_substitution) = env.existential_substitution(&impl_a.binder);
    (env, b_substitution) = env.existential_substitution(&impl_b.binder);
    let (
        TraitRef {
            trait_id: trait_id_a,
            parameters: parameters_a,
        },
        wcs_a,
    ) = impl_a
        .binder
        .instantiate_with(&a_substitution)
        .unwrap()
        .into_header();
    let (
        TraitRef {
            trait_id: trait_id_b,
            parameters: parameters_b,
        },
        wcs_b,
    ) = impl_b
        .binder
        .instantiate_with(&b_substitution)
        .unwrap()
        .into_header();

    // Distinct traits? No intersection.
    if trait_id_a != trait_id_b {
        return set![];
    }

    let a_substitution: Vec<Parameter> = a_substitution.upcast();
    let b_substitution: Vec<Parameter> = b_substitution.upcast();
    let all_wcs = (wcs_a, wcs_b).to_wcs();

    // Second, we equate the impl header types. In this case, that means we prove the goal:
    //
    // ```
    // Vec<?T> = Vec<Set<?U>>
    // ```
    prove(
        decls,
        env,
        &all_wcs,
        Goal::all_eq(&parameters_a, &parameters_b),
    )
    .into_iter()
    .map(
        |Constraints {
             env,
             known_true,
             substitution,
         }| {
            // This will yield a set of `Constraints` with one member:
            //
            // ```
            // Constraints = {
            //     env: { ?T, ?U },
            //     substitution: {?T => Set<?U>}
            // }
            // ```
            //
            // Third, we identify the variables in the environment that
            // are NOT mapped by the substitution. In this case, that is `[?U]`.
            //
            // For the final step, we will create a binder from each of the unbound
            // variables, yielding `for<a> { ... }`.
            let unbound_vars: Vec<Variable> = env
                .variables()
                .iter()
                .copied()
                .filter(|&v| !substitution.maps(v))
                .collect();

            Binder::new(
                &unbound_vars,
                ImplIntersection {
                    substitution_a: substitution.apply(&a_substitution),
                    substitution_b: substitution.apply(&b_substitution),
                    assumptions: substitution.apply(&all_wcs),
                    known_true,
                },
            )
        },
    )
    .collect()
}
