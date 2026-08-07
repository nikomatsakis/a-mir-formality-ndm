use crate::{
    grammar::{
        AliasName, AliasTy, AssociatedItemId, AssociatedTyName, ExistentialVar, Fallible,
        Parameter, Predicate, Relation, RigidTy, TraitImplBoundData, TraitRef, Ty, TyData, Upto,
        Wc, WcData, Wcs,
    },
    prove::prove::{prove::prove_match_impl::MatchedImpl, Constrained},
};
use formality_core::{judgment_fn, Downcast};

use crate::prove::prove::{
    decls::{ImplCandidate, Program},
    prove::{
        combinators::zip, env::Env, prove_after::prove_after, prove_eq::prove_existential_var_eq,
        prove_match_impl::match_impl_candidate,
    },
};

use super::constraints::Constraints;

fn associated_ty_parts<'a>(
    decls: &Program,
    alias: &'a AliasTy,
    item_arity: usize,
) -> Fallible<(&'a [Parameter], TraitRef, Wcs)> {
    let trait_parameter_count =
        alias
            .parameters
            .len()
            .checked_sub(item_arity)
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "associated type alias {:?} has fewer parameters than its item arity",
                    alias,
                )
            })?;
    let (_, gat_parameters) = alias.parameters.split_at(trait_parameter_count);
    let (trait_ref, gat_where_clauses) = decls.associated_ty_requirements(alias)?;
    Ok((gat_parameters, trait_ref, gat_where_clauses))
}

fn associated_ty_value(
    trait_impl: &TraitImplBoundData,
    item_id: &AssociatedItemId,
    gat_parameters: &[Parameter],
) -> Fallible<Ty> {
    let associated_value = trait_impl.assoc_ty_value(item_id).ok_or_else(|| {
        anyhow::anyhow!("impl has no unique value for associated type {item_id:?}")
    })?;
    Ok(associated_value.binder.instantiate_with(gat_parameters)?.ty)
}

judgment_fn! {
    /// Normalize `p` one step using exactly the assumptions supplied by the caller.
    ///
    /// Returns constraints and a semantically equivalent parameter `q`. For example,
    /// `<Vec<T> as IntoIterator>::Item` normalizes to `T`.
    pub fn prove_normalize(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        p: Parameter,
    ) => Constrained<Parameter> {
        debug(p, assumptions, env)

        (
            (a in assumptions)!
            (prove_normalize_via(decls, env, assumptions, a, goal) => c)
            ----------------------------- ("normalize-via-assumption")
            (prove_normalize(decls, env, assumptions, goal) => c)
        )

        (
            (let impl_validation = Upto::gat_bounds(trait_id))
            (candidate in decls.raw_trait_impls_for(trait_id))
            (prove_normalize_via_impl_candidate(
                decls,
                env,
                assumptions,
                a,
                impl_validation,
                candidate,
            ) => normalized)
            ----------------------------- ("normalize-via-impl")
            (prove_normalize(
                decls,
                env,
                assumptions,
                a @ AliasTy {
                    name: AliasName::AssociatedTyId(AssociatedTyName {
                        trait_id,
                        ..
                    }),
                    ..
                },
            ) => normalized)
        )
    }
}

judgment_fn! {
    /// Reveal an associated value for use within a validation proof.
    ///
    /// Unlike [`prove_normalize`], this does not establish that the selected value is well formed
    /// or satisfies its declared bounds. Its result must therefore remain inside the validation
    /// judgment that requested it.
    pub(super) fn prove_normalize_for_validation(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        a: AliasTy,
    ) => Constrained<Parameter> {
        debug(a, assumptions, env)

        (
            (let impl_validation = Upto::supertraits(trait_id))
            (candidate in decls.raw_trait_impls_for(trait_id))
            (prove_normalize_via_impl_candidate(
                decls,
                env,
                assumptions,
                a,
                impl_validation,
                candidate,
            ) => normalized)
            ----------------------------- ("normalize value via impl")
            (prove_normalize_for_validation(
                decls,
                env,
                assumptions,
                a @ AliasTy {
                    name: AliasName::AssociatedTyId(AssociatedTyName {
                        trait_id,
                        ..
                    }),
                    ..
                },
            ) => normalized)
        )
    }
}

judgment_fn! {
    /// Normalize an associated type through an impl whose where-clauses must be available at
    /// `impl_validation`.
    fn prove_normalize_via_impl_candidate(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        a: AliasTy,
        impl_validation: Upto,
        candidate: ImplCandidate,
    ) => Constrained<Parameter> {
        debug(a, impl_validation, candidate, assumptions, env)

        (
            (let (gat_parameters, requested_trait_ref, gat_where_clauses) =
                associated_ty_parts(decls, a, *item_arity)?)

            (match_impl_candidate(
                decls,
                env,
                assumptions,
                requested_trait_ref,
                candidate,
            ) => Constrained(
                matched @ MatchedImpl {
                    trait_impl: trait_impl @ TraitImplBoundData {
                        trait_id: impl_trait_id,
                        where_clauses: impl_where_clauses,
                        ..
                    },
                    ..
                },
                c,
            ))

            // The selected impl fixes its associated value before any dictionaries witnessing
            // that value's bounds exist. Make that equation available only inside this candidate
            // branch, then commit it only after every residual obligation below succeeds.
            (let provisional_ty =
                associated_ty_value(trait_impl, item_id, gat_parameters)?)
            (let provisional_alias_eq = Predicate::alias_eq(a, provisional_ty))

            // Selecting the impl makes its header available at the supertrait frontier. Ordinary
            // normalization passes `GatBounds[ImplTrait]` as `impl_validation`, matching the
            // stronger inputs assumed by the GAT contract checked in `ImplWF`. Value-only
            // normalization passes `Supertraits[ImplTrait]` instead, but its result remains
            // confined to the surrounding validation proof.
            //
            // FIXME: Value-only normalization still requires declaration-side GAT conditions at
            // `GatBounds[ImplTrait]`. Determine whether selecting the value should require those
            // conditions only at an earlier frontier too.
            (let gat_validation = Upto::gat_bounds(impl_trait_id))
            (let provisional_impl_header =
                Upto::supertraits(impl_trait_id).apply_assumption(trait_impl.trait_ref()))
            (prove_after(
                decls,
                c,
                (
                    assumptions,
                    provisional_impl_header,
                    provisional_alias_eq,
                ),
                (
                    impl_validation.apply_goals(impl_where_clauses),
                    gat_validation.apply_goals(gat_where_clauses),
                ),
            ) => c)

            // Where-clauses may have inferred impl parameters absent from the header, so apply the
            // latest substitution before selecting and instantiating the associated value.
            (let ty = c.substitution().apply(provisional_ty))
            (let c = matched.pop_constraints(c))
            // Rust's constrained-impl-parameter rules guarantee that an impl-local variable cannot
            // escape through the associated value after the impl conditions have been proven.
            // a-mir-formality does not enforce those rules yet; see
            // https://github.com/rust-lang/a-mir-formality/issues/57.
            (assert c.env().encloses(ty))
            ----------------------------- ("normalize-via-impl")
            (prove_normalize_via_impl_candidate(
                decls,
                env,
                assumptions,
                a @ AliasTy {
                    name: AliasName::AssociatedTyId(AssociatedTyName {
                        item_id,
                        item_arity,
                        ..
                    }),
                    ..
                },
                impl_validation,
                candidate,
            ) => Constrained(ty, c))
        )
    }
}

judgment_fn! {
    fn prove_normalize_via(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        via: Wc,
        goal: Parameter,
    ) => Constrained<Parameter> {
        debug(goal, via, assumptions, env)

        // An associated-type equality is an oriented normalization witness. Unlike a general
        // equality, it may rewrite only its alias (the left-hand side) to the selected value. In
        // particular, it cannot rewrite that value back to the alias or use an existential in the
        // value as a pattern for an unrelated normalization goal.
        (
            (prove_syntactically_eq(
                decls,
                env,
                assumptions,
                via_alias,
                goal_alias,
            ) => c)
            (let ty = c.substitution().apply(via_ty))
            (let goal = c.substitution().apply(TyData::alias_ty(goal_alias)))
            (if goal != ty)!
            ----------------------------- ("alias-eq")
            (prove_normalize_via(
                decls,
                env,
                assumptions,
                Predicate::AliasEq(via_alias, via_ty),
                goal_alias @ AliasTy { .. },
            ) => Constrained(ty, c))
        )

        (
            (prove_normalize_via_eq(decls, env, assumptions, a, b, goal) => c)
            ----------------------------- ("equality")
            (prove_normalize_via(
                decls,
                env,
                assumptions,
                Relation::Equals(a, b),
                goal,
            ) => c)
        )

        // These rules handle the the ∀ and ⇒ cases.

        (
            (let (env, subst) = env.existential_substitution(binder))
            (let via1 = binder.instantiate_with(subst)?)
            (prove_normalize_via(decls, env, assumptions, via1, goal) => Constrained(p, c))
            (let c = c.pop_subst(subst))
            (assert c.env().encloses(p))
            ----------------------------- ("forall")
            (prove_normalize_via(decls, env, assumptions, WcData::ForAll(binder), goal) => Constrained(p, c))
        )

        (
            (prove_normalize_via(decls, env, assumptions, wc_consequence, goal) => Constrained(p, c))
            (prove_after(decls, c, assumptions, wc_condition) => c)
            (let p = c.substitution().apply(p))
            ----------------------------- ("implies")
            (prove_normalize_via(decls, env, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => Constrained(p, c))
        )
    }
}

judgment_fn! {
    /// Normalize through an equality assumption, considering either orientation.
    fn prove_normalize_via_eq(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        left: Parameter,
        right: Parameter,
        goal: Parameter,
    ) => Constrained<Parameter> {
        debug(goal, left, right, assumptions, env)

        // Normalize an existential variable only through an equality that names that exact
        // variable. Allowing the general rules below to match an existential goal generates many
        // spurious paths.
        (
            (if goal_var == left_var)!
            ----------------------------- ("var-axiom-l")
            (prove_normalize_via_eq(
                _decls,
                env,
                _assumptions,
                left_var @ ExistentialVar { .. },
                right,
                goal_var @ ExistentialVar { .. },
            ) => Constrained::none(env, right))
        )

        (
            (if goal_var == right_var)!
            ----------------------------- ("var-axiom-r")
            (prove_normalize_via_eq(
                _decls,
                env,
                _assumptions,
                left,
                right_var @ ExistentialVar { .. },
                goal_var @ ExistentialVar { .. },
            ) => Constrained::none(env, left))
        )

        // For a non-variable goal, syntactic equality may infer variables nested within the
        // matched side. For example, `R<u32> = Y` normalizes `R<?X>` to `Y` with `?X = u32`.
        (
            (if goal.downcast::<ExistentialVar>().is_none())
            (if goal != right)!
            (prove_syntactically_eq(decls, env, assumptions, left, goal) => c)
            (let right = c.substitution().apply(right))
            ----------------------------- ("axiom-l")
            (prove_normalize_via_eq(
                decls,
                env,
                assumptions,
                left,
                right,
                goal,
            ) => Constrained(right, c))
        )

        (
            (if goal.downcast::<ExistentialVar>().is_none())
            (if goal != left)!
            (prove_syntactically_eq(decls, env, assumptions, right, goal) => c)
            (let left = c.substitution().apply(left))
            ----------------------------- ("axiom-r")
            (prove_normalize_via_eq(
                decls,
                env,
                assumptions,
                left,
                right,
                goal,
            ) => Constrained(left, c))
        )
    }
}

judgment_fn! {
    fn prove_syntactically_eq(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        a: Parameter,
        b: Parameter,
    ) => Constraints {
        debug(a, b, assumptions, env)

        trivial(a == b => Constraints::none(env))

        (
            (prove_syntactically_eq(decls, env, assumptions, b, a) => c)
            ----------------------------- ("symmetric")
            (prove_syntactically_eq(decls, env, assumptions, a, b) => c)
        )

        (
            (if a_name == b_name)!
            (zip(decls, env, assumptions, a_parameters, b_parameters, &prove_syntactically_eq) => c)
            ----------------------------- ("rigid")
            (prove_syntactically_eq(
                decls,
                env,
                assumptions,
                RigidTy {
                    name: a_name,
                    parameters: a_parameters,
                },
                RigidTy {
                    name: b_name,
                    parameters: b_parameters,
                },
            ) => c)
        )

        (
            (if a_name == b_name)!
            (zip(decls, env, assumptions, a_parameters, b_parameters, &prove_syntactically_eq) => c)
            ----------------------------- ("alias")
            (prove_syntactically_eq(
                decls,
                env,
                assumptions,
                AliasTy {
                    name: a_name,
                    parameters: a_parameters,
                },
                AliasTy {
                    name: b_name,
                    parameters: b_parameters,
                },
            ) => c)
        )

        (
            (prove_existential_var_eq(decls, env, assumptions, v, t) => c)
            ----------------------------- ("existential-nonvar")
            (prove_syntactically_eq(decls, env, assumptions, v @ ExistentialVar { .. }, t) => c)
        )
    }
}
