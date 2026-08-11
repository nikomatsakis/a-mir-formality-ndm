use crate::{
    grammar::{
        AliasName, AliasTy, AssociatedItemId, AssociatedTyName, ExistentialVar, Fallible, Mode,
        Parameter, Predicate, Relation, RigidTy, TraitImplBoundData, TraitRef, Ty, TyData, Wc,
        WcData, Wcs,
    },
    prove::prove::{Constrained, ProvedImpl},
};
use formality_core::{judgment_fn, Downcast};

use crate::prove::prove::{
    decls::{ImplCandidate, Program},
    prove::{
        combinators::zip, env::Env, prove_after::prove_after, prove_eq::prove_existential_var_eq,
        prove_via_impl,
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
    /// Select the value of `p` one step using exactly the assumptions supplied by the caller.
    ///
    /// Returns constraints and the value selected by the applicable impl. For example,
    /// `<Vec<T> as IntoIterator>::Item` selects `T`. This judgment does not by itself establish
    /// that the selected value is well formed or satisfies the associated type's declared bounds.
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
            (candidate in decls.raw_trait_impls_for(trait_id))
            (prove_normalize_via_impl_candidate(
                decls,
                env,
                assumptions,
                a,
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
    /// Select an associated value for use within a validation proof.
    ///
    /// Like [`prove_normalize`], this establishes only which value the impl selected, not that the
    /// value is well formed or satisfies its declared bounds. Its result remains inside the
    /// validation judgment that requested it.
    pub(super) fn prove_normalize_for_validation(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        a: AliasTy,
    ) => Constrained<Parameter> {
        debug(a, assumptions, env)

        (
            (definition in assumptions)!
            (prove_normalize_via_later_alias_eq(
                decls,
                env,
                assumptions,
                definition,
                a,
            ) => normalized)
            ----------------------------- ("normalize value via later definition")
            (prove_normalize_for_validation(
                decls,
                env,
                assumptions,
                a,
            ) => normalized)
        )

        (
            (candidate in decls.raw_trait_impls_for(trait_id))
            (prove_normalize_via_impl_candidate(
                decls,
                env,
                assumptions,
                a,
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
    /// Select an associated type's value through an applicable impl.
    fn prove_normalize_via_impl_candidate(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        a: AliasTy,
        candidate: ImplCandidate,
    ) => Constrained<Parameter> {
        debug(a, candidate, assumptions, env)

        (
            (let (gat_parameters, requested_trait_ref, gat_where_clauses) =
                associated_ty_parts(decls, a, *item_arity)?)

            (prove_via_impl(
                decls,
                env,
                assumptions,
                requested_trait_ref,
                candidate,
            ) => Constrained(
                ProvedImpl { trait_impl, .. },
                c,
            ))

            // Selecting the impl makes its header available only as `Later`. Prove the
            // declaration-side GAT conditions before returning the selected value. In particular,
            // do not assume `a = provisional_ty` while proving those conditions: this judgment is
            // deliberately not an implied-bounds rule for the selected value.
            (prove_after(
                decls,
                c,
                (assumptions, Mode::Later.apply_assumption(trait_impl.trait_ref())),
                gat_where_clauses,
            ) => c)

            // Impl selection and the GAT conditions may have further constrained caller variables
            // appearing in the selected value, so apply the latest substitution before returning.
            (let provisional_ty = associated_ty_value(trait_impl, item_id, gat_parameters)?)
            (let ty = c.substitution().apply(provisional_ty))

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

        (
            (prove_normalize_via_alias_eq(
                decls,
                env,
                assumptions,
                via_alias,
                via_ty,
                goal_alias,
            ) => normalized)
            ----------------------------- ("alias-eq")
            (prove_normalize_via(
                decls,
                env,
                assumptions,
                Predicate::AliasEq(via_alias, via_ty),
                goal_alias @ AliasTy { .. },
            ) => normalized)
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

        // An impl fixes its associated values as soon as its header has matched. During impl
        // application those equalities are available only as `Later`: the application still has
        // to prove the impl's where-clauses before they become ordinary, well-formed equalities.
        // Their computational content is nevertheless fixed, so normalization may inspect it
        // while retaining the candidate branch that owns the eventual proof. For a GAT, require
        // its declaration-side conditions at the same frontier before revealing the value.
        (
            (prove_normalize_via_later_alias_eq(
                decls,
                env,
                assumptions,
                definition,
                goal_alias,
            ) => normalized)
            ----------------------------- ("later alias-eq")
            (prove_normalize_via(
                decls,
                env,
                assumptions,
                definition @ WcData::Mode(
                    Mode::Later,
                    crate::grammar::AtomicPredicate::Predicate(Predicate::AliasEq(
                        _,
                        _,
                    )),
                ),
                goal_alias @ AliasTy { .. },
            ) => normalized)
        )

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
    /// Reveal the value promised by an impl's application-scoped associated-type definition.
    fn prove_normalize_via_later_alias_eq(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        definition: Wc,
        goal_alias: AliasTy,
    ) => Constrained<Parameter> {
        debug(definition, goal_alias, assumptions, env)

        (
            (prove_normalize_via_alias_eq(
                decls,
                env,
                assumptions,
                via_alias,
                via_ty,
                goal_alias,
            ) => Constrained(ty, c))
            (let (_, gat_where_clauses) = decls.associated_ty_requirements(via_alias)?)
            (prove_after(
                decls,
                c,
                assumptions,
                Mode::Later.apply_goals(gat_where_clauses),
            ) => c)
            (let ty = c.substitution().apply(ty))
            ----------------------------- ("later alias-eq")
            (prove_normalize_via_later_alias_eq(
                decls,
                env,
                assumptions,
                WcData::Mode(
                    Mode::Later,
                    crate::grammar::AtomicPredicate::Predicate(Predicate::AliasEq(
                        via_alias,
                        via_ty,
                    )),
                ),
                goal_alias @ AliasTy { .. },
            ) => Constrained(ty, c))
        )
    }
}

judgment_fn! {
    /// Use an associated-type equality as an oriented normalization witness.
    fn prove_normalize_via_alias_eq(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        via_alias: AliasTy,
        via_ty: Ty,
        goal_alias: AliasTy,
    ) => Constrained<Parameter> {
        debug(via_alias, via_ty, goal_alias, assumptions, env)

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
            (prove_normalize_via_alias_eq(
                decls,
                env,
                assumptions,
                via_alias,
                via_ty,
                goal_alias,
            ) => Constrained(ty, c))
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
