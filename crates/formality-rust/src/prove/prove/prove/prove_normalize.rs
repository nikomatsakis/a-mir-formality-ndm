use crate::{
    grammar::{
        AliasName, AliasTy, AssociatedItemId, ExistentialVar, Fallible, Parameter, Predicate,
        Relation, RigidTy, TraitRef, Ty, TyData, Upto, Variable, Wc, WcData, Wcs,
    },
    prove::prove::Constrained,
};
use formality_core::{judgment_fn, Downcast};

use crate::prove::prove::{
    decls::{ImplCandidate, Program},
    prove::{
        combinators::zip,
        env::Env,
        prove_after::prove_after,
        prove_eq::prove_existential_var_eq,
        prove_match_impl::{match_impl_candidate, MatchedImpl},
    },
};
use crate::prove::ToWcs;

use super::constraints::Constraints;

fn associated_ty_parts(
    decls: &Program,
    alias: &AliasTy,
) -> Fallible<(AssociatedItemId, Vec<Parameter>, TraitRef, Wcs)> {
    let AliasName::AssociatedTyId(name) = &alias.name;
    let trait_parameter_count = alias
        .parameters
        .len()
        .checked_sub(name.item_arity)
        .ok_or_else(|| {
            anyhow::anyhow!(
                "associated type alias {:?} has fewer parameters than its item arity",
                alias,
            )
        })?;
    let (_, gat_parameters) = alias.parameters.split_at(trait_parameter_count);
    let (trait_ref, gat_where_clauses) = decls.associated_ty_requirements(alias)?;
    Ok((
        name.item_id.clone(),
        gat_parameters.to_vec(),
        trait_ref,
        gat_where_clauses,
    ))
}

fn associated_ty_value(
    matched: &MatchedImpl,
    constraints: &Constraints,
    item_id: &AssociatedItemId,
    gat_parameters: &[Parameter],
) -> Fallible<Ty> {
    let trait_impl = matched.trait_impl(constraints);
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
            (let AliasName::AssociatedTyId(name) = &a.name)
            (let impl_validation = Upto::gat_bounds(&name.trait_id))
            (candidate in decls.raw_trait_impls_for(&name.trait_id))
            (prove_normalize_via_impl_candidate(
                decls,
                env,
                assumptions,
                a,
                impl_validation,
                candidate,
            ) => normalized)
            ----------------------------- ("normalize-via-impl")
            (prove_normalize(decls, env, assumptions, TyData::AliasTy(a)) => normalized)
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
            (let AliasName::AssociatedTyId(name) = &a.name)
            (let impl_validation = Upto::supertraits(&name.trait_id))
            (candidate in decls.raw_trait_impls_for(&name.trait_id))
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
                a,
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
            (let (item_id, gat_parameters, requested_trait_ref, gat_where_clauses) =
                associated_ty_parts(decls, a)?)

            // Normalizing through an impl is itself a coinductive application. The recursive
            // handle is deliberately zero-capability: it can close an exact occurrence but cannot
            // expose requirements of the dictionary whose associated value we are selecting.
            (let recursive_assumption =
                Wc::validate(Upto::Zero, requested_trait_ref))

            (match_impl_candidate(
                decls,
                env,
                (assumptions, &recursive_assumption),
                requested_trait_ref,
                candidate,
            ) => Constrained(matched, c))

            // The selected impl fixes its associated value before any dictionaries witnessing
            // that value's bounds exist. Make that equation available only inside this candidate
            // branch, then commit it only after every residual obligation below succeeds.
            (let provisional_ty = associated_ty_value(matched, c, item_id, gat_parameters)?)
            (let provisional_alias_eq =
                Predicate::AliasEq(a.clone(), provisional_ty.clone()))

            // Selecting the impl makes its header available at the supertrait frontier. Ordinary
            // normalization passes `GatBounds(ImplTrait)` as `impl_validation`, matching the
            // stronger inputs assumed by the GAT contract checked in `ImplWF`. Value-only
            // normalization passes `Supertraits(ImplTrait)` instead, but its result remains
            // confined to the surrounding validation proof.
            //
            // FIXME: Value-only normalization still requires declaration-side GAT conditions at
            // `GatBounds(ImplTrait)`. Determine whether selecting the value should require those
            // conditions only at an earlier frontier too.
            (let trait_impl = matched.trait_impl(c))
            (let gat_validation = Upto::gat_bounds(&trait_impl.trait_id))
            (let provisional_impl_header =
                Wc::validate(Upto::supertraits(&trait_impl.trait_id), trait_impl.trait_ref()))
            (let impl_where_clauses = trait_impl
                .where_clauses
                .to_wcs()
                .validated(impl_validation))
            (let conditions = (
                &impl_where_clauses,
                gat_where_clauses.validated(gat_validation),
            ).to_wcs())
            (prove_after(
                decls,
                c,
                (
                    assumptions,
                    provisional_impl_header,
                    provisional_alias_eq,
                ),
                conditions,
            ) => c)

            // Where-clauses may have inferred impl parameters absent from the header, so apply the
            // latest substitution before selecting and instantiating the associated value.
            (let ty = c.substitution().apply(provisional_ty))
            (let c = matched.pop_constraints(c))
            // An ambiguous, nonmatching candidate can leave one of its local variables in the
            // provisional value. Such a value cannot escape the candidate binder; discard that
            // path while retaining ambiguous paths whose result is well scoped.
            (if c.env().encloses(ty))!
            ----------------------------- ("normalize-via-impl")
            (prove_normalize_via_impl_candidate(
                decls,
                env,
                assumptions,
                a,
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
                TyData::AliasTy(via_alias.clone()),
                TyData::AliasTy(goal_alias.clone()),
            ) => c)
            (let ty = c.substitution().apply(via_ty))
            (let goal = c
                .substitution()
                .apply(TyData::AliasTy(goal_alias.clone())))
            (if goal != ty)!
            ----------------------------- ("alias-eq")
            (prove_normalize_via(
                decls,
                env,
                assumptions,
                Predicate::AliasEq(via_alias, via_ty),
                TyData::AliasTy(goal_alias),
            ) => Constrained(ty, c))
        )

        // The following 2 rules handle normalization of existential variables. We look specifically for
        // the case of a assumption `?X = Y`, which lets us normalize `?X` to `Y`, and ignore
        // everything else. In principle, we could allow the more general normalization rules
        // below handle this case too, but that generates a LOT of false paths, and I *believe*
        // it is unnecessary

        (
            (if let Some(Variable::ExistentialVar(v_a)) = a.downcast())
            (if v_goal == v_a)!
            ----------------------------- ("var-axiom-l")
            (prove_normalize_via(_decls, env, _assumptions, Relation::Equals(a, b), Variable::ExistentialVar(v_goal)) => Constrained::none(env, b))
        )

        (
            (if let Some(Variable::ExistentialVar(v_a)) = a.downcast())
            (if v_goal == v_a)!
            ----------------------------- ("var-axiom-r")
            (prove_normalize_via(_decls, env, _assumptions, Relation::Equals(b, a), Variable::ExistentialVar(v_goal)) => Constrained::none(env, b))
        )

        // The following 2 rules handle normalization of a type `X` given an assumption `X = Y`.
        // We can't just check for `goal == a` though because we sometimes need to bind existential
        // variables. Consider normalizing `R<?X>` given an assumption `R<u32> = Y`: this can be
        // normalized to `Y` given the constraint `?X = u32`.
        //
        // We don't use these rules to normalize an existential variable `?X` because such a goal
        // could be equated to everything, and thus generates a ton of spurious paths.

        (
            (if let None = goal.downcast::<ExistentialVar>())
            (if goal != b)!
            (prove_syntactically_eq(decls, env, assumptions, a, goal) => c)
            (let b = c.substitution().apply(b))
            ----------------------------- ("axiom-l")
            (prove_normalize_via(decls, env, assumptions, Relation::Equals(a, b), goal) => Constrained(b, c))
        )

        (
            (if let None = goal.downcast::<ExistentialVar>())
            (if goal != b)!
            (prove_syntactically_eq(decls, env, assumptions, a, goal) => c)
            (let b = c.substitution().apply(b))
            ----------------------------- ("axiom-r")
            (prove_normalize_via(decls, env, assumptions, Relation::Equals(b, a), goal) => Constrained(b, c))
        )

        // These rules handle the the ∀ and ⇒ cases.

        (
            (let (env, subst) = env.existential_substitution(binder))
            (let via1 = binder.instantiate_with(&subst).unwrap())
            (prove_normalize_via(decls, env, assumptions, via1, goal) => Constrained(p, c))
            (let c = c.pop_subst(&subst))
            (assert c.env().encloses(&p))
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
            (let RigidTy { name: a_name, parameters: a_parameters } = a)
            (let RigidTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)!
            (zip(decls, env, assumptions, a_parameters, b_parameters, &prove_syntactically_eq) => c)
            ----------------------------- ("rigid")
            (prove_syntactically_eq(decls, env, assumptions, TyData::RigidTy(a), TyData::RigidTy(b)) => c)
        )

        (
            (let AliasTy { name: a_name, parameters: a_parameters } = a)
            (let AliasTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)!
            (zip(decls, env, assumptions, a_parameters, b_parameters, &prove_syntactically_eq) => c)
            ----------------------------- ("alias")
            (prove_syntactically_eq(decls, env, assumptions, TyData::AliasTy(a), TyData::AliasTy(b)) => c)
        )

        (
            (prove_existential_var_eq(decls, env, assumptions, v, t) => c)
            ----------------------------- ("existential-nonvar")
            (prove_syntactically_eq(decls, env, assumptions, Variable::ExistentialVar(v), t) => c)
        )
    }
}
