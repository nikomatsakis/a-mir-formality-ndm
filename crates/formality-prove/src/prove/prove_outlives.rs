use formality_types::{
    grammar::{AliasTy, Parameter, PredicateTy, Relation, RigidTy, TyData, Wcs},
    judgment_fn,
};

use crate::{
    decls::Decls,
    prove::{prove, prove_after::prove_after, prove_normalize::prove_normalize},
};

use super::{constraints::Constraints, env::Env};

judgment_fn! {
    /// Rules for proving `P1 : P2` (pronounced P1 outlives P2).
    ///
    /// The intuition for outlives is as follows:
    ///
    /// > P1 : P2 means that, if P1 references an invalidated loan,
    /// > then P2 must also reference an invalidated loan.
    ///
    /// In Polonius, a lifetime `'a` is a set of loans, and `'a : 'b`
    /// if `'a` is a subset of `'b`. That fits this definition, since if any
    /// of the loans in `'a` are invalidated, that loan also appears in `'b`
    /// and hence `'b` is invalidated.
    ///
    /// Transitivity follows because, if `P : 'a` and `'a : Q`, then this means
    /// that invalidating a loan in `P` must invalidate a loan in `'a`, and
    /// invalidating a loan in `'a` must invalidate a loan in `Q`, so `P : Q`.
    ///
    /// Examples of outlives:
    ///
    /// * `u32 : 'a` for all `a` because `u32` does not reference any loans, so it can never reference any invalidated loans.
    /// * `'static : P` for all `P`, since `'static` is the empty set of loans, and so it can never be invalidated.
    /// * `&'a u32 : 'a` because invalidating a loan in `'a` invalidates both sides.
    /// * `'a : (&'a u32, &'b u32)` is true (note that Polonius allows types on the right hand side)
    ///   because invalidating a loan in `'a` invalidates the first part of the tuple type
    ///   (if not necessarily the right-hand side).
    ///
    /// Counterexamples where outlives does not hold:
    ///
    /// * `(&'a u32, &'b u32) : 'a` is false (at least, assuming no relationship between `'a` and `'b`)
    ///   because `'a` and `'b` could be disjoint sets of loans, so invalidating the tuple may happen
    ///   because of some loan in `'b`.
    /// * `'a : 'static` isn't true for all `'a` because `'static` is the empty set of loans, and `'a` may have loans in it
    pub fn prove_outlives(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        a: Parameter,
        b: Parameter,
    ) => Constraints {
        debug(a, b, assumptions, env, decls)

        trivial(a == b => Constraints::none(env))
        trivial(a.is_static() => Constraints::none(env))

        // `R<Pa..> : Pb` if `Pa[i] : Pb` for all `i`
        (
            (let RigidTy { name: _, parameters: a_parameters } = a)
            (let all_outlives = Wcs::from_iter(a_parameters.iter()
                .map(|a| Relation::outlives(a, &b))))
            (prove(decls, env, assumptions, all_outlives) => c)
            ----------------------------- ("rigid-l")
            (prove_outlives(decls, env, assumptions, TyData::RigidTy(a), b) => c)
        )

        // `A<Pa..> : Pb` if `Pa[i] : Pb` for all `i`
        //
        // Note that there is no equivalent `alias-r` rule.
        // The only way to prove that `Pa : A<Pb...>` is to
        // normalize `A`. After all, if could well be be
        // normalized to `()` or something.
        (
            (let AliasTy { name: _, parameters: a_parameters } = a)
            (let all_outlives = Wcs::from_iter(a_parameters.iter()
                .map(|a| Relation::outlives(a, &b))))
            (prove(decls, env, assumptions, all_outlives) => c)
            ----------------------------- ("alias-l")
            (prove_outlives(decls, env, assumptions, TyData::AliasTy(a), b) => c)
        )

        // `Pa : R<Pb..>` if `Pa : Pb[i]` for some `i`
        (
            (let RigidTy { name: _, parameters: b_parameters } = b)
            (b_parameters => b_parameter)
            (prove(&decls, &env, &assumptions, Relation::outlives(&a, b_parameter)) => c)
            ----------------------------- ("rigid-r")
            (prove_outlives(decls, env, assumptions, a, TyData::RigidTy(b)) => c)
        )

        // `∀X.Pa : Pb` if there is some mapping `S` of `X` such that `S Pa : Pb`.
        (
            (let (env, subst) = env.existential_substitution(&a_binder))
            (let a = a_binder.instantiate_with(&subst).unwrap())
            (prove_outlives(decls, env, assumptions, a, b) => c)
            --- ("forall-l")
            (prove_outlives(decls, env, assumptions, PredicateTy::ForAll(a_binder), b) => c.pop_subst(&subst))
        )

        // `Pa : ∀X.Pb` if `Pa : S Pb` for all mappings `S` for `X`.
        (
            (let (env, subst) = env.universal_substitution(&b_binder))
            (let b = b_binder.instantiate_with(&subst).unwrap())
            (prove_outlives(decls, env, assumptions, a, b) => c)
            --- ("forall-r")
            (prove_outlives(decls, env, assumptions, a, PredicateTy::ForAll(b_binder)) => c.pop_subst(&subst))
        )

        // Normalize left
        (
            (prove_normalize(&decls, env, &assumptions, &x) => (c, y))
            (prove_after(&decls, c, &assumptions, Relation::outlives(y, &z)) => c)
            ----------------------------- ("normalize-l")
            (prove_outlives(decls, env, assumptions, x, z) => c)
        )

        // Normalize right
        (
            (prove_normalize(&decls, env, &assumptions, &y) => (c, z))
            (prove_after(&decls, c, &assumptions, Relation::outlives(&x, z)) => c)
            ----------------------------- ("normalize-r")
            (prove_outlives(decls, env, assumptions, x, y) => c)
        )

        // Other rules we don't use yet
        //
        // (Pa : (WC => Pb)) if (WC => Pa : Pb)
        // ((WC => Pa) : Pb) if (WC && Pa : Pb)
    }
}
