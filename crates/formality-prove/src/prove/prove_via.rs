use formality_types::{
    grammar::{Relation, WcData, Wcs, PR},
    judgment_fn,
};

use crate::{
    decls::Decls,
    prove::{constraints::Constraints, env::Env, prove_after::prove_after, prove_eq::prove_all_eq},
};

judgment_fn! {
    pub fn prove_via(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        via: WcData,
        goal: PR,
    ) => Constraints {
        debug(goal, via, assumptions, env, decls)

        // Check whether the parameters can be equated -- this is called "congruence"
        // because the parameters don't have to be syntactically equal, just semantically equal.
        //
        // Subtle: for equality predicates in particular, we use a different rule.
        (
            (if !predicate.is_equals())
            (let (skel_c, parameters_c) = predicate.debone())
            (let (skel_g, parameters_g) = goal.debone())
            (if skel_c == skel_g)
            (prove_all_eq(decls, env, assumptions, parameters_c, parameters_g) => c)
            ----------------------------- ("predicate-congruence-axiom")
            (prove_via(decls, env, assumptions, predicate: PR, goal) => c)
        )

        // Subtle: For equality predicates in particular, we require a 100% syntactic match.
        // This is because when attempting to judge equality of `a` and `b` we already normalize
        // `a` and `b` fully, so any non-syntactic match will be explored through that loop.
        (
            (if a1 == a2)
            (if b1 == b2)
            ----------------------------- ("equality")
            (prove_via(_decls, env, _assumptions, Relation::Equals(a1, b1), Relation::Equals(a2, b2)) => Constraints::none(env))
        )

        (
            (let (env, subst) = env.existential_substitution(&binder))
            (let via1 = binder.instantiate_with(&subst).unwrap())
            (prove_via(decls, env, assumptions, via1, goal) => c)
            ----------------------------- ("forall")
            (prove_via(decls, env, assumptions, WcData::ForAll(binder), goal) => c.pop_subst(&subst))
        )

        (
            (prove_via(&decls, env, &assumptions, wc_consequence, goal) => c)
            (prove_after(&decls, c, &assumptions, &wc_condition) => c)
            ----------------------------- ("implies")
            (prove_via(decls, env, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => c)
        )
    }
}
