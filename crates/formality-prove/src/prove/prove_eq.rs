use formality_types::{
    cast::{Downcast, Upcast, Upcasted},
    collections::{Deduplicate, Set},
    grammar::{
        AliasTy, AtomicRelation, InferenceVar, Parameter, PlaceholderVar, RigidTy, Substitution,
        Ty, TyData, Variable, Wcs,
    },
    judgment_fn, set,
    visit::Visit,
};

use crate::{
    program::Program,
    prove::{
        constraints::occurs_in, prove, prove_after::prove_after, prove_normalize::prove_normalize,
    },
};

use super::{constraints::Constraints, env::Env};

/// Goal(s) to prove `a` and `b` are equal (they must have equal length)
pub fn all_eq(a: impl Upcast<Vec<Parameter>>, b: impl Upcast<Vec<Parameter>>) -> Wcs {
    let a: Vec<Parameter> = a.upcast();
    let b: Vec<Parameter> = b.upcast();
    assert_eq!(a.len(), b.len());
    a.into_iter()
        .zip(b)
        .map(|(a, b)| AtomicRelation::eq(a, b))
        .upcasted()
        .collect()
}

/// Goal(s) to prove `a` and `b` are equal
pub fn eq(a: impl Upcast<Parameter>, b: impl Upcast<Parameter>) -> AtomicRelation {
    AtomicRelation::eq(a, b)
}

judgment_fn! {
    pub fn prove_ty_eq(
        program: Program,
        env: Env,
        assumptions: Wcs,
        a: Ty,
        b: Ty,
    ) => Constraints {
        debug(a, b, assumptions, env, program)

        trivial(a == b => Constraints::none(env))

        (
            (prove_ty_eq(program, env, assumptions, r, l) => env_c)
            ----------------------------- ("symmetric")
            (prove_ty_eq(program, env, assumptions, l, r) => env_c)
        )

        (
            (let RigidTy { name: a_name, parameters: a_parameters } = a)
            (let RigidTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)
            (prove(program, env, assumptions, all_eq(a_parameters, b_parameters)) => c)
            ----------------------------- ("rigid")
            (prove_ty_eq(program, env, assumptions, TyData::RigidTy(a), TyData::RigidTy(b)) => c)
        )

        (
            (let AliasTy { name: a_name, parameters: a_parameters } = a)
            (let AliasTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)
            (prove(program, env, assumptions, all_eq(a_parameters, b_parameters)) => env_c)
            ----------------------------- ("alias")
            (prove_ty_eq(program, env, assumptions, TyData::AliasTy(a), TyData::AliasTy(b)) => env_c)
        )

        (
            (prove_existential_var_eq(program, env, assumptions, v, r) => c)
            ----------------------------- ("existential-nonvar")
            (prove_ty_eq(program, env, assumptions, Variable::InferenceVar(v), r) => c)
        )

        (
            (prove_normalize(&program, env, &assumptions, &x) => (c, y))
            (prove_after(&program, c, &assumptions, eq(y, &z)) => c)
            ----------------------------- ("normalize-l")
            (prove_ty_eq(program, env, assumptions, x, z) => c)
        )
    }
}

judgment_fn! {
    pub fn prove_existential_var_eq(
        program: Program,
        env: Env,
        assumptions: Wcs,
        v: InferenceVar,
        b: Parameter,
    ) => Constraints {
        debug(v, b, assumptions, env, program)

        (
            (if let None = t.downcast::<Variable>())
            (equate_variable(program, env, assumptions, v, t) => c)
            ----------------------------- ("existential-nonvar")
            (prove_existential_var_eq(program, env, assumptions, v, t) => c)
        )

        (
            // Map the higher rank variable to the lower rank one.
            (let (a, b) = env.order_by_universe(l, r))
            ----------------------------- ("existential-existential")
            (prove_existential_var_eq(_program, env, _assumptions, l, Variable::InferenceVar(r)) => (env, (b, a)))
        )

        (
            (if env.universe(p) < env.universe(v))
            ----------------------------- ("existential-placeholder")
            (prove_existential_var_eq(_program, env, _assumptions, v, Variable::PlaceholderVar(p)) => (env, (v, p)))
        )
    }
}

fn equate_variable(
    program: Program,
    mut env: Env,
    assumptions: Wcs,
    x: InferenceVar,
    p: impl Upcast<Parameter>,
) -> Set<Constraints> {
    let p: Parameter = p.upcast();

    let span = tracing::debug_span!("equate_variable", ?x, ?p, ?env);
    let _guard = span.enter();

    // Preconditions:
    // * Environment contains all free variables
    // * `p` is some compound type, not a variable
    //   (variables are handled via special rules above)
    assert!(env.encloses((x, (&assumptions, &p))));
    assert!(!p.is_a::<Variable>());

    let fvs = p.free_variables().deduplicate();

    // Ensure that `x` passes the occurs check for the free variables in `p`.
    if occurs_in(x, &fvs) {
        return set![];
    }

    // Map each free variable `fv` in `p` that is of higher universe than `x`
    // to a fresh variable `y` of lower universe than `x`.
    //
    // e.g., in an environment `[X, Y]`, if we have `X = Vec<Y>`:
    // * we would create `Z` before `X` (so new env is `[Z, X, Y]`)
    // * and map `Y` to `Z`
    let universe_x = env.universe(x);
    let universe_subst: Substitution = fvs
        .iter()
        .flat_map(|fv| {
            if universe_x < env.universe(fv) {
                let y = env.insert_fresh_before(fv.kind(), universe_x);
                Some((fv, y))
            } else {
                None
            }
        })
        .collect();

    // Introduce the following constraints:
    //
    // * `fv = universe_subst(fv)` for each free existential variable `fv` in `p` (e.g., `Y => Z` in our example above)
    // * `x = universe_subst(p)` (e.g., `Vec<Z>` in our example above)
    let constraints: Constraints = Constraints::from(
        env,
        universe_subst
            .iter()
            .filter(|(v, _)| v.is_a::<InferenceVar>())
            .chain(Some((x, universe_subst.apply(&p)).upcast())),
    );

    // For each placeholder variable that we replaced with an inference variable
    // above, we now have to prove that goal. e.g., if we had `X = Vec<!Y>`, we would replace `!Y` with `?Z`
    // (where `?Z` is in a lower universe than `X`), but now we must prove that `!Y = ?Z`
    // (this may be posible due to assumptions).
    let goals: Wcs = universe_subst
        .iter()
        .filter(|(v, _)| v.is_a::<PlaceholderVar>())
        .map(|(v, p)| eq(v, p))
        .upcasted()
        .collect();

    tracing::debug!("equated: constraints={:?}, goals={:?}", constraints, goals);

    prove_after(program, constraints, assumptions, goals)
}
