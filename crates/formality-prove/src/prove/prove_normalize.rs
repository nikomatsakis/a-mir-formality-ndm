use formality_types::{
    grammar::{AliasTy, Parameter, Predicate, RigidTy, TyData, Variable, Wc, WcData, Wcs},
    judgment_fn,
};

use crate::{
    decls::{AliasEqDeclBoundData, Decls},
    prove::{
        combinators::zip,
        env::Env,
        prove_after::prove_after,
        prove_eq::{prove_all_eq, prove_existential_var_eq},
    },
};

use super::constraints::Constraints;

judgment_fn! {
    pub fn prove_normalize(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        p: Parameter,
    ) => (Constraints, Parameter) {
        debug(p, assumptions, env, decls)

        (
            (&assumptions => a)
            (prove_normalize_via(&decls, &env, &assumptions, a, &goal) => c)
            ----------------------------- ("normalize-via-assumption")
            (prove_normalize(decls, env, assumptions, goal) => c)
        )

        (
            (decls.alias_eq_decls(&a.name) => decl)
            (let (env, subst) = env.existential_substitution(&decl.binder))
            (let decl = decl.binder.instantiate_with(&subst).unwrap())
            (let AliasEqDeclBoundData { alias: AliasTy { name, parameters }, ty, where_clause } = decl)
            (assert a.name == name)
            (prove_all_eq(&decls, env, &assumptions, &a.parameters, &parameters) => c)
            (prove_after(&decls, c, &assumptions, &where_clause) => c)
            (let ty = c.substitution().apply(&ty))
            (let c = c.pop_subst(&subst))
            (assert c.env().encloses(&ty))
            ----------------------------- ("normalize-via-impl")
            (prove_normalize(decls, env, assumptions, TyData::AliasTy(a)) => (c, ty))
        )
    }
}

judgment_fn! {
    fn prove_normalize_via(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        via: Wc,
        goal: Parameter,
    ) => (Constraints, Parameter) {
        debug(goal, via, assumptions, env, decls)

        // If the thing we are normalizing is an associated type, and we have something in the environment,
        // then consider applying that rule.

        (
            (if name_a == name_b)
            (prove_all_eq(decls, env, assumptions, parameters_a, parameters_b) => c)
            (let t = c.substitution().apply(&t))
            ----------------------------- ("axiom-l")
            (prove_normalize_via(decls, env, assumptions, Predicate::AliasEq(AliasTy { name: name_a, parameters: parameters_a }, t), AliasTy { name: name_b, parameters: parameters_b }) => (c, t))
        )

        // These rules handle the the ∀ and ⇒ cases.

        (
            (let (env, subst) = env.existential_substitution(&binder))
            (let via1 = binder.instantiate_with(&subst).unwrap())
            (prove_normalize_via(decls, env, assumptions, via1, goal) => (c, p))
            (let c = c.pop_subst(&subst))
            (assert c.env().encloses(&p))
            ----------------------------- ("forall")
            (prove_normalize_via(decls, env, assumptions, WcData::ForAll(binder), goal) => (c, p))
        )

        (
            (prove_normalize_via(&decls, &env, &assumptions, &wc_consequence, goal) => (c, p))
            (prove_after(&decls, c, &assumptions, &wc_condition) => c)
            (let p = c.substitution().apply(&p))
            ----------------------------- ("implies")
            (prove_normalize_via(decls, env, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => (c, p))
        )
    }
}

judgment_fn! {
    fn prove_syntactically_eq(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        a: Parameter,
        b: Parameter,
    ) => Constraints {
        debug(a, b, assumptions, env, decls)

        trivial(a == b => Constraints::none(env))

        (
            (prove_syntactically_eq(decls, env, assumptions, b, a) => c)
            ----------------------------- ("symmetric")
            (prove_syntactically_eq(decls, env, assumptions, a, b) => c)
        )

        (
            (let RigidTy { name: a_name, parameters: a_parameters } = a)
            (let RigidTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)
            (zip(&decls, &env, &assumptions, a_parameters, b_parameters, &prove_syntactically_eq) => c)
            ----------------------------- ("rigid")
            (prove_syntactically_eq(decls, env, assumptions, TyData::RigidTy(a), TyData::RigidTy(b)) => c)
        )

        (
            (let AliasTy { name: a_name, parameters: a_parameters } = a)
            (let AliasTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)
            (zip(&decls, &env, &assumptions, a_parameters, b_parameters, &prove_syntactically_eq) => c)
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
