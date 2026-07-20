//! Canonicalization of monomorphization keys before worklist insertion.

use crate::grammar::{Parameter, Wcs};
use crate::prove::prove::{
    prove_fully_normalize_parameter, Constrained, Constraints, Env, Program,
};
use crate::rust::Fold;
use formality_core::{judgment_fn, Upcast};

use super::scope::MonoKey;

judgment_fn! {
    pub(super) fn normalize_mono_key(
        _program: Program,
        env: Env,
        assumptions: Wcs,
        key: MonoKey,
    ) => Constrained<MonoKey> {
        debug(key, assumptions, env)

        (
            (let MonoKey { id, args } = key)
            (let c = Constraints::none(env))
            (let normalized: Vec<Parameter> = vec![])
            (for_all(parameter in args) with(c, normalized)
                (let assumptions = c.substitution().apply(&assumptions))
                (let parameter = c.substitution().apply(parameter))
                (prove_fully_normalize_parameter(program, c.env(), assumptions, parameter) => Constrained(parameter, c1))
                (let c = c.seq(c1))
                (let normalized = append_normalized(c, normalized.upcast(), parameter.upcast())))
            (let key = MonoKey::new(id, normalized))
            ---------------------------------------------------- ("free function")
            (normalize_mono_key(program, env, assumptions, key) => Constrained(key, c))
        )
    }
}

fn append_normalized<T>(c: &Constraints, normalized: Vec<T>, value: T) -> Vec<T>
where
    T: Fold<Output = T> + Clone,
{
    let mut normalized = c.substitution().apply(normalized);
    normalized.push(c.substitution().apply(value));
    normalized
}
