//! Canonicalization of monomorphization keys before worklist insertion.

use crate::grammar::{Parameter, TraitRef, Wcs};
use crate::prove::prove::{
    prove_fully_normalize_parameter, Constrained, Constraints, Env, Program,
};
use crate::rust::Fold;
use formality_core::{judgment_fn, Upcast};

use super::scope::MonoKey;

judgment_fn! {
    pub(super) fn normalize_parameters(
        _program: Program,
        env: Env,
        assumptions: Wcs,
        parameters: Vec<Parameter>,
    ) => Constrained<Vec<Parameter>> {
        debug(parameters, assumptions, env)

        (
            (let c = Constraints::none(env))
            (let normalized: Vec<Parameter> = vec![])
            (for_all(parameter in parameters) with(c, normalized)
                (let assumptions = c.substitution().apply(&assumptions))
                (let parameter = c.substitution().apply(parameter))
                (prove_fully_normalize_parameter(program, c.env(), assumptions, parameter) => Constrained(parameter, c1))
                (let c = c.seq(c1))
                (let normalized = append_normalized(c, normalized.upcast(), parameter.upcast())))
            ---------------------------------------------------- ("parameters")
            (normalize_parameters(program, env, assumptions, parameters) => Constrained(normalized, c))
        )
    }
}

judgment_fn! {
    pub(super) fn normalize_mono_key(
        _program: Program,
        env: Env,
        assumptions: Wcs,
        key: MonoKey,
    ) => Constrained<MonoKey> {
        debug(key, assumptions, env)

        (
            (normalize_parameters(program, env, assumptions, fn_args) => Constrained(fn_args, c))
            (let key = MonoKey::free_fn(id, fn_args))
            ---------------------------------------------------- ("free function")
            (normalize_mono_key(
                program,
                env,
                assumptions,
                MonoKey::FreeFn { id, fn_args },
            ) => Constrained(key, c))
        )

        (
            (let trait_arity = trait_ref.parameters.len())
            (let parameters: Vec<Parameter> = trait_ref
                .parameters
                .iter()
                .chain(method_args)
                .map(|parameter| parameter.upcast())
                .collect())
            (normalize_parameters(program, env, assumptions, parameters) => Constrained(parameters, c))
            (let (trait_parameters, method_args) = split_parameters(parameters, *trait_arity))
            (let trait_ref = TraitRef {
                trait_id: (&trait_ref.trait_id).upcast(),
                parameters: trait_parameters.upcast(),
            })
            (let key = MonoKey::trait_method(trait_ref, method_id, method_args))
            ---------------------------------------------------- ("trait method")
            (normalize_mono_key(
                program,
                env,
                assumptions,
                MonoKey::TraitMethod {
                    trait_ref,
                    method_id,
                    method_args,
                },
            ) => Constrained(key, c))
        )
    }
}

fn split_parameters(
    parameters: &[Parameter],
    trait_arity: usize,
) -> (Vec<Parameter>, Vec<Parameter>) {
    let (trait_parameters, method_args) = parameters.split_at(trait_arity);
    (trait_parameters.upcast(), method_args.upcast())
}

fn append_normalized<T>(c: &Constraints, normalized: Vec<T>, value: T) -> Vec<T>
where
    T: Fold<Output = T> + Clone,
{
    let mut normalized = c.substitution().apply(normalized);
    normalized.push(c.substitution().apply(value));
    normalized
}
