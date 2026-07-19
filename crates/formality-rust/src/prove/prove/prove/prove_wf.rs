use crate::grammar::{
    AliasTy, ConstData, Lt, LtData, Parameter, Parameters, Relation, RigidName, RigidTy, Ty,
    UniversalVar, Wcs,
};
use formality_core::{judgment_fn, Cons, Downcast};

use crate::prove::prove::decls::Program;

use super::{constraints::Constraints, env::Env, prove_after::prove_after};

judgment_fn! {
    /// Prove that `goal` is well formed in the ordinary proof mode.
    pub fn prove_wf(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        goal: Parameter,
    ) => Constraints {
        debug(goal, assumptions, env)

        assert(env.encloses((assumptions, goal)))

        (
            // `prove_wf` has one rule for every enclosed parameter. Commit before generating
            // requirements so declaration lookup errors remain visible to the caller.
            (if env.encloses(&(assumptions, goal)))!
            (wf_requirements(decls, goal) => requirements)
            (prove_after(decls, env, assumptions, requirements) => c)
            ----------------------------- ("well-formedness requirements now")
            (prove_wf(decls, env, assumptions, goal) => c)
        )
    }
}

judgment_fn! {
    /// Generate the obligations that make one parameter well formed.
    ///
    /// Keeping this decomposition independent of proof mode ensures ordinary proof and validation
    /// agree on the shape of well-formedness while choosing how to establish the resulting
    /// obligations.
    pub(super) fn wf_requirements(
        _decls: Program,
        goal: Parameter,
    ) => Wcs {
        debug(goal)

        (
            // Universal variables are well formed by construction.
            ----------------------------- ("universal variable")
            (wf_requirements(_decls, UniversalVar { .. }) => ())
        )

        (
            // `&'a T` is well formed if `T` is well formed and `T: 'a`.
            (let (lt, ty) = parameters.downcast_err::<(Lt, Ty)>()?)
            ----------------------------- ("reference")
            (wf_requirements(_decls, RigidTy { name: RigidName::Ref(_), parameters }) =>
                (Relation::well_formed(ty), Relation::outlives(ty, lt)))
        )

        (
            // `*const T` and `*mut T` are well formed if `T` is.
            (let (ty,) = parameters.downcast_err::<(Ty,)>()?)
            ----------------------------- ("raw pointer")
            (wf_requirements(_decls, RigidTy { name: RigidName::Raw(_), parameters }) =>
                Relation::well_formed(ty))
        )

        (
            (parameter_requirements(parameters) => requirements)
            ----------------------------- ("tuple")
            (wf_requirements(_decls, RigidTy { name: RigidName::Tuple(_), parameters }) => requirements)
        )

        (
            (parameter_requirements(parameters) => requirements)
            ----------------------------- ("integer or boolean")
            (wf_requirements(_decls, RigidTy { name: RigidName::ScalarId(_), parameters }) => requirements)
        )

        (
            // Once the ADT-shaped input has been selected, a missing declaration is a real
            // well-formedness error rather than an inapplicable normalization path.
            (parameter_requirements(parameters) => parameter_wcs)!
            (let adt = decls.program().adt_item_named(adt_id)?.to_adt())
            (let adt = adt.binder.instantiate_with(parameters).unwrap())
            ----------------------------- ("ADT")
            (wf_requirements(decls, RigidTy { name: RigidName::AdtId(adt_id), parameters }) =>
                (parameter_wcs, &adt.where_clauses))
        )

        (
            ----------------------------- ("static lifetime")
            (wf_requirements(_decls, LtData::Static) => ())
        )

        (
            ----------------------------- ("scalar constant")
            (wf_requirements(_decls, ConstData::Scalar(_)) => ())
        )

        (
            (parameter_requirements(parameters) => parameter_requirements)
            (let alias = AliasTy::new(name, parameters))
            (let (trait_ref, associated_ty_conditions) =
                decls.associated_ty_requirements(alias)?)
            ----------------------------- ("alias")
            (wf_requirements(decls, AliasTy { name, parameters }) =>
                (parameter_requirements, trait_ref, associated_ty_conditions))
        )
    }
}

judgment_fn! {
    /// Require every parameter in a type constructor to be well formed.
    fn parameter_requirements(
        parameters: Parameters,
    ) => Wcs {
        debug(parameters)

        (
            ----------------------------- ("none")
            (parameter_requirements(()) => ())
        )

        (
            (parameter_requirements(rest) => rest_requirements)
            ----------------------------- ("some")
            (parameter_requirements(Cons(parameter, rest)) =>
                (Relation::well_formed(parameter), rest_requirements))
        )
    }
}
