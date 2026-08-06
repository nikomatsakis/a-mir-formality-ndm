use crate::grammar::{Const, ConstData, Lt, Parameter, RigidConstData, RigidTy, Ty, TyData, Wcs};
use crate::prove::prove::prove_normalize;
use crate::prove::prove::{Constrained, Constraints, Env, Program};
use formality_core::{judgment_fn, Downcast, Upcast};

judgment_fn! {
    /// Recursively normalize `ty` to a rigid type suitable for monomorphization.
    pub(crate) fn prove_fully_normalize_ty(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        ty: Ty,
    ) => Constrained<RigidTy> {
        debug(ty, assumptions, env)

        (
            (prove_fully_normalize_parameters(decls, env, assumptions, parameters) => Constrained(parameters, c))
            ---------------------------------------------------- ("rigid")
            (prove_fully_normalize_ty(
                decls,
                env,
                assumptions,
                TyData::RigidTy(RigidTy { name, parameters }),
            ) => Constrained(RigidTy::new(name, parameters), c))
        )

        (
            (prove_normalize(decls, env, assumptions, &alias) => Constrained(parameter, c0))
            (let parameter = c0.substitution().apply(parameter))
            (if let Some(ty) = parameter.downcast::<Ty>())!
            (let assumptions = c0.substitution().apply(assumptions))
            (prove_fully_normalize_ty(decls, c0.env(), assumptions, ty) => Constrained(ty, c1))
            (let c = c0.seq(c1))
            (let ty = c.substitution().apply(ty))
            ---------------------------------------------------- ("alias")
            (prove_fully_normalize_ty(
                decls,
                env,
                assumptions,
                TyData::AliasTy(alias),
            ) => Constrained(ty, c))
        )
    }
}

judgment_fn! {
    /// Recursively normalize a generic parameter suitable for monomorphization.
    pub(crate) fn prove_fully_normalize_parameter(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        parameter: Parameter,
    ) => Constrained<Parameter> {
        debug(parameter, assumptions, env)

        (
            (prove_fully_normalize_ty(decls, env, assumptions, ty) => Constrained(ty, c))
            (let parameter: Parameter = ty.upcast())
            ---------------------------------------------------- ("type")
            (prove_fully_normalize_parameter(decls, env, assumptions, ty: Ty) => Constrained(parameter, c))
        )

        (
            (if matches!(lt, Lt::Static | Lt::Erased))!
            (let parameter: Parameter = lt.upcast())
            ---------------------------------------------------- ("lifetime")
            (prove_fully_normalize_parameter(_decls, env, _assumptions, lt: Lt) => Constrained::none(env, parameter))
        )

        (
            (prove_fully_normalize_const(decls, env, assumptions, constant) => Constrained(constant, c))
            (let parameter: Parameter = constant.upcast())
            ---------------------------------------------------- ("const")
            (prove_fully_normalize_parameter(decls, env, assumptions, constant: Const) => Constrained(parameter, c))
        )
    }
}

judgment_fn! {
    fn prove_fully_normalize_parameters(
        _decls: Program,
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
                (prove_fully_normalize_parameter(decls, c.env(), assumptions, parameter) => Constrained(parameter, c1))
                (let c = c.seq(c1))
                (let normalized = append_normalized(c, normalized.upcast(), parameter.upcast())))
            ---------------------------------------------------- ("parameters")
            (prove_fully_normalize_parameters(decls, env, assumptions, parameters) => Constrained(normalized, c))
        )
    }
}

judgment_fn! {
    fn prove_fully_normalize_const(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        constant: Const,
    ) => Constrained<Const> {
        debug(constant, assumptions, env)

        (
            (let constant: Const = value.upcast())
            ---------------------------------------------------- ("scalar")
            (prove_fully_normalize_const(
                _decls,
                env,
                _assumptions,
                ConstData::Scalar(value),
            ) => Constrained::none(env, constant))
        )

        (
            (prove_fully_normalize_parameters(decls, env, assumptions, parameters) => Constrained(parameters, c0))
            (let assumptions = c0.substitution().apply(assumptions))
            (let values = c0.substitution().apply(values))
            (prove_fully_normalize_consts(decls, c0.env(), assumptions, values) => Constrained(values, c1))
            (let c = c0.seq(c1))
            (let parameters = c.substitution().apply(parameters))
            (let values = c.substitution().apply(values))
            (let constant: Const = RigidConstData::new(name, parameters, values).upcast())
            ---------------------------------------------------- ("rigid")
            (prove_fully_normalize_const(
                decls,
                env,
                assumptions,
                ConstData::RigidValue(RigidConstData { name, parameters, values }),
            ) => Constrained(constant, c))
        )
    }
}

judgment_fn! {
    fn prove_fully_normalize_consts(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        constants: Vec<Const>,
    ) => Constrained<Vec<Const>> {
        debug(constants, assumptions, env)

        (
            (let c = Constraints::none(env))
            (let normalized: Vec<Const> = vec![])
            (for_all(constant in constants) with(c, normalized)
                (let assumptions = c.substitution().apply(&assumptions))
                (let constant = c.substitution().apply(constant))
                (prove_fully_normalize_const(decls, c.env(), assumptions, constant) => Constrained(constant, c1))
                (let c = c.seq(c1))
                (let normalized = append_normalized(c, normalized.upcast(), constant.upcast())))
            ---------------------------------------------------- ("constants")
            (prove_fully_normalize_consts(decls, env, assumptions, constants) => Constrained(normalized, c))
        )
    }
}

fn append_normalized<T>(c: &Constraints, normalized: Vec<T>, value: T) -> Vec<T>
where
    T: crate::rust::Fold<Output = T> + Clone,
{
    let mut normalized = c.substitution().apply(normalized);
    normalized.push(c.substitution().apply(value));
    normalized
}

#[cfg(test)]
mod tests {
    use super::{prove_fully_normalize_parameter, prove_fully_normalize_ty};
    use crate::grammar::{
        expr::Block, Binder, Const, Crates, Lt, Parameter, ParameterKind, PredicateTy,
        RigidConstData, RigidName, ScalarValue, Ty, Wcs,
    };
    use crate::prove::prove::{Constrained, Env, Program};
    use crate::rust::term;
    use formality_core::Upcast;
    use std::sync::Arc;

    fn program(source: &str) -> Program {
        let crates: Crates = term(source);
        crates.to_prove_decls()
    }

    fn normalized_ty(program: Program, ty: Ty) -> Ty {
        let (Constrained(ty, constraints), _) =
            prove_fully_normalize_ty(program, Env::default(), Wcs::t(), ty)
                .into_singleton()
                .expect("expected one full normal form");
        assert!(constraints.unconditionally_true());
        assert_eq!(constraints.env(), &Env::default());
        ty.upcast()
    }

    fn normalized_parameter(program: Program, parameter: impl Upcast<Parameter>) -> Parameter {
        let (Constrained(parameter, constraints), _) =
            prove_fully_normalize_parameter(program, Env::default(), Wcs::t(), parameter)
                .into_singleton()
                .expect("expected one full normal form");
        assert!(constraints.unconditionally_true());
        assert_eq!(constraints.env(), &Env::default());
        parameter
    }

    #[test]
    fn fully_normalizes_one_alias_step() {
        let program = program(
            "[
                crate test {
                    trait Family { type Output : []; }
                    impl Family for () { type Output = i32; }
                }
            ]",
        );

        assert_eq!(
            normalized_ty(program, term("<() as Family>::Output")),
            term("i32"),
        );
    }

    #[test]
    fn fully_normalizes_alias_chain() {
        let program = program(
            "[
                crate test {
                    trait First { type Output : []; }
                    trait Second { type Output : []; }
                    impl First for () { type Output = <() as Second>::Output; }
                    impl Second for () { type Output = i32; }
                }
            ]",
        );

        assert_eq!(
            normalized_ty(program, term("<() as First>::Output")),
            term("i32"),
        );
    }

    #[test]
    fn fully_normalizes_alias_nested_in_rigid_type() {
        let program = program(
            "[
                crate test {
                    trait Family { type Output : []; }
                    struct Wrapper<T> {}
                    impl Family for () { type Output = i32; }
                }
            ]",
        );

        assert_eq!(
            normalized_ty(program, term("Wrapper<<() as Family>::Output>")),
            term("Wrapper<i32>"),
        );
    }

    #[test]
    fn type_variable_has_no_ground_normal_form() {
        let mut env = Env::default();
        let variable = env.fresh_universal(ParameterKind::Ty);

        assert!(!prove_fully_normalize_ty(Program::empty(), env, Wcs::t(), variable,).is_proven());
    }

    #[test]
    fn predicate_type_has_no_codegen_normal_form() {
        let predicate_ty = PredicateTy::ForAll(Binder::dummy(Arc::new(Ty::unit())));

        assert!(!prove_fully_normalize_ty(
            Program::empty(),
            Env::default(),
            Wcs::t(),
            predicate_ty,
        )
        .is_proven());
    }

    #[test]
    fn preserves_supported_lifetimes_and_constants() {
        let static_lifetime: Parameter = Lt::Static.upcast();
        let erased_lifetime: Parameter = Lt::Erased.upcast();
        let scalar_constant: Parameter = ScalarValue::Usize(22).upcast();

        assert_eq!(
            normalized_parameter(Program::empty(), &static_lifetime),
            static_lifetime,
        );
        assert_eq!(
            normalized_parameter(Program::empty(), &erased_lifetime),
            erased_lifetime,
        );
        assert_eq!(
            normalized_parameter(Program::empty(), &scalar_constant),
            scalar_constant,
        );
    }

    #[test]
    fn recursively_normalizes_rigid_constant_parameters() {
        let program = program(
            "[
                crate test {
                    trait Family { type Output : []; }
                    impl Family for () { type Output = i32; }
                }
            ]",
        );
        let actual: Parameter = RigidConstData::new(
            RigidName::Tuple(0),
            vec![term::<Ty>("<() as Family>::Output")],
            vec![Const::Scalar(ScalarValue::Usize(22))],
        )
        .upcast();
        let expected: Parameter = RigidConstData::new(
            RigidName::Tuple(0),
            vec![term::<Ty>("i32")],
            vec![Const::Scalar(ScalarValue::Usize(22))],
        )
        .upcast();

        assert_eq!(normalized_parameter(program, actual), expected);
    }

    #[test]
    fn lifetime_and_const_variables_have_no_ground_normal_form() {
        for kind in [ParameterKind::Lt, ParameterKind::Const] {
            let mut env = Env::default();
            let variable = env.fresh_universal(kind);
            assert!(
                !prove_fully_normalize_parameter(Program::empty(), env, Wcs::t(), variable,)
                    .is_proven()
            );
        }
    }

    #[test]
    fn const_block_has_no_codegen_normal_form() {
        let block: Block = term("{}");
        let parameter: Parameter = Const::Block(block).upcast();

        assert!(!prove_fully_normalize_parameter(
            Program::empty(),
            Env::default(),
            Wcs::t(),
            parameter,
        )
        .is_proven());
    }
}
