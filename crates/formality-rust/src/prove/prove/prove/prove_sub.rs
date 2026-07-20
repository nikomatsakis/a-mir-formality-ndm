use crate::grammar::{AliasTy, Lt, Parameter, Relation, RigidTy, TyData, Wcs};
use crate::prove::prove::Constrained;
use formality_core::{judgment_fn, Downcast};

use crate::prove::prove::prove::prove_outlives::prove_outlives;
use crate::prove::prove::{
    decls::Program,
    prove::{
        prove_after::prove_after, prove_after_validation::prove_after_validation,
        prove_normalize::prove_normalize_after_validation,
    },
};

use super::{constraints::Constraints, env::Env};

judgment_fn! {
    pub fn prove_sub(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        a: Parameter,
        b: Parameter,
    ) => Constraints {
        debug(a, b, assumptions, env)

        assert(a.kind() == b.kind())

        trivial(a == b => Constraints::none(env))

        (
            (prove_normalize_after_validation(
                decls,
                env,
                assumptions,
                TyData::alias_ty(alias),
            ) => Constrained(y, c))
            (prove_after_validation(decls, c, assumptions, Relation::sub(y, z)) => c)
            ----------------------------- ("normalize alias left after validation")
            (prove_sub(decls, env, assumptions, TyData::AliasTy(alias), z) => c)
        )

        (
            (if let None = x.downcast::<AliasTy>())!
            (prove_normalize_after_validation(decls, env, assumptions, x) => Constrained(y, c))
            (prove_after(decls, c, assumptions, Relation::sub(y, z)) => c)
            ----------------------------- ("normalize non-alias left now")
            (prove_sub(decls, env, assumptions, x, z) => c)
        )

        (
            (prove_normalize_after_validation(
                decls,
                env,
                assumptions,
                TyData::alias_ty(alias),
            ) => Constrained(z, c))
            (prove_after_validation(decls, c, assumptions, Relation::sub(x, &z)) => c)
            ----------------------------- ("normalize alias right after validation")
            (prove_sub(decls, env, assumptions, x, TyData::AliasTy(alias)) => c)
        )

        (
            (if let None = y.downcast::<AliasTy>())!
            (prove_normalize_after_validation(decls, env, assumptions, y) => Constrained(z, c))
            (prove_after(decls, c, assumptions, Relation::sub(x, &z)) => c)
            ----------------------------- ("normalize non-alias right now")
            (prove_sub(decls, env, assumptions, x, y) => c)
        )

        (
            (let RigidTy { name: a_name, parameters: a_parameters } = a)
            (let RigidTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)!
            (prove_after(decls, env, assumptions, Wcs::all_sub(a_parameters, b_parameters)) => c)
            ----------------------------- ("rigid")
            (prove_sub(decls, env, assumptions, TyData::RigidTy(a), TyData::RigidTy(b)) => c)
        )

        (
            (prove_outlives(decls, env, assumptions, a, b) => c)
            ----------------------------- ("lifetime => outlives")
            (prove_sub(decls, env, assumptions, a: Lt, b: Lt) => c)
        )
    }
}

#[cfg(test)]
mod test {
    use std::sync::Arc;

    use crate::grammar::{Parameter, ValidationState, Wc};
    use crate::prove::prove::decls::Program;
    use crate::rust::term;
    use formality_macros::test;

    use super::prove_sub;

    fn normalization_decls() -> Program {
        Program {
            crates: Arc::new(Program::program_from_items(vec![
                term("trait Marker where {}"),
                term("trait Family where { type Output : []; }"),
                term("impl Family for u32 where u32 : Marker { type Output = bool; }"),
            ])),
            ..Program::empty()
        }
    }

    fn continuation_decls() -> Program {
        Program {
            crates: Arc::new(Program::program_from_items(vec![
                term("trait Family where { type Output : []; }"),
                term("impl Family for u32 { type Output = u32; }"),
            ])),
            ..Program::empty()
        }
    }

    #[test]
    fn subtyping_alias_enters_post_validation_context() {
        let result = prove_sub(
            normalization_decls(),
            (),
            Wc::validate(ValidationState::A, term::<Wc>("Marker(u32)")),
            term::<Parameter>("<u32 as Family>::Output"),
            term::<Parameter>("bool"),
        );

        assert!(result.is_proven());
    }

    #[test]
    fn subtyping_after_alias_normalization_stays_post_validation() {
        let result = prove_sub(
            continuation_decls(),
            (),
            Wc::validate(ValidationState::A, term::<Wc>("u32 = bool")),
            term::<Parameter>("<u32 as Family>::Output"),
            term::<Parameter>("bool"),
        );

        assert!(result.is_proven());
    }
}
