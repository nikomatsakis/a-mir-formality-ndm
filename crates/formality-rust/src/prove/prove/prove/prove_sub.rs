use crate::grammar::{AliasTy, Lt, Parameter, Relation, RigidTy, TyData, Wcs};
use crate::prove::prove::Constrained;
use formality_core::{judgment_fn, Downcast};

use crate::prove::prove::prove::prove_outlives::prove_outlives;
use crate::prove::prove::{
    decls::Program,
    prove::{prove_after::prove_after, prove_normalize::prove_normalize},
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
            (prove_normalize(
                decls,
                env,
                assumptions,
                TyData::alias_ty(alias),
            ) => Constrained(y, c))
            (prove_after(decls, c, assumptions, Relation::sub(y, z)) => c)
            ----------------------------- ("normalize alias left")
            (prove_sub(decls, env, assumptions, TyData::AliasTy(alias), z) => c)
        )

        (
            (if let None = x.downcast::<AliasTy>())!
            (prove_normalize(decls, env, assumptions, x) => Constrained(y, c))
            (prove_after(decls, c, assumptions, Relation::sub(y, z)) => c)
            ----------------------------- ("normalize non-alias left now")
            (prove_sub(decls, env, assumptions, x, z) => c)
        )

        (
            (prove_normalize(
                decls,
                env,
                assumptions,
                TyData::alias_ty(alias),
            ) => Constrained(z, c))
            (prove_after(decls, c, assumptions, Relation::sub(x, &z)) => c)
            ----------------------------- ("normalize alias right")
            (prove_sub(decls, env, assumptions, x, TyData::AliasTy(alias)) => c)
        )

        (
            (if let None = y.downcast::<AliasTy>())!
            (prove_normalize(decls, env, assumptions, y) => Constrained(z, c))
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

    use crate::grammar::{Parameter, TraitId, ValidationContext, ValidationState, Wc};
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
    fn subtyping_alias_uses_only_explicit_assumptions() {
        let program = normalization_decls();
        let alias = term::<Parameter>("<u32 as Family>::Output");
        let target = term::<Parameter>("bool");

        let from_validation = prove_sub(
            &program,
            (),
            Wc::validate(
                ValidationContext::new(ValidationState::A, TraitId::new("Family")),
                term::<Wc>("Marker(u32)"),
            ),
            &alias,
            &target,
        );
        assert!(!from_validation.is_proven());

        let from_ordinary = prove_sub(program, (), term::<Wc>("Marker(u32)"), alias, target);
        assert!(from_ordinary.is_proven());
    }

    #[test]
    fn subtyping_continuation_uses_only_explicit_assumptions() {
        let result = prove_sub(
            continuation_decls(),
            (),
            Wc::validate(
                ValidationContext::new(ValidationState::A, TraitId::new("Family")),
                term::<Wc>("u32 = bool"),
            ),
            term::<Parameter>("<u32 as Family>::Output"),
            term::<Parameter>("bool"),
        );

        // Normalizing the alias yields `u32`, but the remaining `u32 <: bool` goal receives the
        // caller's assumptions unchanged. The wrapped equality therefore cannot discharge the
        // ordinary subtyping goal.
        assert!(!result.is_proven());
    }
}
