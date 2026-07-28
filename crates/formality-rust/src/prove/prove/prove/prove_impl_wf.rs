use crate::grammar::{
    AssociatedTyBoundData, AssociatedTyValueBoundData, Relation, TraitImpl, TraitImplBoundData,
    TraitItem, ValidationState, Wc, Wcs,
};
use crate::prove::prove::decls::Program;
use crate::prove::prove::prove::prove;
use crate::prove::prove::{trait_requirement, TraitRequirementBoundData};
use crate::prove::ToWcs;
use formality_core::{judgment_fn, Upcast};

use super::{constraints::Constraints, env::Env};

judgment_fn! {
    /// Prove that an impl declaration satisfies all requirements imposed by
    /// its trait.
    ///
    /// This is a closed judgment: it deliberately has no environment or
    /// assumptions supplied by its caller. The impl binder is instantiated
    /// universally. In particular, the impl itself is not available while
    /// proving its own well-formedness.
    pub(crate) fn prove_impl_wf(
        program: Program,
        trait_impl: TraitImpl,
    ) => () {
        debug(trait_impl, program)

        (
            (let (env, trait_impl) =
                Env::default().instantiate_universally(&trait_impl.binder))
            (let trait_ref = trait_impl.trait_ref())

            (let trait_def = program.trait_def(&trait_ref.trait_id))
            (trait_requirement(trait_def) => requirements)

            (for_all(requirement in requirements)
                (let requirement =
                    requirement.binder.instantiate_with(&trait_ref.parameters)?)
                (validate_impl_against_requirement(
                    program,
                    env,
                    trait_impl,
                    requirement) => c)
                (if c.unconditionally_true()))
            ----------------------------- ("requirements")
            (prove_impl_wf(program, trait_impl) => ())
        )
    }
}

judgment_fn! {
    /// Validate a selected impl against one instantiated trait requirement.
    fn validate_impl_against_requirement(
        program: Program,
        env: Env,
        trait_impl: TraitImplBoundData,
        requirement: TraitRequirementBoundData,
    ) => Constraints {
        debug(trait_impl, requirement, env, program)

        (
            (let assumptions = trait_impl.where_clauses.to_wcs().validated(ValidationState::A))
            (let goal = Wc::validate(ValidationState::A, Wc::for_all(supertrait)))
            (prove(program, env, assumptions, goal) => c)
            ----------------------------- ("supertrait")
            (validate_impl_against_requirement(
                program,
                env,
                trait_impl,
                TraitRequirementBoundData::Supertrait(supertrait),
            ) => c)
        )

        (
            (let assumptions = trait_impl.where_clauses.to_wcs().validated(ValidationState::A))
            (let goal = Wc::validate(ValidationState::A, Wc::for_all(outlives)))
            (prove(program, env, assumptions, goal) => c)
            ----------------------------- ("outlives")
            (validate_impl_against_requirement(
                program,
                env,
                trait_impl,
                TraitRequirementBoundData::Outlives(outlives),
            ) => c)
        )

        (
            // An associated type requirement derives from a trait like
            //
            // ```
            // trait Foo {
            //     type Bar<A>: Debug;
            // }
            // ```
            //
            // and has the form `forall<A> forall<I> Z: Debug`.
            // Where `I` is meant to be the value from the impl.
            //
            // Meanwhile the impl has something like this
            //
            // ```
            // impl<B> Foo for SomeType<B> {
            //     type Bar<C> = Vec<(C, B)>;
            // }
            // ```
            //
            // We have an environment with the impl parameters (`!B`) universally
            // instantiated. We create a GAT universal substitution `!A` from
            // the trait defintiion.
            (let (env, gat_subst) = env.universal_substitution(&associated.binder))

            // Extract the `type Bar<C> = Vec<(C, !B)>` from the impl
            // and instantiate it with `C = !A`, yielding `Vec<(!A, !B)>`.
            (if let Some(assoc_ty_value) = trait_impl.assoc_ty_value(&associated.id))
            (let AssociatedTyValueBoundData {
                where_clauses: _,
                ty: impl_ty,
            } = assoc_ty_value.binder.instantiate_with(&gat_subst)?)

            // Instantiate the GAt requirement first with `A = !A` and then
            // with `I = Vec<(!A, !B)>` to get `Vec<(!A, !B)>: Debug`.
            (let gat_goals = associated.binder
                .instantiate_with(&gat_subst)?
                .instantiate_with(vec![impl_ty.clone()])?)

            // The declaration-side GAT where-clauses determine when callers
            // may rely on the associated type's bounds.
            (let trait_def = program.trait_def(&trait_impl.trait_id))
            (let trait_data =
                trait_def.binder.instantiate_with(&trait_impl.trait_ref().parameters)?)
            (let trait_associated_ty = trait_data
                .trait_items
                .iter()
                .find_map(|item| match item {
                    TraitItem::AssociatedTy(associated_ty)
                        if associated_ty.id == associated.id =>
                    {
                        Some(associated_ty)
                    }
                    _ => None,
                })
                .ok_or_else(|| anyhow::anyhow!(
                    "trait has no associated type {:?}",
                    associated.id,
                ))?)
            (let AssociatedTyBoundData {
                ensures: _,
                where_clauses: trait_gat_wc,
            } = trait_associated_ty.binder.instantiate_with(&gat_subst)?)

            // Each associated value must itself be well formed and must
            // satisfy every bound promised by the trait. The impl and GAT
            // conditions are completed inputs; the conclusions are the
            // provisional requirements being verified.
            (let validation_conditions =
                (trait_impl.where_clauses.to_wcs(), trait_gat_wc.to_wcs())
                    .to_wcs()
                    .validated(ValidationState::B))
            (let value_wf: Wc = Relation::well_formed(impl_ty).upcast())
            (let validation_goals: Wcs = std::iter::once(value_wf.clone())
                .chain(gat_goals.iter())
                .map(|goal| {
                    Wc::implies(
                        &validation_conditions,
                        Wc::validate(ValidationState::A, goal),
                    )
                })
                .collect())
            (prove(program, env, (), validation_goals) => c)
            ----------------------------- ("associated type")
            (validate_impl_against_requirement(
                program,
                env,
                trait_impl,
                TraitRequirementBoundData::AssociatedTyRequirement(associated),
            ) => c)
        )
    }
}

#[cfg(test)]
mod tests {
    use super::prove_impl_wf;
    use crate::grammar::{Crates, TraitId};
    use crate::prove::prove::Program;
    use crate::rust::term;
    use formality_macros::test;

    fn program(source: &str) -> Program {
        let crates: Crates = term(source);
        crates.to_prove_decls()
    }

    fn impl_wf(program: &Program, trait_id: &str) -> bool {
        let candidate = program
            .raw_trait_impls_for(&term::<TraitId>(trait_id))
            .into_iter()
            .next()
            .unwrap();
        prove_impl_wf(program, candidate.trait_impl).is_proven()
    }

    #[test]
    fn impl_where_clause_can_verify_supertrait_requirement() {
        let program = program(
            "[
                crate test {
                    trait Super {}
                    trait Sub where Self: Super {}
                    impl<T> Sub for T where T: Super {}
                }
            ]",
        );

        assert!(impl_wf(&program, "Sub"));
    }

    #[test]
    fn missing_supertrait_requirement_is_not_wf() {
        let program = program(
            "[
                crate test {
                    trait Super {}
                    trait Sub where Self: Super {}
                    struct Ground {}
                    impl Sub for Ground {}
                }
            ]",
        );

        assert!(!impl_wf(&program, "Sub"));
    }

    #[test]
    fn impl_cannot_verify_its_own_associated_type_bound() {
        let program = program(
            "[
                crate test {
                    trait Foo {
                        type Bar: [Foo];
                    }

                    impl Foo for u32 {
                        type Bar = u32;
                    }
                }
            ]",
        );

        assert!(!impl_wf(&program, "Foo"));
    }

    #[test]
    fn mutual_impl_wf_cycle_is_not_a_proof() {
        let program = program(
            "[
                crate test {
                    trait Foo {}

                    trait Bar {
                        type Baz: [Foo];
                    }

                    impl<T> Foo for T where T: Bar {}

                    impl Bar for u32 {
                        type Baz = u32;
                    }
                }
            ]",
        );

        assert!(!impl_wf(&program, "Bar"));
    }

}
