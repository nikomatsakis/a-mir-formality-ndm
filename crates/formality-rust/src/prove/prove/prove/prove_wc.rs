use crate::grammar::{Predicate, Relation, Wc, WcData, Wcs};
use crate::prove::ToWcs;
use formality_core::judgment_fn;

use crate::prove::prove::{
    decls::Program,
    prove::{
        combinators::for_all,
        env::{Bias, Env},
        is_local::{is_local_trait_ref, may_be_remote},
        prove_after::prove_after,
        prove_const_has_type::prove_const_has_type,
        prove_eq::prove_eq,
        prove_outlives::prove_outlives,
        prove_sub::prove_sub,
        prove_via_assumption::prove_via_assumption,
        prove_via_impl::prove_via_impl,
        prove_wf::prove_wf,
    },
    requirements::trait_requirement,
};

use super::constraints::{Constrained, Constraints};

judgment_fn! {
    /// The "heart" of the trait system -- prove that a where-clause holds given a set of declarations, variable environment, and set of assumptions.
    /// If successful, returns the constraints under which the where-clause holds.
    pub fn prove_wc(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        goal: Wc,
    ) => Constraints {
        debug(goal, assumptions, env)

        (
            (let (env, subst) = env.universal_substitution(binder))
            (let p1 = binder.instantiate_with(&subst).unwrap())
            (prove_wc(decls, env, assumptions, p1) => c)
            --- ("forall")
            (prove_wc(decls, env, assumptions, WcData::ForAll(binder)) => c.pop_subst(&subst))
        )

        (
            (prove_wc(decls, env, (assumptions, p1), p2) => c)
            --- ("implies")
            (prove_wc(decls, env, assumptions, WcData::Implies(p1, p2)) => c)
        )

        (
            (a in assumptions)!
            (prove_via_assumption(decls, env, assumptions, a, goal) => c)
            ----------------------------- ("assumption - predicate")
            (prove_wc(decls, env, assumptions, WcData::Predicate(goal)) => c)
        )
        (
            (a in assumptions)!
            (prove_via_assumption(decls, env, assumptions, a, goal) => c)
            ----------------------------- ("assumption - relation")
            (prove_wc(decls, env, assumptions, WcData::Relation(goal)) => c)
        )


        // This rule is: prove `T: Foo<U>` holds on the basis of an `impl<A,B> Foo<B> for A where WC` impl somewhere.
        (
            (candidate in decls.raw_trait_impls_for(&trait_ref.trait_id))!
            (prove_via_impl(
                decls,
                env,
                assumptions,
                trait_ref,
                candidate,
            ) => Constrained(application, c))
            (let c = application.proof_constraints(c))
            ----------------------------- ("positive impl")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c)
        )

        (
            (if env.bias() == Bias::Completeness)!
            (may_be_remote(decls, env, assumptions, trait_ref) => c)
            ----------------------------- ("coherence / remote impl")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c)
        )

        (
            (i in decls.neg_trait_impls_for(&trait_ref.trait_id))
            (let (env, subst) = env.existential_substitution(&i.binder))
            (let i = i.binder.instantiate_with(&subst).unwrap())
            (let impl_trait_ref = i.trait_ref())
            (let impl_where_clauses = i.where_clauses.to_wcs())
            (prove_after(decls, env, assumptions, Wcs::all_eq(&trait_ref.parameters, &impl_trait_ref.parameters)) => c)
            (prove_after(decls, c, assumptions, impl_where_clauses) => c)
            ----------------------------- ("negative impl")
            (prove_wc(decls, env, assumptions, Predicate::NotImplemented(trait_ref)) => c.pop_subst(&subst))
        )

        (
            (prove_eq(decls, env, assumptions, alias_ty, ty) => c)
            ----------------------------- ("alias eq")
            (prove_wc(decls, env, assumptions, Predicate::AliasEq(alias_ty, ty)) => c)
        )

        // The Rust declaration `trait Eq: PartialEq` gives rise to the requirement template
        // `forall<T> Eq(T) => PartialEq(T)`. Apply that requirement by backward chaining: for
        // the goal `PartialEq(U)`, instantiate `T` with an inference variable, match the
        // requirement's `required` clause against the goal, and then prove its `source`
        // (`Eq(U)`). This lazily elaborates implied bounds rather than adding all of their
        // consequences to the assumptions eagerly.
        (
            (trait_def in decls.traits())
            // Requirement generation is partial, so commit only after this trait yields one.
            (trait_requirement(trait_def) => requirement)!
            (let (env, subst) = env.existential_substitution(&requirement.binder))
            (let requirement = requirement.binder.instantiate_with(&subst).unwrap())
            (prove_via_assumption(decls, env, assumptions, &requirement.required, trait_ref) => c)
            (prove_after(decls, c, assumptions, &requirement.source) => c)
            ----------------------------- ("trait requirement")
            (prove_wc(decls, env, assumptions, Predicate::IsImplemented(trait_ref)) => c.pop_subst(&subst))
        )

        (
            (prove_eq(decls, env, assumptions, a, b) => c)
            ----------------------------- ("eq")
            (prove_wc(decls, env, assumptions, Relation::Equals(a, b)) => c)
        )

        (
            (prove_sub(decls, env, assumptions, a, b) => c)
            ----------------------------- ("subtype")
            (prove_wc(decls, env, assumptions, WcData::Relation(Relation::Sub(a, b))) => c)
        )

        // For example, `trait Foo<T> where T: Debug` means that the trait-ref `S: Foo<U>` is
        // well formed only if `S` and `U` are well formed and `U: Debug`. In general, substitute
        // the trait-ref's parameters into the Rust trait declaration and prove every resulting
        // where-clause. This checks every trait-header condition; it is distinct from the
        // trait-requirement rule above, which exposes only the clauses classified as implied
        // requirements.
        (
            (for_all(decls, env, assumptions, &trait_ref.parameters, &prove_wf) => c)
            (let trait_def = decls.trait_def(&trait_ref.trait_id))
            (let trait_data = trait_def.binder.instantiate_with(&trait_ref.parameters).unwrap())
            (let trait_where_clauses = trait_data.where_clauses.to_wcs())
            (prove_after(decls, c, assumptions, trait_where_clauses) => c)
            ----------------------------- ("trait well formed")
            (prove_wc(decls, env, assumptions, Predicate::WellFormedTraitRef(trait_ref)) => c)
        )

        (
            (is_local_trait_ref(decls, env, assumptions, trait_ref) => c)
            ----------------------------- ("trait ref is local")
            (prove_wc(decls, env, assumptions, Predicate::IsLocal(trait_ref)) => c)
        )

        (
            (prove_outlives(decls, env, assumptions, a, b) => c)
            ----------------------------- ("outlives")
            (prove_wc(decls, env, assumptions, Relation::Outlives(a, b)) => c)
        )


        (
            (prove_wf(decls, env, assumptions, p) => c)
            ----------------------------- ("parameter well formed")
            (prove_wc(decls, env, assumptions, Relation::WellFormed(p)) => c)
        )

        (
            (prove_const_has_type(decls, env, assumptions, constant) => (ty_constant, c))
            (prove_after(decls, c, assumptions, Relation::equals(ty_constant, ty)) => c)
            ----------------------------- ("const has ty")
            (prove_wc(decls, env, assumptions, Predicate::ConstHasType(constant, ty)) => c)
        )
    }
}
