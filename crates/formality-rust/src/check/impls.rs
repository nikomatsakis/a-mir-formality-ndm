use anyhow::bail;

use crate::grammar::{
    Adt, AdtBoundData, AdtId, AssociatedTy, AssociatedTyBoundData, AssociatedTyValue,
    AssociatedTyValueBoundData, Binder, CrateId, Fallible, Fn, FnBoundData, ImplItem, InputArg,
    MaybeFnBody, NegTraitImpl, NegTraitImplBoundData, Predicate, Relation, RigidName, Substitution,
    Trait, TraitBoundData, TraitImpl, TraitImplBoundData, TraitItem, TraitRef, Ty, ValueId, Wcs,
};
use crate::prove::prove::{
    prove_impl_wf, trait_associated_ty, trait_input_wf_requirements, Env, Program, Safety,
};
use crate::rust::Term;
use formality_core::{judgment::ProofTree, judgment_fn, Downcasted};

judgment_fn! {
    pub(super) fn check_trait_impl(
        program: Program,
        trait_impl: TraitImpl,
        crate_id: CrateId,
    ) => () {
        debug(program, trait_impl, crate_id)
        (
            (let (env, bound_data) = Env::default().instantiate_universally(binder))
            (let TraitImplBoundData { trait_id, self_ty, trait_parameters, where_clauses, impl_items } = bound_data)
            (let trait_ref @ TraitRef { parameters: trait_ref_parameters, .. } =
                trait_id.with(self_ty, trait_parameters))
            (let trait_decl = program.program().trait_named(trait_id)?)
            (let input_wf_requirements =
                trait_input_wf_requirements(trait_decl, trait_ref)?)

            (super::where_clauses::prove_where_clauses_well_formed(program, env, where_clauses, where_clauses) => ())
            (super::prove_goal(program, env, where_clauses, input_wf_requirements) => ())
            (super::prove_not_goal(program, env, where_clauses, Predicate::not_implemented(trait_ref)) => ())

            (let Trait { binder: trait_binder, .. } = trait_decl)
            (let TraitBoundData { where_clauses: _, trait_items } =
                trait_binder.instantiate_with(trait_ref_parameters)?)
            (check_safety_matches(trait_decl, trait_impl) => ())

            (for_all(impl_item in impl_items)
                (check_trait_impl_item(program, env, where_clauses, trait_items, impl_item, crate_id) => ()))

            (check_unique_impl_item_names(impl_items) => ())
            (check_all_required_items_present(trait_items, impl_items) => ())

            // Impl well-formedness is closed to caller assumptions. `prove_impl_wf` introduces
            // the header locally at the construction frontier appropriate to each requirement, but
            // never as an ordinary trait assumption; the impl's where-clauses are likewise
            // available only in validated form.
            (prove_impl_wf(program, trait_impl) => ())

            ---- ("check_trait_impl")
            (check_trait_impl(
                program,
                trait_impl @ TraitImpl { binder, safety: _ },
                crate_id,
            ) => ())
        )
    }
}

judgment_fn! {
    pub(super) fn check_neg_trait_impl(
        program: Program,
        trait_impl: NegTraitImpl,
    ) => () {
        debug(program, trait_impl)

        (
            (fail "negative impls cannot be unsafe")
            ---- ("check_neg_trait_impl")
            (check_neg_trait_impl(program, NegTraitImpl { binder: _, safety: Safety::Unsafe }) => ())
        )

        (
            (let (env, bound_data) = Env::default().instantiate_universally(binder))
            (let NegTraitImplBoundData { trait_id, self_ty, trait_parameters, where_clauses } = bound_data)
            (let trait_ref = trait_id.with(self_ty, trait_parameters))
            (let trait_decl = program.program().trait_named(trait_id)?)
            (let input_wf_requirements =
                trait_input_wf_requirements(trait_decl, trait_ref)?)
            (super::where_clauses::prove_where_clauses_well_formed(program, env, where_clauses, where_clauses) => ())
            (super::prove_goal(program, env, where_clauses, input_wf_requirements) => ())
            (super::prove_not_goal(program, env, where_clauses, Predicate::is_implemented(trait_ref)) => ())
            ---- ("check_neg_trait_impl")
            (check_neg_trait_impl(program, NegTraitImpl { binder, safety: Safety::Safe }) => ())
        )
    }
}

judgment_fn! {
    /// Validate that the declared safety of an impl matches the one from the trait declaration.
    fn check_safety_matches(
        trait_decl: Trait,
        trait_impl: TraitImpl,
    ) => () {
        debug(trait_decl, trait_impl)
        (
            (if trait_decl.safety == trait_impl.safety)
            ---- ("safety matches")
            (check_safety_matches(trait_decl, trait_impl) => ())
        )
    }
}

/// Check that every required trait item has a corresponding impl item.
/// A trait fn is required if it has no default body (`NoFnBody`).
/// Associated types are always required (no defaults supported yet).
fn check_all_required_items_present(
    trait_items: &[TraitItem],
    impl_items: &[ImplItem],
) -> Fallible<ProofTree> {
    for trait_item in trait_items {
        match trait_item {
            TraitItem::Fn(trait_fn) => {
                let (_, bound_data) = trait_fn.binder.open();
                if matches!(bound_data.body, MaybeFnBody::NoFnBody) {
                    if !impl_items
                        .iter()
                        .downcasted::<Fn>()
                        .any(|impl_fn| impl_fn.id == trait_fn.id)
                    {
                        bail!(
                            "not all trait items implemented, missing: `{:?}`",
                            trait_fn.id
                        );
                    }
                }
            }
            TraitItem::AssociatedTy(trait_assoc_ty) => {
                if !impl_items
                    .iter()
                    .downcasted::<AssociatedTyValue>()
                    .any(|impl_assoc| impl_assoc.id == trait_assoc_ty.id)
                {
                    bail!(
                        "not all trait items implemented, missing: `{:?}`",
                        trait_assoc_ty.id
                    );
                }
            }
        }
    }
    Ok(ProofTree::leaf("check_all_required_items_present"))
}

judgment_fn! {
    fn check_trait_impl_item(
        program: Program,
        env: Env,
        assumptions: Wcs,
        trait_items: Vec<TraitItem>,
        impl_item: ImplItem,
        crate_id: CrateId,
    ) => () {
        debug(program, env, assumptions, impl_item, crate_id)

        (
            (check_fn_in_impl(program, env, assumptions, trait_items, v, crate_id) => ())
            ---- ("fn in impl")
            (check_trait_impl_item(program, env, assumptions, trait_items, ImplItem::Fn(v), crate_id) => ())
        )

        (
            (check_associated_ty_value(program, env, assumptions, trait_items, v) => ())
            ---- ("associated ty value")
            (check_trait_impl_item(program, env, assumptions, trait_items, ImplItem::AssociatedTyValue(v), _crate_id) => ())
        )
    }
}

fn trait_fn<'t>(trait_items: &'t [TraitItem], id: &ValueId) -> Fallible<&'t Fn> {
    trait_items
        .iter()
        .find_map(|item| match item {
            TraitItem::Fn(function) if function.id == *id => Some(function),
            _ => None,
        })
        .ok_or_else(|| anyhow::anyhow!("trait has no function named `{id:?}`"))
}

judgment_fn! {
    fn check_fn_in_impl(
        program: Program,
        env: Env,
        impl_assumptions: Wcs,
        trait_items: Vec<TraitItem>,
        ii_fn: Fn,
        crate_id: CrateId,
    ) => () {
        debug(program, env, impl_assumptions, ii_fn, crate_id)
        (
            // Find the corresponding function from the trait
            (let ti_fn @ Fn { binder: ti_binder, .. } =
                trait_fn(trait_items, ii_id)?)

            // A safe trait call must not dispatch to an unsafe override (or
            // vice versa); they are the same callable interface.
            (if ii_fn.safety == ti_fn.safety)

            // Check the fn itself
            (super::fns::check_fn(program, env, impl_assumptions, ii_fn, crate_id) => ())

            // Merge binders and instantiate universally
            (let merged_binder = merge_binders(ii_binder, ti_binder)?)
            (let (env, (ii_bound, ti_bound)) = env.instantiate_universally(merged_binder))
            (let FnBoundData { input_args: ii_input_args, output_ty: ii_output_ty, where_clauses: ii_where_clauses, body: _ } = ii_bound)
            (let FnBoundData { input_args: ti_input_args, output_ty: ti_output_ty, where_clauses: ti_where_clauses, body: _ } = ti_bound)

            // Prove impl where-clauses follow from trait where-clauses
            (super::prove_goal(program, env, (impl_assumptions, ti_where_clauses), ii_where_clauses) => ())

            // Check argument count matches
            (if ii_input_args.len() == ti_input_args.len())

            // Check each argument: trait arg is subtype of impl arg (contravariance)
            (for_all(pair in ii_input_args.iter().zip(ti_input_args.iter()))
                (let (
                    InputArg { ty: ii_input_ty, .. },
                    InputArg { ty: ti_input_ty, .. },
                ) = pair)
                (super::prove_goal(program, env, (impl_assumptions, ii_where_clauses), Relation::sub(ti_input_ty, ii_input_ty)) => ()))

            // Check return type: impl return is subtype of trait return (covariance)
            (super::prove_goal(program, env, (impl_assumptions, ii_where_clauses), Relation::sub(ii_output_ty, ti_output_ty)) => ())

            ---- ("check_fn_in_impl")
            (check_fn_in_impl(
                program,
                env,
                impl_assumptions,
                trait_items,
                ii_fn @ Fn {
                    id: ii_id,
                    binder: ii_binder,
                    ..
                },
                crate_id,
            ) => ())
        )
    }
}

fn check_unique_impl_item_names(impl_items: &[ImplItem]) -> Fallible<ProofTree> {
    let methods: Vec<&Fn> = impl_items
        .iter()
        .filter_map(|item| match item {
            ImplItem::Fn(method) => Some(method),
            ImplItem::AssociatedTyValue(_) => None,
        })
        .collect();
    for (index, method) in methods.iter().enumerate() {
        if methods[..index]
            .iter()
            .any(|earlier| earlier.id == method.id)
        {
            bail!("multiple impl methods named `{:?}`", method.id);
        }
    }

    let associated_types: Vec<&AssociatedTyValue> = impl_items
        .iter()
        .filter_map(|item| match item {
            ImplItem::Fn(_) => None,
            ImplItem::AssociatedTyValue(value) => Some(value),
        })
        .collect();
    for (index, associated_type) in associated_types.iter().enumerate() {
        if associated_types[..index]
            .iter()
            .any(|earlier| earlier.id == associated_type.id)
        {
            bail!(
                "multiple impl associated types named `{:?}`",
                associated_type.id
            );
        }
    }

    Ok(ProofTree::leaf("check_unique_impl_item_names"))
}

judgment_fn! {
    fn check_associated_ty_value(
        program: Program,
        impl_env: Env,
        impl_assumptions: Wcs,
        trait_items: Vec<TraitItem>,
        impl_value: AssociatedTyValue,
    ) => () {
        debug(program, impl_env, impl_assumptions, impl_value)
        (
            // Find the corresponding associated type from the trait
            (trait_associated_ty(trait_items, id) =>
                AssociatedTy { binder: trait_binder, .. })!

            // Merge binders and instantiate universally
            (let merged_binder = merge_binders(binder, trait_binder)?)
            (let (env, (ii_bound, ti_bound)) =
                impl_env.instantiate_universally(merged_binder))
            (let AssociatedTyValueBoundData { where_clauses: ii_where_clauses, ty: _ } = ii_bound)
            (let AssociatedTyBoundData { ensures: _, where_clauses: ti_where_clauses } = ti_bound)

            // Prove impl where-clauses are well-formed
            (super::where_clauses::prove_where_clauses_well_formed(program, env, (impl_assumptions, ii_where_clauses), ii_where_clauses) => ())

            // Prove impl where-clauses follow from trait where-clauses
            (super::prove_goal(program, env, (impl_assumptions, ti_where_clauses), ii_where_clauses) => ())

            ---- ("check_associated_ty_value")
            (check_associated_ty_value(
                program,
                impl_env,
                impl_assumptions,
                trait_items,
                AssociatedTyValue { id, binder },
            ) => ())
        )
    }
}

/// Given a binder from some impl item `I` and a binder from the corresponding trait item `T`,
/// check that the binders have the same number/kinds of parameters, and then merge them
/// into a single binder over `(I, T)`
fn merge_binders<I: Term, T: Term>(
    impl_binder: &Binder<I>,
    trait_binder: &Binder<T>,
) -> Fallible<Binder<(I, T)>> {
    if impl_binder.kinds() != trait_binder.kinds() {
        bail!(
            "distinct binder kinds: impl {:?} vs trait {:?}",
            impl_binder.kinds(),
            trait_binder.kinds()
        );
    }

    let (impl_names, impl_value) = impl_binder.open();

    let (trait_names, trait_value) = trait_binder.open();

    assert_eq!(impl_names.len(), trait_names.len());
    let trait_to_impl_subst: Substitution = trait_names.iter().zip(impl_names.iter()).collect();

    Ok(Binder::new(
        &impl_names,
        (impl_value, trait_to_impl_subst.apply(&trait_value)),
    ))
}

/// Extract the ADT id from a Drop impl's self type (peeking through the binder).
fn drop_impl_adt_id(trait_impl: &TraitImpl) -> Fallible<&AdtId> {
    let bound = trait_impl.binder.peek();
    let Ty::RigidTy(rigid) = &bound.self_ty else {
        bail!(
            "Drop impl self type must be a struct or enum, got `{:?}`",
            bound.self_ty
        );
    };
    let RigidName::AdtId(adt_id) = &rigid.name else {
        bail!(
            "Drop impl self type must be a struct or enum, got `{:?}`",
            bound.self_ty
        );
    };
    Ok(adt_id)
}

judgment_fn! {
    /// Check that a `Drop` impl is "always applicable": for any instance of the ADT
    /// (with its where-clauses satisfied), the Drop impl must apply.
    pub(super) fn check_drop_impl_always_applicable(
        program: Program,
        trait_impl: TraitImpl,
    ) => () {
        debug(program, trait_impl)

        (
            (if trait_impl.trait_id().as_str() != "Drop")!
            ---- ("not a Drop impl")
            (check_drop_impl_always_applicable(program, trait_impl) => ())
        )

        (
            (if trait_impl.trait_id().as_str() == "Drop")!
            // Extract the ADT id and look up its definition.
            (let adt_id = drop_impl_adt_id(trait_impl)?)
            (let adt = program.program().adt_item_named(adt_id)?.to_adt())
            (let Adt { binder: adt_binder, .. } = adt)
            // Universally instantiate the ADT: forall<T...> { (T: Bounds) => ... }
            (let (env, adt_vars) = Env::default().universal_substitution(adt_binder))
            (let AdtBoundData { where_clauses, .. } =
                adt_binder.instantiate_with(adt_vars)?)
            (let adt_self_ty = Ty::rigid(adt_id, adt_vars))
            // Prove: under the ADT's where-clauses, Drop is implemented for the ADT.
            // This will find the impl, unify its self type, and verify its where-clauses.
            (let drop_trait_ref = crate::grammar::TraitId::new("Drop").with(adt_self_ty, ()))
            (super::prove_goal(program, env, where_clauses,
                Predicate::is_implemented(drop_trait_ref)) => ())
            ---- ("Drop impl is always applicable")
            (check_drop_impl_always_applicable(program, trait_impl) => ())
        )
    }
}
