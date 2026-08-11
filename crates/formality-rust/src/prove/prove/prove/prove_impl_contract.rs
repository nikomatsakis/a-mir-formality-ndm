use crate::grammar::{
    AliasTy, AssociatedTyValue, AssociatedTyValueBoundData, AtomicPredicate, Binder, ImplItem,
    Parameter, Predicate, TraitId, TraitImplBoundData, TraitRef, Wc, Wcs, WhereClause,
};
use formality_core::{judgment_fn, Cons};

judgment_fn! {
    /// Extract the implication represented by an instantiated impl declaration.
    ///
    /// Impl well-formedness assumes the returned conditions while checking the constructor body.
    /// Impl application proves those same conditions before invoking the constructor. The returned
    /// definitions are the impl's oriented associated-type equalities; application exposes them as
    /// `Later` facts while it proves the conditions. Each caller applies its own validation
    /// frontier and logical polarity.
    pub(crate) fn impl_contract(
        trait_impl: TraitImplBoundData,
    ) => (TraitRef, Vec<WhereClause>, Wcs) {
        debug(trait_impl)

        (
            (let header @ TraitRef { trait_id, parameters } = trait_impl.trait_ref())
            (impl_definitions(trait_id, parameters, impl_items) => definitions)
            ----------------------------- ("impl contract")
            (impl_contract(
                trait_impl @ TraitImplBoundData { where_clauses, impl_items, .. },
            ) => (header, where_clauses, definitions))
        )
    }
}

judgment_fn! {
    /// Extract the oriented associated-type equalities fixed by an instantiated impl.
    fn impl_definitions(
        trait_id: TraitId,
        trait_parameters: Vec<Parameter>,
        impl_items: Vec<ImplItem>,
    ) => Wcs {
        debug(trait_id, trait_parameters, impl_items)

        (
            ----------------------------- ("empty")
            (impl_definitions(_trait_id, _trait_parameters, ()) => ())
        )

        (
            (impl_definitions(trait_id, trait_parameters, rest) => definitions)
            ----------------------------- ("function")
            (impl_definitions(
                trait_id,
                trait_parameters,
                Cons(ImplItem::Fn(_function), rest),
            ) => definitions)
        )

        (
            (let (gat_parameters, AssociatedTyValueBoundData {
                where_clauses: _,
                ty,
            }) = binder.open())
            (let alias = AliasTy::associated_ty(
                trait_id,
                id,
                gat_parameters.len(),
                (trait_parameters, gat_parameters),
            ))
            (let definition = Wc::Atomic(AtomicPredicate::Predicate(
                Predicate::alias_eq(alias, ty),
            )))
            (let definition = Wc::for_all(Binder::new(gat_parameters, definition)))
            (impl_definitions(trait_id, trait_parameters, rest) => definitions)
            ----------------------------- ("associated type")
            (impl_definitions(
                trait_id,
                trait_parameters,
                Cons(ImplItem::AssociatedTyValue(AssociatedTyValue { id, binder }), rest),
            ) => (definition, definitions))
        )
    }
}
