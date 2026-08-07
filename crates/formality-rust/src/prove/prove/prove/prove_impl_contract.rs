use crate::grammar::{TraitImplBoundData, TraitRef, WhereClause};
use formality_core::judgment_fn;

judgment_fn! {
    /// Extract the implication represented by an instantiated impl declaration.
    ///
    /// Impl well-formedness assumes the returned conditions while checking the constructor body.
    /// Impl application proves those same conditions before invoking the constructor. Each caller
    /// applies its own validation frontier and logical polarity.
    pub(crate) fn impl_contract(
        trait_impl: TraitImplBoundData,
    ) => (TraitRef, Vec<WhereClause>) {
        debug(trait_impl)

        (
            (let header = trait_impl.trait_ref())
            ----------------------------- ("impl contract")
            (impl_contract(
                trait_impl @ TraitImplBoundData { where_clauses, .. },
            ) => (header, where_clauses))
        )
    }
}
