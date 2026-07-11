//! Elaboration of Rust trait declarations into implied requirements.

use crate::grammar::{
    AliasTy, AssociatedTy, AssociatedTyBoundData, Binder, Parameter, Predicate, Relation, Trait,
    TraitBoundData, TraitItem, TraitRef, Wc,
};
use crate::prove::ToWcs;
use formality_core::{judgment_fn, Upcast};
use formality_macros::term;

/// A trait requirement is a rule like
/// `forall<T> Implemented(T: Eq) => Implemented(T: PartialEq)`.
///
/// The `source` trait-ref implies the `required` where-clause. The ordinary solver uses this
/// implication to elaborate implied bounds. Impl validation will also use these requirements
/// when validating a selected impl.
#[term]
pub struct TraitRequirement {
    pub binder: Binder<TraitRequirementBoundData>,
}

/// The data bound by a [`TraitRequirement`].
#[term($source => $required)]
pub struct TraitRequirementBoundData {
    /// The implemented trait-ref that gives rise to this requirement.
    pub source: TraitRef,

    /// The where-clause implied by the source trait-ref.
    pub required: Wc,
}

judgment_fn! {
    /// Elaborate one implied requirement from a Rust trait declaration.
    ///
    /// A trait can yield several requirements, represented by the several successful results of
    /// this judgment.
    pub fn trait_requirement(
        trait_def: Trait,
    ) => TraitRequirement {
        debug(trait_def)

        // The Rust declaration `trait Eq: PartialEq` is lowered to a where-clause on `Self`.
        // It gives rise to `forall<Self> Eq(Self) => PartialEq(Self)`.
        (
            (let (variables, TraitBoundData { where_clauses, trait_items: _ }) = trait_def.binder.open())
            (let self_parameter: Parameter = variables[0].upcast())
            (required in where_clauses.to_wcs())
            // A clause that is not an implied bound on `Self` makes this rule inapplicable.
            (implied_trait_clause(self_parameter, required) => ())!
            (let source = TraitRef::new(&trait_def.id, variables))
            (let requirement = TraitRequirement::new(Binder::new(
                variables,
                TraitRequirementBoundData::new(source, required),
            )))
            ----------------------------- ("trait where-clause")
            (trait_requirement(trait_def) => requirement)
        )

        // The Rust declaration
        //
        //     trait Foo {
        //         type Bar<T>: Ord where T: Copy;
        //     }
        //
        // gives rise to
        //
        //     forall<Self> Foo(Self) =>
        //         forall<T> if { Copy(T) } Ord(<Self as Foo>::Bar<T>).
        //
        // The associated type's variables are bound inside `required`; the trait's variables
        // bind the entire requirement, including its `source`.
        (
            (let (variables, TraitBoundData { where_clauses: _, trait_items }) = trait_def.binder.open())
            (trait_item in trait_items)
            (if let TraitItem::AssociatedTy(AssociatedTy { id, binder }) = trait_item)
            (let (associated_variables, AssociatedTyBoundData { ensures, where_clauses }) = binder.open())
            // Functions and associated types without bounds contribute no requirement.
            (ensure in ensures)!
            (let alias = AliasTy::associated_ty(
                &trait_def.id,
                id,
                associated_variables.len(),
                (variables, associated_variables),
            ))
            (let required =
                Wc::for_all(Binder::new(associated_variables,
                    Wc::implies(where_clauses, ensure.to_wc(alias)))))
            (let source = TraitRef::new(&trait_def.id, variables))
            (let requirement = TraitRequirement::new(Binder::new(
                variables,
                TraitRequirementBoundData::new(source, required),
            )))
            ----------------------------- ("associated type bound")
            (trait_requirement(trait_def) => requirement)
        )
    }
}

judgment_fn! {
    /// Classify a lowered trait where-clause as an implied requirement on `Self`.
    fn implied_trait_clause(
        self_parameter: Parameter,
        clause: Wc,
    ) => () {
        debug(clause, self_parameter)

        (
            (if trait_ref.parameters[0] == *self_parameter)!
            ----------------------------- ("trait bound on Self")
            (implied_trait_clause(self_parameter, Predicate::IsImplemented(trait_ref)) => ())
        )

        (
            (if source == self_parameter)!
            ----------------------------- ("outlives bound on Self")
            (implied_trait_clause(self_parameter, Relation::Outlives(source, _target)) => ())
        )

        (
            (implied_trait_clause(self_parameter, binder.peek()) => ())
            ----------------------------- ("higher-ranked trait requirement")
            (implied_trait_clause(self_parameter, Wc::ForAll(binder)) => ())
        )

        (
            (implied_trait_clause(self_parameter, consequence.as_ref()) => ())
            ----------------------------- ("conditional trait requirement")
            (implied_trait_clause(self_parameter, Wc::Implies(_conditions, consequence)) => ())
        )
    }
}
