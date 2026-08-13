//! A syntactically derived strict partial order between traits.
//!
//! The source graph records when one trait declaration depends on another. The
//! graph is permitted to contain cycles. We order two traits only when they are
//! in distinct strongly connected components: `A < B` when `B` can reach `A`
//! but `A` cannot reach `B`.
//!
//! The graph must overapproximate dictionary projection: whenever evidence for
//! trait `C` can expose evidence for trait `D`, there must be a path `C ->* D`.
//! This makes the traits below any cutoff closed under projection. In
//! particular, if `C < A` and `C ->* D`, then `A ->* C ->* D`. Moreover,
//! `D ->* A` would imply `C ->* A`, contradicting `C < A`; hence `D < A`.
//!
//! This closure property is what permits `Partial[A](B: C)` to translate to
//! complete evidence for `B: C` when `C < A`: every dictionary reachable from
//! the `C` dictionary is also below `A`. Any new way of projecting trait
//! evidence must therefore be reflected in the edges constructed by this
//! module.

use crate::grammar::{
    AssociatedTyBoundData, CrateItem, Fallible, Trait, TraitBoundData, TraitId, TraitImplBoundData,
    TraitRef, Ty, Variable, WhereBound, WhereClause,
};
use crate::prove::prove::{as_associated_ty, trait_header_clause, Program, TraitHeaderClause};
use crate::prove::ToWcs;
use formality_core::judgment_fn;

judgment_fn! {
    /// The impl self type is exactly one bound type parameter.
    fn exact_generic_self_ty(
        self_ty: Ty,
    ) => () {
        debug(self_ty)

        (
            -------------------------------------------- ("bound type parameter")
            (exact_generic_self_ty(Ty::Variable(Variable::BoundVar(_))) => ())
        )
    }
}

judgment_fn! {
    /// One direct edge in the syntactic trait-dependency graph.
    pub(crate) fn trait_edge(
        program: Program,
        source: TraitId,
    ) => TraitId {
        debug(program, source)

        // Traits declared by a dependency crate are dependencies of every
        // trait declared in the current crate.
        (
            (target in traits_from_dependency_crates(program, source))
            -------------------------------------------- ("crate dependency")
            (trait_edge(program, source) => target)
        )

        // Every positive trait predicate in the trait header contributes an
        // edge. This includes, but is not limited to, direct supertraits.
        (
            (let Trait { binder, .. } = require_trait_def(program, source)?)
            (let TraitBoundData {
                where_clauses,
                trait_items: _,
            } = binder.explicit_binder.peek())
            (where_clause in where_clauses)
            (target in traits_in_where_clause(where_clause))
            -------------------------------------------- ("trait where-clause")
            (trait_edge(program, source) => target)
        )

        // Bounds promised by an associated type contribute edges from the
        // trait that declares the associated type.
        (
            (let Trait { binder, .. } = require_trait_def(program, source)?)
            (let TraitBoundData {
                where_clauses: _,
                trait_items,
            } = binder.explicit_binder.peek())
            (trait_item in trait_items)
            (as_associated_ty(trait_item) => associated_ty)!
            (let AssociatedTyBoundData {
                ensures,
                where_clauses: _,
            } = associated_ty.binder.peek())
            (ensure in ensures)
            (target in traits_in_where_bound(ensure))
            -------------------------------------------- ("associated type bound")
            (trait_edge(program, source) => target)
        )

        // The conditions on an associated type likewise contribute edges.
        (
            (let Trait { binder, .. } = require_trait_def(program, source)?)
            (let TraitBoundData {
                where_clauses: _,
                trait_items,
            } = binder.explicit_binder.peek())
            (trait_item in trait_items)
            (as_associated_ty(trait_item) => associated_ty)!
            (let AssociatedTyBoundData {
                ensures: _,
                where_clauses,
            } = associated_ty.binder.peek())
            (where_clause in where_clauses)
            (target in traits_in_where_clause(where_clause))
            -------------------------------------------- ("associated type where-clause")
            (trait_edge(program, source) => target)
        )

        // A companion blanket impl `impl<T: Target> Source for T` contributes
        // `Source -> Target`. Restricting this to an exact generic `Self` and
        // to the trait's defining crate keeps upstream ordering stable when a
        // downstream crate is added.
        (
            (candidate in program.raw_trait_impls_for(source))
            (if trait_crate_index(program, source) == Some(candidate.id.crate_index))
            (let TraitImplBoundData {
                self_ty,
                where_clauses,
                ..
            } = candidate.trait_impl.binder.peek())
            (exact_generic_self_ty(self_ty) => ())!
            (where_clause in where_clauses.to_wcs())
            (trait_header_clause(self_ty, where_clause) =>
                TraitHeaderClause::Supertrait(supertrait))
            (let TraitRef {
                trait_id: target,
                parameters: _,
            } = supertrait.peek())
            -------------------------------------------- ("companion blanket impl")
            (trait_edge(program, source) => target)
        )
    }
}

judgment_fn! {
    /// Traits transitively reachable by one or more dependency edges.
    pub(crate) fn trait_reachable(
        program: Program,
        source: TraitId,
    ) => TraitId {
        debug(program, source)

        (
            (trait_edge(program, source) => target)
            -------------------------------------------- ("edge")
            (trait_reachable(program, source) => target)
        )

        (
            (trait_reachable(program, source) => intermediate)
            (trait_edge(program, intermediate) => target)
            -------------------------------------------- ("transitive")
            (trait_reachable(program, source) => target)
        )
    }
}

judgment_fn! {
    /// The strict partial order induced by asymmetric graph reachability.
    ///
    ///
    /// `lower < upper` when `upper` transitively depends on `lower`, but
    /// `lower` does not transitively depend on `upper`. For example,
    /// `trait Sub: Super` has an edge `Sub -> Super` and therefore
    /// `Super < Sub`.
    ///
    /// Traits in the same dependency cycle are therefore incomparable.
    ///
    /// The negative premise is stratified: `trait_reachable` depends only
    /// on the finite, immutable edge graph and has no dependency back
    /// on this judgment.
    pub(crate) fn trait_less_than(
        program: Program,
        lower: TraitId,
        upper: TraitId,
    ) => () {
        debug(program, lower, upper)

        (
            (trait_reachable(program, upper) => reachable)
            (if reachable == lower)!
            (if !is_reachable(program, lower, upper))
            -------------------------------------------- ("asymmetric reachability")
            (trait_less_than(program, lower, upper) => ())
        )
    }
}

fn trait_crate_index(program: &Program, trait_id: &TraitId) -> Option<usize> {
    program.program().crates.iter().position(|krate| {
        krate
            .items
            .iter()
            .any(|item| matches!(item, CrateItem::Trait(trait_def) if trait_def.id == *trait_id))
    })
}

fn trait_def<'p>(program: &'p Program, trait_id: &TraitId) -> Option<&'p Trait> {
    program
        .program()
        .items_from_all_crates()
        .find_map(|item| match item {
            CrateItem::Trait(trait_def) if trait_def.id == *trait_id => Some(trait_def),
            _ => None,
        })
}

fn require_trait_def<'p>(program: &'p Program, trait_id: &TraitId) -> Fallible<&'p Trait> {
    trait_def(program, trait_id).ok_or_else(|| anyhow::anyhow!("no trait named `{trait_id:?}`"))
}

fn traits_from_dependency_crates<'p>(program: &'p Program, source: &TraitId) -> Vec<&'p TraitId> {
    let Some(source_crate_index) = trait_crate_index(program, source) else {
        return vec![];
    };

    program.program().crates[..source_crate_index]
        .iter()
        .flat_map(|krate| &krate.items)
        .filter_map(|item| match item {
            CrateItem::Trait(trait_def) => Some(&trait_def.id),
            _ => None,
        })
        .collect()
}

fn traits_in_where_clause(where_clause: &WhereClause) -> Vec<&TraitId> {
    // This initial edge policy records the trait named by a positive predicate, but not traits
    // mentioned only inside its parameters or as projection owners. Omitting such edges is
    // conservative: it can make traits incomparable and reject an ImplWF derivation, but cannot
    // manufacture an invalid ordering fact.
    match where_clause {
        WhereClause::IsImplemented(_, trait_id, _) => vec![trait_id],
        WhereClause::ForAll(binder) => traits_in_where_clause(binder.peek()),

        WhereClause::AliasEq(_, _)
        | WhereClause::Outlives(_, _)
        | WhereClause::TypeOfConst(_, _) => vec![],
    }
}

fn traits_in_where_bound(where_bound: &WhereBound) -> Vec<&TraitId> {
    match where_bound {
        WhereBound::IsImplemented(trait_id, _) => vec![trait_id],
        WhereBound::ForAll(binder) => traits_in_where_bound(binder.peek()),
        WhereBound::Outlives(_) => vec![],
    }
}

fn is_reachable(program: &Program, source: &TraitId, target: &TraitId) -> bool {
    trait_reachable(program, source)
        .iter()
        .any(|(reachable, _)| reachable == *target)
}

#[cfg(test)]
mod tests {
    use super::{trait_edge, trait_less_than};
    use crate::grammar::{Crates, TraitId};
    use crate::prove::prove::Program;
    use crate::rust::term;

    fn program(source: &str) -> Program {
        term::<Crates>(source).to_prove_decls()
    }

    fn has_edge(program: &Program, source: &str, target: &str) -> bool {
        let target = TraitId::new(target);
        trait_edge(program, TraitId::new(source))
            .iter()
            .any(|(reachable, _)| reachable == target)
    }

    fn less_than(program: &Program, lower: &str, upper: &str) -> bool {
        trait_less_than(program, TraitId::new(lower), TraitId::new(upper)).is_proven()
    }

    #[test]
    fn trait_declaration_edges_are_transitive() {
        let program = program(
            "[
                crate test {
                    trait Base {}
                    trait Middle where Self: Base {}
                    trait Top where Self: Middle {}
                }
            ]",
        );

        assert!(has_edge(&program, "Middle", "Base"));
        assert!(has_edge(&program, "Top", "Middle"));
        assert!(less_than(&program, "Base", "Top"));
        assert!(!less_than(&program, "Top", "Base"));
        assert!(!less_than(&program, "Top", "Top"));
    }

    #[test]
    fn traits_in_the_same_scc_are_incomparable() {
        let program = program(
            "[
                crate test {
                    trait A {}
                    trait B {}

                    impl<T> A for T where T: B {}
                    impl<T> B for T where T: A {}
                }
            ]",
        );

        assert!(has_edge(&program, "A", "B"));
        assert!(has_edge(&program, "B", "A"));
        assert!(!less_than(&program, "A", "B"));
        assert!(!less_than(&program, "B", "A"));
    }

    #[test]
    fn blanket_impl_edges_require_an_exact_generic_self_type() {
        let program = program(
            "[
                crate test {
                    trait Bound {}
                    trait Blanket {}
                    trait Structured {}

                    struct Wrapper<T> { value: T, }

                    impl<T> Blanket for T where T: Bound {}
                    impl<T> Structured for Wrapper<T> where T: Bound {}
                }
            ]",
        );

        assert!(has_edge(&program, "Blanket", "Bound"));
        assert!(less_than(&program, "Bound", "Blanket"));
        assert!(!has_edge(&program, "Structured", "Bound"));
        assert!(!less_than(&program, "Bound", "Structured"));
    }

    #[test]
    fn associated_type_bounds_and_conditions_contribute_edges() {
        let program = program(
            "[
                crate test {
                    trait Ensured {}
                    trait Condition {}

                    trait Family {
                        type Item<T>: [Ensured]
                        where
                            T: Condition;
                    }
                }
            ]",
        );

        assert!(has_edge(&program, "Family", "Ensured"));
        assert!(has_edge(&program, "Family", "Condition"));
        assert!(less_than(&program, "Ensured", "Family"));
        assert!(less_than(&program, "Condition", "Family"));
    }

    #[test]
    fn higher_ranked_trait_clause_contributes_an_edge() {
        let program = program(
            "[
                crate test {
                    trait Lives<'a> {}
                    trait Root where for<'a> Self: Lives<'a> {}
                }
            ]",
        );

        assert!(has_edge(&program, "Root", "Lives"));
        assert!(less_than(&program, "Lives", "Root"));
    }

    #[test]
    fn dependency_crate_traits_are_less_than_local_traits() {
        let program = program(
            "[
                crate dependency {
                    trait Upstream {}
                },

                crate current {
                    trait Local {}
                }
            ]",
        );

        assert!(has_edge(&program, "Local", "Upstream"));
        assert!(less_than(&program, "Upstream", "Local"));
        assert!(!less_than(&program, "Local", "Upstream"));
    }

    #[test]
    fn downstream_blanket_impl_does_not_reorder_dependency_traits() {
        let program = program(
            "[
                crate dependency {
                    trait A {}
                    trait B {}
                },

                crate current {
                    impl<T> A for T where T: B {}
                }
            ]",
        );

        assert!(!has_edge(&program, "A", "B"));
        assert!(!less_than(&program, "B", "A"));
    }
}
