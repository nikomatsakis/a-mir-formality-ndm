use crate::grammar::{
    AdtId, AliasName, AliasTy, AssociatedTy, AssociatedTyBoundData, AssociatedTyValue,
    AssociatedTyValueBoundData, Binder, Crate, CrateId, CrateItem, Crates, ImplItem, NegTraitImpl,
    Parameter, Predicate, Relation, Trait, TraitBoundData, TraitId, TraitImpl, TraitImplBoundData,
    TraitItem, TraitRef, Ty, Wc, Wcs,
};
use crate::prove::ToWcs;
use formality_core::{seq, Downcasted, Set, To, Upcast, Upcasted};
use formality_macros::term;
use std::sync::Arc;

#[term]
pub struct Program {
    pub crates: Arc<Crates>,
    pub max_size: usize,
}

impl Program {
    /// Max size used in unit tests that are not stress testing maximum size.
    pub const DEFAULT_MAX_SIZE: usize = 222;

    pub fn program(&self) -> &Crates {
        &self.crates
    }

    pub fn is_local_trait_id(&self, trait_id: &TraitId) -> bool {
        self.crates
            .crates
            .last()
            .into_iter()
            .flat_map(|c| c.items.iter())
            .any(|item| match item {
                CrateItem::Trait(t) => t.id == *trait_id,
                _ => false,
            })
    }

    pub fn is_local_adt_id(&self, adt_id: &AdtId) -> bool {
        self.crates
            .crates
            .last()
            .into_iter()
            .flat_map(|c| c.items.iter())
            .any(|item| match item {
                CrateItem::AdtItem(s) => s.name() == adt_id,
                _ => false,
            })
    }

    pub fn trait_impls(&self) -> Vec<TraitImpl> {
        self.crates.items_from_all_crates().downcasted().collect()
    }

    /// Enumerate the raw positive impl declarations for `trait_id`, preserving
    /// their source identity and all impl items.
    pub(crate) fn raw_trait_impls_for(&self, trait_id: &TraitId) -> Vec<ImplCandidate> {
        self.crates
            .crates
            .iter()
            .enumerate()
            .flat_map(|(crate_index, krate)| {
                krate
                    .items
                    .iter()
                    .enumerate()
                    .filter_map(move |(item_index, item)| match item {
                        CrateItem::TraitImpl(trait_impl) if trait_impl.trait_id() == trait_id => {
                            Some(ImplCandidate {
                                id: ImplId {
                                    crate_index,
                                    item_index,
                                },
                                trait_impl: trait_impl.upcast(),
                            })
                        }
                        _ => None,
                    })
            })
            .collect()
    }

    pub fn trait_impls_in_crate(&self, krate: &Crate) -> Vec<TraitImpl> {
        krate.items.iter().downcasted().collect()
    }

    pub fn neg_trait_impls_in_crate(&self, krate: &Crate) -> Vec<NegTraitImpl> {
        krate.items.iter().downcasted().collect()
    }

    pub fn neg_trait_impls_for(&self, trait_id: &TraitId) -> Vec<NegTraitImpl> {
        self.crates
            .items_from_all_crates()
            .filter_map(|item| match item {
                CrateItem::NegTraitImpl(neg_trait_impl)
                    if neg_trait_impl.binder.peek().trait_id == *trait_id =>
                {
                    Some(neg_trait_impl.clone())
                }
                _ => None,
            })
            .collect()
    }

    /// Look up a raw trait definition by id.
    pub fn trait_def(&self, trait_id: &TraitId) -> Trait {
        self.crates.trait_named(trait_id).unwrap().clone()
    }

    pub fn alias_eq_decls(&self, name: &AliasName) -> Vec<AliasEqDecl> {
        self.crates
            .items_from_all_crates()
            .filter_map(|item| match item {
                CrateItem::TraitImpl(ti) => Some(ti),
                _ => None,
            })
            .flat_map(|ti| {
                let (
                    impl_vars,
                    TraitImplBoundData {
                        trait_id,
                        self_ty,
                        trait_parameters,
                        where_clauses: impl_wc,
                        impl_items,
                    },
                ) = ti.binder.open();

                impl_items
                    .iter()
                    .filter_map(|impl_item| match impl_item {
                        ImplItem::Fn(_) => None,
                        ImplItem::AssociatedTyValue(AssociatedTyValue {
                            id: item_id,
                            binder,
                        }) => {
                            let (
                                assoc_vars,
                                AssociatedTyValueBoundData {
                                    where_clauses: assoc_wc,
                                    ty,
                                },
                            ) = binder.open();
                            Some(AliasEqDecl {
                                binder: Binder::new(
                                    (&impl_vars, &assoc_vars),
                                    AliasEqDeclBoundData {
                                        alias: AliasTy::associated_ty(
                                            &trait_id,
                                            item_id,
                                            assoc_vars.len(),
                                            seq![
                                                self_ty.to(),
                                                ..trait_parameters.iter().cloned(),
                                                ..assoc_vars.iter().upcasted(),
                                            ],
                                        ),
                                        ty,
                                        where_clause: (&impl_wc, assoc_wc).to_wcs(),
                                    },
                                ),
                            })
                        }
                    })
                    .collect::<Vec<_>>()
            })
            .filter(|a| a.alias_name() == *name)
            .collect()
    }

    /// Return the set of implied requirements for all traits.
    pub fn trait_requirements(&self) -> Set<TraitRequirement> {
        self.crates
            .items_from_all_crates()
            .filter_map(|item| match item {
                CrateItem::Trait(t) => Some(t),
                _ => None,
            })
            .flat_map(Self::requirements_for_trait)
            .collect()
    }

    /// Return the implied requirements declared by `trait_def`.
    ///
    /// For example, `trait Eq where Self: PartialEq` yields the requirement
    /// `forall<Self> Eq(Self) => PartialEq(Self)`.
    fn requirements_for_trait(trait_def: &Trait) -> Set<TraitRequirement> {
        let (
            variables,
            TraitBoundData {
                where_clauses,
                trait_items,
            },
        ) = trait_def.binder.open();
        let self_var: Parameter = variables[0].upcast();
        let source = TraitRef::new(&trait_def.id, &variables);

        let make_requirement = |required| TraitRequirement {
            binder: Binder::new(
                &variables,
                TraitRequirementBoundData {
                    source: source.clone(),
                    required,
                },
            ),
        };

        fn is_supertrait(self_var: &Parameter, wc: &Wc) -> bool {
            match wc {
                Wc::Predicate(Predicate::IsImplemented(trait_ref)) => {
                    trait_ref.parameters[0] == *self_var
                }
                Wc::Relation(Relation::Outlives(a, _)) => *a == *self_var,
                Wc::Predicate(_) => false,
                Wc::Relation(_) => false,
                Wc::ForAll(binder) => is_supertrait(self_var, binder.peek()),
                Wc::Implies(_, consequence) => is_supertrait(self_var, consequence),
            }
        }

        let mut requirements: Set<_> = where_clauses
            .to_wcs()
            .into_iter()
            .filter(|required| is_supertrait(&self_var, required))
            .map(&make_requirement)
            .collect();

        // A Rust declaration like
        //
        //     trait Foo {
        //         type Bar<T>: Ord where T: Copy;
        //     }
        //
        // gives rise to the requirement
        //
        //     forall<Self> Foo(Self) =>
        //         forall<T> if { Copy(T) } Ord(<Self as Foo>::Bar<T>).
        //
        // The associated type's own variables are therefore bound inside `required`, while
        // the trait's variables bind the entire requirement, including its `source`.
        for trait_item in trait_items {
            let TraitItem::AssociatedTy(AssociatedTy { id, binder }) = trait_item else {
                continue;
            };
            let (
                associated_variables,
                AssociatedTyBoundData {
                    ensures,
                    where_clauses,
                },
            ) = binder.open();
            let alias_parameters: Vec<Parameter> = variables
                .iter()
                .chain(&associated_variables)
                .upcasted()
                .collect();
            let alias = AliasTy::associated_ty(
                &trait_def.id,
                &id,
                associated_variables.len(),
                alias_parameters,
            );
            let has_conditions = !where_clauses.is_empty();
            let conditions = where_clauses.to_wcs();

            for ensure in ensures {
                let required = ensure.to_wc(&alias);
                let required = if has_conditions {
                    Wc::Implies(conditions.clone(), Arc::new(required))
                } else {
                    required
                };
                let required = if associated_variables.is_empty() {
                    required
                } else {
                    Wc::for_all(Binder::new(&associated_variables, required))
                };
                requirements.insert(make_requirement(required));
            }
        }

        requirements
    }

    /// Create a `Program` wrapping the given items in a single crate named "test".
    pub fn program_from_items(items: Vec<CrateItem>) -> Crates {
        Crates {
            crates: vec![Crate {
                id: CrateId::new("test"),
                items,
            }],
        }
    }

    pub fn empty() -> Self {
        Self {
            crates: Arc::new(Crates { crates: vec![] }),
            max_size: Program::DEFAULT_MAX_SIZE,
        }
    }
}

/// Stable source identity for an impl declaration.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct ImplId {
    pub(crate) crate_index: usize,
    pub(crate) item_index: usize,
}

/// One raw impl declaration selected for candidate-specific proof.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct ImplCandidate {
    pub(crate) id: ImplId,
    pub(crate) trait_impl: TraitImpl,
}

formality_core::cast_impl!(ImplCandidate);
/// Mark a trait or trait impl as `unsafe`.
#[term]
#[derive(Default)]
pub enum Safety {
    #[default]
    Safe,
    Unsafe,
}

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

/// An "alias equal declaration" declares when an alias type can be normalized
/// to something else. They are derived from `type Foo = Bar` declarations in
/// impls, which would generate an alias eq decl saying that `<T as SomeTrait>::Foo = Bar`.
#[term(alias $binder)]
pub struct AliasEqDecl {
    /// The binder includes the generics from the impl and also any generics on the GAT.
    pub binder: Binder<AliasEqDeclBoundData>,
}

impl AliasEqDecl {
    pub fn alias_name(&self) -> AliasName {
        self.binder.peek().alias.name.clone()
    }
}

/// Data bound under the impl generics for a [`AliasEqDecl`][]
#[term($alias = $ty $:where $where_clause)]
pub struct AliasEqDeclBoundData {
    /// The alias that is equal
    pub alias: AliasTy,

    /// The type the alias is equal to
    pub ty: Ty,

    /// The where-clauses that must hold for this rule to be applicable; derived from the impl and the GAT
    pub where_clause: Wcs,
}
