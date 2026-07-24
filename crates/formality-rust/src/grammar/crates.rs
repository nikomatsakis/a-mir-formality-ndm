use crate::grammar::{Adt, AdtId, Binder, CrateId, Parameter, TraitId, TraitRef, Ty};
use crate::grammar::{Enum, Fn, NegTraitImpl, Struct, Trait, TraitImpl, WhereClause};
use formality_core::term;

use crate::grammar::feature::FeatureGate;

#[term(crate $id { $*items })]
pub struct Crate {
    pub id: CrateId,
    pub items: Vec<CrateItem>,
}

#[term]
pub enum CrateItem {
    #[cast]
    FeatureGate(FeatureGate),
    #[cast]
    AdtItem(AdtItem),
    #[cast]
    Trait(Trait),
    #[cast]
    TraitImpl(TraitImpl),
    #[cast]
    NegTraitImpl(NegTraitImpl),
    #[cast]
    Fn(Fn),
    #[cast]
    Test(Test),
}

#[term]
pub enum AdtItem {
    #[cast]
    Struct(Struct),
    #[cast]
    Enum(Enum),
}

impl AdtItem {
    pub fn name(&self) -> &AdtId {
        match self {
            AdtItem::Struct(s) => &s.id,
            AdtItem::Enum(e) => &e.id,
        }
    }

    /// Convert from an "adt item" (which is either a struct or an enum)
    /// to a unified `Adt` that the variants/members of both in a consistent
    /// way. This is a form that doesn't exist in Rust grammar.
    pub fn to_adt(&self) -> Adt {
        match self {
            AdtItem::Struct(s) => s.to_adt(),
            AdtItem::Enum(e) => e.to_adt(),
        }
    }

    pub fn where_clauses(&self) -> &Vec<WhereClause> {
        match self {
            AdtItem::Struct(s) => &s.binder.peek().where_clauses,
            AdtItem::Enum(e) => &e.binder.peek().where_clauses,
        }
    }
}

#[term(test $binder)]
pub struct Test {
    pub binder: Binder<TestBoundData>,
}

#[term($:where $,assumptions { $,goals })]
pub struct TestBoundData {
    pub assumptions: Vec<WhereClause>,
    pub goals: Vec<TestGoal>,
}

/// One assertion made by a `test` declaration.
#[term]
pub enum TestGoal {
    /// Prove a trait-ref and verify that an explicit impl application supplies it.
    #[grammar($v0 : $v1 $<?v2>)]
    TraitRef(Ty, TraitId, Vec<Parameter>),

    /// Prove an arbitrary where-clause without requiring an impl application.
    #[grammar(prove($v0))]
    Prove(WhereClause),
}

impl TestGoal {
    pub fn as_trait_ref(&self) -> Option<TraitRef> {
        match self {
            TestGoal::TraitRef(self_ty, trait_id, trait_parameters) => {
                Some(trait_id.with(self_ty, trait_parameters))
            }
            TestGoal::Prove(_) => None,
        }
    }
}
