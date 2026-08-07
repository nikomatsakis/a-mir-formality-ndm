use std::sync::Arc;

use formality_core::{
    cast_impl, set, term, Cons, DowncastTo, Set, Upcast, UpcastFrom, Upcasted as _,
};

use crate::{grammar::WhereClause, prove::ToWcs};

use super::{Binder, Parameter, Predicate, Relation, TraitId, TraitRef};

#[term($set)]
#[derive(Default)]
pub struct Wcs {
    set: Set<Wc>,
}

impl Wcs {
    pub fn t() -> Self {
        set![].upcast()
    }

    /// Goal(s) to prove `a` and `b` are equal (they must have equal length)
    pub fn all_eq(a: impl Upcast<Vec<Parameter>>, b: impl Upcast<Vec<Parameter>>) -> Wcs {
        let a: Vec<Parameter> = a.upcast();
        let b: Vec<Parameter> = b.upcast();
        assert_eq!(a.len(), b.len());
        a.into_iter()
            .zip(b)
            .map(|(a, b)| Relation::equals(a, b))
            .collect()
    }

    /// Goal(s) to prove `a` and `b` are subtypes.
    ///
    /// FIXME(#220): This should take variance into account.
    pub fn all_sub(a: impl Upcast<Vec<Parameter>>, b: impl Upcast<Vec<Parameter>>) -> Wcs {
        let a: Vec<Parameter> = a.upcast();
        let b: Vec<Parameter> = b.upcast();
        assert_eq!(a.len(), b.len());
        a.into_iter()
            .zip(b)
            .map(|(a, b)| Relation::sub(a, b))
            .collect()
    }

    /// Goal(s) to prove `a0: b` for all `a0` in `a`
    pub fn all_outlives(a: impl Upcast<Vec<Parameter>>, b: impl Upcast<Parameter>) -> Wcs {
        let a: Vec<Parameter> = a.upcast();
        let b: Parameter = b.upcast();
        a.into_iter().map(|a| Relation::outlives(a, &b)).collect()
    }

    /// Iterate over where-clauses
    pub fn iter(&self) -> impl Iterator<Item = Wc> + use<'_> {
        self.into_iter()
    }
}

impl<'w> IntoIterator for &'w Wcs {
    type Item = Wc;

    type IntoIter = Box<dyn Iterator<Item = Wc> + 'w>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.set.iter().upcasted())
    }
}

impl IntoIterator for Wcs {
    type Item = Wc;

    type IntoIter = Box<dyn Iterator<Item = Wc>>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.set.into_iter())
    }
}

impl<I> FromIterator<I> for Wcs
where
    I: Upcast<Wc>,
{
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        Wcs {
            set: iter.into_iter().upcasted().collect(),
        }
    }
}

macro_rules! tuple_upcast {
    ($($name:ident),*) => {
        #[allow(non_snake_case)]
        impl<$($name,)*> UpcastFrom<($($name,)*)> for Wcs
        where
            $($name: Upcast<Wcs>,)*
        {
            fn upcast_from(($($name,)*): ($($name,)*)) -> Self {
                let c = None.into_iter();
                $(
                    let $name: Wcs = $name.upcast();
                    let c = c.chain($name);
                )*
                c.collect()
            }
        }
    }
}

tuple_upcast!(A, B);
tuple_upcast!(A, B, C);
tuple_upcast!(A, B, C, D);

impl UpcastFrom<Vec<WhereClause>> for Wcs {
    fn upcast_from(clauses: Vec<WhereClause>) -> Self {
        clauses.to_wcs()
    }
}

impl UpcastFrom<&[WhereClause]> for Wcs {
    fn upcast_from(clauses: &[WhereClause]) -> Self {
        clauses.to_wcs()
    }
}

impl DowncastTo<Cons<Wc, Wcs>> for Wcs {
    fn downcast_to(&self) -> Option<Cons<Wc, Wcs>> {
        let Cons(wc, set) = self.set.downcast_to()?;
        Some(Cons(wc, set.upcast()))
    }
}

impl UpcastFrom<()> for Wcs {
    fn upcast_from((): ()) -> Self {
        Wcs::default()
    }
}

impl DowncastTo<()> for Wcs {
    fn downcast_to(&self) -> Option<()> {
        if self.set.is_empty() {
            Some(())
        } else {
            None
        }
    }
}

/// The index in a modal `Upto(P)` judgment that describes how much of the
/// proposition `P` must be (or has been, for assumptions) proven.
///
/// Alternatively, it can be viewed as describing what parts of the dictionary
/// for `P` are initialized/accessible.
#[term]
pub enum Upto {
    /// No implications of `P` are available.
    /// An uninitialized dictionary.
    #[grammar(Zero)]
    Zero,

    /// Supertrait bounds `Tr1: Tr2` implied by the proposition
    /// are available if `Tr1 < $0` and `Tr2 < $0`.
    #[grammar(Supertraits[$v0])]
    Supertraits(TraitId),

    /// All supertrait bounds implied by the proposition are available.
    /// GAT bounds implied by the proposition are available if
    /// they are declared on a trait `Tr < $0`.
    #[grammar(GatBounds[$v0])]
    GatBounds(TraitId),
}

#[derive(Copy, Clone)]
enum ModePosition {
    Goal,
    Assumption,
}

impl Upto {
    /// Interpret `wc` as a goal at this dictionary-construction frontier.
    ///
    /// Modes attach only to atomic predicates. Quantifiers preserve the current polarity, while
    /// the premise of an implication flips between goal and assumption position.
    pub fn apply_goal(&self, wc: impl Upcast<Wc>) -> Wc {
        self.apply_at(wc.upcast(), ModePosition::Goal)
    }

    /// Interpret `wc` as an assumption at this dictionary-construction frontier.
    pub fn apply_assumption(&self, wc: impl Upcast<Wc>) -> Wc {
        self.apply_at(wc.upcast(), ModePosition::Assumption)
    }

    /// Interpret every clause in `wcs` as a goal at this frontier.
    pub fn apply_goals(&self, wcs: impl Upcast<Wcs>) -> Wcs {
        self.apply_wcs_at(wcs.upcast(), ModePosition::Goal)
    }

    /// Interpret every clause in `wcs` as an assumption at this frontier.
    pub fn apply_assumptions(&self, wcs: impl Upcast<Wcs>) -> Wcs {
        self.apply_wcs_at(wcs.upcast(), ModePosition::Assumption)
    }

    fn apply_wcs_at(&self, wcs: Wcs, position: ModePosition) -> Wcs {
        wcs.into_iter()
            .map(|wc| self.apply_at(wc, position))
            .collect()
    }

    fn apply_at(&self, wc: Wc, position: ModePosition) -> Wc {
        match wc {
            Wc::Atomic(atomic) => Wc::Mode(self.clone(), atomic),

            Wc::ForAll(binder) => Wc::for_all(binder.map(|wc| self.apply_at(wc, position))),

            Wc::Implies(conditions, consequence) => match position {
                ModePosition::Goal => Wc::implies(
                    self.apply_wcs_at(conditions, ModePosition::Assumption),
                    self.apply_at((*consequence).clone(), ModePosition::Goal),
                ),

                ModePosition::Assumption => Wc::implies(
                    self.apply_wcs_at(conditions, ModePosition::Goal),
                    self.apply_at((*consequence).clone(), ModePosition::Assumption),
                ),
            },

            Wc::Mode(_, _) => {
                panic!("cannot apply a mode to a where-clause that is already mode-qualified")
            }
        }
    }
}

/// An atomic proposition to which a proof mode can be attached.
#[term]
pub enum AtomicPredicate {
    /// Means the built-in relation holds.
    #[cast]
    Relation(Relation),

    /// Means the predicate holds.
    #[cast]
    Predicate(Predicate),
}

#[term]
pub enum Wc {
    /// An ordinary, unqualified atomic proposition.
    #[cast]
    Atomic(AtomicPredicate),

    // Equivalent to `for<'a>` except that it can also express `for<T>` and so forth:
    // means `$v0` is true for any value of the bound variables (e.g., `'a` or `T`).
    #[grammar(for $v0)]
    ForAll(Arc<Binder<Wc>>),

    #[grammar(if $v0 $v1)]
    Implies(Wcs, Arc<Wc>),

    /// Prove (or assume) an atomic proposition at one dictionary-construction frontier.
    ///
    /// This constructor is internal to Rust's well-formedness semantics. Use
    /// [`Upto::apply_goal`] or [`Upto::apply_assumption`] to apply a frontier to a compound
    /// where-clause in the corresponding logical position.
    #[grammar($v0($v1))]
    Mode(Upto, AtomicPredicate),
}

/// Temporary alias for migration -- allows `WcData::Variant` to still compile.
pub type WcData = Wc;

// ---

cast_impl!((Predicate) <: (AtomicPredicate) <: (Wc));
cast_impl!((Relation) <: (AtomicPredicate) <: (Wc));
cast_impl!((TraitRef) <: (Predicate) <: (AtomicPredicate));
cast_impl!((TraitRef) <: (Predicate) <: (Wc));
cast_impl!((Relation) <: (Wc) <: (Arc<Wc>));
cast_impl!((Predicate) <: (Wc) <: (Arc<Wc>));
cast_impl!((TraitRef) <: (Wc) <: (Arc<Wc>));

impl UpcastFrom<Wc> for Wcs {
    fn upcast_from(term: Wc) -> Self {
        Wcs { set: set![term] }
    }
}

impl DowncastTo<Wc> for Wcs {
    fn downcast_to(&self) -> Option<Wc> {
        if self.set.len() == 1 {
            self.set.iter().next().map(Upcast::upcast)
        } else {
            None
        }
    }
}

cast_impl!((Relation) <: (Wc) <: (Wcs));
cast_impl!((Predicate) <: (Wc) <: (Wcs));
cast_impl!((TraitRef) <: (Wc) <: (Wcs));

#[cfg(test)]
mod tests {
    use super::{TraitId, Upto, Wc};
    use crate::rust::term;

    #[test]
    fn modes_use_constructor_notation() {
        let atom = term::<Wc>("u32: Debug");
        let cases = [
            ("Zero(u32: Debug)", Upto::Zero.apply_goal(&atom)),
            (
                "Supertraits[Root](u32: Debug)",
                Upto::supertraits(TraitId::new("Root")).apply_goal(&atom),
            ),
            (
                "GatBounds[Root](u32: Debug)",
                Upto::gat_bounds(TraitId::new("Root")).apply_goal(&atom),
            ),
        ];

        for (text, expected) in cases {
            let parsed = term::<Wc>(text);
            assert_eq!(parsed, expected);
            assert_eq!(format!("{parsed:?}"), text);
        }
    }

    #[test]
    fn mode_application_distributes_through_implication() {
        let mode = Upto::supertraits(TraitId::new("Root"));
        let condition = term::<Wc>("u32: Debug");
        let consequence = term::<Wc>("u32: Clone");
        let implication = Wc::implies(&condition, &consequence);

        assert_eq!(
            mode.apply_goal(implication),
            Wc::implies(
                mode.apply_assumption(condition),
                mode.apply_goal(consequence),
            ),
        );
    }

    #[test]
    fn mode_application_to_an_assumption_flips_implication_positions() {
        let mode = Upto::supertraits(TraitId::new("Root"));
        let condition = term::<Wc>("u32: Debug");
        let consequence = term::<Wc>("u32: Clone");
        let implication = Wc::implies(&condition, &consequence);

        assert_eq!(
            mode.apply_assumption(implication),
            Wc::implies(
                mode.apply_goal(condition),
                mode.apply_assumption(consequence),
            ),
        );
    }

    #[test]
    fn mode_application_distributes_through_binder() {
        let mode = Upto::supertraits(TraitId::new("Root"));
        let Wc::ForAll(binder) = term::<Wc>("for<'a> 'a : 'a") else {
            unreachable!()
        };

        assert_eq!(
            mode.apply_goal(Wc::for_all(&binder)),
            Wc::for_all(binder.map(|goal| mode.apply_goal(goal))),
        );
    }

    #[test]
    #[should_panic(expected = "cannot apply a mode")]
    fn applying_a_mode_twice_is_rejected() {
        let mode = Upto::supertraits(TraitId::new("Root"));
        let once = mode.apply_goal(term::<Wc>("u32: Debug"));
        mode.apply_goal(once);
    }
}
