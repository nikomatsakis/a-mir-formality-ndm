use crate::grammar::{AdtId, AdtItem, Crate, CrateItem, Fn, Struct, Trait, ValueId};
use crate::grammar::{Fallible, TraitId};
use formality_core::{term, Size};

#[term($crates)]
#[customize(size)]
pub struct Crates {
    /// List of all crates.
    /// The last crate in the list is the current crate.
    pub crates: Vec<Crate>,
}

// Like `Program`, this is a declaration database rather than a recursively
// growing proof-search term.
impl Size for Crates {
    fn size(&self) -> usize {
        0
    }
}

impl Crates {
    pub fn len(&self) -> usize {
        self.crates.len()
    }

    /// Inclusive prefix.
    pub fn prefix(&self, tail: usize) -> Crates {
        assert!(
            tail < self.crates.len(),
            "Crates::prefix: tail {tail} out of range (len is {})",
            self.crates.len()
        );
        Crates {
            crates: self.crates[..=tail].to_vec(),
        }
    }

    pub fn items_from_all_crates(&self) -> impl Iterator<Item = &CrateItem> {
        self.crates.iter().flat_map(|c| &c.items)
    }

    pub fn fn_named(&self, fn_id: &ValueId) -> Fallible<&Fn> {
        let mut fns: Vec<&Fn> = self
            .items_from_all_crates()
            .filter_map(|crate_item| match crate_item {
                CrateItem::Fn(t) if t.id == *fn_id => Some(t),
                _ => None,
            })
            .collect();
        if fns.is_empty() {
            anyhow::bail!("no fn named `{fn_id:?}`")
        } else if fns.len() > 1 {
            anyhow::bail!("multiple fn named `{fn_id:?}`")
        } else {
            Ok(fns.pop().unwrap())
        }
    }

    pub fn trait_named(&self, trait_id: &TraitId) -> Fallible<&Trait> {
        let mut traits: Vec<&Trait> = self
            .items_from_all_crates()
            .filter_map(|crate_item| match crate_item {
                CrateItem::Trait(t) if t.id == *trait_id => Some(t),
                _ => None,
            })
            .collect();
        if traits.is_empty() {
            anyhow::bail!("no trait named `{trait_id:?}`")
        } else if traits.len() > 1 {
            anyhow::bail!("multiple traits named `{trait_id:?}`")
        } else {
            Ok(traits.pop().unwrap())
        }
    }

    pub fn struct_named(&self, adt_id: &AdtId) -> Fallible<&Struct> {
        let mut structs: Vec<&Struct> = self
            .items_from_all_crates()
            .filter_map(|crate_item| match crate_item {
                CrateItem::AdtItem(AdtItem::Struct(s)) if s.id == *adt_id => Some(s),

                CrateItem::AdtItem(_)
                | CrateItem::FeatureGate(_)
                | CrateItem::Trait(_)
                | CrateItem::TraitImpl(_)
                | CrateItem::NegTraitImpl(_)
                | CrateItem::Fn(_)
                | CrateItem::Test(_) => None,
            })
            .collect();

        if structs.is_empty() {
            anyhow::bail!("no ADT named `{adt_id:?}`")
        } else if structs.len() > 1 {
            anyhow::bail!("multiple ADTs named `{adt_id:?}`")
        } else {
            Ok(structs.pop().unwrap())
        }
    }

    pub fn adt_item_named(&self, adt_id: &AdtId) -> Fallible<&AdtItem> {
        let mut adts: Vec<&AdtItem> = self
            .items_from_all_crates()
            .filter_map(|crate_item| match crate_item {
                CrateItem::AdtItem(a) if a.name() == adt_id => Some(a),

                CrateItem::AdtItem(_)
                | CrateItem::FeatureGate(_)
                | CrateItem::Trait(_)
                | CrateItem::TraitImpl(_)
                | CrateItem::NegTraitImpl(_)
                | CrateItem::Fn(_)
                | CrateItem::Test(_) => None,
            })
            .collect();

        if adts.is_empty() {
            anyhow::bail!("no ADT named `{adt_id:?}`")
        } else if adts.len() > 1 {
            anyhow::bail!("multiple ADTs named `{adt_id:?}`")
        } else {
            Ok(adts.pop().unwrap())
        }
    }
}

#[cfg(test)]
mod size_tests {
    use formality_core::{term, Size};

    #[term]
    struct SizeProbe {
        first: u32,
        rest: Vec<u32>,
    }

    #[test]
    fn terms_derive_structural_size_and_contexts_can_override_it() {
        let probe = SizeProbe {
            first: 0,
            rest: vec![1, 2],
        };
        assert_eq!(probe.size(), 4);

        let declarations = super::Crates { crates: Vec::new() };
        assert_eq!(declarations.size(), 0);
    }
}
