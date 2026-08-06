//! Ground impl selection for monomorphized trait-method calls.

use std::collections::{BTreeMap, BTreeSet};

use crate::grammar::{Parameter, TraitImpl, TraitRef, Wcs};
use crate::prove::prove::{prove_via_impl, Constrained, Env, ImplApplication, ImplId, Program};
use formality_core::{visit::CoreVisit, Upcast};

use super::normalize::normalize_parameters;
use super::scope::is_normalized_parameter;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct ApplicationKey {
    impl_id: ImplId,
    impl_arguments: Vec<Parameter>,
}

formality_core::cast_impl!(ApplicationKey);

#[derive(Debug)]
struct ApplicationGroup {
    application: ImplApplication,
    definite: bool,
    ambiguous: bool,
}

impl ApplicationGroup {
    fn record(&mut self, definite: bool) {
        self.definite |= definite;
        self.ambiguous |= !definite;
    }
}

/// The unique declaration and impl-binder substitution selected for one
/// monomorphic trait-method call.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(super) struct SelectedImpl {
    pub(super) impl_id: ImplId,
    pub(super) trait_impl: TraitImpl,
    pub(super) impl_arguments: Vec<Parameter>,
}

/// Reconstruct the concrete dictionary promised by type checking.
///
/// Codegen deliberately uses no ambient assumptions: every prerequisite must
/// be witnessed by declarations in the fully monomorphized program.
pub(super) fn select_impl(program: &Program, trait_ref: &TraitRef) -> SelectedImpl {
    assert!(
        trait_ref.parameters.iter().all(is_normalized_parameter),
        "trait-method impl selection requires a ground, recursively alias-free trait-ref: {trait_ref:?}",
    );

    let initial_env = Env::default();
    let mut groups = BTreeMap::<ApplicationKey, ApplicationGroup>::new();
    let mut unresolved = BTreeSet::<ImplId>::new();

    for candidate in program.raw_trait_impls_for(&trait_ref.trait_id) {
        let Ok(paths) =
            prove_via_impl(program, &initial_env, Wcs::t(), trait_ref, &candidate).into_map()
        else {
            continue;
        };

        for (Constrained(application, constraints), _) in paths {
            let impl_arguments = application.inferred_impl_arguments(&constraints);
            let proof_constraints = application.proof_constraints(&constraints);
            let proof_is_definite =
                proof_constraints.env() == &initial_env && proof_constraints.unconditionally_true();

            if !impl_arguments.free_variables().is_empty() {
                unresolved.insert(application.impl_id);
                continue;
            }

            let Ok(normal_forms) =
                normalize_parameters(program, &initial_env, Wcs::t(), impl_arguments).into_map()
            else {
                unresolved.insert(application.impl_id);
                continue;
            };

            if normal_forms.is_empty() {
                unresolved.insert(application.impl_id);
                continue;
            }

            for (Constrained(impl_arguments, normalization_constraints), _) in normal_forms {
                if !impl_arguments.iter().all(is_normalized_parameter) {
                    unresolved.insert(application.impl_id);
                    continue;
                }

                let normalization_is_definite = normalization_constraints.env() == &initial_env
                    && normalization_constraints.unconditionally_true();
                let definite = proof_is_definite && normalization_is_definite;
                let key = ApplicationKey {
                    impl_id: application.impl_id,
                    impl_arguments,
                };

                match groups.entry(key) {
                    std::collections::btree_map::Entry::Vacant(entry) => {
                        entry.insert(ApplicationGroup {
                            application: (&application).upcast(),
                            definite,
                            ambiguous: !definite,
                        });
                    }
                    std::collections::btree_map::Entry::Occupied(mut entry) => {
                        entry.get_mut().record(definite);
                    }
                }
            }
        }
    }

    choose_application(trait_ref, groups, unresolved)
}

fn choose_application(
    trait_ref: &TraitRef,
    mut groups: BTreeMap<ApplicationKey, ApplicationGroup>,
    unresolved: BTreeSet<ImplId>,
) -> SelectedImpl {
    let definite: Vec<ApplicationKey> = groups
        .iter()
        .filter(|(_, group)| group.definite)
        .map(|(key, _)| key.upcast())
        .collect();

    if definite.len() > 1 {
        panic!(
            "internal error: type checking proved `{trait_ref:?}`, but codegen found multiple applicable impls: {definite:?}",
        );
    }

    let Some(selected_key) = definite.into_iter().next() else {
        if groups.is_empty() && unresolved.is_empty() {
            panic!(
                "internal error: type checking proved `{trait_ref:?}`, but codegen found no applicable impl",
            );
        }

        panic!(
            "internal error: type checking proved `{trait_ref:?}`, but codegen impl selection remained ambiguous: groups={groups:?}, unresolved={unresolved:?}",
        );
    };

    if !unresolved.is_empty() || groups.keys().any(|key| key != &selected_key) {
        panic!(
            "internal error: type checking proved `{trait_ref:?}`, but codegen impl selection had an ambiguous competitor: selected={selected_key:?}, groups={groups:?}, unresolved={unresolved:?}",
        );
    }

    let group = groups.remove(&selected_key).unwrap();
    SelectedImpl {
        impl_id: selected_key.impl_id,
        trait_impl: group.application.trait_impl,
        impl_arguments: selected_key.impl_arguments,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, BTreeSet};

    use super::{choose_application, select_impl, ApplicationGroup, ApplicationKey};
    use crate::grammar::{Crates, Parameter, TraitRef};
    use crate::prove::prove::{prove_via_impl, Constrained, Env, ImplApplication, Program};
    use crate::rust::term;

    fn program(source: &str) -> Program {
        let crates: Crates = term(source);
        crates.to_prove_decls()
    }

    fn application(program: &Program, trait_ref: &TraitRef) -> ImplApplication {
        let candidate = program
            .raw_trait_impls_for(&trait_ref.trait_id)
            .into_iter()
            .next()
            .unwrap();
        let (Constrained(application, _), _) =
            prove_via_impl(program, Env::default(), (), trait_ref, candidate)
                .into_singleton()
                .unwrap();
        application
    }

    #[test]
    #[should_panic(expected = "codegen found no applicable impl")]
    fn no_candidate_panics_at_selection_boundary() {
        let program = program("[crate test { trait Foo {} }]");
        select_impl(&program, &term("u32: Foo"));
    }

    #[test]
    fn one_ground_candidate_is_selected() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    impl Foo for u32 {}
                }
            ]",
        );
        let selected = select_impl(&program, &term("u32: Foo"));

        assert!(selected.impl_arguments.is_empty());
        assert_eq!(
            selected.impl_id,
            program.raw_trait_impls_for(&term("Foo"))[0].id
        );
    }

    #[test]
    #[should_panic(expected = "codegen found multiple applicable impls")]
    fn distinct_definite_candidates_are_a_coherence_panic() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    impl Foo for u32 {}
                    impl Foo for u32 {}
                }
            ]",
        );
        select_impl(&program, &term("u32: Foo"));
    }

    #[test]
    fn candidate_where_clause_is_proved_from_declarations() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    trait Marker {}
                    impl Marker for u32 {}
                    impl<T> Foo for T where T: Marker {}
                }
            ]",
        );
        let selected = select_impl(&program, &term("u32: Foo"));

        assert_eq!(selected.impl_arguments, vec![term::<Parameter>("u32")]);
    }

    #[test]
    #[should_panic(expected = "codegen found no applicable impl")]
    fn candidate_where_clause_cannot_use_missing_ambient_evidence() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    trait Marker {}
                    impl<T> Foo for T where T: Marker {}
                }
            ]",
        );
        select_impl(&program, &term("u32: Foo"));
    }

    #[test]
    #[should_panic(expected = "impl selection remained ambiguous")]
    fn unresolved_impl_argument_is_ambiguous() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    impl<T> Foo for u32 {}
                }
            ]",
        );
        select_impl(&program, &term("u32: Foo"));
    }

    #[test]
    #[should_panic(expected = "ambiguous competitor")]
    fn definite_candidate_plus_distinct_unresolved_candidate_is_ambiguous() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    impl Foo for u32 {}
                    impl<T> Foo for u32 {}
                }
            ]",
        );
        select_impl(&program, &term("u32: Foo"));
    }

    #[test]
    fn ambiguous_path_in_same_application_group_does_not_compete() {
        let program = program(
            "[
                crate test {
                    trait Foo {}
                    impl Foo for u32 {}
                }
            ]",
        );
        let trait_ref: TraitRef = term("u32: Foo");
        let application = application(&program, &trait_ref);
        let key = ApplicationKey {
            impl_id: application.impl_id,
            impl_arguments: vec![],
        };
        let mut groups = BTreeMap::new();
        groups.insert(
            key,
            ApplicationGroup {
                application,
                definite: true,
                ambiguous: true,
            },
        );

        let selected = choose_application(&trait_ref, groups, BTreeSet::new());
        assert!(selected.impl_arguments.is_empty());
    }
}
