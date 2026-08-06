use std::sync::Arc;

use crate::grammar::{AliasTy, Const, Parameter, Predicate, Relation, TraitId, Ty, Upto, Wc, Wcs};
use crate::prove::prove::{
    decls::Program,
    prove::{Constrained, Constraints, Env},
};
use crate::rust::term;
use formality_core::{Downcast, Upcast};
use formality_macros::test;

use crate::prove::prove::prove::{prove_after, prove_normalize::prove_normalize};

fn decls() -> Program {
    Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Super where {}"),
            term("trait Sub where Self : Super {}"),
            term("trait ValidationRoot where Self : Sub {}"),
            term("struct NeedsSub<T> where T : Sub {}"),
        ])),
        ..Program::empty()
    }
}

fn sub() -> Wc {
    term("Sub(u32)")
}

fn validated_at(upto: Upto, wc: impl Upcast<Wc>) -> Wc {
    let wc: Wc = wc.upcast();
    Wc::validate(upto, wc)
}

fn at_supertraits(wc: impl Upcast<Wc>) -> Wc {
    validated_at(Upto::supertraits(TraitId::new("ValidationRoot")), wc)
}

fn at_gat_bounds(wc: impl Upcast<Wc>) -> Wc {
    validated_at(Upto::gat_bounds(TraitId::new("ValidationRoot")), wc)
}

fn normalization_decls() -> Program {
    Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Marker where {}"),
            term("trait Family where { type Output : []; }"),
            term("impl Family for u32 where u32 : Marker { type Output = bool; }"),
        ])),
        ..Program::empty()
    }
}

fn deref_normalization_decls() -> Program {
    Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Copy where {}"),
            term("impl Copy for u32 {}"),
            term("trait Derefable where { type Target : []; }"),
            term(
                "impl<'a, T> Derefable for &'a T where T : 'a {
                    type Target = T;
                }",
            ),
            term(
                "impl<'a, T> Derefable for &mut 'a T where T : 'a {
                    type Target = T;
                }",
            ),
        ])),
        ..Program::empty()
    }
}

fn outlives_decls() -> Program {
    Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Lives<'a> where Self : 'a {}"),
            term("trait ValidationRoot where {}"),
            term("impl<'a, T> Lives<'a> for T where T : 'a {}"),
        ])),
        ..Program::empty()
    }
}

fn ranked_outlives_decls() -> Program {
    Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Lives<'a> where Self : 'a {}"),
            term("trait ValidationRoot where for<'a> Self : Lives<'a> {}"),
        ])),
        ..Program::empty()
    }
}

fn higher_ranked_supertrait_decls() -> Program {
    Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Super<'a> where {}"),
            term("trait Sub where for<'a> Self : Super<'a> {}"),
            term("trait ValidationRoot where Self : Sub {}"),
            term("impl<T> Sub for T where for<'a> T : Super<'a> {}"),
        ])),
        ..Program::empty()
    }
}

fn transitive_supertrait_decls() -> Program {
    Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Super where {}"),
            term("trait Mid where Self : Super {}"),
            term("trait Sub where Self : Mid {}"),
            term("trait ValidationRoot where Self : Sub {}"),
        ])),
        ..Program::empty()
    }
}

fn implication_validation_decls() -> Program {
    Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Super where {}"),
            term("trait Family where { type Item : [Super]; }"),
            term("trait ValidationRoot where {}"),
        ])),
        ..Program::empty()
    }
}

fn associated_requirement_decls() -> Program {
    Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Copy where {}"),
            term("trait Super where {}"),
            term(
                "trait Family where {
                    type Item<T> : [Super]
                    where
                        T : Copy;
                }",
            ),
        ])),
        ..Program::empty()
    }
}

#[test]
fn normalization_can_use_a_sufficient_validated_input() {
    let program = normalization_decls();
    let alias = term::<Parameter>("<u32 as Family>::Output");

    let from_validation = prove_normalize(
        &program,
        (),
        at_supertraits(term::<Wc>("Marker(u32)")),
        &alias,
    );
    assert!(from_validation.is_proven());

    let from_ordinary = prove_normalize(&program, (), term::<Wc>("Marker(u32)"), alias);
    assert!(from_ordinary.is_proven());
}

#[test]
fn normalization_ignores_an_incompatible_rigid_impl() {
    let Wc::ForAll(binder) =
        term::<Wc>("for<'a, 'b, 'c, 'd, 'e, 'f> Copy(<&mut 'a u32 as Derefable>::Target)")
    else {
        unreachable!()
    };
    let (env, variables) = Env::default().existential_substitution(&binder);
    let goal = binder.instantiate_with(&variables).unwrap();
    let env = env.with_allow_pending_outlives(true);
    let assumptions: Wcs = variables
        .iter()
        .map(|variable| -> Wc { Relation::well_formed(variable).upcast() })
        .collect();
    let result = prove_after(
        deref_normalization_decls(),
        Constraints::none(&env),
        assumptions,
        goal,
    );

    let solutions = result.into_map().expect("projection should normalize");
    assert!(solutions.keys().any(|constraints| constraints.known_true));
}

#[test]
fn equality_with_alias_stays_in_current_validation_context() {
    let alias = term::<Parameter>("<u32 as Family>::Output")
        .downcast::<AliasTy>()
        .unwrap();
    let alias_eq: Wc = Predicate::AliasEq(alias, term::<Ty>("bool")).upcast();
    let result = prove_after(
        Program::empty(),
        Constraints::none(()),
        at_supertraits(alias_eq),
        Relation::equals(
            term::<Parameter>("<u32 as Family>::Output"),
            term::<Parameter>("bool"),
        ),
    );

    assert!(!result.is_proven());
}

#[test]
fn associated_type_equality_normalizes_only_from_alias_to_value() {
    let alias = term::<Parameter>("<u32 as Family>::Output")
        .downcast::<AliasTy>()
        .unwrap();
    let alias_eq: Wc = Predicate::AliasEq(alias, term::<Ty>("bool")).upcast();

    let forward = prove_normalize(
        Program::empty(),
        (),
        &alias_eq,
        term::<Parameter>("<u32 as Family>::Output"),
    );
    assert!(forward.is_proven());

    let reverse = prove_normalize(Program::empty(), (), alias_eq, term::<Parameter>("bool"));
    assert!(!reverse.is_proven());
}

#[test]
fn normalizing_non_alias_uses_only_explicit_assumptions() {
    let result = prove_normalize(
        Program::empty(),
        (),
        at_supertraits(term::<Wc>("u32 = bool")),
        term::<Parameter>("u32"),
    );

    assert!(!result.is_proven());
}

#[test]
fn validation_evidence_is_not_ordinary_evidence() {
    let validated_sub = at_supertraits(sub());

    let validation_result = prove_after(
        decls(),
        Constraints::none(()),
        &validated_sub,
        &validated_sub,
    );
    assert!(validation_result.is_proven());

    let ordinary_result = prove_after(decls(), Constraints::none(()), validated_sub, sub());
    assert!(!ordinary_result.is_proven());
}

#[test]
fn observationally_zero_evidence_can_be_rebased() {
    let source = Wc::validate(Upto::supertraits(TraitId::new("SourceRoot")), sub());
    let goal = Wc::validate(Upto::supertraits(TraitId::new("GoalRoot")), sub());

    let result = prove_after(decls(), Constraints::none(()), source, goal);

    // `Sub` is unrelated to both roots, so neither frontier exposes any field of its dictionary.
    // Both wrappers therefore denote the same observationally-zero evidence.
    assert!(result.is_proven());
}

#[test]
fn gat_bound_frontier_can_discharge_supertrait_frontier() {
    let root = term::<Wc>("ValidationRoot(u32)");
    let result = prove_after(
        decls(),
        Constraints::none(()),
        at_gat_bounds(&root),
        at_supertraits(root),
    );

    assert!(result.is_proven());
}

#[test]
fn supertrait_frontier_cannot_discharge_gat_bound_frontier() {
    let root = term::<Wc>("ValidationRoot(u32)");
    let result = prove_after(
        decls(),
        Constraints::none(()),
        at_supertraits(&root),
        at_gat_bounds(root),
    );

    assert!(!result.is_proven());
}

#[test]
fn completed_impl_can_construct_evidence_at_either_frontier() {
    let program = Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Prerequisite where {}"),
            term("trait Marker where {}"),
            term("impl Prerequisite for u32 {}"),
            term("impl<T> Marker for T where T: Prerequisite {}"),
        ])),
        ..Program::empty()
    };

    let supertrait_result = prove_after(
        &program,
        Constraints::none(()),
        (),
        at_supertraits(term::<Wc>("Marker(u32)")),
    );
    assert!(supertrait_result.is_proven());

    let gat_bound_result = prove_after(
        &program,
        Constraints::none(()),
        (),
        at_gat_bounds(term::<Wc>("Marker(u32)")),
    );
    assert!(gat_bound_result.is_proven());

    let unsatisfied = prove_after(
        program,
        Constraints::none(()),
        (),
        at_gat_bounds(term::<Wc>("Marker(bool)")),
    );
    assert!(!unsatisfied.is_proven());
}

#[test]
fn opaque_validated_input_can_construct_an_opaque_result() {
    let program = Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Prerequisite where {}"),
            term("trait Marker where {}"),
            term("impl<T> Marker for T where T: Prerequisite {}"),
        ])),
        ..Program::empty()
    };

    let from_ordinary = prove_after(
        &program,
        Constraints::none(()),
        term::<Wc>("Prerequisite(u32)"),
        at_gat_bounds(term::<Wc>("Marker(u32)")),
    );
    assert!(from_ordinary.is_proven());

    let from_validated = prove_after(
        program,
        Constraints::none(()),
        at_supertraits(term::<Wc>("Prerequisite(u32)")),
        at_gat_bounds(term::<Wc>("Marker(u32)")),
    );
    assert!(from_validated.is_proven());
}

#[test]
fn ordinary_evidence_can_discharge_gat_bound_goal() {
    let result = prove_after(decls(), Constraints::none(()), sub(), at_gat_bounds(sub()));

    assert!(result.is_proven());
}

#[test]
fn ranked_gat_bound_evidence_elaborates_supertrait() {
    let result = prove_after(
        decls(),
        Constraints::none(()),
        at_gat_bounds(sub()),
        at_supertraits(term::<Wc>("Super(u32)")),
    );

    assert!(result.is_proven());
}

#[test]
fn gat_bound_validation_preserves_frontier_through_implication() {
    let implication = Wc::implies(sub(), term::<Wc>("Super(u32)"));
    let result = prove_after(
        decls(),
        Constraints::none(()),
        (),
        at_gat_bounds(implication),
    );

    assert!(result.is_proven());
}

#[test]
fn ranked_gat_bound_evidence_elaborates_transitive_supertrait() {
    let result = prove_after(
        transitive_supertrait_decls(),
        Constraints::none(()),
        at_gat_bounds(sub()),
        at_supertraits(term::<Wc>("Super(u32)")),
    );

    assert!(result.is_proven());
}

#[test]
fn ranked_gat_bound_evidence_elaborates_higher_ranked_supertrait() {
    let result = prove_after(
        higher_ranked_supertrait_decls(),
        Constraints::none(()),
        at_gat_bounds(sub()),
        at_supertraits(term::<Wc>("for<'a> Super(u32, 'a)")),
    );

    assert!(result.is_proven());
}

#[test]
fn ordinary_evidence_can_validate_an_atomic_clause() {
    let alias = term::<Parameter>("<u32 as Family>::Output")
        .downcast::<AliasTy>()
        .unwrap();
    let alias_eq: Wc = Predicate::AliasEq(alias, term::<Ty>("bool")).upcast();
    let const_has_type: Wc =
        Predicate::ConstHasType(term::<Const>("true"), term::<Ty>("bool")).upcast();
    let equality: Wc =
        Relation::equals(term::<Parameter>("u32"), term::<Parameter>("bool")).upcast();
    let outlives: Wc =
        Relation::outlives(term::<Parameter>("u32"), term::<Parameter>("'static")).upcast();

    for clause in [sub(), alias_eq, const_has_type, equality, outlives] {
        let result = prove_after(
            decls(),
            Constraints::none(()),
            &clause,
            at_supertraits(&clause),
        );
        assert!(result.is_proven(), "failed to validate {clause:?}");
    }
}

#[test]
fn every_atomic_relation_has_an_ordinary_validation_fallback() {
    let subtype: Wc = Relation::sub(term::<Parameter>("u32"), term::<Parameter>("u32")).upcast();

    let ordinary_result = prove_after(Program::empty(), Constraints::none(()), (), &subtype);
    assert!(ordinary_result.is_proven());

    let validation_result = prove_after(
        Program::empty(),
        Constraints::none(()),
        (),
        at_supertraits(subtype),
    );
    assert!(validation_result.is_proven());
}

#[test]
fn validated_relation_is_exact_and_not_ordinary_evidence() {
    let equality: Wc =
        Relation::equals(term::<Parameter>("u32"), term::<Parameter>("bool")).upcast();
    let reverse_equality: Wc =
        Relation::equals(term::<Parameter>("bool"), term::<Parameter>("u32")).upcast();
    let validated_equality = at_supertraits(&equality);

    let exact_result = prove_after(
        Program::empty(),
        Constraints::none(()),
        &validated_equality,
        &validated_equality,
    );
    assert!(exact_result.is_proven());

    let reverse_result = prove_after(
        Program::empty(),
        Constraints::none(()),
        &validated_equality,
        at_supertraits(reverse_equality),
    );
    assert!(!reverse_result.is_proven());

    let ordinary_result = prove_after(
        Program::empty(),
        Constraints::none(()),
        validated_equality,
        equality,
    );
    assert!(!ordinary_result.is_proven());
}

#[test]
fn validated_outlives_is_not_ordinary_evidence() {
    let Wc::ForAll(binder) = term::<Wc>("for<'a, 'b> 'a : 'b") else {
        unreachable!()
    };
    let (env, substitution) = Env::default().universal_substitution(&binder);
    let outlives = binder.instantiate_with(substitution).unwrap();
    let validated_outlives = at_supertraits(&outlives);

    let validation_result = prove_after(
        Program::empty(),
        Constraints::none(&env),
        &validated_outlives,
        &validated_outlives,
    );
    assert!(validation_result.is_proven());

    let ordinary_result = prove_after(
        Program::empty(),
        Constraints::none(env),
        validated_outlives,
        outlives,
    );
    assert!(!ordinary_result.is_proven());
}

#[test]
fn validation_preserves_predicate_congruence() {
    let assumptions: Wcs = (
        at_supertraits(term::<Wc>("Sub(u32)")),
        term::<Wc>("u32 = bool"),
    )
        .upcast();
    let result = prove_after(
        decls(),
        Constraints::none(()),
        assumptions,
        at_supertraits(term::<Wc>("Sub(bool)")),
    );

    assert!(result.is_proven());
}

#[test]
fn ranked_validation_elaborates_supertrait_through_implication() {
    let implication = Wc::implies(sub(), term::<Wc>("Super(u32)"));
    let result = prove_after(
        decls(),
        Constraints::none(()),
        (),
        at_supertraits(implication),
    );

    assert!(result.is_proven());
}

#[test]
fn validation_implication_introduces_only_validation_antecedents() {
    // The implication introduces `Family(u32)` only as evidence rooted at `ValidationRoot`.
    // Neither `Family` nor `Super` is below that root, so ranked associated-bound elaboration
    // cannot use it. If the antecedent leaked into the ordinary assumptions, unrestricted
    // ordinary elaboration would incorrectly prove the projected `Super` bound.
    let implication: Wc = term("if { Family(u32) } Super(<u32 as Family>::Item)");
    let result = prove_after(
        implication_validation_decls(),
        Constraints::none(()),
        (),
        at_supertraits(implication),
    );

    assert!(!result.is_proven());
}

#[test]
fn validation_implication_applies_consequence_constraints_to_antecedents() {
    let quantified: Wc = term("for<T> if { T = bool } Sub(T)");
    let assumptions: Wcs = (
        at_supertraits(quantified),
        at_supertraits(term::<Wc>("u32 = bool")),
    )
        .upcast();
    let result = prove_after(
        decls(),
        Constraints::none(()),
        assumptions,
        at_supertraits(sub()),
    );

    assert!(result.is_proven());
}

#[test]
fn ordinary_associated_bound_requires_originating_trait_and_gat_conditions() {
    let sufficient: Wc = term("for<T, U> if { Family(T), Copy(U) } Super(<T as Family>::Item<U>)");
    let sufficient_result = prove_after(
        associated_requirement_decls(),
        Constraints::none(()),
        (),
        sufficient,
    );
    assert!(sufficient_result.is_proven());

    let missing_gat_condition: Wc =
        term("for<T, U> if { Family(T) } Super(<T as Family>::Item<U>)");
    let missing_result = prove_after(
        associated_requirement_decls(),
        Constraints::none(()),
        (),
        missing_gat_condition,
    );
    assert!(!missing_result.is_proven());
}

#[test]
fn validation_preserves_frontier_through_well_formedness() {
    let validated_sub = at_supertraits(sub());
    let wf: Wc = Relation::well_formed(term::<Parameter>("NeedsSub<u32>")).upcast();
    let validated_wf = at_supertraits(&wf);

    let validation_result =
        prove_after(decls(), Constraints::none(()), &validated_sub, validated_wf);
    assert!(validation_result.is_proven());

    let ordinary_result = prove_after(decls(), Constraints::none(()), validated_sub, wf);
    assert!(!ordinary_result.is_proven());
}

#[test]
fn failed_impl_candidate_validation_assumptions_do_not_leak() {
    // The first `Target` candidate temporarily validates `Bad: Required`; the second temporarily
    // validates `Bad: Consume`. Neither candidate can prove its final where-clause on its own.
    // Combining hypotheses across failed candidate branches would incorrectly prove the goal.
    let program = Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Required where {}"),
            term("trait Super where {}"),
            term("trait Target where Self : Super {}"),
            term("trait Consume where {}"),
            term("struct Bad {}"),
            term("struct X {}"),
            term("impl Super for X {}"),
            term("impl Target for X where Bad : Required {}"),
            term("impl Target for X where Bad : Consume {}"),
            term("impl<T> Consume for T where T : Required {}"),
        ])),
        ..Program::empty()
    };

    let result = prove_after(program, Constraints::none(()), (), term::<Wc>("Target(X)"));

    assert!(!result.is_proven());
}

#[test]
fn failed_normalization_candidate_does_not_leak_its_provisional_alias_equality() {
    // The first `Family(X)` candidate temporarily fixes `Family::Output` to `Bad`, but its
    // `Missing: Required` residual fails. The second candidate could satisfy its recursive
    // `Family::Output: Marker` residual only if that first candidate's provisional equation
    // escaped into the sibling branch. Its own provisional equation fixes the output to `Good`,
    // which deliberately does not implement `Marker`.
    let program = Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Required where {}"),
            term("trait Marker where {}"),
            term("trait Family where { type Output : []; }"),
            term("struct Missing {}"),
            term("struct X {}"),
            term("struct Bad {}"),
            term("struct Good {}"),
            term("impl Marker for Bad {}"),
            term(
                "impl Family for X where Missing : Required {
                    type Output = Bad;
                }",
            ),
            term(
                "impl Family for X where <X as Family>::Output : Marker {
                    type Output = Good;
                }",
            ),
        ])),
        ..Program::empty()
    };

    let result = prove_normalize(program, (), (), term::<Parameter>("<X as Family>::Output"));

    assert!(result
        .iter()
        .all(|(Constrained(_, constraints), _)| !constraints.known_true));
}

#[test]
fn impl_validation_preserves_outlives_requirement() {
    let goal: Wc = term("for<'a, T> if {T : 'a} Lives(T, 'a)");
    let result = prove_after(outlives_decls(), Constraints::none(()), (), goal);

    assert!(result.is_proven());
}

#[test]
fn ordinary_trait_evidence_does_not_imply_outlives() {
    let goal: Wc = term("for<'a, T> if {Lives(T, 'a)} T : 'a");
    let result = prove_after(outlives_decls(), Constraints::none(()), (), goal);

    assert!(!result.is_proven());
}

#[test]
fn validation_without_rank_does_not_elaborate_outlives_through_implication() {
    // `Lives` is not below `ValidationRoot`, so its provisional evidence cannot expose the
    // declaration-side outlives requirement.
    let implication: Wc = term("for<'a, T> if {Lives(T, 'a)} T : 'a");
    let result = prove_after(
        outlives_decls(),
        Constraints::none(()),
        (),
        at_supertraits(implication),
    );

    assert!(!result.is_proven());
}

#[test]
fn ranked_validation_elaborates_outlives_through_implication() {
    let implication: Wc = term("for<'a, T> if {Lives(T, 'a)} T : 'a");
    let result = prove_after(
        ranked_outlives_decls(),
        Constraints::none(()),
        (),
        at_supertraits(implication),
    );

    assert!(result.is_proven());
}

#[test]
fn impl_validation_preserves_higher_ranked_supertrait_binder() {
    let goal: Wc = term("for<T> if {for<'a> Super(T, 'a)} Sub(T)");
    let result = prove_after(
        higher_ranked_supertrait_decls(),
        Constraints::none(()),
        (),
        goal,
    );

    assert!(result.is_proven());
}
