use std::sync::Arc;

use crate::grammar::{
    AliasTy, Const, Parameter, Predicate, Relation, TraitId, Ty, ValidationContext,
    ValidationState, Wc, Wcs,
};
use crate::prove::prove::{
    decls::Program,
    prove::{Constraints, Env},
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

fn validated_at(state: ValidationState, wc: impl Upcast<Wc>) -> Wc {
    let wc: Wc = wc.upcast();
    Wc::validate(
        ValidationContext::new(state, TraitId::new("ValidationRoot")),
        wc,
    )
}

fn validated(wc: impl Upcast<Wc>) -> Wc {
    validated_at(ValidationState::A, wc)
}

fn validated_b(wc: impl Upcast<Wc>) -> Wc {
    validated_at(ValidationState::B, wc)
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
fn normalizing_alias_uses_only_explicit_assumptions() {
    let program = normalization_decls();
    let alias = term::<Parameter>("<u32 as Family>::Output");

    let from_validation =
        prove_normalize(&program, (), validated(term::<Wc>("Marker(u32)")), &alias);
    assert!(!from_validation.is_proven());

    let from_ordinary = prove_normalize(&program, (), term::<Wc>("Marker(u32)"), alias);
    assert!(from_ordinary.is_proven());
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
        validated(alias_eq),
        Relation::equals(
            term::<Parameter>("<u32 as Family>::Output"),
            term::<Parameter>("bool"),
        ),
    );

    assert!(!result.is_proven());
}

#[test]
fn normalizing_non_alias_uses_only_explicit_assumptions() {
    let result = prove_normalize(
        Program::empty(),
        (),
        validated(term::<Wc>("u32 = bool")),
        term::<Parameter>("u32"),
    );

    assert!(!result.is_proven());
}

#[test]
fn validation_evidence_is_not_ordinary_evidence() {
    let validated_sub = validated(sub());

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
fn validation_evidence_is_scoped_to_one_impl_trait() {
    let source = Wc::validate(
        ValidationContext::new(ValidationState::A, TraitId::new("SourceRoot")),
        sub(),
    );
    let goal = Wc::validate(
        ValidationContext::new(ValidationState::A, TraitId::new("GoalRoot")),
        sub(),
    );

    let result = prove_after(decls(), Constraints::none(()), source, goal);

    assert!(!result.is_proven());
}

#[test]
fn stage_b_validation_evidence_can_discharge_stage_a_goal() {
    let result = prove_after(
        decls(),
        Constraints::none(()),
        validated_b(sub()),
        validated(sub()),
    );

    assert!(result.is_proven());
}

#[test]
fn stage_a_validation_evidence_cannot_discharge_stage_b_goal() {
    let result = prove_after(
        decls(),
        Constraints::none(()),
        validated(sub()),
        validated_b(sub()),
    );

    assert!(!result.is_proven());
}

#[test]
fn completed_impl_can_construct_stage_b_validation_evidence() {
    let program = Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Prerequisite where {}"),
            term("trait Marker where {}"),
            term("impl Prerequisite for u32 {}"),
            term("impl<T> Marker for T where T: Prerequisite {}"),
        ])),
        ..Program::empty()
    };

    let stage_a = prove_after(
        &program,
        Constraints::none(()),
        (),
        validated(term::<Wc>("Marker(u32)")),
    );
    assert!(stage_a.is_proven());

    let stage_b = prove_after(
        &program,
        Constraints::none(()),
        (),
        validated_b(term::<Wc>("Marker(u32)")),
    );
    assert!(stage_b.is_proven());

    let unsatisfied = prove_after(
        program,
        Constraints::none(()),
        (),
        validated_b(term::<Wc>("Marker(bool)")),
    );
    assert!(!unsatisfied.is_proven());
}

#[test]
fn stage_b_impl_inherits_only_completed_evidence() {
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
        validated_b(term::<Wc>("Marker(u32)")),
    );
    assert!(from_ordinary.is_proven());

    let from_stage_a = prove_after(
        program,
        Constraints::none(()),
        validated(term::<Wc>("Prerequisite(u32)")),
        validated_b(term::<Wc>("Marker(u32)")),
    );
    assert!(!from_stage_a.is_proven());
}

#[test]
fn ordinary_evidence_can_discharge_stage_b_goal() {
    let result = prove_after(decls(), Constraints::none(()), sub(), validated_b(sub()));

    assert!(result.is_proven());
}

#[test]
fn ranked_stage_b_validation_evidence_elaborates_supertrait() {
    let result = prove_after(
        decls(),
        Constraints::none(()),
        validated_b(sub()),
        validated(term::<Wc>("Super(u32)")),
    );

    assert!(result.is_proven());
}

#[test]
fn stage_b_validation_preserves_stage_through_implication() {
    let implication = Wc::implies(sub(), term::<Wc>("Super(u32)"));
    let result = prove_after(decls(), Constraints::none(()), (), validated_b(implication));

    assert!(result.is_proven());
}

#[test]
fn ranked_stage_b_validation_evidence_elaborates_transitive_supertrait() {
    let result = prove_after(
        transitive_supertrait_decls(),
        Constraints::none(()),
        validated_b(sub()),
        validated(term::<Wc>("Super(u32)")),
    );

    assert!(result.is_proven());
}

#[test]
fn ranked_stage_b_validation_evidence_elaborates_higher_ranked_supertrait() {
    let result = prove_after(
        higher_ranked_supertrait_decls(),
        Constraints::none(()),
        validated_b(sub()),
        validated(term::<Wc>("for<'a> Super(u32, 'a)")),
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
        let result = prove_after(decls(), Constraints::none(()), &clause, validated(&clause));
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
        validated(subtype),
    );
    assert!(validation_result.is_proven());
}

#[test]
fn validated_relation_is_exact_and_not_ordinary_evidence() {
    let equality: Wc =
        Relation::equals(term::<Parameter>("u32"), term::<Parameter>("bool")).upcast();
    let reverse_equality: Wc =
        Relation::equals(term::<Parameter>("bool"), term::<Parameter>("u32")).upcast();
    let validated_equality = validated(&equality);

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
        validated(reverse_equality),
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
    let validated_outlives = validated(&outlives);

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
    let assumptions: Wcs = (validated(term::<Wc>("Sub(u32)")), term::<Wc>("u32 = bool")).upcast();
    let result = prove_after(
        decls(),
        Constraints::none(()),
        assumptions,
        validated(term::<Wc>("Sub(bool)")),
    );

    assert!(result.is_proven());
}

#[test]
fn ranked_validation_elaborates_supertrait_through_implication() {
    let implication = Wc::implies(sub(), term::<Wc>("Super(u32)"));
    let result = prove_after(decls(), Constraints::none(()), (), validated(implication));

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
        validated(implication),
    );

    assert!(!result.is_proven());
}

#[test]
fn validation_implication_applies_consequence_constraints_to_antecedents() {
    let quantified: Wc = term("for<T> if { T = bool } Sub(T)");
    let assumptions: Wcs = (validated(quantified), validated(term::<Wc>("u32 = bool"))).upcast();
    let result = prove_after(
        decls(),
        Constraints::none(()),
        assumptions,
        validated(sub()),
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
fn validation_preserves_mode_through_well_formedness() {
    let validated_sub = validated(sub());
    let wf: Wc = Relation::well_formed(term::<Parameter>("NeedsSub<u32>")).upcast();
    let validated_wf = validated(&wf);

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
        validated(implication),
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
        validated(implication),
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
