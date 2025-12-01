use expect_test::expect;
use formality_core::test;
use formality_types::{
    grammar::{Parameter, Relation, Wcs},
    rust::term,
};

use crate::{decls::Decls, prove::prove, Env};

fn decls() -> Decls {
    Decls {
        trait_decls: vec![term("trait Foo<ty Self> where {}")],
        impl_decls: vec![term("impl Foo(u32) where {}")],
        adt_decls: vec![term("adt X<ty T> where {Foo(T)} {}")],
        ..Decls::empty()
    }
}

#[test]
fn well_formed_adt() {
    let assumptions: Wcs = Wcs::t();
    let goal: Parameter = term("X<u32>");
    let constraints = prove(
        decls(),
        Env::default(),
        assumptions,
        Relation::WellFormed(goal),
    );
    expect![[r#"
        {
            Constraints {
                env: Env {
                    variables: [],
                    bias: Soundness,
                    pending: [],
                },
                known_true: true,
                substitution: {},
            }: ProofTree {
                judgment: "c = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                rule_name: Some(
                    "some",
                ),
                file: "crates/formality-prove/src/prove/prove_wc_list.rs",
                line: 11,
                column: 1,
                children: [
                    ProofTree {
                        judgment: "c = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                        rule_name: Some(
                            "parameter well formed",
                        ),
                        file: "crates/formality-prove/src/prove/prove_wc.rs",
                        line: 22,
                        column: 1,
                        children: [
                            ProofTree {
                                judgment: "c = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                rule_name: Some(
                                    "ADT",
                                ),
                                file: "crates/formality-prove/src/prove/prove_wf.rs",
                                line: 14,
                                column: 1,
                                children: [
                                    ProofTree {
                                        judgment: "for_all",
                                        rule_name: None,
                                        file: "crates/formality-prove/src/prove/combinators.rs",
                                        line: 73,
                                        column: 17,
                                        children: [
                                            ProofTree {
                                                judgment: "c = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                rule_name: Some(
                                                    "integers and booleans",
                                                ),
                                                file: "crates/formality-prove/src/prove/prove_wf.rs",
                                                line: 14,
                                                column: 1,
                                                children: [
                                                    ProofTree {
                                                        judgment: "Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                        rule_name: None,
                                                        file: "crates/formality-prove/src/prove/combinators.rs",
                                                        line: 61,
                                                        column: 13,
                                                        children: [],
                                                    },
                                                ],
                                            },
                                            ProofTree {
                                                judgment: "Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                rule_name: None,
                                                file: "crates/formality-prove/src/prove/combinators.rs",
                                                line: 61,
                                                column: 13,
                                                children: [],
                                            },
                                        ],
                                    },
                                    ProofTree {
                                        judgment: "t = adt X <ty> where {Foo(^ty0_0)} { }",
                                        rule_name: None,
                                        file: "crates/formality-prove/src/prove/prove_wf.rs",
                                        line: 14,
                                        column: 1,
                                        children: [],
                                    },
                                    ProofTree {
                                        judgment: "t = where {Foo(u32)} { }",
                                        rule_name: None,
                                        file: "crates/formality-prove/src/prove/prove_wf.rs",
                                        line: 14,
                                        column: 1,
                                        children: [],
                                    },
                                    ProofTree {
                                        judgment: "c1.seq(c2) = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                        rule_name: Some(
                                            "prove_after",
                                        ),
                                        file: "crates/formality-prove/src/prove/prove_after.rs",
                                        line: 8,
                                        column: 1,
                                        children: [
                                            ProofTree {
                                                judgment: "(assumptions, goal) = ({}, {Foo(u32)})",
                                                rule_name: None,
                                                file: "crates/formality-prove/src/prove/prove_after.rs",
                                                line: 8,
                                                column: 1,
                                                children: [],
                                            },
                                            ProofTree {
                                                judgment: "c = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                rule_name: Some(
                                                    "some",
                                                ),
                                                file: "crates/formality-prove/src/prove/prove_wc_list.rs",
                                                line: 11,
                                                column: 1,
                                                children: [
                                                    ProofTree {
                                                        judgment: "c.pop_subst(&subst) = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                        rule_name: Some(
                                                            "positive impl",
                                                        ),
                                                        file: "crates/formality-prove/src/prove/prove_wc.rs",
                                                        line: 22,
                                                        column: 1,
                                                        children: [
                                                            ProofTree {
                                                                judgment: "item = impl Foo(u32)",
                                                                rule_name: None,
                                                                file: "crates/formality-prove/src/prove/prove_wc.rs",
                                                                line: 22,
                                                                column: 1,
                                                                children: [],
                                                            },
                                                            ProofTree {
                                                                judgment: "(env, subst) = (Env { variables: [], bias: Soundness, pending: [] }, [])",
                                                                rule_name: None,
                                                                file: "crates/formality-prove/src/prove/prove_wc.rs",
                                                                line: 22,
                                                                column: 1,
                                                                children: [],
                                                            },
                                                            ProofTree {
                                                                judgment: "i = Foo(u32)",
                                                                rule_name: None,
                                                                file: "crates/formality-prove/src/prove/prove_wc.rs",
                                                                line: 22,
                                                                column: 1,
                                                                children: [],
                                                            },
                                                            ProofTree {
                                                                judgment: "t = ",
                                                                rule_name: None,
                                                                file: "crates/formality-prove/src/prove/prove_wc.rs",
                                                                line: 22,
                                                                column: 1,
                                                                children: [],
                                                            },
                                                            ProofTree {
                                                                judgment: "co_assumptions = ({}, Foo(u32))",
                                                                rule_name: None,
                                                                file: "crates/formality-prove/src/prove/prove_wc.rs",
                                                                line: 22,
                                                                column: 1,
                                                                children: [],
                                                            },
                                                            ProofTree {
                                                                judgment: "c = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                                rule_name: Some(
                                                                    "some",
                                                                ),
                                                                file: "crates/formality-prove/src/prove/prove_wc_list.rs",
                                                                line: 11,
                                                                column: 1,
                                                                children: [
                                                                    ProofTree {
                                                                        judgment: "c = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                                        rule_name: Some(
                                                                            "eq",
                                                                        ),
                                                                        file: "crates/formality-prove/src/prove/prove_wc.rs",
                                                                        line: 22,
                                                                        column: 1,
                                                                        children: [
                                                                            ProofTree {
                                                                                judgment: "trivial, as trivial_expr is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                                                rule_name: None,
                                                                                file: "crates/formality-prove/src/prove/prove_eq.rs",
                                                                                line: 35,
                                                                                column: 17,
                                                                                children: [],
                                                                            },
                                                                        ],
                                                                    },
                                                                    ProofTree {
                                                                        judgment: "c1.seq(c2) = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                                        rule_name: Some(
                                                                            "prove_after",
                                                                        ),
                                                                        file: "crates/formality-prove/src/prove/prove_after.rs",
                                                                        line: 8,
                                                                        column: 1,
                                                                        children: [
                                                                            ProofTree {
                                                                                judgment: "(assumptions, goal) = ({Foo(u32)}, {})",
                                                                                rule_name: None,
                                                                                file: "crates/formality-prove/src/prove/prove_after.rs",
                                                                                line: 8,
                                                                                column: 1,
                                                                                children: [],
                                                                            },
                                                                            ProofTree {
                                                                                judgment: "Constraints::none(env) = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                                                rule_name: Some(
                                                                                    "none",
                                                                                ),
                                                                                file: "crates/formality-prove/src/prove/prove_wc_list.rs",
                                                                                line: 11,
                                                                                column: 1,
                                                                                children: [],
                                                                            },
                                                                        ],
                                                                    },
                                                                ],
                                                            },
                                                            ProofTree {
                                                                judgment: "c1.seq(c2) = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                                rule_name: Some(
                                                                    "prove_after",
                                                                ),
                                                                file: "crates/formality-prove/src/prove/prove_after.rs",
                                                                line: 8,
                                                                column: 1,
                                                                children: [
                                                                    ProofTree {
                                                                        judgment: "(assumptions, goal) = ({Foo(u32)}, {})",
                                                                        rule_name: None,
                                                                        file: "crates/formality-prove/src/prove/prove_after.rs",
                                                                        line: 8,
                                                                        column: 1,
                                                                        children: [],
                                                                    },
                                                                    ProofTree {
                                                                        judgment: "Constraints::none(env) = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                                        rule_name: Some(
                                                                            "none",
                                                                        ),
                                                                        file: "crates/formality-prove/src/prove/prove_wc_list.rs",
                                                                        line: 11,
                                                                        column: 1,
                                                                        children: [],
                                                                    },
                                                                ],
                                                            },
                                                            ProofTree {
                                                                judgment: "c1.seq(c2) = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                                rule_name: Some(
                                                                    "prove_after",
                                                                ),
                                                                file: "crates/formality-prove/src/prove/prove_after.rs",
                                                                line: 8,
                                                                column: 1,
                                                                children: [
                                                                    ProofTree {
                                                                        judgment: "(assumptions, goal) = ({}, {})",
                                                                        rule_name: None,
                                                                        file: "crates/formality-prove/src/prove/prove_after.rs",
                                                                        line: 8,
                                                                        column: 1,
                                                                        children: [],
                                                                    },
                                                                    ProofTree {
                                                                        judgment: "Constraints::none(env) = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                                        rule_name: Some(
                                                                            "none",
                                                                        ),
                                                                        file: "crates/formality-prove/src/prove/prove_wc_list.rs",
                                                                        line: 11,
                                                                        column: 1,
                                                                        children: [],
                                                                    },
                                                                ],
                                                            },
                                                        ],
                                                    },
                                                    ProofTree {
                                                        judgment: "c1.seq(c2) = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                        rule_name: Some(
                                                            "prove_after",
                                                        ),
                                                        file: "crates/formality-prove/src/prove/prove_after.rs",
                                                        line: 8,
                                                        column: 1,
                                                        children: [
                                                            ProofTree {
                                                                judgment: "(assumptions, goal) = ({}, {})",
                                                                rule_name: None,
                                                                file: "crates/formality-prove/src/prove/prove_after.rs",
                                                                line: 8,
                                                                column: 1,
                                                                children: [],
                                                            },
                                                            ProofTree {
                                                                judgment: "Constraints::none(env) = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                                                rule_name: Some(
                                                                    "none",
                                                                ),
                                                                file: "crates/formality-prove/src/prove/prove_wc_list.rs",
                                                                line: 11,
                                                                column: 1,
                                                                children: [],
                                                            },
                                                        ],
                                                    },
                                                ],
                                            },
                                        ],
                                    },
                                ],
                            },
                        ],
                    },
                    ProofTree {
                        judgment: "c1.seq(c2) = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                        rule_name: Some(
                            "prove_after",
                        ),
                        file: "crates/formality-prove/src/prove/prove_after.rs",
                        line: 8,
                        column: 1,
                        children: [
                            ProofTree {
                                judgment: "(assumptions, goal) = ({}, {})",
                                rule_name: None,
                                file: "crates/formality-prove/src/prove/prove_after.rs",
                                line: 8,
                                column: 1,
                                children: [],
                            },
                            ProofTree {
                                judgment: "Constraints::none(env) = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }",
                                rule_name: Some(
                                    "none",
                                ),
                                file: "crates/formality-prove/src/prove/prove_wc_list.rs",
                                line: 11,
                                column: 1,
                                children: [],
                            },
                        ],
                    },
                ],
            },
        }
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn not_well_formed_adt() {
    let assumptions: Wcs = Wcs::t();
    let goal: Parameter = term("X<u64>");
    prove(
        decls(),
        Env::default(),
        assumptions,
        Relation::WellFormed(goal),
    ).assert_err(expect![[r#"
        judgment `prove { goal: {@ wf(X<u64>)}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {}) }` failed at the following rule(s):
          failed at (src/file.rs:LL:CC) because
            judgment `prove_wc_list { goals: {@ wf(X<u64>)}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                judgment `prove_wc { goal: @ wf(X<u64>), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                  the rule "parameter well formed" failed at step #0 (src/file.rs:LL:CC) because
                    judgment `prove_wf { goal: X<u64>, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "ADT" failed at step #3 (src/file.rs:LL:CC) because
                        judgment `prove_after { constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }, goal: {Foo(u64)}, assumptions: {} }` failed at the following rule(s):
                          the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                            judgment `prove { goal: {Foo(u64)}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {}) }` failed at the following rule(s):
                              failed at (src/file.rs:LL:CC) because
                                judgment `prove_wc_list { goals: {Foo(u64)}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                    judgment `prove_wc { goal: Foo(u64), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                      the rule "trait implied bound" failed at step #0 (src/file.rs:LL:CC) because
                                        expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}
