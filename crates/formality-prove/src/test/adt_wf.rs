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
            }: └─ prove_wc_list: (some) at prove_wc_list.rs:11
                   _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                   env: Env { variables: [], bias: Soundness, pending: [] }
                   assumptions: {}
                   goals: {@ wf(X<u32>)}
                   result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
               └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                      _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                      env: Env { variables: [], bias: Soundness, pending: [] }
                      assumptions: {}
                      goal: @ wf(X<u32>)
                      result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ prove_wf: (ADT) at prove_wf.rs:14
                         _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                         env: Env { variables: [], bias: Soundness, pending: [] }
                         assumptions: {}
                         goal: X<u32>
                         result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ for_all: at combinators.rs:73
                        └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                               _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: u32
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                     └─ t = adt X <ty> where {Foo(^ty0_0)} { }: at prove_wf.rs:14
                     └─ t = where {Foo(u32)} { }: at prove_wf.rs:14
                     └─ prove_after: (prove_after) at prove_after.rs:8
                            _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                            constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                            assumptions: {}
                            goal: {Foo(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ (assumptions, goal) = ({}, {Foo(u32)}): at prove_after.rs:8
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {Foo(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (positive impl) at prove_wc.rs:22
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: Foo(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ item = impl Foo(u32): at prove_wc.rs:22
                              └─ (env, subst) = (Env { variables: [], bias: Soundness, pending: [] }, []): at prove_wc.rs:22
                              └─ i = Foo(u32): at prove_wc.rs:22
                              └─ t = : at prove_wc.rs:22
                              └─ co_assumptions = ({}, Foo(u32)): at prove_wc.rs:22
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Foo(u32)}
                                     goals: {u32 = u32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (eq) at prove_wc.rs:22
                                        _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Foo(u32)}
                                        goal: u32 = u32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                                        constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {Foo(u32)}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({Foo(u32)}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {Foo(u32)}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Foo(u32)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({Foo(u32)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Foo(u32)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
               └─ prove_after: (prove_after) at prove_after.rs:8
                      _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                      constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                      assumptions: {}
                      goal: {}
                      result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                  └─ prove_wc_list: (none) at prove_wc_list.rs:11
                         _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)} { }], {}, {})
                         env: Env { variables: [], bias: Soundness, pending: [] }
                         assumptions: {}
                         goals: {}
                         result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
            ,
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
