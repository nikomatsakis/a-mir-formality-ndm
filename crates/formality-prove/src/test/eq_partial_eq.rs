use expect_test::expect;
use formality_macros::test;
use formality_types::{
    grammar::{Wc, Wcs},
    rust::term,
};

use crate::{decls::Decls, prove::prove};

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        trait_decls: vec![
            term("trait Eq<ty Self> where {PartialEq(Self)}"),
            term("trait PartialEq<ty Self> where {}"),
        ],
        ..Decls::empty()
    }
}

#[test]
fn eq_implies_partial_eq() {
    let assumptions: Wcs = Wcs::t();
    let goal: Wc = term("for<ty T> if {Eq(T)} PartialEq(T)");
    let constraints = prove(decls(), (), assumptions, goal);
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
                   _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                   env: Env { variables: [], bias: Soundness, pending: [] }
                   assumptions: {}
                   goals: {for <ty> if {Eq(^ty0_0)} PartialEq(^ty0_0)}
                   result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
               └─ prove_wc: (forall) at prove_wc.rs:22
                      _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                      env: Env { variables: [], bias: Soundness, pending: [] }
                      assumptions: {}
                      goal: for <ty> if {Eq(^ty0_0)} PartialEq(^ty0_0)
                      result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ (env, subst) = (Env { variables: [!ty_1], bias: Soundness, pending: [] }, [!ty_1]): at prove_wc.rs:22
                  └─ p1 = if {Eq(!ty_1)} PartialEq(!ty_1): at prove_wc.rs:22
                  └─ prove_wc: (implies) at prove_wc.rs:22
                         _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                         env: Env { variables: [!ty_1], bias: Soundness, pending: [] }
                         assumptions: {}
                         goal: if {Eq(!ty_1)} PartialEq(!ty_1)
                         result: Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc: (trait implied bound) at prove_wc.rs:22
                            _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                            env: Env { variables: [!ty_1], bias: Soundness, pending: [] }
                            assumptions: {Eq(!ty_1)}
                            goal: PartialEq(!ty_1)
                            result: Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ item = trait_invariant(<ty> Eq(^ty0_0) => PartialEq(^ty0_0)): at prove_wc.rs:22
                        └─ (env, subst) = (Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [] }, [?ty_2]): at prove_wc.rs:22
                        └─ ti = Eq(?ty_2) => PartialEq(?ty_2): at prove_wc.rs:22
                        └─ prove_via: (predicate-congruence-axiom) at prove_via.rs:9
                               _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                               env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [] }
                               assumptions: {Eq(!ty_1)}
                               via: PartialEq(?ty_2)
                               goal: PartialEq(!ty_1)
                               result: Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_1} }
                           └─ (skel_c, parameters_c) = (is_implemented(PartialEq), [?ty_2]): at prove_via.rs:9
                           └─ (skel_g, parameters_g) = (is_implemented(PartialEq), [!ty_1]): at prove_via.rs:9
                           └─ IfThen { expression: "skel_c == skel_g", skel_c: is_implemented(PartialEq), skel_g: is_implemented(PartialEq) }: at prove_via.rs:9
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                                  env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                  assumptions: {Eq(!ty_0)}
                                  goals: {?ty_1 = !ty_0}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                                     env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                     assumptions: {Eq(!ty_0)}
                                     goal: ?ty_1 = !ty_0
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                 └─ prove_eq: (existential) at prove_eq.rs:23
                                        _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                                        env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                        assumptions: {Eq(!ty_0)}
                                        a: ?ty_1
                                        b: !ty_0
                                        result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                    └─ prove_existential_var_eq: (existential-universal) at prove_eq.rs:76
                                           _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                                           env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                           assumptions: {Eq(!ty_0)}
                                           v: ?ty_1
                                           b: !ty_0
                                           result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                       └─ IfThen { expression: "env.universe(p) < env.universe(v)" }: at prove_eq.rs:76
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                                     constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                     assumptions: {Eq(!ty_0)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                 └─ (assumptions, goal) = ({Eq(!ty_0)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                        assumptions: {Eq(!ty_0)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                               constraints: Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_1} }
                               assumptions: {Eq(!ty_1)}
                               goal: {Eq(?ty_2)}
                               result: Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_1} }
                           └─ (assumptions, goal) = ({Eq(!ty_1)}, {Eq(!ty_1)}): at prove_after.rs:8
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Eq(!ty_0)}
                                  goals: {Eq(!ty_0)}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (assumption - predicate) at prove_wc.rs:22
                                     _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Eq(!ty_0)}
                                     goal: Eq(!ty_0)
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ item = Eq(!ty_0): at prove_wc.rs:22
                                 └─ prove_via: (predicate-congruence-axiom) at prove_via.rs:9
                                        _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                        assumptions: {Eq(!ty_0)}
                                        via: Eq(!ty_0)
                                        goal: Eq(!ty_0)
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (skel_c, parameters_c) = (is_implemented(Eq), [!ty_0]): at prove_via.rs:9
                                    └─ (skel_g, parameters_g) = (is_implemented(Eq), [!ty_0]): at prove_via.rs:9
                                    └─ IfThen { expression: "skel_c == skel_g", skel_c: is_implemented(Eq), skel_g: is_implemented(Eq) }: at prove_via.rs:9
                                    └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                                           env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                           assumptions: {Eq(!ty_0)}
                                           goals: {!ty_0 = !ty_0}
                                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ prove_wc: (eq) at prove_wc.rs:22
                                              _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                                              env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                              assumptions: {Eq(!ty_0)}
                                              goal: !ty_0 = !ty_0
                                              result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ trivial, as a == b is true: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                       └─ prove_after: (prove_after) at prove_after.rs:8
                                              _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                                              constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                              assumptions: {Eq(!ty_0)}
                                              goal: {}
                                              result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ (assumptions, goal) = ({Eq(!ty_0)}, {}): at prove_after.rs:8
                                          └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                 _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                                                 env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                 assumptions: {Eq(!ty_0)}
                                                 goals: {}
                                                 result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                                     constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Eq(!ty_0)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({Eq(!ty_0)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                        assumptions: {Eq(!ty_0)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
               └─ prove_after: (prove_after) at prove_after.rs:8
                      _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
                      constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                      assumptions: {}
                      goal: {}
                      result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                  └─ prove_wc_list: (none) at prove_wc_list.rs:11
                         _decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {})
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
fn not_partial_eq_implies_eq() {
    let goal: Wc = term("for<ty T> if {PartialEq(T)} Eq(T)");
    prove(decls(), (), (), goal)
    .assert_err(
    expect![[r#"
        judgment `prove { goal: {for <ty> if {PartialEq(^ty0_0)} Eq(^ty0_0)}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
          failed at (src/file.rs:LL:CC) because
            judgment `prove_wc_list { goals: {for <ty> if {PartialEq(^ty0_0)} Eq(^ty0_0)}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                judgment `prove_wc { goal: for <ty> if {PartialEq(^ty0_0)} Eq(^ty0_0), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                  the rule "forall" failed at step #2 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: if {PartialEq(!ty_1)} Eq(!ty_1), assumptions: {}, env: Env { variables: [!ty_1], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "implies" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: Eq(!ty_1), assumptions: {PartialEq(!ty_1)}, env: Env { variables: [!ty_1], bias: Soundness, pending: [] } }` failed at the following rule(s):
                          the rule "assumption - predicate" failed at step #1 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_via { goal: Eq(!ty_1), via: PartialEq(!ty_1), assumptions: {PartialEq(!ty_1)}, env: Env { variables: [!ty_1], bias: Soundness, pending: [] } }`
                          the rule "trait implied bound" failed at step #3 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_via { goal: Eq(!ty_1), via: PartialEq(?ty_2), assumptions: {PartialEq(!ty_1)}, env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [] } }`"#]]);
}

#[test]
fn universals_not_eq() {
    let goal: Wc = term("for<ty T, ty U> if {Eq(T)} PartialEq(U)");
    prove(decls(), (), (), goal)
    .assert_err(
    expect![[r#"
        judgment `prove { goal: {for <ty, ty> if {Eq(^ty0_0)} PartialEq(^ty0_1)}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
          failed at (src/file.rs:LL:CC) because
            judgment `prove_wc_list { goals: {for <ty, ty> if {Eq(^ty0_0)} PartialEq(^ty0_1)}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                judgment `prove_wc { goal: for <ty, ty> if {Eq(^ty0_0)} PartialEq(^ty0_1), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                  the rule "forall" failed at step #2 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: if {Eq(!ty_1)} PartialEq(!ty_2), assumptions: {}, env: Env { variables: [!ty_1, !ty_2], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "implies" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: PartialEq(!ty_2), assumptions: {Eq(!ty_1)}, env: Env { variables: [!ty_1, !ty_2], bias: Soundness, pending: [] } }` failed at the following rule(s):
                          the rule "assumption - predicate" failed at step #1 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_via { goal: PartialEq(!ty_2), via: Eq(!ty_1), assumptions: {Eq(!ty_1)}, env: Env { variables: [!ty_1, !ty_2], bias: Soundness, pending: [] } }`
                          the rule "trait implied bound" failed at step #4 (src/file.rs:LL:CC) because
                            judgment `prove_after { constraints: Constraints { env: Env { variables: [!ty_1, !ty_2, ?ty_3], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_3 => !ty_2} }, goal: {Eq(?ty_3)}, assumptions: {Eq(!ty_1)} }` failed at the following rule(s):
                              the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                                judgment `prove { goal: {Eq(!ty_1)}, assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                                  failed at (src/file.rs:LL:CC) because
                                    judgment `prove_wc_list { goals: {Eq(!ty_1)}, assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                        judgment `prove_wc { goal: Eq(!ty_1), assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                          the rule "assumption - predicate" failed at step #1 (src/file.rs:LL:CC) because
                                            judgment `prove_via { goal: Eq(!ty_1), via: Eq(!ty_0), assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                              the rule "predicate-congruence-axiom" failed at step #3 (src/file.rs:LL:CC) because
                                                judgment `prove { goal: {!ty_0 = !ty_1}, assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                                                  failed at (src/file.rs:LL:CC) because
                                                    judgment `prove_wc_list { goals: {!ty_0 = !ty_1}, assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                                        judgment `prove_wc { goal: !ty_0 = !ty_1, assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                                          the rule "assumption - relation" failed at step #1 (src/file.rs:LL:CC) because
                                                            judgment had no applicable rules: `prove_via { goal: !ty_0 = !ty_1, via: Eq(!ty_0), assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] } }`
                                                          the rule "eq" failed at step #0 (src/file.rs:LL:CC) because
                                                            judgment `prove_eq { a: !ty_0, b: !ty_1, assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                                              the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                                                                judgment `prove_normalize { p: !ty_0, assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                                                  the rule "normalize-via-assumption" failed at step #1 (src/file.rs:LL:CC) because
                                                                    judgment had no applicable rules: `prove_normalize_via { goal: !ty_0, via: Eq(!ty_0), assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] } }`
                                                              the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                                                judgment `prove_eq { a: !ty_1, b: !ty_0, assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                                                  the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                                                                    judgment `prove_normalize { p: !ty_1, assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                                                      the rule "normalize-via-assumption" failed at step #1 (src/file.rs:LL:CC) because
                                                                        judgment had no applicable rules: `prove_normalize_via { goal: !ty_1, via: Eq(!ty_0), assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] } }`
                                                                  the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                                                    cyclic proof attempt: `prove_eq { a: !ty_0, b: !ty_1, assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] } }`
                                          the rule "trait implied bound" failed at step #3 (src/file.rs:LL:CC) because
                                            judgment had no applicable rules: `prove_via { goal: Eq(!ty_1), via: PartialEq(?ty_2), assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1, ?ty_2], bias: Soundness, pending: [] } }`"#]]);
}
