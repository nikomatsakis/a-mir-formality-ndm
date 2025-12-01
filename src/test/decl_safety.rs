#![allow(non_snake_case)]

#[test]
fn unsafe_trait() {
    crate::assert_ok!(
        //@check-pass
        [
            crate baguette {
                unsafe trait Foo {}
                unsafe impl Foo for u32 {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(baguette): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {Foo(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: Foo(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = unsafe impl Foo(u32): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [], bias: Soundness, pending: [] }, []): at prove_wc.rs:22
                           └─ i = Foo(u32): at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ co_assumptions = ({}, Foo(u32)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {Foo(u32)}
                                  goals: {u32 = u32}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Foo(u32)}
                                     goal: u32 = u32
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Foo(u32)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({Foo(u32)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Foo(u32)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Foo(u32)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Foo(u32)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Foo(u32)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Foo(u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Foo(u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Foo(u32), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(unsafe, unsafe): at impls.rs:122
                  └─ check_coherence(baguette): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(Foo(u32))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(Foo(u32))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: Foo(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {}), &goal.trait_id: Foo }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [unsafe trait Foo <ty> ], [unsafe impl Foo(u32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
        "#]]
    )
}

#[test]
fn safe_trait() {
    crate::assert_ok!(
        //@check-pass
        [
            crate baguette {
                safe trait Foo {}
                safe impl Foo for u32 {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(baguette): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {Foo(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
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
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {Foo(u32)}
                                  goals: {u32 = u32}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Foo(u32)}
                                     goal: u32 = u32
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Foo(u32)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({Foo(u32)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Foo(u32)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Foo(u32)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Foo(u32)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Foo(u32)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Foo(u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Foo(u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Foo(u32), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_coherence(baguette): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(Foo(u32))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(Foo(u32))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: Foo(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {}), &goal.trait_id: Foo }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
        "#]]
    )
}

#[test]
fn unsafe_trait_negative_impl() {
    crate::assert_ok!(
        //@check-pass
        [
            crate baguette {
                unsafe trait Foo {}
                impl !Foo for u32 {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(baguette): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_neg_trait_impl(Foo(u32)): at impls.rs:101
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {! Foo(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (negative impl) at prove_wc.rs:22
                               _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: ! Foo(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl ! Foo(u32): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [], bias: Soundness, pending: [] }, []): at prove_wc.rs:22
                           └─ i = ! Foo(u32): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {u32 = u32}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32 = u32
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_coherence(baguette): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(Foo(u32))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(Foo(u32))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: Foo(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {}), &goal.trait_id: Foo }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [unsafe trait Foo <ty> ], [], [impl ! Foo(u32)], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
        "#]]
    )
}

#[test]
fn unsafe_trait_negative_impl_mismatch() {
    crate::assert_err!(
        [
            crate baguette {
                unsafe trait Foo {}
                unsafe impl !Foo for u32 {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_neg_trait_impl(unsafe impl ! Foo for u32 {})

            Caused by:
                negative impls cannot be unsafe"#]]
    )
}

#[test]
fn safe_trait_negative_impl_mismatch() {
    crate::assert_err!(
        [
            crate baguette {
                trait Foo {}
                unsafe impl !Foo for u32 {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_neg_trait_impl(unsafe impl ! Foo for u32 {})

            Caused by:
                negative impls cannot be unsafe"#]]
    )
}

#[test]
fn unsafe_trait_mismatch() {
    crate::assert_err!(
        [
            crate baguette {
                unsafe trait Foo {}
                impl Foo for u32 {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_trait_impl(impl Foo for u32 { })

            Caused by:
                the trait `Foo` requires an `unsafe impl` declaration"#]]
    )
}

#[test]
fn safe_trait_mismatch() {
    crate::assert_err!(
        [
            crate baguette {
                trait Foo {}
                unsafe impl Foo for u32 {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_trait_impl(unsafe impl Foo for u32 { })

            Caused by:
                implementing the trait `Foo` is not unsafe"#]]
    )
}
