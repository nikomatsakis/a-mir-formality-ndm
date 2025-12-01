#![allow(non_snake_case)]

#[test]
fn neg_CoreTrait_for_CoreStruct_in_Foo() {
    crate::assert_err!(
        [
            crate core {
                trait CoreTrait {}
                struct CoreStruct {}
            },
            crate foo {
                impl !CoreTrait for CoreStruct {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            orphan_check_neg(impl ! CoreTrait for CoreStruct {})

            Caused by:
                judgment `prove { goal: {@ IsLocal(CoreTrait(CoreStruct))}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {}, {}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goals: {@ IsLocal(CoreTrait(CoreStruct))}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: @ IsLocal(CoreTrait(CoreStruct)), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                          the rule "trait ref is local" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `is_local_trait_ref { goal: CoreTrait(CoreStruct), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                              the rule "local parameter" failed at step #1 (src/file.rs:LL:CC) because
                                judgment `is_local_parameter { goal: CoreStruct, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                  the rule "fundamental rigid type" failed at step #0 (src/file.rs:LL:CC) because
                                    condition evaluted to false: `is_fundamental(&decls, &name)`
                                      &decls = decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {}, {})
                                      &name = (adt CoreStruct)
                                  the rule "local rigid type" failed at step #0 (src/file.rs:LL:CC) because
                                    condition evaluted to false: `decls.is_local_adt_id(&a)`
                                      decls = decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {}, {})
                                      &a = CoreStruct
                              the rule "local trait" failed at step #0 (src/file.rs:LL:CC) because
                                condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                                  decls = decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {}, {})
                                  &goal.trait_id = CoreTrait"#]]
    )
}

#[test]
fn mirror_CoreStruct() {
    crate::assert_err!(
        [
            crate core {
                trait CoreTrait {}
                struct CoreStruct {}

                trait Mirror {
                    type Assoc : [];
                }

                impl<ty T> Mirror for T {
                    type Assoc = T;
                }
            },
            crate foo {
                impl CoreTrait for <CoreStruct as Mirror>::Assoc {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            orphan_check(impl CoreTrait for <CoreStruct as Mirror>::Assoc { })

            Caused by:
                judgment `prove { goal: {@ IsLocal(CoreTrait(<CoreStruct as Mirror>::Assoc))}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<CoreStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt CoreStruct { struct { } }], {}, {}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goals: {@ IsLocal(CoreTrait(<CoreStruct as Mirror>::Assoc))}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: @ IsLocal(CoreTrait(<CoreStruct as Mirror>::Assoc)), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                          the rule "trait ref is local" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `is_local_trait_ref { goal: CoreTrait(<CoreStruct as Mirror>::Assoc), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                              the rule "local parameter" failed at step #1 (src/file.rs:LL:CC) because
                                judgment `is_local_parameter { goal: <CoreStruct as Mirror>::Assoc, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                  the rule "local parameter" failed at step #2 (src/file.rs:LL:CC) because
                                    judgment `is_local_parameter { goal: CoreStruct, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                      the rule "fundamental rigid type" failed at step #0 (src/file.rs:LL:CC) because
                                        condition evaluted to false: `is_fundamental(&decls, &name)`
                                          &decls = decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<CoreStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt CoreStruct { struct { } }], {}, {})
                                          &name = (adt CoreStruct)
                                      the rule "local rigid type" failed at step #0 (src/file.rs:LL:CC) because
                                        condition evaluted to false: `decls.is_local_adt_id(&a)`
                                          decls = decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<CoreStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt CoreStruct { struct { } }], {}, {})
                                          &a = CoreStruct
                              the rule "local trait" failed at step #0 (src/file.rs:LL:CC) because
                                condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                                  decls = decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<CoreStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt CoreStruct { struct { } }], {}, {})
                                  &goal.trait_id = CoreTrait"#]]
    )
}

#[test]
fn mirror_FooStruct() {
    crate::assert_ok!(
        [
            crate core {
                trait CoreTrait {}

                trait Mirror {
                    type Assoc : [];
                }

                impl<ty T> Mirror for T {
                    type Assoc = T;
                }
            },
            crate foo {
                struct FooStruct {}
                impl CoreTrait for <FooStruct as Mirror>::Assoc {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(core): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ check_associated_ty(Assoc): at traits.rs:121
                        └─ prove_wc_list: (none) at prove_wc_list.rs:11
                               _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {Mirror(!ty_0)}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: Mirror(!ty_0)
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl <ty> Mirror(^ty0_0): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, [?ty_1]): at prove_wc.rs:22
                           └─ i = Mirror(?ty_1): at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ co_assumptions = ({}, Mirror(!ty_0)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                  env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                  assumptions: {Mirror(!ty_0)}
                                  goals: {!ty_0 = ?ty_1}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                     env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                     assumptions: {Mirror(!ty_0)}
                                     goal: !ty_0 = ?ty_1
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                 └─ prove_eq: (symmetric) at prove_eq.rs:23
                                        _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                        env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                        assumptions: {Mirror(!ty_0)}
                                        a: !ty_0
                                        b: ?ty_1
                                        result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                    └─ prove_eq: (existential) at prove_eq.rs:23
                                           _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                           env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                           assumptions: {Mirror(!ty_0)}
                                           a: ?ty_1
                                           b: !ty_0
                                           result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                       └─ prove_existential_var_eq: (existential-universal) at prove_eq.rs:76
                                              _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                              env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                              assumptions: {Mirror(!ty_0)}
                                              v: ?ty_1
                                              b: !ty_0
                                              result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                          └─ IfThen { expression: "env.universe(p) < env.universe(v)" }: at prove_eq.rs:76
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                     constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                     assumptions: {Mirror(!ty_0)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                 └─ (assumptions, goal) = ({Mirror(!ty_0)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                        assumptions: {Mirror(!ty_0)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                  assumptions: {Mirror(!ty_0)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ (assumptions, goal) = ({Mirror(!ty_0)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Mirror(!ty_0)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Mirror(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Mirror(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Mirror(?ty_0), assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                     └─ check_associated_ty_value(Assoc): at impls.rs:305
                        └─ prove_wc_list: (none) at prove_wc_list.rs:11
                               _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc_list: (none) at prove_wc_list.rs:11
                               _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(!ty_0)}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(!ty_0)
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (universal variables) at prove_wf.rs:14
                                     _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: !ty_0
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc_list: (none) at prove_wc_list.rs:11
                               _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_coherence(core): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(Mirror(!ty_0))}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(Mirror(!ty_0))
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: Mirror(!ty_0)
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {}), &goal.trait_id: Mirror }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [], {CoreTrait, Mirror}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
               └─ check_current_crate(foo): at lib.rs:73
                  └─ check_adt(FooStruct): at adts.rs:12
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {CoreTrait(<FooStruct as Mirror>::Assoc)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: CoreTrait(<FooStruct as Mirror>::Assoc)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl CoreTrait(<FooStruct as Mirror>::Assoc): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [], bias: Soundness, pending: [] }, []): at prove_wc.rs:22
                           └─ i = CoreTrait(<FooStruct as Mirror>::Assoc): at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ co_assumptions = ({}, CoreTrait(<FooStruct as Mirror>::Assoc)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {CoreTrait(<FooStruct as Mirror>::Assoc)}
                                  goals: {<FooStruct as Mirror>::Assoc = <FooStruct as Mirror>::Assoc}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {CoreTrait(<FooStruct as Mirror>::Assoc)}
                                     goal: <FooStruct as Mirror>::Assoc = <FooStruct as Mirror>::Assoc
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {CoreTrait(<FooStruct as Mirror>::Assoc)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({CoreTrait(<FooStruct as Mirror>::Assoc)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {CoreTrait(<FooStruct as Mirror>::Assoc)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {CoreTrait(<FooStruct as Mirror>::Assoc)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({CoreTrait(<FooStruct as Mirror>::Assoc)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {CoreTrait(<FooStruct as Mirror>::Assoc)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! CoreTrait(<FooStruct as Mirror>::Assoc)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! CoreTrait(<FooStruct as Mirror>::Assoc)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! CoreTrait(<FooStruct as Mirror>::Assoc), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_coherence(foo): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(CoreTrait(<FooStruct as Mirror>::Assoc))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(CoreTrait(<FooStruct as Mirror>::Assoc))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local parameter) at is_local.rs:199
                                  _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: CoreTrait(<FooStruct as Mirror>::Assoc)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ item = 0: at is_local.rs:199
                              └─ is_local_parameter: (local parameter) at is_local.rs:277
                                     _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: <FooStruct as Mirror>::Assoc
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_normalize: (normalize-via-impl) at prove_normalize.rs:16
                                        _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        p: <FooStruct as Mirror>::Assoc
                                        result: (Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }, FooStruct)
                                    └─ item = alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0: at prove_normalize.rs:16
                                    └─ (env, subst) = (Env { variables: [?ty_1], bias: Soundness, pending: [] }, [?ty_1]): at prove_normalize.rs:16
                                    └─ decl = <?ty_1 as Mirror>::Assoc = ?ty_1: at prove_normalize.rs:16
                                    └─ AliasEqDeclBoundData { alias: AliasTy { name, parameters }, ty, where_clause } = <?ty_1 as Mirror>::Assoc = ?ty_1: at prove_normalize.rs:16
                                    └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                           env: Env { variables: [?ty_0], bias: Soundness, pending: [] }
                                           assumptions: {}
                                           goals: {FooStruct = ?ty_0}
                                           result: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => FooStruct} }
                                       └─ prove_wc: (eq) at prove_wc.rs:22
                                              _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                              env: Env { variables: [?ty_0], bias: Soundness, pending: [] }
                                              assumptions: {}
                                              goal: FooStruct = ?ty_0
                                              result: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => FooStruct} }
                                          └─ prove_eq: (symmetric) at prove_eq.rs:23
                                                 _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                                 env: Env { variables: [?ty_0], bias: Soundness, pending: [] }
                                                 assumptions: {}
                                                 a: FooStruct
                                                 b: ?ty_0
                                                 result: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => FooStruct} }
                                             └─ prove_eq: (existential) at prove_eq.rs:23
                                                    _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                                    env: Env { variables: [?ty_0], bias: Soundness, pending: [] }
                                                    assumptions: {}
                                                    a: ?ty_0
                                                    b: FooStruct
                                                    result: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => FooStruct} }
                                                └─ prove_existential_var_eq: (existential-nonvar) at prove_eq.rs:76
                                                       _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                                       env: Env { variables: [?ty_0], bias: Soundness, pending: [] }
                                                       assumptions: {}
                                                       v: ?ty_0
                                                       b: FooStruct
                                                       result: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => FooStruct} }
                                                   └─ prove_after: (prove_after) at prove_after.rs:8
                                                          _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                                          constraints: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => FooStruct} }
                                                          assumptions: {}
                                                          goal: {}
                                                          result: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => FooStruct} }
                                                      └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                                      └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                             _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                                             env: Env { variables: [], bias: Soundness, pending: [] }
                                                             assumptions: {}
                                                             goals: {}
                                                             result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ prove_after: (prove_after) at prove_after.rs:8
                                              _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                              constraints: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => FooStruct} }
                                              assumptions: {}
                                              goal: {}
                                              result: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => FooStruct} }
                                          └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                          └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                 _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                                 env: Env { variables: [], bias: Soundness, pending: [] }
                                                 assumptions: {}
                                                 goals: {}
                                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                           constraints: Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => FooStruct} }
                                           assumptions: {}
                                           goal: {}
                                           result: Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => FooStruct} }
                                       └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {}
                                              goals: {}
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ ty = FooStruct: at prove_normalize.rs:16
                                    └─ c = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_normalize.rs:16
                                 └─ assumptions = {}: at is_local.rs:277
                                 └─ is_local_parameter: (local rigid type) at is_local.rs:277
                                        _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: FooStruct
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ IfThen { expression: "decls.is_local_adt_id(&a)", decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct}), &a: FooStruct }: at is_local.rs:277
                              └─ assumptions = {}: at is_local.rs:199
                              └─ goal = CoreTrait(<FooStruct as Mirror>::Assoc): at is_local.rs:199
                              └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<FooStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
        "#]]
    )
}

#[test]
fn covered_VecT() {
    crate::assert_ok!(
        [
            crate core {
                trait CoreTrait<ty T> {}
                struct Vec<ty T> {}
            },
            crate foo {
                struct FooStruct {}
                impl<ty T> CoreTrait<FooStruct> for Vec<T> {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(core): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty, ty> ], [], [], [], [], [adt Vec <ty> { struct { } }], {CoreTrait}, {Vec})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_adt(Vec): at adts.rs:12
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty, ty> ], [], [], [], [], [adt Vec <ty> { struct { } }], {CoreTrait}, {Vec})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_coherence(core): at coherence.rs:13
               └─ check_current_crate(foo): at lib.rs:73
                  └─ check_adt(FooStruct): at adts.rs:12
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {CoreTrait(Vec<!ty_0>, FooStruct)}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: CoreTrait(Vec<!ty_0>, FooStruct)
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, [?ty_1]): at prove_wc.rs:22
                           └─ i = CoreTrait(Vec<?ty_1>, FooStruct): at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ co_assumptions = ({}, CoreTrait(Vec<!ty_0>, FooStruct)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                  env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                  assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                  goals: {FooStruct = FooStruct, Vec<!ty_0> = Vec<?ty_1>}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                     env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                     assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                     goal: FooStruct = FooStruct
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                     constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                     goal: {Vec<!ty_0> = Vec<?ty_1>}
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                 └─ (assumptions, goal) = ({CoreTrait(Vec<!ty_0>, FooStruct)}, {Vec<!ty_0> = Vec<?ty_1>}): at prove_after.rs:8
                                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                        env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                        assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                        goals: {Vec<!ty_0> = Vec<?ty_1>}
                                        result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                    └─ prove_wc: (eq) at prove_wc.rs:22
                                           _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                           env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                           assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                           goal: Vec<!ty_0> = Vec<?ty_1>
                                           result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                       └─ prove_eq: (rigid) at prove_eq.rs:23
                                              _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                              env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                              assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                              a: Vec<!ty_0>
                                              b: Vec<?ty_1>
                                              result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                          └─ RigidTy { name: a_name, parameters: a_parameters } = Vec<!ty_0>: at prove_eq.rs:23
                                          └─ RigidTy { name: b_name, parameters: b_parameters } = Vec<?ty_1>: at prove_eq.rs:23
                                          └─ IfThen { expression: "a_name == b_name", a_name: (adt Vec), b_name: (adt Vec) }: at prove_eq.rs:23
                                          └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                                 _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                                 env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                                 assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                                 goals: {!ty_0 = ?ty_1}
                                                 result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                             └─ prove_wc: (eq) at prove_wc.rs:22
                                                    _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                                    env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                                    assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                                    goal: !ty_0 = ?ty_1
                                                    result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                                └─ prove_eq: (symmetric) at prove_eq.rs:23
                                                       _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                                       env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                                       assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                                       a: !ty_0
                                                       b: ?ty_1
                                                       result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                                   └─ prove_eq: (existential) at prove_eq.rs:23
                                                          _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                                          env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                                          assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                                          a: ?ty_1
                                                          b: !ty_0
                                                          result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                                      └─ prove_existential_var_eq: (existential-universal) at prove_eq.rs:76
                                                             _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                                             env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                                             assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                                             v: ?ty_1
                                                             b: !ty_0
                                                             result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                                         └─ IfThen { expression: "env.universe(p) < env.universe(v)" }: at prove_eq.rs:76
                                             └─ prove_after: (prove_after) at prove_after.rs:8
                                                    _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                                    constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                                    assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                                    goal: {}
                                                    result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                                └─ (assumptions, goal) = ({CoreTrait(Vec<!ty_0>, FooStruct)}, {}): at prove_after.rs:8
                                                └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                       _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                                       env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                       assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                                       goals: {}
                                                       result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                           constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                           assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                           goal: {}
                                           result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                       └─ (assumptions, goal) = ({CoreTrait(Vec<!ty_0>, FooStruct)}, {}): at prove_after.rs:8
                                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                              env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                              assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                              goals: {}
                                              result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                  constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                  assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ (assumptions, goal) = ({CoreTrait(Vec<!ty_0>, FooStruct)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {CoreTrait(Vec<!ty_0>, FooStruct)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                  constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! CoreTrait(Vec<?ty_0>, FooStruct)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! CoreTrait(Vec<?ty_0>, FooStruct)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! CoreTrait(Vec<?ty_0>, FooStruct), assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_coherence(foo): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(CoreTrait(Vec<!ty_0>, FooStruct))}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(CoreTrait(Vec<!ty_0>, FooStruct))
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local parameter) at is_local.rs:199
                                  _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: CoreTrait(Vec<!ty_0>, FooStruct)
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ item = 1: at is_local.rs:199
                              └─ is_local_parameter: (local rigid type) at is_local.rs:277
                                     _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: FooStruct
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ IfThen { expression: "decls.is_local_adt_id(&a)", decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct}), &a: FooStruct }: at is_local.rs:277
                              └─ assumptions = {}: at is_local.rs:199
                              └─ goal = CoreTrait(Vec<!ty_0>, FooStruct): at is_local.rs:199
                              └─ for_all: at combinators.rs:73
                                 └─ is_not_downstream: (rigid) at is_local.rs:229
                                        _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        parameter: Vec<!ty_0>
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(Vec<^ty0_0>, FooStruct)], [], [], [], [adt Vec <ty> { struct { } }, adt FooStruct { struct { } }], {}, {FooStruct})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
        "#]]
    )
}

#[test]
fn uncovered_T() {
    crate::assert_err!(
        [
            crate core {
                trait CoreTrait<ty T> {}
            },
            crate foo {
                struct FooStruct {}
                impl<ty T> CoreTrait<FooStruct> for T {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            orphan_check(impl <ty> CoreTrait <FooStruct> for ^ty0_0 { })

            Caused by:
                judgment `prove { goal: {@ IsLocal(CoreTrait(!ty_0, FooStruct))}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(^ty0_0, FooStruct)], [], [], [], [adt FooStruct { struct { } }], {}, {FooStruct}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goals: {@ IsLocal(CoreTrait(!ty_0, FooStruct))}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: @ IsLocal(CoreTrait(!ty_0, FooStruct)), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                          the rule "trait ref is local" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `is_local_trait_ref { goal: CoreTrait(!ty_0, FooStruct), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                              the rule "local trait" failed at step #0 (src/file.rs:LL:CC) because
                                condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                                  decls = decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(^ty0_0, FooStruct)], [], [], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                  &goal.trait_id = CoreTrait"#]]
    )
}

#[test]
fn alias_to_unit() {
    crate::assert_err!(
        [
            crate core {
                trait CoreTrait {}

                trait Unit {
                    type Assoc : [];
                }

                impl<ty T> Unit for T {
                    type Assoc = ();
                }
            },
            crate foo {
                struct FooStruct {}
                impl CoreTrait for <FooStruct as Unit>::Assoc {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            orphan_check(impl CoreTrait for <FooStruct as Unit>::Assoc { })

            Caused by:
                judgment `prove { goal: {@ IsLocal(CoreTrait(<FooStruct as Unit>::Assoc))}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [trait CoreTrait <ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<FooStruct as Unit>::Assoc)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goals: {@ IsLocal(CoreTrait(<FooStruct as Unit>::Assoc))}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: @ IsLocal(CoreTrait(<FooStruct as Unit>::Assoc)), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                          the rule "trait ref is local" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `is_local_trait_ref { goal: CoreTrait(<FooStruct as Unit>::Assoc), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                              the rule "local parameter" failed at step #1 (src/file.rs:LL:CC) because
                                judgment `is_local_parameter { goal: <FooStruct as Unit>::Assoc, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                  the rule "local parameter" failed at step #2 (src/file.rs:LL:CC) because
                                    judgment `is_local_parameter { goal: (), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                      the rule "fundamental rigid type" failed at step #0 (src/file.rs:LL:CC) because
                                        condition evaluted to false: `is_fundamental(&decls, &name)`
                                          &decls = decls(222, [trait CoreTrait <ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<FooStruct as Unit>::Assoc)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                          &name = tuple(0)
                              the rule "local trait" failed at step #0 (src/file.rs:LL:CC) because
                                condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                                  decls = decls(222, [trait CoreTrait <ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<FooStruct as Unit>::Assoc)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                  &goal.trait_id = CoreTrait"#]]
    )
}

#[test]
fn CoreTrait_for_CoreStruct_in_Foo() {
    crate::assert_err!(
        [
            crate core {
                trait CoreTrait {}
                struct CoreStruct {}
            },
            crate foo {
                impl CoreTrait for CoreStruct {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            orphan_check(impl CoreTrait for CoreStruct { })

            Caused by:
                judgment `prove { goal: {@ IsLocal(CoreTrait(CoreStruct))}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [trait CoreTrait <ty> ], [impl CoreTrait(CoreStruct)], [], [], [], [adt CoreStruct { struct { } }], {}, {}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goals: {@ IsLocal(CoreTrait(CoreStruct))}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: @ IsLocal(CoreTrait(CoreStruct)), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                          the rule "trait ref is local" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `is_local_trait_ref { goal: CoreTrait(CoreStruct), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                              the rule "local parameter" failed at step #1 (src/file.rs:LL:CC) because
                                judgment `is_local_parameter { goal: CoreStruct, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                  the rule "fundamental rigid type" failed at step #0 (src/file.rs:LL:CC) because
                                    condition evaluted to false: `is_fundamental(&decls, &name)`
                                      &decls = decls(222, [trait CoreTrait <ty> ], [impl CoreTrait(CoreStruct)], [], [], [], [adt CoreStruct { struct { } }], {}, {})
                                      &name = (adt CoreStruct)
                                  the rule "local rigid type" failed at step #0 (src/file.rs:LL:CC) because
                                    condition evaluted to false: `decls.is_local_adt_id(&a)`
                                      decls = decls(222, [trait CoreTrait <ty> ], [impl CoreTrait(CoreStruct)], [], [], [], [adt CoreStruct { struct { } }], {}, {})
                                      &a = CoreStruct
                              the rule "local trait" failed at step #0 (src/file.rs:LL:CC) because
                                condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                                  decls = decls(222, [trait CoreTrait <ty> ], [impl CoreTrait(CoreStruct)], [], [], [], [adt CoreStruct { struct { } }], {}, {})
                                  &goal.trait_id = CoreTrait"#]]
    )
}

#[test]
fn CoreTraitLocal_for_AliasToKnown_in_Foo() {
    // TODO: see comment in `orphan_check` from prev commit
    crate::assert_ok!(
    [
        crate core {
            trait CoreTrait<ty T> {}

            trait Unit {
                type Assoc : [];
            }

            impl<ty T> Unit for T {
                type Assoc = ();
            }
        },
        crate foo {
            struct FooStruct {}
            impl CoreTrait<FooStruct> for <() as Unit>::Assoc {}
        }
    ]

    expect_test::expect![[r#"
        └─ check_all_crates: at lib.rs:27
           └─ check_current_crate(core): at lib.rs:73
              └─ check_trait: at traits.rs:13
                 └─ check_trait_items_have_unique_names: at traits.rs:71
                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                        _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                        env: Env { variables: [], bias: Soundness, pending: [] }
                        assumptions: {}
                        goals: {}
                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
              └─ check_trait: at traits.rs:13
                 └─ check_trait_items_have_unique_names: at traits.rs:71
                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                        _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                        env: Env { variables: [], bias: Soundness, pending: [] }
                        assumptions: {}
                        goals: {}
                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                 └─ check_associated_ty(Assoc): at traits.rs:121
                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                           _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                           env: Env { variables: [], bias: Soundness, pending: [] }
                           assumptions: {}
                           goals: {}
                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
              └─ check_trait_impl: at impls.rs:27
                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                        _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                        env: Env { variables: [], bias: Soundness, pending: [] }
                        assumptions: {}
                        goals: {}
                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                        _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                        assumptions: {}
                        goals: {Unit(!ty_0)}
                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                    └─ prove_wc: (positive impl) at prove_wc.rs:22
                           _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                           env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                           assumptions: {}
                           goal: Unit(!ty_0)
                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                       └─ item = impl <ty> Unit(^ty0_0): at prove_wc.rs:22
                       └─ (env, subst) = (Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, [?ty_1]): at prove_wc.rs:22
                       └─ i = Unit(?ty_1): at prove_wc.rs:22
                       └─ t = : at prove_wc.rs:22
                       └─ co_assumptions = ({}, Unit(!ty_0)): at prove_wc.rs:22
                       └─ prove_wc_list: (some) at prove_wc_list.rs:11
                              _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                              env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                              assumptions: {Unit(!ty_0)}
                              goals: {!ty_0 = ?ty_1}
                              result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                          └─ prove_wc: (eq) at prove_wc.rs:22
                                 _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                                 env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                 assumptions: {Unit(!ty_0)}
                                 goal: !ty_0 = ?ty_1
                                 result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                             └─ prove_eq: (symmetric) at prove_eq.rs:23
                                    _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                                    env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                    assumptions: {Unit(!ty_0)}
                                    a: !ty_0
                                    b: ?ty_1
                                    result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                └─ prove_eq: (existential) at prove_eq.rs:23
                                       _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                                       env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                       assumptions: {Unit(!ty_0)}
                                       a: ?ty_1
                                       b: !ty_0
                                       result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                   └─ prove_existential_var_eq: (existential-universal) at prove_eq.rs:76
                                          _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                                          env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                          assumptions: {Unit(!ty_0)}
                                          v: ?ty_1
                                          b: !ty_0
                                          result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                      └─ IfThen { expression: "env.universe(p) < env.universe(v)" }: at prove_eq.rs:76
                          └─ prove_after: (prove_after) at prove_after.rs:8
                                 _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                                 constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                 assumptions: {Unit(!ty_0)}
                                 goal: {}
                                 result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                             └─ (assumptions, goal) = ({Unit(!ty_0)}, {}): at prove_after.rs:8
                             └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                    _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                                    env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                    assumptions: {Unit(!ty_0)}
                                    goals: {}
                                    result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                       └─ prove_after: (prove_after) at prove_after.rs:8
                              _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                              constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              assumptions: {Unit(!ty_0)}
                              goal: {}
                              result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                          └─ (assumptions, goal) = ({Unit(!ty_0)}, {}): at prove_after.rs:8
                          └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                 _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                                 env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                 assumptions: {Unit(!ty_0)}
                                 goals: {}
                                 result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                       └─ prove_after: (prove_after) at prove_after.rs:8
                              _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                              constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              assumptions: {}
                              goal: {}
                              result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                          └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                          └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                 _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                                 env: Env { variables: [], bias: Soundness, pending: [] }
                                 assumptions: {}
                                 goals: {}
                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                    └─ prove_after: (prove_after) at prove_after.rs:8
                           _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                           constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           assumptions: {}
                           goal: {}
                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                       └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                              _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                              env: Env { variables: [], bias: Soundness, pending: [] }
                              assumptions: {}
                              goals: {}
                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                 └─ negation succeeded: judgment `prove { goal: {! Unit(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {}) }` failed at the following rule(s):
          failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
            judgment `prove_wc_list { goals: {! Unit(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
              the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                judgment `prove_wc { goal: ! Unit(?ty_0), assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                    expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                 └─ safety_matches(safe, safe): at impls.rs:122
                 └─ check_associated_ty_value(Assoc): at impls.rs:305
                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                           _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                           env: Env { variables: [], bias: Soundness, pending: [] }
                           assumptions: {}
                           goals: {}
                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                           _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                           env: Env { variables: [], bias: Soundness, pending: [] }
                           assumptions: {}
                           goals: {}
                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                    └─ prove_wc_list: (some) at prove_wc_list.rs:11
                           _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                           env: Env { variables: [], bias: Soundness, pending: [] }
                           assumptions: {}
                           goals: {@ wf(())}
                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                       └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                              _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                              env: Env { variables: [], bias: Soundness, pending: [] }
                              assumptions: {}
                              goal: @ wf(())
                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                          └─ prove_wf: (tuples) at prove_wf.rs:14
                                 _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                                 env: Env { variables: [], bias: Soundness, pending: [] }
                                 assumptions: {}
                                 goal: ()
                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                             └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                       └─ prove_after: (prove_after) at prove_after.rs:8
                              _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                              constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              assumptions: {}
                              goal: {}
                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                          └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                          └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                 _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                                 env: Env { variables: [], bias: Soundness, pending: [] }
                                 assumptions: {}
                                 goals: {}
                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                           _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                           env: Env { variables: [], bias: Soundness, pending: [] }
                           assumptions: {}
                           goals: {}
                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
              └─ check_coherence(core): at coherence.rs:13
                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                        _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                        assumptions: {}
                        goals: {@ IsLocal(Unit(!ty_0))}
                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                    └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                           _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                           env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                           assumptions: {}
                           goal: @ IsLocal(Unit(!ty_0))
                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                       └─ is_local_trait_ref: (local trait) at is_local.rs:199
                              _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                              env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                              assumptions: {}
                              goal: Unit(!ty_0)
                              result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                          └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {}), &goal.trait_id: Unit }: at is_local.rs:199
                    └─ prove_after: (prove_after) at prove_after.rs:8
                           _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                           constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           assumptions: {}
                           goal: {}
                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                       └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                              _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [], {CoreTrait, Unit}, {})
                              env: Env { variables: [], bias: Soundness, pending: [] }
                              assumptions: {}
                              goals: {}
                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
           └─ check_current_crate(foo): at lib.rs:73
              └─ check_adt(FooStruct): at adts.rs:12
                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                        _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                        env: Env { variables: [], bias: Soundness, pending: [] }
                        assumptions: {}
                        goals: {}
                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
              └─ check_trait_impl: at impls.rs:27
                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                        _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                        env: Env { variables: [], bias: Soundness, pending: [] }
                        assumptions: {}
                        goals: {}
                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                        _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                        env: Env { variables: [], bias: Soundness, pending: [] }
                        assumptions: {}
                        goals: {CoreTrait(<() as Unit>::Assoc, FooStruct)}
                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                    └─ prove_wc: (positive impl) at prove_wc.rs:22
                           _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                           env: Env { variables: [], bias: Soundness, pending: [] }
                           assumptions: {}
                           goal: CoreTrait(<() as Unit>::Assoc, FooStruct)
                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                       └─ item = impl CoreTrait(<() as Unit>::Assoc, FooStruct): at prove_wc.rs:22
                       └─ (env, subst) = (Env { variables: [], bias: Soundness, pending: [] }, []): at prove_wc.rs:22
                       └─ i = CoreTrait(<() as Unit>::Assoc, FooStruct): at prove_wc.rs:22
                       └─ t = : at prove_wc.rs:22
                       └─ co_assumptions = ({}, CoreTrait(<() as Unit>::Assoc, FooStruct)): at prove_wc.rs:22
                       └─ prove_wc_list: (some) at prove_wc_list.rs:11
                              _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                              env: Env { variables: [], bias: Soundness, pending: [] }
                              assumptions: {CoreTrait(<() as Unit>::Assoc, FooStruct)}
                              goals: {FooStruct = FooStruct, <() as Unit>::Assoc = <() as Unit>::Assoc}
                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                          └─ prove_wc: (eq) at prove_wc.rs:22
                                 _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                 env: Env { variables: [], bias: Soundness, pending: [] }
                                 assumptions: {CoreTrait(<() as Unit>::Assoc, FooStruct)}
                                 goal: FooStruct = FooStruct
                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                             └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                          └─ prove_after: (prove_after) at prove_after.rs:8
                                 _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                 constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 assumptions: {CoreTrait(<() as Unit>::Assoc, FooStruct)}
                                 goal: {<() as Unit>::Assoc = <() as Unit>::Assoc}
                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                             └─ (assumptions, goal) = ({CoreTrait(<() as Unit>::Assoc, FooStruct)}, {<() as Unit>::Assoc = <() as Unit>::Assoc}): at prove_after.rs:8
                             └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                    _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                    env: Env { variables: [], bias: Soundness, pending: [] }
                                    assumptions: {CoreTrait(<() as Unit>::Assoc, FooStruct)}
                                    goals: {<() as Unit>::Assoc = <() as Unit>::Assoc}
                                    result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                └─ prove_wc: (eq) at prove_wc.rs:22
                                       _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                       env: Env { variables: [], bias: Soundness, pending: [] }
                                       assumptions: {CoreTrait(<() as Unit>::Assoc, FooStruct)}
                                       goal: <() as Unit>::Assoc = <() as Unit>::Assoc
                                       result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                   └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                └─ prove_after: (prove_after) at prove_after.rs:8
                                       _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                       constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       assumptions: {CoreTrait(<() as Unit>::Assoc, FooStruct)}
                                       goal: {}
                                       result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                   └─ (assumptions, goal) = ({CoreTrait(<() as Unit>::Assoc, FooStruct)}, {}): at prove_after.rs:8
                                   └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                          _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                          env: Env { variables: [], bias: Soundness, pending: [] }
                                          assumptions: {CoreTrait(<() as Unit>::Assoc, FooStruct)}
                                          goals: {}
                                          result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                       └─ prove_after: (prove_after) at prove_after.rs:8
                              _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                              constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              assumptions: {CoreTrait(<() as Unit>::Assoc, FooStruct)}
                              goal: {}
                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                          └─ (assumptions, goal) = ({CoreTrait(<() as Unit>::Assoc, FooStruct)}, {}): at prove_after.rs:8
                          └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                 _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                 env: Env { variables: [], bias: Soundness, pending: [] }
                                 assumptions: {CoreTrait(<() as Unit>::Assoc, FooStruct)}
                                 goals: {}
                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                       └─ prove_after: (prove_after) at prove_after.rs:8
                              _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                              constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              assumptions: {}
                              goal: {}
                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                          └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                          └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                 _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                 env: Env { variables: [], bias: Soundness, pending: [] }
                                 assumptions: {}
                                 goals: {}
                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                    └─ prove_after: (prove_after) at prove_after.rs:8
                           _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                           constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           assumptions: {}
                           goal: {}
                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                       └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                              _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                              env: Env { variables: [], bias: Soundness, pending: [] }
                              assumptions: {}
                              goals: {}
                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                 └─ negation succeeded: judgment `prove { goal: {! CoreTrait(<() as Unit>::Assoc, FooStruct)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct}) }` failed at the following rule(s):
          failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
            judgment `prove_wc_list { goals: {! CoreTrait(<() as Unit>::Assoc, FooStruct)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
              the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                judgment `prove_wc { goal: ! CoreTrait(<() as Unit>::Assoc, FooStruct), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                    expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                 └─ safety_matches(safe, safe): at impls.rs:122
              └─ check_coherence(foo): at coherence.rs:13
                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                        _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                        env: Env { variables: [], bias: Soundness, pending: [] }
                        assumptions: {}
                        goals: {@ IsLocal(CoreTrait(<() as Unit>::Assoc, FooStruct))}
                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                    └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                           _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                           env: Env { variables: [], bias: Soundness, pending: [] }
                           assumptions: {}
                           goal: @ IsLocal(CoreTrait(<() as Unit>::Assoc, FooStruct))
                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                       └─ is_local_trait_ref: (local parameter) at is_local.rs:199
                              _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                              env: Env { variables: [], bias: Soundness, pending: [] }
                              assumptions: {}
                              goal: CoreTrait(<() as Unit>::Assoc, FooStruct)
                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                          └─ item = 1: at is_local.rs:199
                          └─ is_local_parameter: (local rigid type) at is_local.rs:277
                                 _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                 env: Env { variables: [], bias: Soundness, pending: [] }
                                 assumptions: {}
                                 goal: FooStruct
                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                             └─ IfThen { expression: "decls.is_local_adt_id(&a)", decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct}), &a: FooStruct }: at is_local.rs:277
                          └─ assumptions = {}: at is_local.rs:199
                          └─ goal = CoreTrait(<() as Unit>::Assoc, FooStruct): at is_local.rs:199
                          └─ for_all: at combinators.rs:73
                             └─ is_not_downstream: (via normalize) at is_local.rs:229
                                    _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                    env: Env { variables: [], bias: Soundness, pending: [] }
                                    assumptions: {}
                                    parameter: <() as Unit>::Assoc
                                    result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                └─ prove_normalize: (normalize-via-impl) at prove_normalize.rs:16
                                       _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                       env: Env { variables: [], bias: Soundness, pending: [] }
                                       assumptions: {}
                                       p: <() as Unit>::Assoc
                                       result: (Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }, ())
                                   └─ item = alias <ty> <^ty0_0 as Unit>::Assoc = (): at prove_normalize.rs:16
                                   └─ (env, subst) = (Env { variables: [?ty_1], bias: Soundness, pending: [] }, [?ty_1]): at prove_normalize.rs:16
                                   └─ decl = <?ty_1 as Unit>::Assoc = (): at prove_normalize.rs:16
                                   └─ AliasEqDeclBoundData { alias: AliasTy { name, parameters }, ty, where_clause } = <?ty_1 as Unit>::Assoc = (): at prove_normalize.rs:16
                                   └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                          _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                          env: Env { variables: [?ty_0], bias: Soundness, pending: [] }
                                          assumptions: {}
                                          goals: {() = ?ty_0}
                                          result: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => ()} }
                                      └─ prove_wc: (eq) at prove_wc.rs:22
                                             _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                             env: Env { variables: [?ty_0], bias: Soundness, pending: [] }
                                             assumptions: {}
                                             goal: () = ?ty_0
                                             result: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => ()} }
                                         └─ prove_eq: (symmetric) at prove_eq.rs:23
                                                _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                                env: Env { variables: [?ty_0], bias: Soundness, pending: [] }
                                                assumptions: {}
                                                a: ()
                                                b: ?ty_0
                                                result: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => ()} }
                                            └─ prove_eq: (existential) at prove_eq.rs:23
                                                   _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                                   env: Env { variables: [?ty_0], bias: Soundness, pending: [] }
                                                   assumptions: {}
                                                   a: ?ty_0
                                                   b: ()
                                                   result: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => ()} }
                                               └─ prove_existential_var_eq: (existential-nonvar) at prove_eq.rs:76
                                                      _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                                      env: Env { variables: [?ty_0], bias: Soundness, pending: [] }
                                                      assumptions: {}
                                                      v: ?ty_0
                                                      b: ()
                                                      result: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => ()} }
                                                  └─ prove_after: (prove_after) at prove_after.rs:8
                                                         _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                                         constraints: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => ()} }
                                                         assumptions: {}
                                                         goal: {}
                                                         result: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => ()} }
                                                     └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                            _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                                            env: Env { variables: [], bias: Soundness, pending: [] }
                                                            assumptions: {}
                                                            goals: {}
                                                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                      └─ prove_after: (prove_after) at prove_after.rs:8
                                             _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                             constraints: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => ()} }
                                             assumptions: {}
                                             goal: {}
                                             result: Constraints { env: Env { variables: [?ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_0 => ()} }
                                         └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                         └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                                env: Env { variables: [], bias: Soundness, pending: [] }
                                                assumptions: {}
                                                goals: {}
                                                result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                   └─ prove_after: (prove_after) at prove_after.rs:8
                                          _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                          constraints: Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => ()} }
                                          assumptions: {}
                                          goal: {}
                                          result: Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => ()} }
                                      └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                      └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                             _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                             env: Env { variables: [], bias: Soundness, pending: [] }
                                             assumptions: {}
                                             goals: {}
                                             result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                   └─ ty = (): at prove_normalize.rs:16
                                   └─ c = Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_normalize.rs:16
                                └─ assumptions = {}: at is_local.rs:229
                                └─ is_not_downstream: (rigid) at is_local.rs:229
                                       _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                                       env: Env { variables: [], bias: Soundness, pending: [] }
                                       assumptions: {}
                                       parameter: ()
                                       result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                             └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                    └─ prove_after: (prove_after) at prove_after.rs:8
                           _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                           constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           assumptions: {}
                           goal: {}
                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                       └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                              _decls: decls(222, [trait CoreTrait <ty, ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<() as Unit>::Assoc, FooStruct)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct})
                              env: Env { variables: [], bias: Soundness, pending: [] }
                              assumptions: {}
                              goals: {}
                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
    "#]]
    )
}
