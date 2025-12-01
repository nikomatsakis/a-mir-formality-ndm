#[test]
fn dependent_where_clause() {
    crate::assert_ok!(
        //@check-pass
        [
            crate foo {
                trait Trait1 {}

                trait Trait2 {}

                struct S1<ty T> where T: Trait1 {
                    dummy: T,
                }

                struct S2<ty T> where T: Trait1, S1<T> : Trait2 {
                    dummy: T,
                }
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(foo): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_adt(S1): at adts.rs:12
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {Trait1(!ty_0)}
                            goals: {@ WellFormedTraitRef(Trait1(!ty_0))}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {Trait1(!ty_0)}
                               goal: @ WellFormedTraitRef(Trait1(!ty_0))
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ for_all: at combinators.rs:73
                              └─ prove_wf: (universal variables) at prove_wf.rs:14
                                     _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Trait1(!ty_0)}
                                     goal: !ty_0
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                           └─ t = trait Trait1 <ty> : at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                  constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Trait1(!ty_0)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Trait1(!ty_0)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Trait1(!ty_0)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Trait1(!ty_0)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Trait1(!ty_0)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Trait1(!ty_0)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {Trait1(!ty_0)}
                            goals: {@ wf(!ty_0)}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {Trait1(!ty_0)}
                               goal: @ wf(!ty_0)
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (universal variables) at prove_wf.rs:14
                                  _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Trait1(!ty_0)}
                                  goal: !ty_0
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Trait1(!ty_0)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Trait1(!ty_0)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Trait1(!ty_0)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_adt(S2): at adts.rs:12
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                            goals: {@ WellFormedTraitRef(Trait1(!ty_0)), @ WellFormedTraitRef(Trait2(S1<!ty_0>))}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                               goal: @ WellFormedTraitRef(Trait1(!ty_0))
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ for_all: at combinators.rs:73
                              └─ prove_wf: (universal variables) at prove_wf.rs:14
                                     _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                     goal: !ty_0
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                           └─ t = trait Trait1 <ty> : at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                  constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Trait1(!ty_0), Trait2(S1<!ty_0>)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                               goal: {@ WellFormedTraitRef(Trait2(S1<!ty_0>))}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Trait1(!ty_0), Trait2(S1<!ty_0>)}, {@ WellFormedTraitRef(Trait2(S1<!ty_0>))}): at prove_after.rs:8
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                  goals: {@ WellFormedTraitRef(Trait2(S1<!ty_0>))}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (trait well formed) at prove_wc.rs:22
                                     _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                     goal: @ WellFormedTraitRef(Trait2(S1<!ty_0>))
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ for_all: at combinators.rs:73
                                    └─ prove_wf: (ADT) at prove_wf.rs:14
                                           _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                           env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                           assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                           goal: S1<!ty_0>
                                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ for_all: at combinators.rs:73
                                          └─ prove_wf: (universal variables) at prove_wf.rs:14
                                                 _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                                 env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                 assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                                 goal: !ty_0
                                                 result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                                       └─ t = adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }: at prove_wf.rs:14
                                       └─ t = where {Trait1(!ty_0)} { struct { dummy : !ty_0 } }: at prove_wf.rs:14
                                       └─ prove_after: (prove_after) at prove_after.rs:8
                                              _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                              constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                              assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                              goal: {Trait1(!ty_0)}
                                              result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ (assumptions, goal) = ({Trait1(!ty_0), Trait2(S1<!ty_0>)}, {Trait1(!ty_0)}): at prove_after.rs:8
                                          └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                                 _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                                 env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                 assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                                 goals: {Trait1(!ty_0)}
                                                 result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ prove_wc: (assumption - predicate) at prove_wc.rs:22
                                                    _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                                    env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                    assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                                    goal: Trait1(!ty_0)
                                                    result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                └─ item = Trait1(!ty_0): at prove_wc.rs:22
                                                └─ prove_via: (predicate-congruence-axiom) at prove_via.rs:9
                                                       _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                                       env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                       assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                                       via: Trait1(!ty_0)
                                                       goal: Trait1(!ty_0)
                                                       result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                   └─ (skel_c, parameters_c) = (is_implemented(Trait1), [!ty_0]): at prove_via.rs:9
                                                   └─ (skel_g, parameters_g) = (is_implemented(Trait1), [!ty_0]): at prove_via.rs:9
                                                   └─ IfThen { expression: "skel_c == skel_g", skel_c: is_implemented(Trait1), skel_g: is_implemented(Trait1) }: at prove_via.rs:9
                                                   └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                                          _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                                          env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                          assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                                          goals: {!ty_0 = !ty_0}
                                                          result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                      └─ prove_wc: (eq) at prove_wc.rs:22
                                                             _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                                             env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                             assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                                             goal: !ty_0 = !ty_0
                                                             result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                         └─ trivial, as a == b is true: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                                      └─ prove_after: (prove_after) at prove_after.rs:8
                                                             _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                                             constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                             assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                                             goal: {}
                                                             result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                         └─ (assumptions, goal) = ({Trait1(!ty_0), Trait2(S1<!ty_0>)}, {}): at prove_after.rs:8
                                                         └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                                _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                                                env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                                assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                                                goals: {}
                                                                result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ prove_after: (prove_after) at prove_after.rs:8
                                                    _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                                    constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                    assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                                    goal: {}
                                                    result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                └─ (assumptions, goal) = ({Trait1(!ty_0), Trait2(S1<!ty_0>)}, {}): at prove_after.rs:8
                                                └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                       _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                                       env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                       assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                                       goals: {}
                                                       result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                                 └─ t = trait Trait2 <ty> : at prove_wc.rs:22
                                 └─ t = : at prove_wc.rs:22
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                        constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({Trait1(!ty_0), Trait2(S1<!ty_0>)}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                           env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                           assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                     constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({Trait1(!ty_0), Trait2(S1<!ty_0>)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                        assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                            goals: {@ wf(!ty_0)}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                               goal: @ wf(!ty_0)
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (universal variables) at prove_wf.rs:14
                                  _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                  goal: !ty_0
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Trait1(!ty_0), Trait2(S1<!ty_0>)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait1(^ty0_0), Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Trait1(!ty_0), Trait2(S1<!ty_0>)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_coherence(foo): at coherence.rs:13
        "#]]
    )
}

#[test]
fn missing_dependent_where_clause() {
    crate::assert_err!(
        [
            crate foo {
                trait Trait1 {}

                trait Trait2 {}

                struct S1<ty T> where T: Trait1 {
                    dummy: T,
                }

                struct S2<ty T> where S1<T> : Trait2 {
                    dummy: T,
                }
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            prove_where_clauses_well_formed([S1<!ty_1> : Trait2])

            Caused by:
                judgment `prove { goal: {@ WellFormedTraitRef(Trait2(S1<!ty_0>))}, assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goals: {@ WellFormedTraitRef(Trait2(S1<!ty_0>))}, assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: @ WellFormedTraitRef(Trait2(S1<!ty_0>)), assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                          the rule "trait well formed" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `prove_wf { goal: S1<!ty_0>, assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                              the rule "ADT" failed at step #3 (src/file.rs:LL:CC) because
                                judgment `prove_after { constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }, goal: {Trait1(!ty_0)}, assumptions: {Trait2(S1<!ty_0>)} }` failed at the following rule(s):
                                  the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                                    judgment `prove { goal: {Trait1(!ty_0)}, assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, decls: decls(222, [trait Trait1 <ty> , trait Trait2 <ty> ], [], [], [], [], [adt S1 <ty> where {Trait1(^ty0_0)} { struct { dummy : ^ty0_0 } }, adt S2 <ty> where {Trait2(S1<^ty0_0>)} { struct { dummy : ^ty0_0 } }], {Trait1, Trait2}, {S1, S2}) }` failed at the following rule(s):
                                      failed at (src/file.rs:LL:CC) because
                                        judgment `prove_wc_list { goals: {Trait1(!ty_0)}, assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                          the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                            judgment `prove_wc { goal: Trait1(!ty_0), assumptions: {Trait2(S1<!ty_0>)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                              the rule "trait implied bound" failed at step #0 (src/file.rs:LL:CC) because
                                                expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn lifetime_param() {
    crate::assert_ok!(
        //@check-pass
        [
            crate foo {
                trait Trait1<lt a> {}

                struct S1 {}

                struct S2<lt a> where S1: Trait1<a> {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(foo): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty, lt> ], [], [], [], [], [adt S1 { struct { } }, adt S2 <lt> where {Trait1(S1, ^lt0_0)} { struct { } }], {Trait1}, {S1, S2})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_adt(S1): at adts.rs:12
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty, lt> ], [], [], [], [], [adt S1 { struct { } }, adt S2 <lt> where {Trait1(S1, ^lt0_0)} { struct { } }], {Trait1}, {S1, S2})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_adt(S2): at adts.rs:12
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty, lt> ], [], [], [], [], [adt S1 { struct { } }, adt S2 <lt> where {Trait1(S1, ^lt0_0)} { struct { } }], {Trait1}, {S1, S2})
                            env: Env { variables: [!lt_0], bias: Soundness, pending: [] }
                            assumptions: {Trait1(S1, !lt_0)}
                            goals: {@ WellFormedTraitRef(Trait1(S1, !lt_0))}
                            result: Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Trait1 <ty, lt> ], [], [], [], [], [adt S1 { struct { } }, adt S2 <lt> where {Trait1(S1, ^lt0_0)} { struct { } }], {Trait1}, {S1, S2})
                               env: Env { variables: [!lt_0], bias: Soundness, pending: [] }
                               assumptions: {Trait1(S1, !lt_0)}
                               goal: @ WellFormedTraitRef(Trait1(S1, !lt_0))
                               result: Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ for_all: at combinators.rs:73
                              └─ prove_wf: (ADT) at prove_wf.rs:14
                                     _decls: decls(222, [trait Trait1 <ty, lt> ], [], [], [], [], [adt S1 { struct { } }, adt S2 <lt> where {Trait1(S1, ^lt0_0)} { struct { } }], {Trait1}, {S1, S2})
                                     env: Env { variables: [!lt_0], bias: Soundness, pending: [] }
                                     assumptions: {Trait1(S1, !lt_0)}
                                     goal: S1
                                     result: Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                                 └─ t = adt S1 { struct { } }: at prove_wf.rs:14
                                 └─ t = { struct { } }: at prove_wf.rs:14
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Trait1 <ty, lt> ], [], [], [], [], [adt S1 { struct { } }, adt S2 <lt> where {Trait1(S1, ^lt0_0)} { struct { } }], {Trait1}, {S1, S2})
                                        constraints: Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {Trait1(S1, !lt_0)}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({Trait1(S1, !lt_0)}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Trait1 <ty, lt> ], [], [], [], [], [adt S1 { struct { } }, adt S2 <lt> where {Trait1(S1, ^lt0_0)} { struct { } }], {Trait1}, {S1, S2})
                                           env: Env { variables: [!lt_0], bias: Soundness, pending: [] }
                                           assumptions: {Trait1(S1, !lt_0)}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ for_all: at combinators.rs:73
                                 └─ prove_wf: (universal variables) at prove_wf.rs:14
                                        _decls: decls(222, [trait Trait1 <ty, lt> ], [], [], [], [], [adt S1 { struct { } }, adt S2 <lt> where {Trait1(S1, ^lt0_0)} { struct { } }], {Trait1}, {S1, S2})
                                        env: Env { variables: [!lt_0], bias: Soundness, pending: [] }
                                        assumptions: {Trait1(S1, !lt_0)}
                                        goal: !lt_0
                                        result: Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                           └─ t = trait Trait1 <ty, lt> : at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Trait1 <ty, lt> ], [], [], [], [], [adt S1 { struct { } }, adt S2 <lt> where {Trait1(S1, ^lt0_0)} { struct { } }], {Trait1}, {S1, S2})
                                  constraints: Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Trait1(S1, !lt_0)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Trait1(S1, !lt_0)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Trait1 <ty, lt> ], [], [], [], [], [adt S1 { struct { } }, adt S2 <lt> where {Trait1(S1, ^lt0_0)} { struct { } }], {Trait1}, {S1, S2})
                                     env: Env { variables: [!lt_0], bias: Soundness, pending: [] }
                                     assumptions: {Trait1(S1, !lt_0)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Trait1 <ty, lt> ], [], [], [], [], [adt S1 { struct { } }, adt S2 <lt> where {Trait1(S1, ^lt0_0)} { struct { } }], {Trait1}, {S1, S2})
                               constraints: Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Trait1(S1, !lt_0)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Trait1(S1, !lt_0)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Trait1 <ty, lt> ], [], [], [], [], [adt S1 { struct { } }, adt S2 <lt> where {Trait1(S1, ^lt0_0)} { struct { } }], {Trait1}, {S1, S2})
                                  env: Env { variables: [!lt_0], bias: Soundness, pending: [] }
                                  assumptions: {Trait1(S1, !lt_0)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!lt_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_coherence(foo): at coherence.rs:13
        "#]]
    )
}

#[test]
fn static_lifetime_param() {
    crate::assert_ok!(
        //@check-pass
        [
            crate foo {
                trait Trait1<lt a> {}

                struct S1 {}

                impl Trait1<static> for S1 {}

                struct S2 where S1: Trait1<static> {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(foo): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_adt(S1): at adts.rs:12
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {Trait1(S1, static)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: Trait1(S1, static)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl Trait1(S1, static): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [], bias: Soundness, pending: [] }, []): at prove_wc.rs:22
                           └─ i = Trait1(S1, static): at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ co_assumptions = ({}, Trait1(S1, static)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {Trait1(S1, static)}
                                  goals: {S1 = S1, static = static}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Trait1(S1, static)}
                                     goal: S1 = S1
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Trait1(S1, static)}
                                     goal: {static = static}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({Trait1(S1, static)}, {static = static}): at prove_after.rs:8
                                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Trait1(S1, static)}
                                        goals: {static = static}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_wc: (eq) at prove_wc.rs:22
                                           _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {Trait1(S1, static)}
                                           goal: static = static
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                           constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                           assumptions: {Trait1(S1, static)}
                                           goal: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (assumptions, goal) = ({Trait1(S1, static)}, {}): at prove_after.rs:8
                                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {Trait1(S1, static)}
                                              goals: {}
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Trait1(S1, static)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Trait1(S1, static)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Trait1(S1, static)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Trait1(S1, static)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Trait1(S1, static)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Trait1(S1, static), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_adt(S2): at adts.rs:12
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {Trait1(S1, static)}
                            goals: {@ WellFormedTraitRef(Trait1(S1, static))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {Trait1(S1, static)}
                               goal: @ WellFormedTraitRef(Trait1(S1, static))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ for_all: at combinators.rs:73
                              └─ prove_wf: (ADT) at prove_wf.rs:14
                                     _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Trait1(S1, static)}
                                     goal: S1
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                                 └─ t = adt S1 { struct { } }: at prove_wf.rs:14
                                 └─ t = { struct { } }: at prove_wf.rs:14
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                        constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {Trait1(S1, static)}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({Trait1(S1, static)}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {Trait1(S1, static)}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ for_all: at combinators.rs:73
                                 └─ prove_wf: (static lifetime) at prove_wf.rs:14
                                        _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Trait1(S1, static)}
                                        goal: static
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                           └─ t = trait Trait1 <ty, lt> : at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Trait1(S1, static)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Trait1(S1, static)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Trait1(S1, static)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Trait1(S1, static)}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Trait1(S1, static)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {Trait1(S1, static)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_coherence(foo): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(Trait1(S1, static))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(Trait1(S1, static))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: Trait1(S1, static)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2}), &goal.trait_id: Trait1 }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Trait1 <ty, lt> ], [impl Trait1(S1, static)], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, static)} { struct { } }], {Trait1}, {S1, S2})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
        "#]]
    )
}

#[test]
fn const_param() {
    crate::assert_ok!(
        //@check-pass
        [
            crate foo {
                trait Trait1<const C> where type_of_const C is u32 {}

                struct S1 {}

                impl Trait1<const 3_u32> for S1 {}

                struct S2 where S1: Trait1<const 3_u32> {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(foo): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                            env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                            assumptions: {@ ConstHasType(!const_0 , u32)}
                            goals: {@ wf(u32), @ wf(const !const_0)}
                            result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                               env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                               assumptions: {@ ConstHasType(!const_0 , u32)}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                  env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(!const_0 , u32)}
                                  goal: u32
                                  result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                               constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {@ ConstHasType(!const_0 , u32)}
                               goal: {@ wf(const !const_0)}
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , u32)}, {@ wf(const !const_0)}): at prove_after.rs:8
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                  env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(!const_0 , u32)}
                                  goals: {@ wf(const !const_0)}
                                  result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                     _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                     env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                     assumptions: {@ ConstHasType(!const_0 , u32)}
                                     goal: @ wf(const !const_0)
                                     result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wf: (universal variables) at prove_wf.rs:14
                                        _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                        env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                        assumptions: {@ ConstHasType(!const_0 , u32)}
                                        goal: const !const_0
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                     constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {@ ConstHasType(!const_0 , u32)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , u32)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                        env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                        assumptions: {@ ConstHasType(!const_0 , u32)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_adt(S1): at adts.rs:12
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {Trait1(S1, const value(3, u32))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: Trait1(S1, const value(3, u32))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl Trait1(S1, const value(3, u32)): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [], bias: Soundness, pending: [] }, []): at prove_wc.rs:22
                           └─ i = Trait1(S1, const value(3, u32)): at prove_wc.rs:22
                           └─ t = where {@ ConstHasType(value(3, u32) , u32)}: at prove_wc.rs:22
                           └─ co_assumptions = ({}, Trait1(S1, const value(3, u32))): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {Trait1(S1, const value(3, u32))}
                                  goals: {S1 = S1, const value(3, u32) = const value(3, u32)}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Trait1(S1, const value(3, u32))}
                                     goal: S1 = S1
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Trait1(S1, const value(3, u32))}
                                     goal: {const value(3, u32) = const value(3, u32)}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({Trait1(S1, const value(3, u32))}, {const value(3, u32) = const value(3, u32)}): at prove_after.rs:8
                                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Trait1(S1, const value(3, u32))}
                                        goals: {const value(3, u32) = const value(3, u32)}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_wc: (eq) at prove_wc.rs:22
                                           _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {Trait1(S1, const value(3, u32))}
                                           goal: const value(3, u32) = const value(3, u32)
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                           constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                           assumptions: {Trait1(S1, const value(3, u32))}
                                           goal: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (assumptions, goal) = ({Trait1(S1, const value(3, u32))}, {}): at prove_after.rs:8
                                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {Trait1(S1, const value(3, u32))}
                                              goals: {}
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Trait1(S1, const value(3, u32))}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Trait1(S1, const value(3, u32))}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Trait1(S1, const value(3, u32))}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {@ ConstHasType(value(3, u32) , u32)}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {@ ConstHasType(value(3, u32) , u32)}): at prove_after.rs:8
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {@ ConstHasType(value(3, u32) , u32)}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (const has ty) at prove_wc.rs:22
                                        _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: @ ConstHasType(value(3, u32) , u32)
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {}
                                           goals: {u32 = u32}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ prove_wc: (eq) at prove_wc.rs:22
                                              _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {}
                                              goal: u32 = u32
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                       └─ prove_after: (prove_after) at prove_after.rs:8
                                              _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                              constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                              assumptions: {}
                                              goal: {}
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                          └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                 _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                                 env: Env { variables: [], bias: Soundness, pending: [] }
                                                 assumptions: {}
                                                 goals: {}
                                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                        constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Trait1(S1, const value(3, u32))}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Trait1(S1, const value(3, u32))}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Trait1(S1, const value(3, u32)), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_adt(S2): at adts.rs:12
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {Trait1(S1, const value(3, u32))}
                            goals: {@ WellFormedTraitRef(Trait1(S1, const value(3, u32)))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {Trait1(S1, const value(3, u32))}
                               goal: @ WellFormedTraitRef(Trait1(S1, const value(3, u32)))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ for_all: at combinators.rs:73
                              └─ prove_wf: (ADT) at prove_wf.rs:14
                                     _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Trait1(S1, const value(3, u32))}
                                     goal: S1
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                                 └─ t = adt S1 { struct { } }: at prove_wf.rs:14
                                 └─ t = { struct { } }: at prove_wf.rs:14
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                        constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {Trait1(S1, const value(3, u32))}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({Trait1(S1, const value(3, u32))}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {Trait1(S1, const value(3, u32))}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ for_all: at combinators.rs:73
                                 └─ prove_wf: (rigid constants) at prove_wf.rs:14
                                        _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Trait1(S1, const value(3, u32))}
                                        goal: const value(3, u32)
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                           _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {Trait1(S1, const value(3, u32))}
                                           goal: u32
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                                 └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                           └─ t = trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}: at prove_wc.rs:22
                           └─ t = where {@ ConstHasType(value(3, u32) , u32)}: at prove_wc.rs:22
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Trait1(S1, const value(3, u32))}
                                  goal: {@ ConstHasType(value(3, u32) , u32)}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Trait1(S1, const value(3, u32))}, {@ ConstHasType(value(3, u32) , u32)}): at prove_after.rs:8
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Trait1(S1, const value(3, u32))}
                                     goals: {@ ConstHasType(value(3, u32) , u32)}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (const has ty) at prove_wc.rs:22
                                        _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Trait1(S1, const value(3, u32))}
                                        goal: @ ConstHasType(value(3, u32) , u32)
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {Trait1(S1, const value(3, u32))}
                                           goals: {u32 = u32}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ prove_wc: (eq) at prove_wc.rs:22
                                              _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {Trait1(S1, const value(3, u32))}
                                              goal: u32 = u32
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                       └─ prove_after: (prove_after) at prove_after.rs:8
                                              _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                              constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                              assumptions: {Trait1(S1, const value(3, u32))}
                                              goal: {}
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ (assumptions, goal) = ({Trait1(S1, const value(3, u32))}, {}): at prove_after.rs:8
                                          └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                 _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                                 env: Env { variables: [], bias: Soundness, pending: [] }
                                                 assumptions: {Trait1(S1, const value(3, u32))}
                                                 goals: {}
                                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                        constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {Trait1(S1, const value(3, u32))}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({Trait1(S1, const value(3, u32))}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {Trait1(S1, const value(3, u32))}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Trait1(S1, const value(3, u32))}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Trait1(S1, const value(3, u32))}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {Trait1(S1, const value(3, u32))}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_coherence(foo): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(Trait1(S1, const value(3, u32)))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(Trait1(S1, const value(3, u32)))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: Trait1(S1, const value(3, u32))
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2}), &goal.trait_id: Trait1 }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Trait1 <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl Trait1(S1, const value(3, u32))], [], [], [], [adt S1 { struct { } }, adt S2 where {Trait1(S1, const value(3, u32))} { struct { } }], {Trait1}, {S1, S2})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
        "#]]
    )
}

#[test]
#[should_panic(expected = "wrong number of parameters")]
fn type_with_wrong_number_of_parameters() {
    let _ = crate::test_program_ok(
        " [
            crate foo {
                trait Trait1 {}

                struct S1 {}

                struct S2<ty T> where S1<T> : Trait1 {
                    dummy: T,
                }
            }
        ] ",
    )
    .unwrap();
}

#[test]
#[should_panic(expected = "no ADT named `Nonex`")]
fn where_clause_with_nonexistent_type() {
    let _ = crate::test_program_ok(
        " [
            crate foo {
                trait Trait1 {}

                struct S1 where Nonex: Trait1 {}
            }
        ] ",
    )
    .unwrap();
}
