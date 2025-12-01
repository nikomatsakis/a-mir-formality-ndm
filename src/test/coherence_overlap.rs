#![allow(non_snake_case)]

use formality_core::test;

#[test]
fn u32_not_u32_impls() {
    crate::assert_err!(
        // Test that a positive and negative impl for the same type (`u32`, here) is rejected.
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl !Foo for u32 {}
            }
        ]

        [
            "check_trait_impl",
            "impl Foo for u32",
        ]

        expect_test::expect![[r#"
            check_trait_impl(impl Foo for u32 { })

            Caused by:
                judgment `negation_via_failure` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    found an unconditionally true solution Constraints { env: Env { variables: [], bias: Completeness, pending: [] }, known_true: true, substitution: {} }"#]]
    )
}

#[test]
fn neg_CoreTrait_for_CoreStruct_implies_no_overlap() {
    crate::assert_ok!(
        //@check-pass
        // Variant of foo_crate_cannot_assume_CoreStruct_does_not_impl_CoreTrait
        // where there is a negative impl, so it is accepted.
        [
            crate core {
                trait CoreTrait {}
                struct CoreStruct {}
                impl !CoreTrait for CoreStruct {}
            },
            crate foo {
                trait FooTrait {}
                impl<ty T> FooTrait for T where T: CoreTrait {}
                impl FooTrait for CoreStruct {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(core): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_adt(CoreStruct): at adts.rs:12
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_neg_trait_impl(CoreTrait(CoreStruct)): at impls.rs:101
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {! CoreTrait(CoreStruct)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (negative impl) at prove_wc.rs:22
                               _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: ! CoreTrait(CoreStruct)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl ! CoreTrait(CoreStruct): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [], bias: Soundness, pending: [] }, []): at prove_wc.rs:22
                           └─ i = ! CoreTrait(CoreStruct): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {CoreStruct = CoreStruct}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: CoreStruct = CoreStruct
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_coherence(core): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(CoreTrait(CoreStruct))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(CoreTrait(CoreStruct))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: CoreTrait(CoreStruct)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct}), &goal.trait_id: CoreTrait }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {CoreTrait}, {CoreStruct})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
               └─ check_current_crate(foo): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {CoreTrait(!ty_0)}
                            goals: {@ WellFormedTraitRef(CoreTrait(!ty_0))}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {CoreTrait(!ty_0)}
                               goal: @ WellFormedTraitRef(CoreTrait(!ty_0))
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ for_all: at combinators.rs:73
                              └─ prove_wf: (universal variables) at prove_wf.rs:14
                                     _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {CoreTrait(!ty_0)}
                                     goal: !ty_0
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                           └─ t = trait CoreTrait <ty> : at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {CoreTrait(!ty_0)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({CoreTrait(!ty_0)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {CoreTrait(!ty_0)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {CoreTrait(!ty_0)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({CoreTrait(!ty_0)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {CoreTrait(!ty_0)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {CoreTrait(!ty_0)}
                            goals: {FooTrait(!ty_0)}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {CoreTrait(!ty_0)}
                               goal: FooTrait(!ty_0)
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}: at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, [?ty_1]): at prove_wc.rs:22
                           └─ i = FooTrait(?ty_1) where {CoreTrait(?ty_1)}: at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ co_assumptions = ({CoreTrait(!ty_0)}, FooTrait(!ty_0)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                  env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                  assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                  goals: {!ty_0 = ?ty_1}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                     env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                     assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                     goal: !ty_0 = ?ty_1
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                 └─ prove_eq: (symmetric) at prove_eq.rs:23
                                        _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                        env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                        assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                        a: !ty_0
                                        b: ?ty_1
                                        result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                    └─ prove_eq: (existential) at prove_eq.rs:23
                                           _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                           env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                           assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                           a: ?ty_1
                                           b: !ty_0
                                           result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                       └─ prove_existential_var_eq: (existential-universal) at prove_eq.rs:76
                                              _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                              env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                              assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                              v: ?ty_1
                                              b: !ty_0
                                              result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                          └─ IfThen { expression: "env.universe(p) < env.universe(v)" }: at prove_eq.rs:76
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                     constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                     assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                 └─ (assumptions, goal) = ({CoreTrait(!ty_0), FooTrait(!ty_0)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                        assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                  assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                  goal: {CoreTrait(?ty_1)}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ (assumptions, goal) = ({CoreTrait(!ty_0), FooTrait(!ty_0)}, {CoreTrait(!ty_0)}): at prove_after.rs:8
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                     goals: {CoreTrait(!ty_0)}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (assumption - predicate) at prove_wc.rs:22
                                        _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                        assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                        goal: CoreTrait(!ty_0)
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ item = CoreTrait(!ty_0): at prove_wc.rs:22
                                    └─ prove_via: (predicate-congruence-axiom) at prove_via.rs:9
                                           _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                           env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                           assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                           via: CoreTrait(!ty_0)
                                           goal: CoreTrait(!ty_0)
                                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (skel_c, parameters_c) = (is_implemented(CoreTrait), [!ty_0]): at prove_via.rs:9
                                       └─ (skel_g, parameters_g) = (is_implemented(CoreTrait), [!ty_0]): at prove_via.rs:9
                                       └─ IfThen { expression: "skel_c == skel_g", skel_c: is_implemented(CoreTrait), skel_g: is_implemented(CoreTrait) }: at prove_via.rs:9
                                       └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                              env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                              assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                              goals: {!ty_0 = !ty_0}
                                              result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ prove_wc: (eq) at prove_wc.rs:22
                                                 _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                                 env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                 assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                                 goal: !ty_0 = !ty_0
                                                 result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ trivial, as a == b is true: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                          └─ prove_after: (prove_after) at prove_after.rs:8
                                                 _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                                 constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                 assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                                 goal: {}
                                                 result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ (assumptions, goal) = ({CoreTrait(!ty_0), FooTrait(!ty_0)}, {}): at prove_after.rs:8
                                             └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                    _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                                    env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                    assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                                    goals: {}
                                                    result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                        constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({CoreTrait(!ty_0), FooTrait(!ty_0)}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                           env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                           assumptions: {CoreTrait(!ty_0), FooTrait(!ty_0)}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                  assumptions: {CoreTrait(!ty_0)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ (assumptions, goal) = ({CoreTrait(!ty_0)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {CoreTrait(!ty_0)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {CoreTrait(!ty_0)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({CoreTrait(!ty_0)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {CoreTrait(!ty_0)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! FooTrait(?ty_0)}, assumptions: {CoreTrait(?ty_0)}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! FooTrait(?ty_0)}, assumptions: {CoreTrait(?ty_0)}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! FooTrait(?ty_0), assumptions: {CoreTrait(?ty_0)}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {FooTrait(CoreStruct)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: FooTrait(CoreStruct)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl FooTrait(CoreStruct): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [], bias: Soundness, pending: [] }, []): at prove_wc.rs:22
                           └─ i = FooTrait(CoreStruct): at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ co_assumptions = ({}, FooTrait(CoreStruct)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {FooTrait(CoreStruct)}
                                  goals: {CoreStruct = CoreStruct}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {FooTrait(CoreStruct)}
                                     goal: CoreStruct = CoreStruct
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {FooTrait(CoreStruct)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({FooTrait(CoreStruct)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {FooTrait(CoreStruct)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {FooTrait(CoreStruct)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({FooTrait(CoreStruct)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {FooTrait(CoreStruct)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! FooTrait(CoreStruct)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! FooTrait(CoreStruct)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! FooTrait(CoreStruct), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_coherence(foo): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {CoreTrait(!ty_0)}
                            goals: {@ IsLocal(FooTrait(!ty_0))}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {CoreTrait(!ty_0)}
                               goal: @ IsLocal(FooTrait(!ty_0))
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {CoreTrait(!ty_0)}
                                  goal: FooTrait(!ty_0)
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {}), &goal.trait_id: FooTrait }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {CoreTrait(!ty_0)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({CoreTrait(!ty_0)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {CoreTrait(!ty_0)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(FooTrait(CoreStruct))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(FooTrait(CoreStruct))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: FooTrait(CoreStruct)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {}), &goal.trait_id: FooTrait }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait CoreTrait <ty> , trait FooTrait <ty> ], [impl <ty> FooTrait(^ty0_0) where {CoreTrait(^ty0_0)}, impl FooTrait(CoreStruct)], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {FooTrait}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ overlap_check: (inverted) at coherence.rs:168
                     └─ overlap_check: (inverted) at coherence.rs:168
        "#]]
    )
}

#[test]
fn foo_crate_cannot_assume_CoreStruct_does_not_impl_CoreTrait() {
    crate::assert_err!(
        [
            crate core {
                trait CoreTrait {}
                struct CoreStruct {}
            },
            crate foo {
                trait FooTrait {}
                impl<ty T> FooTrait for T where T: CoreTrait {}
                impl FooTrait for CoreStruct {}
            }
        ]

        [
            "impls may overlap",
        ]

        expect_test::expect![[r#"
            impls may overlap:
            impl <ty> FooTrait for ^ty0_0 where ^ty0_0 : CoreTrait { }
            impl FooTrait for CoreStruct { }"#]]
    )
}

#[test]
fn T_where_Foo_not_u32_impls() {
    crate::assert_err!(
        // Test positive impl that has a where-clause which checks for itself,
        // i.e., `T: Foo where T: Foo`. This `T: Foo` where-clause isn't harmful
        // in the coinductive interpretation of trait matching, it actually
        // doesn't change the meaning of the impl at all. However, this formulation
        // was erroneously accepted by an earlier variant of negative impls.
        [
            crate core {
                trait Foo {}
                impl<ty T> Foo for T where T: Foo {}
                impl !Foo for u32 {}
            }
        ]

        [
            "check_trait_impl",
            "Foo for ^ty0_0",
        ]

        expect_test::expect![[r#"
            check_trait_impl(impl <ty> Foo for ^ty0_0 where ^ty0_0 : Foo { })

            Caused by:
                failed to prove {! Foo(!ty_1)} given {Foo(!ty_1)}, got [Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [] }, known_true: false, substitution: {} }]"#]]
    )
}

#[test]
fn u32_T_where_T_Is_impls() {
    crate::assert_err!(
        // Test that we detect "indirect" overlap -- here `Foo` is implemented for `u32`
        // and also all `T: Is`, and `u32: Is`.
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl<ty T> Foo for T where T: Is {}

                trait Is {}
                impl Is for u32 {}
            }
        ]

        [
            "impls may overlap",
        ]

        expect_test::expect![[r#"
            impls may overlap:
            impl Foo for u32 { }
            impl <ty> Foo for ^ty0_0 where ^ty0_0 : Is { }"#]]
    )
}

#[test]
fn u32_T_where_T_Not_impls() {
    crate::assert_ok!(
        //@check-pass

        // Test that, within a crate, we are able to rely on the fact
        // that `u32: Not` is not implemented.
        //
        // See also test_foo_crate_cannot_assume_CoreStruct_does_not_impl_CoreTrait
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl<ty T> Foo for T where T: Not {}

                trait Not {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(core): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {Foo(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
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
                                  _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {Foo(u32)}
                                  goals: {u32 = u32}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Foo(u32)}
                                     goal: u32 = u32
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Foo(u32)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({Foo(u32)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Foo(u32)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Foo(u32)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Foo(u32)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Foo(u32)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Foo(u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Foo(u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Foo(u32), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {Not(!ty_0)}
                            goals: {@ WellFormedTraitRef(Not(!ty_0))}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {Not(!ty_0)}
                               goal: @ WellFormedTraitRef(Not(!ty_0))
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ for_all: at combinators.rs:73
                              └─ prove_wf: (universal variables) at prove_wf.rs:14
                                     _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Not(!ty_0)}
                                     goal: !ty_0
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                           └─ t = trait Not <ty> : at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Not(!ty_0)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Not(!ty_0)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Not(!ty_0)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Not(!ty_0)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Not(!ty_0)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Not(!ty_0)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {Not(!ty_0)}
                            goals: {Foo(!ty_0)}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {Not(!ty_0)}
                               goal: Foo(!ty_0)
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}: at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, [?ty_1]): at prove_wc.rs:22
                           └─ i = Foo(?ty_1) where {Not(?ty_1)}: at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ co_assumptions = ({Not(!ty_0)}, Foo(!ty_0)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                  env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                  assumptions: {Foo(!ty_0), Not(!ty_0)}
                                  goals: {!ty_0 = ?ty_1}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                     env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                     assumptions: {Foo(!ty_0), Not(!ty_0)}
                                     goal: !ty_0 = ?ty_1
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                 └─ prove_eq: (symmetric) at prove_eq.rs:23
                                        _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                        env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                        assumptions: {Foo(!ty_0), Not(!ty_0)}
                                        a: !ty_0
                                        b: ?ty_1
                                        result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                    └─ prove_eq: (existential) at prove_eq.rs:23
                                           _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                           env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                           assumptions: {Foo(!ty_0), Not(!ty_0)}
                                           a: ?ty_1
                                           b: !ty_0
                                           result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                       └─ prove_existential_var_eq: (existential-universal) at prove_eq.rs:76
                                              _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                              env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                              assumptions: {Foo(!ty_0), Not(!ty_0)}
                                              v: ?ty_1
                                              b: !ty_0
                                              result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                          └─ IfThen { expression: "env.universe(p) < env.universe(v)" }: at prove_eq.rs:76
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                     constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                     assumptions: {Foo(!ty_0), Not(!ty_0)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                 └─ (assumptions, goal) = ({Foo(!ty_0), Not(!ty_0)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                        assumptions: {Foo(!ty_0), Not(!ty_0)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                  assumptions: {Foo(!ty_0), Not(!ty_0)}
                                  goal: {Not(?ty_1)}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ (assumptions, goal) = ({Foo(!ty_0), Not(!ty_0)}, {Not(!ty_0)}): at prove_after.rs:8
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Foo(!ty_0), Not(!ty_0)}
                                     goals: {Not(!ty_0)}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (assumption - predicate) at prove_wc.rs:22
                                        _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                        assumptions: {Foo(!ty_0), Not(!ty_0)}
                                        goal: Not(!ty_0)
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ item = Not(!ty_0): at prove_wc.rs:22
                                    └─ prove_via: (predicate-congruence-axiom) at prove_via.rs:9
                                           _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                           env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                           assumptions: {Foo(!ty_0), Not(!ty_0)}
                                           via: Not(!ty_0)
                                           goal: Not(!ty_0)
                                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (skel_c, parameters_c) = (is_implemented(Not), [!ty_0]): at prove_via.rs:9
                                       └─ (skel_g, parameters_g) = (is_implemented(Not), [!ty_0]): at prove_via.rs:9
                                       └─ IfThen { expression: "skel_c == skel_g", skel_c: is_implemented(Not), skel_g: is_implemented(Not) }: at prove_via.rs:9
                                       └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                              env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                              assumptions: {Foo(!ty_0), Not(!ty_0)}
                                              goals: {!ty_0 = !ty_0}
                                              result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ prove_wc: (eq) at prove_wc.rs:22
                                                 _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                                 env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                 assumptions: {Foo(!ty_0), Not(!ty_0)}
                                                 goal: !ty_0 = !ty_0
                                                 result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ trivial, as a == b is true: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                          └─ prove_after: (prove_after) at prove_after.rs:8
                                                 _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                                 constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                 assumptions: {Foo(!ty_0), Not(!ty_0)}
                                                 goal: {}
                                                 result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ (assumptions, goal) = ({Foo(!ty_0), Not(!ty_0)}, {}): at prove_after.rs:8
                                             └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                    _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                                    env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                    assumptions: {Foo(!ty_0), Not(!ty_0)}
                                                    goals: {}
                                                    result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                        constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {Foo(!ty_0), Not(!ty_0)}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({Foo(!ty_0), Not(!ty_0)}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                           env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                           assumptions: {Foo(!ty_0), Not(!ty_0)}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                  assumptions: {Not(!ty_0)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ (assumptions, goal) = ({Not(!ty_0)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Not(!ty_0)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Not(!ty_0)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Not(!ty_0)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Not(!ty_0)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Foo(?ty_0)}, assumptions: {Not(?ty_0)}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Foo(?ty_0)}, assumptions: {Not(?ty_0)}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Foo(?ty_0), assumptions: {Not(?ty_0)}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_coherence(core): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(Foo(u32))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(Foo(u32))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: Foo(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {}), &goal.trait_id: Foo }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {Not(!ty_0)}
                            goals: {@ IsLocal(Foo(!ty_0))}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {Not(!ty_0)}
                               goal: @ IsLocal(Foo(!ty_0))
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Not(!ty_0)}
                                  goal: Foo(!ty_0)
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {}), &goal.trait_id: Foo }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Not(!ty_0)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Not(!ty_0)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Not(!ty_0)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ overlap_check: (not_goal) at coherence.rs:127
                        └─ negation succeeded: judgment `prove { goal: {u32 = ?ty_0, Not(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {u32 = ?ty_0, Not(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #1 (crates/formality-prove/src/prove/prove_wc_list.rs:30:14) because
                    judgment `prove_after { constraints: Constraints { env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, known_true: true, substitution: {?ty_0 => u32} }, goal: {Not(?ty_0)}, assumptions: {} }` failed at the following rule(s):
                      the rule "prove_after" failed at step #1 (crates/formality-prove/src/prove/prove_after.rs:19:14) because
                        judgment `prove { goal: {Not(u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {}) }` failed at the following rule(s):
                          failed at (crates/formality-prove/src/prove.rs:89:45) because
                            judgment `prove_wc_list { goals: {Not(u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                              the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                                judgment `prove_wc { goal: Not(u32), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                  the rule "coherence / remote impl" failed at step #1 (crates/formality-prove/src/prove/prove_wc.rs:94:14) because
                                    judgment `may_be_remote { assumptions: {}, goal: Not(u32), env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                      the rule "may be added by upstream in a minor release" failed at step #0 (crates/formality-prove/src/prove/is_local.rs:68:14) because
                                        judgment `negation_via_failure` failed at the following rule(s):
                                          failed at (crates/formality-prove/src/prove/negation.rs:99:17) because
                                            found an unconditionally true solution Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  the rule "trait implied bound" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:116:14) because
                                    expression evaluated to an empty collection: `decls.trait_invariants()`: at negation.rs:125
                     └─ overlap_check: (not_goal) at coherence.rs:127
                        └─ negation succeeded: judgment `prove { goal: {?ty_0 = u32, Not(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {?ty_0 = u32, Not(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #1 (crates/formality-prove/src/prove/prove_wc_list.rs:30:14) because
                    judgment `prove_after { constraints: Constraints { env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, known_true: true, substitution: {?ty_0 => u32} }, goal: {Not(?ty_0)}, assumptions: {} }` failed at the following rule(s):
                      the rule "prove_after" failed at step #1 (crates/formality-prove/src/prove/prove_after.rs:19:14) because
                        judgment `prove { goal: {Not(u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty> , trait Not <ty> ], [impl Foo(u32), impl <ty> Foo(^ty0_0) where {Not(^ty0_0)}], [], [], [], [], {Foo, Not}, {}) }` failed at the following rule(s):
                          failed at (crates/formality-prove/src/prove.rs:89:45) because
                            judgment `prove_wc_list { goals: {Not(u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                              the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                                judgment `prove_wc { goal: Not(u32), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                  the rule "coherence / remote impl" failed at step #1 (crates/formality-prove/src/prove/prove_wc.rs:94:14) because
                                    judgment `may_be_remote { assumptions: {}, goal: Not(u32), env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                      the rule "may be added by upstream in a minor release" failed at step #0 (crates/formality-prove/src/prove/is_local.rs:68:14) because
                                        judgment `negation_via_failure` failed at the following rule(s):
                                          failed at (crates/formality-prove/src/prove/negation.rs:99:17) because
                                            found an unconditionally true solution Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  the rule "trait implied bound" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:116:14) because
                                    expression evaluated to an empty collection: `decls.trait_invariants()`: at negation.rs:125
        "#]]
    )
}

#[test]
fn u32_u32_impls() {
    crate::assert_err!(
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl Foo for u32 {}
            }
        ]

        [
            "duplicate impl",
        ]

        expect_test::expect!["duplicate impl in current crate: impl Foo for u32 { }"]
    )
}

#[test]
fn u32_i32_impls() {
    crate::assert_ok!(
        //@check-pass
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl Foo for i32 {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(core): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {Foo(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
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
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {Foo(u32)}
                                  goals: {u32 = u32}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Foo(u32)}
                                     goal: u32 = u32
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Foo(u32)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({Foo(u32)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Foo(u32)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Foo(u32)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Foo(u32)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Foo(u32)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Foo(u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Foo(u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Foo(u32), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {Foo(i32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: Foo(i32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl Foo(i32): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [], bias: Soundness, pending: [] }, []): at prove_wc.rs:22
                           └─ i = Foo(i32): at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ co_assumptions = ({}, Foo(i32)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {Foo(i32)}
                                  goals: {i32 = i32}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Foo(i32)}
                                     goal: i32 = i32
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Foo(i32)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({Foo(i32)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Foo(i32)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Foo(i32)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Foo(i32)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Foo(i32)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Foo(i32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Foo(i32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Foo(i32), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_coherence(core): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(Foo(u32))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(Foo(u32))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: Foo(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {}), &goal.trait_id: Foo }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(Foo(i32))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(Foo(i32))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: Foo(i32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {}), &goal.trait_id: Foo }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ overlap_check: (not_goal) at coherence.rs:127
                        └─ negation succeeded: judgment `prove { goal: {u32 = i32}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {u32 = i32}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: u32 = i32, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "eq" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:126:14) because
                        judgment `prove_eq { a: u32, b: i32, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                          the rule "normalize-l" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:68:14) because
                            judgment had no applicable rules: `prove_normalize { p: u32, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }`
                          the rule "symmetric" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:38:14) because
                            judgment `prove_eq { a: i32, b: u32, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                              the rule "normalize-l" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:68:14) because
                                judgment had no applicable rules: `prove_normalize { p: i32, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }`
                              the rule "symmetric" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:38:14) because
                                cyclic proof attempt: `prove_eq { a: u32, b: i32, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }`: at negation.rs:125
                     └─ overlap_check: (not_goal) at coherence.rs:127
                        └─ negation succeeded: judgment `prove { goal: {i32 = u32}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty> ], [impl Foo(u32), impl Foo(i32)], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {i32 = u32}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: i32 = u32, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "eq" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:126:14) because
                        judgment `prove_eq { a: i32, b: u32, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                          the rule "normalize-l" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:68:14) because
                            judgment had no applicable rules: `prove_normalize { p: i32, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }`
                          the rule "symmetric" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:38:14) because
                            judgment `prove_eq { a: u32, b: i32, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                              the rule "normalize-l" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:68:14) because
                                judgment had no applicable rules: `prove_normalize { p: u32, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }`
                              the rule "symmetric" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:38:14) because
                                cyclic proof attempt: `prove_eq { a: i32, b: u32, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }`: at negation.rs:125
        "#]]
    )
}

#[test]
fn u32_T_impls() {
    crate::assert_err!(
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl<ty T> Foo for T {}
            }
        ]

        [
            "impls may overlap",
        ]

        expect_test::expect![[r#"
            impls may overlap:
            impl Foo for u32 { }
            impl <ty> Foo for ^ty0_0 { }"#]]
    )
}

#[test]
fn T_and_T_bar() {
    crate::assert_err! {
        [
            crate core {
                trait Foo { }

                trait Bar { }

                impl<ty T> Foo for T { }

                impl<ty T> Foo for T where T: Bar { }
            }
        ]

        [
            "impls may overlap",
        ]

        expect_test::expect![[r#"
            impls may overlap:
            impl <ty> Foo for ^ty0_0 { }
            impl <ty> Foo for ^ty0_0 where ^ty0_0 : Bar { }"#]]
    }
}

#[test]
fn T_and_Local_Bar_T() {
    crate::assert_err! {
        [
            crate core {
                trait Foo { }

                trait Bar<ty U> { }

                impl<ty T> Foo for T { }

                impl<ty T> Foo for T where LocalType: Bar<T> { }

                struct LocalType { }
            }
        ]

        [
            "impls may overlap",
        ]

        expect_test::expect![[r#"
            impls may overlap:
            impl <ty> Foo for ^ty0_0 { }
            impl <ty> Foo for ^ty0_0 where LocalType : Bar <^ty0_0> { }"#]]
    }
}

#[test]
fn is_local_unknowable_trait_ref() {
    crate::assert_ok! {
        [
            crate core {
                trait Project {
                    type Assoc: [];
                }

                impl<ty T> Project for T {
                    type Assoc = T;
                }

                trait Foo<ty U> { }
            },
            crate foo {
                struct LocalType {}

                trait Overlap<ty U> {}
                impl<ty T, ty U> Overlap<U> for T
                where
                    <T as Project>::Assoc: Foo<U> {}
                impl<ty T> Overlap<LocalType> for () {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(core): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ check_associated_ty(Assoc): at traits.rs:121
                        └─ prove_wc_list: (none) at prove_wc_list.rs:11
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {Project(!ty_0)}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: Project(!ty_0)
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl <ty> Project(^ty0_0): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, [?ty_1]): at prove_wc.rs:22
                           └─ i = Project(?ty_1): at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ co_assumptions = ({}, Project(!ty_0)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                  env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                  assumptions: {Project(!ty_0)}
                                  goals: {!ty_0 = ?ty_1}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                     env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                     assumptions: {Project(!ty_0)}
                                     goal: !ty_0 = ?ty_1
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                 └─ prove_eq: (symmetric) at prove_eq.rs:23
                                        _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                        env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                        assumptions: {Project(!ty_0)}
                                        a: !ty_0
                                        b: ?ty_1
                                        result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                    └─ prove_eq: (existential) at prove_eq.rs:23
                                           _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                           env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                           assumptions: {Project(!ty_0)}
                                           a: ?ty_1
                                           b: !ty_0
                                           result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                       └─ prove_existential_var_eq: (existential-universal) at prove_eq.rs:76
                                              _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                              env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                              assumptions: {Project(!ty_0)}
                                              v: ?ty_1
                                              b: !ty_0
                                              result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                          └─ IfThen { expression: "env.universe(p) < env.universe(v)" }: at prove_eq.rs:76
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                     constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                     assumptions: {Project(!ty_0)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                 └─ (assumptions, goal) = ({Project(!ty_0)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                        assumptions: {Project(!ty_0)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                  assumptions: {Project(!ty_0)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ (assumptions, goal) = ({Project(!ty_0)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Project(!ty_0)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Project(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Project(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Project(?ty_0), assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                     └─ check_associated_ty_value(Assoc): at impls.rs:305
                        └─ prove_wc_list: (none) at prove_wc_list.rs:11
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc_list: (none) at prove_wc_list.rs:11
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(!ty_0)}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(!ty_0)
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (universal variables) at prove_wf.rs:14
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: !ty_0
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc_list: (none) at prove_wc_list.rs:11
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_coherence(core): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(Project(!ty_0))}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(Project(!ty_0))
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: Project(!ty_0)
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {}), &goal.trait_id: Project }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> ], [impl <ty> Project(^ty0_0)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [], {Foo, Project}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
               └─ check_current_crate(foo): at lib.rs:73
                  └─ check_adt(LocalType): at adts.rs:12
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                            env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                            assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                            goals: {@ WellFormedTraitRef(Foo(<!ty_0 as Project>::Assoc, !ty_1))}
                            result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                               env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                               assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                               goal: @ WellFormedTraitRef(Foo(<!ty_0 as Project>::Assoc, !ty_1))
                               result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ for_all: at combinators.rs:73
                              └─ prove_wf: (aliases) at prove_wf.rs:14
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                     env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                     assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                                     goal: <!ty_0 as Project>::Assoc
                                     result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ for_all: at combinators.rs:73
                                    └─ prove_wf: (universal variables) at prove_wf.rs:14
                                           _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                           env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                           assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                                           goal: !ty_0
                                           result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                              └─ for_all: at combinators.rs:73
                                 └─ prove_wf: (universal variables) at prove_wf.rs:14
                                        _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                        env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                        assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                                        goal: !ty_1
                                        result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                           └─ t = trait Foo <ty, ty> : at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                  constraints: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Foo(<!ty_0 as Project>::Assoc, !ty_1)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                     env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                     assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                               constraints: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Foo(<!ty_0 as Project>::Assoc, !ty_1)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                  env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                  assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                            env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                            assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                            goals: {Overlap(!ty_0, !ty_1)}
                            result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                               env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                               assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                               goal: Overlap(!ty_0, !ty_1)
                               result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}: at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }, [?ty_2, ?ty_3]): at prove_wc.rs:22
                           └─ i = Overlap(?ty_2, ?ty_3) where {Foo(<?ty_2 as Project>::Assoc, ?ty_3)}: at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ co_assumptions = ({Foo(<!ty_0 as Project>::Assoc, !ty_1)}, Overlap(!ty_0, !ty_1)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                  env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }
                                  assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                  goals: {!ty_0 = ?ty_2, !ty_1 = ?ty_3}
                                  result: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_0, ?ty_3 => !ty_1} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                     env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }
                                     assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                     goal: !ty_0 = ?ty_2
                                     result: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_0} }
                                 └─ prove_eq: (symmetric) at prove_eq.rs:23
                                        _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                        env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }
                                        assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                        a: !ty_0
                                        b: ?ty_2
                                        result: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_0} }
                                    └─ prove_eq: (existential) at prove_eq.rs:23
                                           _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                           env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }
                                           assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                           a: ?ty_2
                                           b: !ty_0
                                           result: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_0} }
                                       └─ prove_existential_var_eq: (existential-universal) at prove_eq.rs:76
                                              _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                              env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }
                                              assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                              v: ?ty_2
                                              b: !ty_0
                                              result: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_0} }
                                          └─ IfThen { expression: "env.universe(p) < env.universe(v)" }: at prove_eq.rs:76
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                     constraints: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_0} }
                                     assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                     goal: {!ty_1 = ?ty_3}
                                     result: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_0, ?ty_3 => !ty_1} }
                                 └─ (assumptions, goal) = ({Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}, {!ty_1 = ?ty_3}): at prove_after.rs:8
                                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                        env: Env { variables: [!ty_0, !ty_1, ?ty_2], bias: Soundness, pending: [] }
                                        assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                        goals: {!ty_1 = ?ty_2}
                                        result: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_1} }
                                    └─ prove_wc: (eq) at prove_wc.rs:22
                                           _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                           env: Env { variables: [!ty_0, !ty_1, ?ty_2], bias: Soundness, pending: [] }
                                           assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                           goal: !ty_1 = ?ty_2
                                           result: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_1} }
                                       └─ prove_eq: (symmetric) at prove_eq.rs:23
                                              _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                              env: Env { variables: [!ty_0, !ty_1, ?ty_2], bias: Soundness, pending: [] }
                                              assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                              a: !ty_1
                                              b: ?ty_2
                                              result: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_1} }
                                          └─ prove_eq: (existential) at prove_eq.rs:23
                                                 _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                                 env: Env { variables: [!ty_0, !ty_1, ?ty_2], bias: Soundness, pending: [] }
                                                 assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                                 a: ?ty_2
                                                 b: !ty_1
                                                 result: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_1} }
                                             └─ prove_existential_var_eq: (existential-universal) at prove_eq.rs:76
                                                    _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                                    env: Env { variables: [!ty_0, !ty_1, ?ty_2], bias: Soundness, pending: [] }
                                                    assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                                    v: ?ty_2
                                                    b: !ty_1
                                                    result: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_1} }
                                                └─ IfThen { expression: "env.universe(p) < env.universe(v)" }: at prove_eq.rs:76
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                           constraints: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_1} }
                                           assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                           goal: {}
                                           result: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_1} }
                                       └─ (assumptions, goal) = ({Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}, {}): at prove_after.rs:8
                                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                              env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                              assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                              goals: {}
                                              result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                  constraints: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_0, ?ty_3 => !ty_1} }
                                  assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                  goal: {Foo(<?ty_2 as Project>::Assoc, ?ty_3)}
                                  result: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_0, ?ty_3 => !ty_1} }
                              └─ (assumptions, goal) = ({Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}, {Foo(<!ty_0 as Project>::Assoc, !ty_1)}): at prove_after.rs:8
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                     env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                     assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                     goals: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                                     result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (assumption - predicate) at prove_wc.rs:22
                                        _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                        env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                        assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                        goal: Foo(<!ty_0 as Project>::Assoc, !ty_1)
                                        result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ item = Foo(<!ty_0 as Project>::Assoc, !ty_1): at prove_wc.rs:22
                                    └─ prove_via: (predicate-congruence-axiom) at prove_via.rs:9
                                           _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                           env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                           assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                           via: Foo(<!ty_0 as Project>::Assoc, !ty_1)
                                           goal: Foo(<!ty_0 as Project>::Assoc, !ty_1)
                                           result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (skel_c, parameters_c) = (is_implemented(Foo), [<!ty_0 as Project>::Assoc, !ty_1]): at prove_via.rs:9
                                       └─ (skel_g, parameters_g) = (is_implemented(Foo), [<!ty_0 as Project>::Assoc, !ty_1]): at prove_via.rs:9
                                       └─ IfThen { expression: "skel_c == skel_g", skel_c: is_implemented(Foo), skel_g: is_implemented(Foo) }: at prove_via.rs:9
                                       └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                              env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                              assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                              goals: {<!ty_0 as Project>::Assoc = <!ty_0 as Project>::Assoc, !ty_1 = !ty_1}
                                              result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ prove_wc: (eq) at prove_wc.rs:22
                                                 _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                                 env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                                 assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                                 goal: <!ty_0 as Project>::Assoc = <!ty_0 as Project>::Assoc
                                                 result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ trivial, as a == b is true: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                          └─ prove_after: (prove_after) at prove_after.rs:8
                                                 _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                                 constraints: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                 assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                                 goal: {!ty_1 = !ty_1}
                                                 result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ (assumptions, goal) = ({Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}, {!ty_1 = !ty_1}): at prove_after.rs:8
                                             └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                                    _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                                    env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                                    assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                                    goals: {!ty_1 = !ty_1}
                                                    result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                └─ prove_wc: (eq) at prove_wc.rs:22
                                                       _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                                       env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                                       assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                                       goal: !ty_1 = !ty_1
                                                       result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                   └─ trivial, as a == b is true: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                                └─ prove_after: (prove_after) at prove_after.rs:8
                                                       _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                                       constraints: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                       assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                                       goal: {}
                                                       result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                   └─ (assumptions, goal) = ({Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}, {}): at prove_after.rs:8
                                                   └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                          _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                                          env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                                          assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                                          goals: {}
                                                          result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                        constraints: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                           env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                           assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1), Overlap(!ty_0, !ty_1)}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                  constraints: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_0, ?ty_3 => !ty_1} }
                                  assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0, !ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_2 => !ty_0, ?ty_3 => !ty_1} }
                              └─ (assumptions, goal) = ({Foo(<!ty_0 as Project>::Assoc, !ty_1)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                     env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                     assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                               constraints: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Foo(<!ty_0 as Project>::Assoc, !ty_1)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                  env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                  assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Overlap(?ty_0, ?ty_1)}, assumptions: {Foo(<?ty_0 as Project>::Assoc, ?ty_1)}, env: Env { variables: [?ty_0, ?ty_1], bias: Completeness, pending: [] }, decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Overlap(?ty_0, ?ty_1)}, assumptions: {Foo(<?ty_0 as Project>::Assoc, ?ty_1)}, env: Env { variables: [?ty_0, ?ty_1], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Overlap(?ty_0, ?ty_1), assumptions: {Foo(<?ty_0 as Project>::Assoc, ?ty_1)}, env: Env { variables: [?ty_0, ?ty_1], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {Overlap((), LocalType)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: Overlap((), LocalType)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl <ty> Overlap((), LocalType): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [?ty_1], bias: Soundness, pending: [] }, [?ty_1]): at prove_wc.rs:22
                           └─ i = Overlap((), LocalType): at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ co_assumptions = ({}, Overlap((), LocalType)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {Overlap((), LocalType)}
                                  goals: {LocalType = LocalType, () = ()}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Overlap((), LocalType)}
                                     goal: LocalType = LocalType
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Overlap((), LocalType)}
                                     goal: {() = ()}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({Overlap((), LocalType)}, {() = ()}): at prove_after.rs:8
                                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Overlap((), LocalType)}
                                        goals: {() = ()}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_wc: (eq) at prove_wc.rs:22
                                           _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {Overlap((), LocalType)}
                                           goal: () = ()
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                           constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                           assumptions: {Overlap((), LocalType)}
                                           goal: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (assumptions, goal) = ({Overlap((), LocalType)}, {}): at prove_after.rs:8
                                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {Overlap((), LocalType)}
                                              goals: {}
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                  constraints: Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Overlap((), LocalType)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Overlap((), LocalType)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Overlap((), LocalType)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                  constraints: Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Overlap((), LocalType)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Overlap((), LocalType)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Overlap((), LocalType), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_coherence(foo): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                            env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                            assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                            goals: {@ IsLocal(Overlap(!ty_0, !ty_1))}
                            result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                               env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                               assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                               goal: @ IsLocal(Overlap(!ty_0, !ty_1))
                               result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                  env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                  assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                                  goal: Overlap(!ty_0, !ty_1)
                                  result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType}), &goal.trait_id: Overlap }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                               constraints: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Foo(<!ty_0 as Project>::Assoc, !ty_1)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                  env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                  assumptions: {Foo(<!ty_0 as Project>::Assoc, !ty_1)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(Overlap((), LocalType))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(Overlap((), LocalType))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: Overlap((), LocalType)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType}), &goal.trait_id: Overlap }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ overlap_check: (not_goal) at coherence.rs:127
                        └─ negation succeeded: judgment `prove { goal: {?ty_0 = (), ?ty_1 = LocalType, Foo(<?ty_0 as Project>::Assoc, ?ty_1)}, assumptions: {}, env: Env { variables: [?ty_0, ?ty_1], bias: Completeness, pending: [] }, decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {?ty_0 = (), ?ty_1 = LocalType, Foo(<?ty_0 as Project>::Assoc, ?ty_1)}, assumptions: {}, env: Env { variables: [?ty_0, ?ty_1], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #1 (crates/formality-prove/src/prove/prove_wc_list.rs:30:14) because
                    judgment `prove_after { constraints: Constraints { env: Env { variables: [?ty_0, ?ty_1], bias: Completeness, pending: [] }, known_true: true, substitution: {?ty_0 => ()} }, goal: {?ty_1 = LocalType, Foo(<?ty_0 as Project>::Assoc, ?ty_1)}, assumptions: {} }` failed at the following rule(s):
                      the rule "prove_after" failed at step #1 (crates/formality-prove/src/prove/prove_after.rs:19:14) because
                        judgment `prove { goal: {?ty_0 = LocalType, Foo(<() as Project>::Assoc, ?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType}) }` failed at the following rule(s):
                          failed at (crates/formality-prove/src/prove.rs:89:45) because
                            judgment `prove_wc_list { goals: {?ty_0 = LocalType, Foo(<() as Project>::Assoc, ?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                              the rule "some" failed at step #1 (crates/formality-prove/src/prove/prove_wc_list.rs:30:14) because
                                judgment `prove_after { constraints: Constraints { env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, known_true: true, substitution: {?ty_0 => LocalType} }, goal: {Foo(<() as Project>::Assoc, ?ty_0)}, assumptions: {} }` failed at the following rule(s):
                                  the rule "prove_after" failed at step #1 (crates/formality-prove/src/prove/prove_after.rs:19:14) because
                                    judgment `prove { goal: {Foo(<() as Project>::Assoc, LocalType)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType}) }` failed at the following rule(s):
                                      failed at (crates/formality-prove/src/prove.rs:89:45) because
                                        judgment `prove_wc_list { goals: {Foo(<() as Project>::Assoc, LocalType)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                          the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                                            judgment `prove_wc { goal: Foo(<() as Project>::Assoc, LocalType), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                              the rule "coherence / remote impl" failed at step #1 (crates/formality-prove/src/prove/prove_wc.rs:94:14) because
                                                judgment `may_be_remote { assumptions: {}, goal: Foo(<() as Project>::Assoc, LocalType), env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                                  the rule "may be added by upstream in a minor release" failed at step #0 (crates/formality-prove/src/prove/is_local.rs:68:14) because
                                                    judgment `negation_via_failure` failed at the following rule(s):
                                                      failed at (crates/formality-prove/src/prove/negation.rs:99:17) because
                                                        found an unconditionally true solution Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                  the rule "may be defined downstream" failed at step #0 (crates/formality-prove/src/prove/is_local.rs:60:14) because
                                                    judgment `may_be_downstream_trait_ref { goal: Foo(<() as Project>::Assoc, LocalType), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                                      the rule "may_be_downstream_trait_ref" failed at step #1 (crates/formality-prove/src/prove/is_local.rs:88:14) because
                                                        judgment `may_be_downstream_parameter { parameter: <() as Project>::Assoc, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                                          the rule "via normalize" failed at step #1 (crates/formality-prove/src/prove/is_local.rs:121:14) because
                                                            judgment `may_contain_downstream_type { parameter: (), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                                              the rule "rigid type parameter" failed at step #0 (crates/formality-prove/src/prove/is_local.rs:152:14) because
                                                                expression evaluated to an empty collection: `parameters.iter()`
                                              the rule "trait implied bound" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:116:14) because
                                                expression evaluated to an empty collection: `decls.trait_invariants()`: at negation.rs:125
                     └─ overlap_check: (not_goal) at coherence.rs:127
                        └─ negation succeeded: judgment `prove { goal: {LocalType = ?ty_0, () = ?ty_1, Foo(<?ty_1 as Project>::Assoc, ?ty_0)}, assumptions: {}, env: Env { variables: [?ty_1, ?ty_0], bias: Completeness, pending: [] }, decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {LocalType = ?ty_0, () = ?ty_1, Foo(<?ty_1 as Project>::Assoc, ?ty_0)}, assumptions: {}, env: Env { variables: [?ty_1, ?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #1 (crates/formality-prove/src/prove/prove_wc_list.rs:30:14) because
                    judgment `prove_after { constraints: Constraints { env: Env { variables: [?ty_1, ?ty_0], bias: Completeness, pending: [] }, known_true: true, substitution: {?ty_0 => LocalType} }, goal: {() = ?ty_1, Foo(<?ty_1 as Project>::Assoc, ?ty_0)}, assumptions: {} }` failed at the following rule(s):
                      the rule "prove_after" failed at step #1 (crates/formality-prove/src/prove/prove_after.rs:19:14) because
                        judgment `prove { goal: {() = ?ty_0, Foo(<?ty_0 as Project>::Assoc, LocalType)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType}) }` failed at the following rule(s):
                          failed at (crates/formality-prove/src/prove.rs:89:45) because
                            judgment `prove_wc_list { goals: {() = ?ty_0, Foo(<?ty_0 as Project>::Assoc, LocalType)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                              the rule "some" failed at step #1 (crates/formality-prove/src/prove/prove_wc_list.rs:30:14) because
                                judgment `prove_after { constraints: Constraints { env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, known_true: true, substitution: {?ty_0 => ()} }, goal: {Foo(<?ty_0 as Project>::Assoc, LocalType)}, assumptions: {} }` failed at the following rule(s):
                                  the rule "prove_after" failed at step #1 (crates/formality-prove/src/prove/prove_after.rs:19:14) because
                                    judgment `prove { goal: {Foo(<() as Project>::Assoc, LocalType)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Project <ty> , trait Foo <ty, ty> , trait Overlap <ty, ty> ], [impl <ty> Project(^ty0_0), impl <ty, ty> Overlap(^ty0_0, ^ty0_1) where {Foo(<^ty0_0 as Project>::Assoc, ^ty0_1)}, impl <ty> Overlap((), LocalType)], [], [alias <ty> <^ty0_0 as Project>::Assoc = ^ty0_0], [], [adt LocalType { struct { } }], {Overlap}, {LocalType}) }` failed at the following rule(s):
                                      failed at (crates/formality-prove/src/prove.rs:89:45) because
                                        judgment `prove_wc_list { goals: {Foo(<() as Project>::Assoc, LocalType)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                          the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                                            judgment `prove_wc { goal: Foo(<() as Project>::Assoc, LocalType), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                              the rule "coherence / remote impl" failed at step #1 (crates/formality-prove/src/prove/prove_wc.rs:94:14) because
                                                judgment `may_be_remote { assumptions: {}, goal: Foo(<() as Project>::Assoc, LocalType), env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                                  the rule "may be added by upstream in a minor release" failed at step #0 (crates/formality-prove/src/prove/is_local.rs:68:14) because
                                                    judgment `negation_via_failure` failed at the following rule(s):
                                                      failed at (crates/formality-prove/src/prove/negation.rs:99:17) because
                                                        found an unconditionally true solution Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                  the rule "may be defined downstream" failed at step #0 (crates/formality-prove/src/prove/is_local.rs:60:14) because
                                                    judgment `may_be_downstream_trait_ref { goal: Foo(<() as Project>::Assoc, LocalType), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                                      the rule "may_be_downstream_trait_ref" failed at step #1 (crates/formality-prove/src/prove/is_local.rs:88:14) because
                                                        judgment `may_be_downstream_parameter { parameter: <() as Project>::Assoc, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                                          the rule "via normalize" failed at step #1 (crates/formality-prove/src/prove/is_local.rs:121:14) because
                                                            judgment `may_contain_downstream_type { parameter: (), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                                                              the rule "rigid type parameter" failed at step #0 (crates/formality-prove/src/prove/is_local.rs:152:14) because
                                                                expression evaluated to an empty collection: `parameters.iter()`
                                              the rule "trait implied bound" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:116:14) because
                                                expression evaluated to an empty collection: `decls.trait_invariants()`: at negation.rs:125
        "#]]
    }
}

#[test]
fn is_local_with_unconstrained_self_ty_blanket_impl() {
    // TODO: this test should pass imho
    crate::assert_err! {
        [
            crate core {
                trait Project {
                    type Assoc: [];
                }

                impl<ty T> Project for T {
                    type Assoc = ();
                }

                trait Foo<ty U> { }
            },
            crate foo {
                struct LocalType {}
                impl Foo<LocalType> for () {}

                trait Overlap<ty U> {}
                impl<ty T, ty U> Overlap<U> for T
                where
                    <T as Project>::Assoc: Foo<U> {}
                impl<ty T> Overlap<LocalType> for T {}
            }
        ]

        [        ]

        expect_test::expect![[r#"
            impls may overlap:
            impl <ty, ty> Overlap <^ty0_1> for ^ty0_0 where <^ty0_0 as Project>::Assoc : Foo <^ty0_1> { }
            impl <ty> Overlap <LocalType> for ^ty0_0 { }"#]]
    }
}
