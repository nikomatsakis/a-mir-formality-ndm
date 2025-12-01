#![allow(non_snake_case)]

#[test]
fn nonsense_rigid_const_bound() {
    crate::assert_err!(
        // This test is the short version of `generic_mismatch`, skipping
        // substituting and directly going to a wrong constant.
        [
            crate Foo {
                trait Foo where type_of_const true is u32 {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_trait(Foo)

            Caused by:
                0: prove_where_clauses_well_formed([type_of_const value(0, bool) is u32])
                1: judgment `prove { goal: {u32 = bool, @ wf(u32), @ wf(const value(0, bool))}, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [trait Foo <ty> where {@ ConstHasType(value(0, bool) , u32)}], [], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
                     failed at (src/file.rs:LL:CC) because
                       judgment `prove_wc_list { goals: {u32 = bool, @ wf(u32), @ wf(const value(0, bool))}, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                         the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                           judgment `prove_wc { goal: u32 = bool, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                             the rule "assumption - relation" failed at step #1 (src/file.rs:LL:CC) because
                               judgment had no applicable rules: `prove_via { goal: u32 = bool, via: @ ConstHasType(value(0, bool) , u32), assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness, pending: [] } }`
                             the rule "eq" failed at step #0 (src/file.rs:LL:CC) because
                               judgment `prove_eq { a: u32, b: bool, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                 the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                                   judgment `prove_normalize { p: u32, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                     the rule "normalize-via-assumption" failed at step #1 (src/file.rs:LL:CC) because
                                       judgment had no applicable rules: `prove_normalize_via { goal: u32, via: @ ConstHasType(value(0, bool) , u32), assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness, pending: [] } }`
                                 the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                   judgment `prove_eq { a: bool, b: u32, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                     the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                                       judgment `prove_normalize { p: bool, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                         the rule "normalize-via-assumption" failed at step #1 (src/file.rs:LL:CC) because
                                           judgment had no applicable rules: `prove_normalize_via { goal: bool, via: @ ConstHasType(value(0, bool) , u32), assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness, pending: [] } }`
                                     the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                       cyclic proof attempt: `prove_eq { a: u32, b: bool, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness, pending: [] } }`"#]]
    )
}

#[test]
fn ok() {
    crate::assert_ok!(
        //@check-pass
        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool {}
                trait Bar<const C> where type_of_const C is u32 {}

                impl<const C> Foo<const C> for u32 where type_of_const C is bool {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(Foo): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                            env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                            assumptions: {@ ConstHasType(!const_0 , bool)}
                            goals: {@ wf(bool), @ wf(const !const_0)}
                            result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                               env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                               assumptions: {@ ConstHasType(!const_0 , bool)}
                               goal: @ wf(bool)
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                  env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(!const_0 , bool)}
                                  goal: bool
                                  result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                               constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {@ ConstHasType(!const_0 , bool)}
                               goal: {@ wf(const !const_0)}
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , bool)}, {@ wf(const !const_0)}): at prove_after.rs:8
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                  env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(!const_0 , bool)}
                                  goals: {@ wf(const !const_0)}
                                  result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                     env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                     assumptions: {@ ConstHasType(!const_0 , bool)}
                                     goal: @ wf(const !const_0)
                                     result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wf: (universal variables) at prove_wf.rs:14
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                        env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                        assumptions: {@ ConstHasType(!const_0 , bool)}
                                        goal: const !const_0
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                     constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {@ ConstHasType(!const_0 , bool)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , bool)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                        env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                        assumptions: {@ ConstHasType(!const_0 , bool)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                            env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                            assumptions: {@ ConstHasType(!const_0 , u32)}
                            goals: {@ wf(u32), @ wf(const !const_0)}
                            result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                               env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                               assumptions: {@ ConstHasType(!const_0 , u32)}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                  env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(!const_0 , u32)}
                                  goal: u32
                                  result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                               constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {@ ConstHasType(!const_0 , u32)}
                               goal: {@ wf(const !const_0)}
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , u32)}, {@ wf(const !const_0)}): at prove_after.rs:8
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                  env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(!const_0 , u32)}
                                  goals: {@ wf(const !const_0)}
                                  result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                     env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                     assumptions: {@ ConstHasType(!const_0 , u32)}
                                     goal: @ wf(const !const_0)
                                     result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wf: (universal variables) at prove_wf.rs:14
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                        env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                        assumptions: {@ ConstHasType(!const_0 , u32)}
                                        goal: const !const_0
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                     constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {@ ConstHasType(!const_0 , u32)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , u32)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                        env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                        assumptions: {@ ConstHasType(!const_0 , u32)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                            env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                            assumptions: {@ ConstHasType(!const_0 , bool)}
                            goals: {@ wf(bool), @ wf(const !const_0)}
                            result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                               env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                               assumptions: {@ ConstHasType(!const_0 , bool)}
                               goal: @ wf(bool)
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                  env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(!const_0 , bool)}
                                  goal: bool
                                  result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                               constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {@ ConstHasType(!const_0 , bool)}
                               goal: {@ wf(const !const_0)}
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , bool)}, {@ wf(const !const_0)}): at prove_after.rs:8
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                  env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(!const_0 , bool)}
                                  goals: {@ wf(const !const_0)}
                                  result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                     env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                     assumptions: {@ ConstHasType(!const_0 , bool)}
                                     goal: @ wf(const !const_0)
                                     result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wf: (universal variables) at prove_wf.rs:14
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                        env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                        assumptions: {@ ConstHasType(!const_0 , bool)}
                                        goal: const !const_0
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                     constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {@ ConstHasType(!const_0 , bool)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , bool)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                        env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                        assumptions: {@ ConstHasType(!const_0 , bool)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                            env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                            assumptions: {@ ConstHasType(!const_0 , bool)}
                            goals: {Foo(u32, const !const_0)}
                            result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                               env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                               assumptions: {@ ConstHasType(!const_0 , bool)}
                               goal: Foo(u32, const !const_0)
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}: at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, [?const_1]): at prove_wc.rs:22
                           └─ i = Foo(u32, const ?const_1) where {@ ConstHasType(?const_1 , bool)}: at prove_wc.rs:22
                           └─ t = where {@ ConstHasType(?const_1 , bool)}: at prove_wc.rs:22
                           └─ co_assumptions = ({@ ConstHasType(!const_0 , bool)}, Foo(u32, const !const_0)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                  env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }
                                  assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                  goals: {u32 = u32, const !const_0 = const ?const_1}
                                  result: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?const_1 => const !const_0} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                     env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }
                                     assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                     goal: u32 = u32
                                     result: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                     constraints: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                     goal: {const !const_0 = const ?const_1}
                                     result: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?const_1 => const !const_0} }
                                 └─ (assumptions, goal) = ({Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}, {const !const_0 = const ?const_1}): at prove_after.rs:8
                                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                        env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }
                                        assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                        goals: {const !const_0 = const ?const_1}
                                        result: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?const_1 => const !const_0} }
                                    └─ prove_wc: (eq) at prove_wc.rs:22
                                           _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                           env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }
                                           assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                           goal: const !const_0 = const ?const_1
                                           result: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?const_1 => const !const_0} }
                                       └─ prove_eq: (symmetric) at prove_eq.rs:23
                                              _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                              env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }
                                              assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                              a: const !const_0
                                              b: const ?const_1
                                              result: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?const_1 => const !const_0} }
                                          └─ prove_eq: (existential) at prove_eq.rs:23
                                                 _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                                 env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }
                                                 assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                                 a: const ?const_1
                                                 b: const !const_0
                                                 result: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?const_1 => const !const_0} }
                                             └─ prove_existential_var_eq: (existential-universal) at prove_eq.rs:76
                                                    _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                                    env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }
                                                    assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                                    v: ?const_1
                                                    b: const !const_0
                                                    result: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?const_1 => const !const_0} }
                                                └─ IfThen { expression: "env.universe(p) < env.universe(v)" }: at prove_eq.rs:76
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                           constraints: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?const_1 => const !const_0} }
                                           assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                           goal: {}
                                           result: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?const_1 => const !const_0} }
                                       └─ (assumptions, goal) = ({Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}, {}): at prove_after.rs:8
                                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                              env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                              assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                              goals: {}
                                              result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                  constraints: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?const_1 => const !const_0} }
                                  assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                  goal: {@ ConstHasType(?const_1 , bool)}
                                  result: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?const_1 => const !const_0} }
                              └─ (assumptions, goal) = ({Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}, {@ ConstHasType(!const_0 , bool)}): at prove_after.rs:8
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                     env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                     assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                     goals: {@ ConstHasType(!const_0 , bool)}
                                     result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (assumption - predicate) at prove_wc.rs:22
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                        env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                        assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                        goal: @ ConstHasType(!const_0 , bool)
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ item = @ ConstHasType(!const_0 , bool): at prove_wc.rs:22
                                    └─ prove_via: (predicate-congruence-axiom) at prove_via.rs:9
                                           _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                           env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                           assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                           via: @ ConstHasType(!const_0 , bool)
                                           goal: @ ConstHasType(!const_0 , bool)
                                           result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (skel_c, parameters_c) = (const_has_type, [const !const_0, bool]): at prove_via.rs:9
                                       └─ (skel_g, parameters_g) = (const_has_type, [const !const_0, bool]): at prove_via.rs:9
                                       └─ IfThen { expression: "skel_c == skel_g", skel_c: const_has_type, skel_g: const_has_type }: at prove_via.rs:9
                                       └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                              env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                              assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                              goals: {bool = bool, const !const_0 = const !const_0}
                                              result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ prove_wc: (eq) at prove_wc.rs:22
                                                 _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                                 env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                                 assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                                 goal: bool = bool
                                                 result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ trivial, as a == b is true: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                          └─ prove_after: (prove_after) at prove_after.rs:8
                                                 _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                                 constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                 assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                                 goal: {const !const_0 = const !const_0}
                                                 result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ (assumptions, goal) = ({Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}, {const !const_0 = const !const_0}): at prove_after.rs:8
                                             └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                                    _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                                    env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                                    assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                                    goals: {const !const_0 = const !const_0}
                                                    result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                └─ prove_wc: (eq) at prove_wc.rs:22
                                                       _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                                       env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                                       assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                                       goal: const !const_0 = const !const_0
                                                       result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                   └─ trivial, as a == b is true: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                                └─ prove_after: (prove_after) at prove_after.rs:8
                                                       _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                                       constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                       assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                                       goal: {}
                                                       result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                   └─ (assumptions, goal) = ({Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}, {}): at prove_after.rs:8
                                                   └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                          _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                                          env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                                          assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                                          goals: {}
                                                          result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                        constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                           env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                           assumptions: {Foo(u32, const !const_0), @ ConstHasType(!const_0 , bool)}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                  constraints: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?const_1 => const !const_0} }
                                  assumptions: {@ ConstHasType(!const_0 , bool)}
                                  goal: {@ ConstHasType(?const_1 , bool)}
                                  result: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?const_1 => const !const_0} }
                              └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , bool)}, {@ ConstHasType(!const_0 , bool)}): at prove_after.rs:8
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                     env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                     assumptions: {@ ConstHasType(!const_0 , bool)}
                                     goals: {@ ConstHasType(!const_0 , bool)}
                                     result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (assumption - predicate) at prove_wc.rs:22
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                        env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                        assumptions: {@ ConstHasType(!const_0 , bool)}
                                        goal: @ ConstHasType(!const_0 , bool)
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ item = @ ConstHasType(!const_0 , bool): at prove_wc.rs:22
                                    └─ prove_via: (predicate-congruence-axiom) at prove_via.rs:9
                                           _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                           env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                           assumptions: {@ ConstHasType(!const_0 , bool)}
                                           via: @ ConstHasType(!const_0 , bool)
                                           goal: @ ConstHasType(!const_0 , bool)
                                           result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (skel_c, parameters_c) = (const_has_type, [const !const_0, bool]): at prove_via.rs:9
                                       └─ (skel_g, parameters_g) = (const_has_type, [const !const_0, bool]): at prove_via.rs:9
                                       └─ IfThen { expression: "skel_c == skel_g", skel_c: const_has_type, skel_g: const_has_type }: at prove_via.rs:9
                                       └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                              env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                              assumptions: {@ ConstHasType(!const_0 , bool)}
                                              goals: {bool = bool, const !const_0 = const !const_0}
                                              result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ prove_wc: (eq) at prove_wc.rs:22
                                                 _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                                 env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                                 assumptions: {@ ConstHasType(!const_0 , bool)}
                                                 goal: bool = bool
                                                 result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ trivial, as a == b is true: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                          └─ prove_after: (prove_after) at prove_after.rs:8
                                                 _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                                 constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                 assumptions: {@ ConstHasType(!const_0 , bool)}
                                                 goal: {const !const_0 = const !const_0}
                                                 result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , bool)}, {const !const_0 = const !const_0}): at prove_after.rs:8
                                             └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                                    _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                                    env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                                    assumptions: {@ ConstHasType(!const_0 , bool)}
                                                    goals: {const !const_0 = const !const_0}
                                                    result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                └─ prove_wc: (eq) at prove_wc.rs:22
                                                       _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                                       env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                                       assumptions: {@ ConstHasType(!const_0 , bool)}
                                                       goal: const !const_0 = const !const_0
                                                       result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                   └─ trivial, as a == b is true: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                                └─ prove_after: (prove_after) at prove_after.rs:8
                                                       _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                                       constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                       assumptions: {@ ConstHasType(!const_0 , bool)}
                                                       goal: {}
                                                       result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                   └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , bool)}, {}): at prove_after.rs:8
                                                   └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                          _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                                          env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                                          assumptions: {@ ConstHasType(!const_0 , bool)}
                                                          goals: {}
                                                          result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                        constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {@ ConstHasType(!const_0 , bool)}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , bool)}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                           env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                           assumptions: {@ ConstHasType(!const_0 , bool)}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                               constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {@ ConstHasType(!const_0 , bool)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , bool)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                  env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(!const_0 , bool)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Foo(u32, const ?const_0)}, assumptions: {@ ConstHasType(?const_0 , bool)}, env: Env { variables: [?const_0], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Foo(u32, const ?const_0)}, assumptions: {@ ConstHasType(?const_0 , bool)}, env: Env { variables: [?const_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Foo(u32, const ?const_0), assumptions: {@ ConstHasType(?const_0 , bool)}, env: Env { variables: [?const_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_coherence(Foo): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                            env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                            assumptions: {@ ConstHasType(!const_0 , bool)}
                            goals: {@ IsLocal(Foo(u32, const !const_0))}
                            result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                               env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                               assumptions: {@ ConstHasType(!const_0 , bool)}
                               goal: @ IsLocal(Foo(u32, const !const_0))
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                  env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(!const_0 , bool)}
                                  goal: Foo(u32, const !const_0)
                                  result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {}), &goal.trait_id: Foo }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                               constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {@ ConstHasType(!const_0 , bool)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , bool)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}, trait Bar <ty, const> where {@ ConstHasType(^const0_1 , u32)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , bool)}], [], [], [], [], {Bar, Foo}, {})
                                  env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(!const_0 , bool)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
        "#]]
    )
}

#[test]
fn mismatch() {
    crate::assert_err!(
        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool {}

                impl Foo<const 42_u32> for u32 {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_trait_impl(impl Foo <const value(42, u32)> for u32 { })

            Caused by:
                judgment `prove { goal: {Foo(u32, const value(42, u32))}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(42, u32))], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goals: {Foo(u32, const value(42, u32))}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: Foo(u32, const value(42, u32)), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                          the rule "trait implied bound" failed at step #0 (src/file.rs:LL:CC) because
                            expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn holds() {
    crate::assert_ok!(
        //@check-pass
        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool {}

                impl Foo<const true> for u32 {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(Foo): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                            env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                            assumptions: {@ ConstHasType(!const_0 , bool)}
                            goals: {@ wf(bool), @ wf(const !const_0)}
                            result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                               env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                               assumptions: {@ ConstHasType(!const_0 , bool)}
                               goal: @ wf(bool)
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(!const_0 , bool)}
                                  goal: bool
                                  result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                               constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {@ ConstHasType(!const_0 , bool)}
                               goal: {@ wf(const !const_0)}
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , bool)}, {@ wf(const !const_0)}): at prove_after.rs:8
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(!const_0 , bool)}
                                  goals: {@ wf(const !const_0)}
                                  result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                     assumptions: {@ ConstHasType(!const_0 , bool)}
                                     goal: @ wf(const !const_0)
                                     result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wf: (universal variables) at prove_wf.rs:14
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                        env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                        assumptions: {@ ConstHasType(!const_0 , bool)}
                                        goal: const !const_0
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                     constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {@ ConstHasType(!const_0 , bool)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , bool)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                        env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                        assumptions: {@ ConstHasType(!const_0 , bool)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {Foo(u32, const value(0, bool))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: Foo(u32, const value(0, bool))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl Foo(u32, const value(0, bool)): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [], bias: Soundness, pending: [] }, []): at prove_wc.rs:22
                           └─ i = Foo(u32, const value(0, bool)): at prove_wc.rs:22
                           └─ t = where {@ ConstHasType(value(0, bool) , bool)}: at prove_wc.rs:22
                           └─ co_assumptions = ({}, Foo(u32, const value(0, bool))): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {Foo(u32, const value(0, bool))}
                                  goals: {u32 = u32, const value(0, bool) = const value(0, bool)}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Foo(u32, const value(0, bool))}
                                     goal: u32 = u32
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Foo(u32, const value(0, bool))}
                                     goal: {const value(0, bool) = const value(0, bool)}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({Foo(u32, const value(0, bool))}, {const value(0, bool) = const value(0, bool)}): at prove_after.rs:8
                                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Foo(u32, const value(0, bool))}
                                        goals: {const value(0, bool) = const value(0, bool)}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_wc: (eq) at prove_wc.rs:22
                                           _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {Foo(u32, const value(0, bool))}
                                           goal: const value(0, bool) = const value(0, bool)
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                           constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                           assumptions: {Foo(u32, const value(0, bool))}
                                           goal: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (assumptions, goal) = ({Foo(u32, const value(0, bool))}, {}): at prove_after.rs:8
                                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {Foo(u32, const value(0, bool))}
                                              goals: {}
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Foo(u32, const value(0, bool))}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Foo(u32, const value(0, bool))}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Foo(u32, const value(0, bool))}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {@ ConstHasType(value(0, bool) , bool)}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {@ ConstHasType(value(0, bool) , bool)}): at prove_after.rs:8
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {@ ConstHasType(value(0, bool) , bool)}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (const has ty) at prove_wc.rs:22
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: @ ConstHasType(value(0, bool) , bool)
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {}
                                           goals: {bool = bool}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ prove_wc: (eq) at prove_wc.rs:22
                                              _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {}
                                              goal: bool = bool
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                       └─ prove_after: (prove_after) at prove_after.rs:8
                                              _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                              constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                              assumptions: {}
                                              goal: {}
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                          └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                 _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                                 env: Env { variables: [], bias: Soundness, pending: [] }
                                                 assumptions: {}
                                                 goals: {}
                                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                        constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Foo(u32, const value(0, bool))}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Foo(u32, const value(0, bool))}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Foo(u32, const value(0, bool)), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_coherence(Foo): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(Foo(u32, const value(0, bool)))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(Foo(u32, const value(0, bool)))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: Foo(u32, const value(0, bool))
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {}), &goal.trait_id: Foo }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(0, bool))], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
        "#]]
    )
}

#[test]
fn rigid_const_bound() {
    crate::assert_ok!(
        // This test is the short version of `holds`, skipping
        // substituting and directly going to a rigid constant.
        //@check-pass
        [
            crate Foo {
                trait Foo where type_of_const true is bool {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(Foo): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty> where {@ ConstHasType(value(0, bool) , bool)}], [], [], [], [], [], {Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {@ ConstHasType(value(0, bool) , bool)}
                            goals: {bool = bool, @ wf(bool), @ wf(const value(0, bool))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (eq) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty> where {@ ConstHasType(value(0, bool) , bool)}], [], [], [], [], [], {Foo}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {@ ConstHasType(value(0, bool) , bool)}
                               goal: bool = bool
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty> where {@ ConstHasType(value(0, bool) , bool)}], [], [], [], [], [], {Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {@ ConstHasType(value(0, bool) , bool)}
                               goal: {@ wf(bool), @ wf(const value(0, bool))}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({@ ConstHasType(value(0, bool) , bool)}, {@ wf(bool), @ wf(const value(0, bool))}): at prove_after.rs:8
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty> where {@ ConstHasType(value(0, bool) , bool)}], [], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(value(0, bool) , bool)}
                                  goals: {@ wf(bool), @ wf(const value(0, bool))}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty> where {@ ConstHasType(value(0, bool) , bool)}], [], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {@ ConstHasType(value(0, bool) , bool)}
                                     goal: @ wf(bool)
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                        _decls: decls(222, [trait Foo <ty> where {@ ConstHasType(value(0, bool) , bool)}], [], [], [], [], [], {Foo}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {@ ConstHasType(value(0, bool) , bool)}
                                        goal: bool
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty> where {@ ConstHasType(value(0, bool) , bool)}], [], [], [], [], [], {Foo}, {})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {@ ConstHasType(value(0, bool) , bool)}
                                     goal: {@ wf(const value(0, bool))}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({@ ConstHasType(value(0, bool) , bool)}, {@ wf(const value(0, bool))}): at prove_after.rs:8
                                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty> where {@ ConstHasType(value(0, bool) , bool)}], [], [], [], [], [], {Foo}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {@ ConstHasType(value(0, bool) , bool)}
                                        goals: {@ wf(const value(0, bool))}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                           _decls: decls(222, [trait Foo <ty> where {@ ConstHasType(value(0, bool) , bool)}], [], [], [], [], [], {Foo}, {})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {@ ConstHasType(value(0, bool) , bool)}
                                           goal: @ wf(const value(0, bool))
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ prove_wf: (rigid constants) at prove_wf.rs:14
                                              _decls: decls(222, [trait Foo <ty> where {@ ConstHasType(value(0, bool) , bool)}], [], [], [], [], [], {Foo}, {})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {@ ConstHasType(value(0, bool) , bool)}
                                              goal: const value(0, bool)
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                                 _decls: decls(222, [trait Foo <ty> where {@ ConstHasType(value(0, bool) , bool)}], [], [], [], [], [], {Foo}, {})
                                                 env: Env { variables: [], bias: Soundness, pending: [] }
                                                 assumptions: {@ ConstHasType(value(0, bool) , bool)}
                                                 goal: bool
                                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [trait Foo <ty> where {@ ConstHasType(value(0, bool) , bool)}], [], [], [], [], [], {Foo}, {})
                                           constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                           assumptions: {@ ConstHasType(value(0, bool) , bool)}
                                           goal: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (assumptions, goal) = ({@ ConstHasType(value(0, bool) , bool)}, {}): at prove_after.rs:8
                                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Foo <ty> where {@ ConstHasType(value(0, bool) , bool)}], [], [], [], [], [], {Foo}, {})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {@ ConstHasType(value(0, bool) , bool)}
                                              goals: {}
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_coherence(Foo): at coherence.rs:13
        "#]]
    )
}

#[test]
fn generic_mismatch() {
    crate::assert_err!(
        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool {}

                impl<const C> Foo<const C> for u32 where type_of_const C is u32 {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_trait_impl(impl <const> Foo <const ^const0_0> for u32 where type_of_const ^const0_0 is u32 { })

            Caused by:
                judgment `prove { goal: {Foo(u32, const !const_0)}, assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [] }, decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , u32)}], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goals: {Foo(u32, const !const_0)}, assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: Foo(u32, const !const_0), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                          the rule "positive impl" failed at step #7 (src/file.rs:LL:CC) because
                            judgment `prove_after { constraints: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?const_1 => const !const_0} }, goal: {@ ConstHasType(?const_1 , bool)}, assumptions: {@ ConstHasType(!const_0 , u32)} }` failed at the following rule(s):
                              the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                                judgment `prove { goal: {@ ConstHasType(!const_0 , bool)}, assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [] }, decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , u32)}], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
                                  failed at (src/file.rs:LL:CC) because
                                    judgment `prove_wc_list { goals: {@ ConstHasType(!const_0 , bool)}, assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                        judgment `prove_wc { goal: @ ConstHasType(!const_0 , bool), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                          the rule "const has ty" failed at step #0 (src/file.rs:LL:CC) because
                                            pattern `Some((_, const_ty))` did not match value `None`
                          the rule "trait implied bound" failed at step #0 (src/file.rs:LL:CC) because
                            expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn multiple_type_of_const() {
    crate::assert_ok!(
        // This test is weird, but it's also not something rustc ever generates.
        // Types on const generics only get exactly one `type_of_const` bound.
        // Either way, it is still sound, because there is no constant that possibly
        // satisfies those bounds (similar to how there is no type that satisfies `Drop` + `Copy`).
        //@check-pass
        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool, type_of_const C is u32 {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(Foo): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , u32), @ ConstHasType(^const0_1 , bool)}], [], [], [], [], [], {Foo}, {})
                            env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                            assumptions: {@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}
                            goals: {@ wf(u32), @ wf(bool), @ wf(const !const_0)}
                            result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , u32), @ ConstHasType(^const0_1 , bool)}], [], [], [], [], [], {Foo}, {})
                               env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                               assumptions: {@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , u32), @ ConstHasType(^const0_1 , bool)}], [], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}
                                  goal: u32
                                  result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , u32), @ ConstHasType(^const0_1 , bool)}], [], [], [], [], [], {Foo}, {})
                               constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}
                               goal: {@ wf(bool), @ wf(const !const_0)}
                               result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}, {@ wf(bool), @ wf(const !const_0)}): at prove_after.rs:8
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , u32), @ ConstHasType(^const0_1 , bool)}], [], [], [], [], [], {Foo}, {})
                                  env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                  assumptions: {@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}
                                  goals: {@ wf(bool), @ wf(const !const_0)}
                                  result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , u32), @ ConstHasType(^const0_1 , bool)}], [], [], [], [], [], {Foo}, {})
                                     env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                     assumptions: {@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}
                                     goal: @ wf(bool)
                                     result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , u32), @ ConstHasType(^const0_1 , bool)}], [], [], [], [], [], {Foo}, {})
                                        env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                        assumptions: {@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}
                                        goal: bool
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , u32), @ ConstHasType(^const0_1 , bool)}], [], [], [], [], [], {Foo}, {})
                                     constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}
                                     goal: {@ wf(const !const_0)}
                                     result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}, {@ wf(const !const_0)}): at prove_after.rs:8
                                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , u32), @ ConstHasType(^const0_1 , bool)}], [], [], [], [], [], {Foo}, {})
                                        env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                        assumptions: {@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}
                                        goals: {@ wf(const !const_0)}
                                        result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                           _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , u32), @ ConstHasType(^const0_1 , bool)}], [], [], [], [], [], {Foo}, {})
                                           env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                           assumptions: {@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}
                                           goal: @ wf(const !const_0)
                                           result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ prove_wf: (universal variables) at prove_wf.rs:14
                                              _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , u32), @ ConstHasType(^const0_1 , bool)}], [], [], [], [], [], {Foo}, {})
                                              env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                              assumptions: {@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}
                                              goal: const !const_0
                                              result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , u32), @ ConstHasType(^const0_1 , bool)}], [], [], [], [], [], {Foo}, {})
                                           constraints: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                           assumptions: {@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}
                                           goal: {}
                                           result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (assumptions, goal) = ({@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}, {}): at prove_after.rs:8
                                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , u32), @ ConstHasType(^const0_1 , bool)}], [], [], [], [], [], {Foo}, {})
                                              env: Env { variables: [!const_0], bias: Soundness, pending: [] }
                                              assumptions: {@ ConstHasType(!const_0 , u32), @ ConstHasType(!const_0 , bool)}
                                              goals: {}
                                              result: Constraints { env: Env { variables: [!const_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_coherence(Foo): at coherence.rs:13
        "#]]
    )
}
