#![allow(non_snake_case)]

#[test]
fn ok() {
    crate::assert_ok!(
        // Test functions, function's arguments, and function's returns
        //@check-pass
        [
            crate Foo {
                // fn simple_fn() {}
                fn simple_fn() -> () { trusted }

                // fn one_arg<T>(_: T) {}
                fn one_arg<ty T>(T) -> () { trusted }

                // fn one_ret<T>(_: T) {}
                fn one_ret<ty T>() -> T { trusted }

                // fn arg_ret<T, U>(_: T) -> U {}
                fn arg_ret<ty T, ty U>(T) -> U { trusted }

                // fn multi_arg_ret<T, Y, U, I>(_: T, _: Y) -> (U, I) {}
                fn multi_arg_ret<ty T, ty Y, ty U, ty I>(T, Y) -> (U, I) { trusted }
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(Foo): at lib.rs:73
                  └─ check_fn(Foo, fn simple_fn () -> () { trusted}, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(())}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(())
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (tuples) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: ()
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_fn(Foo, fn one_arg <ty> (^ty0_0) -> () { trusted}, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(!ty_0)}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(!ty_0)
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (universal variables) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: !ty_0
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(())}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(())
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (tuples) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: ()
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_fn(Foo, fn one_ret <ty> () -> ^ty0_0 { trusted}, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(!ty_0)}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(!ty_0)
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (universal variables) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: !ty_0
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_fn(Foo, fn arg_ret <ty, ty> (^ty0_0) -> ^ty0_1 { trusted}, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(!ty_0)}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(!ty_0)
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (universal variables) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: !ty_0
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(!ty_0)}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(!ty_0)
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (universal variables) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: !ty_0
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_fn(Foo, fn multi_arg_ret <ty, ty, ty, ty> (^ty0_0, ^ty0_1) -> (^ty0_2, ^ty0_3) { trusted}, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(!ty_0)}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(!ty_0)
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (universal variables) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: !ty_0
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(!ty_0)}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(!ty_0)
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (universal variables) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: !ty_0
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf((!ty_0, !ty_1))}
                            result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf((!ty_0, !ty_1))
                               result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (tuples) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: (!ty_0, !ty_1)
                                  result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ for_all: at combinators.rs:73
                                 └─ prove_wf: (universal variables) at prove_wf.rs:14
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: !ty_0
                                        result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ for_all: at combinators.rs:73
                                    └─ prove_wf: (universal variables) at prove_wf.rs:14
                                           _decls: decls(222, [], [], [], [], [], [], {}, {})
                                           env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }
                                           assumptions: {}
                                           goal: !ty_1
                                           result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_coherence(Foo): at coherence.rs:13
        "#]]
    )
}

#[test]
fn lifetime() {
    crate::assert_ok!(
        // Test lifetimes on function
        [
            crate Foo {
                // fn one_lt_arg<'a, T>(_: &'a T) -> () {}
                fn one_lt_arg<lt a, ty T>(&a T) -> ()
                where
                    T: a, // FIXME(#202): Implied bounds should not have to be explicit
                { trusted }
            }
        ]
        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(Foo): at lib.rs:73
                  └─ check_fn(Foo, fn one_lt_arg <lt, ty> (&^lt0_0 ^ty0_1) -> () where ^ty0_1 : ^lt0_0 { trusted}, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                            assumptions: {!ty_0 : !lt_1}
                            goals: {@ wf(!ty_0), @ wf(!lt_1)}
                            result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                               assumptions: {!ty_0 : !lt_1}
                               goal: @ wf(!ty_0)
                               result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (universal variables) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                                  assumptions: {!ty_0 : !lt_1}
                                  goal: !ty_0
                                  result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               constraints: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {!ty_0 : !lt_1}
                               goal: {@ wf(!lt_1)}
                               result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({!ty_0 : !lt_1}, {@ wf(!lt_1)}): at prove_after.rs:8
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                                  assumptions: {!ty_0 : !lt_1}
                                  goals: {@ wf(!lt_1)}
                                  result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                                     assumptions: {!ty_0 : !lt_1}
                                     goal: @ wf(!lt_1)
                                     result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wf: (universal variables) at prove_wf.rs:14
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                                        assumptions: {!ty_0 : !lt_1}
                                        goal: !lt_1
                                        result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     constraints: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {!ty_0 : !lt_1}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({!ty_0 : !lt_1}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                                        assumptions: {!ty_0 : !lt_1}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                            assumptions: {!ty_0 : !lt_1}
                            goals: {@ wf(&!lt_1 !ty_0)}
                            result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                               assumptions: {!ty_0 : !lt_1}
                               goal: @ wf(&!lt_1 !ty_0)
                               result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (references) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                                  assumptions: {!ty_0 : !lt_1}
                                  goal: &!lt_1 !ty_0
                                  result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (lt, ty) = (!lt_1, !ty_0): at prove_wf.rs:14
                              └─ prove_wc: (assumption - relation) at prove_wc.rs:22
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                                     assumptions: {!ty_0 : !lt_1}
                                     goal: !ty_0 : !lt_1
                                     result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ item = !ty_0 : !lt_1: at prove_wc.rs:22
                                 └─ prove_via: (relation-axiom) at prove_via.rs:9
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                                        assumptions: {!ty_0 : !lt_1}
                                        via: !ty_0 : !lt_1
                                        goal: !ty_0 : !lt_1
                                        result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (skel_c, parameters_c) = (outlives, [!ty_0, !lt_1]): at prove_via.rs:9
                                    └─ (skel_g, parameters_g) = (outlives, [!ty_0, !lt_1]): at prove_via.rs:9
                                    └─ IfThen { expression: "skel_c == skel_g", skel_c: outlives, skel_g: outlives }: at prove_via.rs:9
                                    └─ IfThen { expression: "parameters_c == parameters_g", parameters_c: [!ty_0, !lt_1], parameters_g: [!ty_0, !lt_1] }: at prove_via.rs:9
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               constraints: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {!ty_0 : !lt_1}
                               goal: {}
                               result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({!ty_0 : !lt_1}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                                  assumptions: {!ty_0 : !lt_1}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                            assumptions: {!ty_0 : !lt_1}
                            goals: {@ wf(())}
                            result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                               assumptions: {!ty_0 : !lt_1}
                               goal: @ wf(())
                               result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (tuples) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                                  assumptions: {!ty_0 : !lt_1}
                                  goal: ()
                                  result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               constraints: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {!ty_0 : !lt_1}
                               goal: {}
                               result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({!ty_0 : !lt_1}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }
                                  assumptions: {!ty_0 : !lt_1}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_coherence(Foo): at coherence.rs:13
        "#]]
    )
}
