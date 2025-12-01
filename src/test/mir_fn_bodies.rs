use formality_core::test;

/// Test assign statement with locals at rhs.
#[test]
fn test_assign_statement_local_only() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };
            }
        ]
        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(Foo): at lib.rs:73
                  └─ check_fn(Foo, fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
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
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
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
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
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
                     └─ check_body: at mini_rust_check.rs:39
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32
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
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32
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
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {u32 <: u32}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (subtype) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32 <: u32
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                        └─ check_block(bb0): at mini_rust_check.rs:135
                           └─ check_statement(local(v0) = load(local(v1)) ;): at mini_rust_check.rs:156
                              └─ check_value(load(local(v1))): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u32 <: u32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u32 <: u32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_terminator(return): at mini_rust_check.rs:211
                        └─ borrow_check: at nll.rs:136
                           └─ loans_in_basic_block_respected: (basic block) at nll.rs:152
                                  env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                  fn_assumptions: {}
                                  loans_live_on_entry: {}
                                  bb_id: bb0
                                  result: ()
                              └─ BasicBlock { id: _, statements, terminator } = bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }: at nll.rs:152
                              └─ loans_in_statements_respected: (cons) at nll.rs:284
                                     env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     statements: [local(v0) = load(local(v1)) ;]
                                     places_live_on_exit: {}
                                     result: {}
                                 └─ loans_in_statement_respected: (assign) at nll.rs:315
                                        env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statement: local(v0) = load(local(v1)) ;
                                        places_live_on_exit: {}
                                        result: {}
                                    └─ loans_in_value_expression_respected: (load) at nll.rs:449
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           value: load(local(v1))
                                           places_live_on_exit: {}
                                           result: {}
                                       └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                              env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              assumptions: {}
                                              loans_live_before_access: {}
                                              access: access(read, local(v1))
                                              places_live_after_access: {}
                                              result: ()
                                    └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           assumptions: {}
                                           loans_live_before_access: {}
                                           access: access(write, local(v0))
                                           places_live_after_access: {}
                                           result: ()
                                 └─ loans_in_statements_respected: (none) at nll.rs:284
                                        env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statements: []
                                        places_live_on_exit: {}
                                        result: {}
                              └─ loans_in_terminator_respected: (return) at nll.rs:199
                                     env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     terminator: return
                                     result: ()
                  └─ check_coherence(Foo): at coherence.rs:13
        "#]]
    )
}

// Test assign statement with constant at rhs.
#[test]
fn test_assign_constant() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo () -> u8 = minirust() -> v0 {
                    let v0: u8;
                    exists {
                        let v1: u16;
                        let v2: u32;
                        let v3: u64;
                        let v4: usize;
                        let v5: i8;
                        let v6: i16;
                        let v7: i32;
                        let v8: i64;
                        let v9: isize;
                        let v10: bool;

                        bb0: {
                            statements {
                                local(v0) = constant(5: u8);
                                local(v1) = constant(5: u16);
                                local(v2) = constant(5: u32);
                                local(v3) = constant(5: u64);
                                local(v4) = constant(5: usize);
                                local(v5) = constant(5: i8);
                                local(v6) = constant(5: i16);
                                local(v7) = constant(5: i32);
                                local(v8) = constant(5: i64);
                                local(v9) = constant(5: isize);
                                local(v10) = constant(false);
                            }
                            return;
                        }
                    }
                };
            }
        ]
        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(Foo): at lib.rs:73
                  └─ check_fn(Foo, fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ;, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
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
                            goals: {@ wf(u8)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u8)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u8
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
                     └─ check_body: at mini_rust_check.rs:39
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u8)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u8)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u8
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
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {u8 <: u8}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (subtype) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u8 <: u8
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                        └─ check_block(bb0): at mini_rust_check.rs:135
                           └─ check_statement(local(v0) = constant(5 : u8) ;): at mini_rust_check.rs:156
                              └─ check_value(constant(5 : u8)): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u8 <: u8}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u8 <: u8
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_statement(local(v1) = constant(5 : u16) ;): at mini_rust_check.rs:156
                              └─ check_value(constant(5 : u16)): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u16 <: u16}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u16 <: u16
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_statement(local(v2) = constant(5 : u32) ;): at mini_rust_check.rs:156
                              └─ check_value(constant(5 : u32)): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u32 <: u32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u32 <: u32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_statement(local(v3) = constant(5 : u64) ;): at mini_rust_check.rs:156
                              └─ check_value(constant(5 : u64)): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u64 <: u64}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u64 <: u64
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_statement(local(v4) = constant(5 : usize) ;): at mini_rust_check.rs:156
                              └─ check_value(constant(5 : usize)): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {usize <: usize}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: usize <: usize
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_statement(local(v5) = constant(5 : i8) ;): at mini_rust_check.rs:156
                              └─ check_value(constant(5 : i8)): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {i8 <: i8}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: i8 <: i8
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_statement(local(v6) = constant(5 : i16) ;): at mini_rust_check.rs:156
                              └─ check_value(constant(5 : i16)): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {i16 <: i16}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: i16 <: i16
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_statement(local(v7) = constant(5 : i32) ;): at mini_rust_check.rs:156
                              └─ check_value(constant(5 : i32)): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {i32 <: i32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: i32 <: i32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_statement(local(v8) = constant(5 : i64) ;): at mini_rust_check.rs:156
                              └─ check_value(constant(5 : i64)): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {i64 <: i64}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: i64 <: i64
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_statement(local(v9) = constant(5 : isize) ;): at mini_rust_check.rs:156
                              └─ check_value(constant(5 : isize)): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {isize <: isize}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: isize <: isize
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_statement(local(v10) = constant(false) ;): at mini_rust_check.rs:156
                              └─ check_value(constant(false)): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {bool <: bool}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: bool <: bool
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_terminator(return): at mini_rust_check.rs:211
                        └─ borrow_check: at nll.rs:136
                           └─ loans_in_basic_block_respected: (basic block) at nll.rs:152
                                  env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                  fn_assumptions: {}
                                  loans_live_on_entry: {}
                                  bb_id: bb0
                                  result: ()
                              └─ BasicBlock { id: _, statements, terminator } = bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }: at nll.rs:152
                              └─ loans_in_statements_respected: (cons) at nll.rs:284
                                     env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     statements: [local(v0) = constant(5 : u8) ;, local(v1) = constant(5 : u16) ;, local(v2) = constant(5 : u32) ;, local(v3) = constant(5 : u64) ;, local(v4) = constant(5 : usize) ;, local(v5) = constant(5 : i8) ;, local(v6) = constant(5 : i16) ;, local(v7) = constant(5 : i32) ;, local(v8) = constant(5 : i64) ;, local(v9) = constant(5 : isize) ;, local(v10) = constant(false) ;]
                                     places_live_on_exit: {}
                                     result: {}
                                 └─ loans_in_statement_respected: (assign) at nll.rs:315
                                        env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statement: local(v0) = constant(5 : u8) ;
                                        places_live_on_exit: {}
                                        result: {}
                                    └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                           env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           value: constant(5 : u8)
                                           places_live_on_exit: {}
                                           result: {}
                                    └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                           env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           assumptions: {}
                                           loans_live_before_access: {}
                                           access: access(write, local(v0))
                                           places_live_after_access: {}
                                           result: ()
                                 └─ loans_in_statements_respected: (cons) at nll.rs:284
                                        env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statements: [local(v1) = constant(5 : u16) ;, local(v2) = constant(5 : u32) ;, local(v3) = constant(5 : u64) ;, local(v4) = constant(5 : usize) ;, local(v5) = constant(5 : i8) ;, local(v6) = constant(5 : i16) ;, local(v7) = constant(5 : i32) ;, local(v8) = constant(5 : i64) ;, local(v9) = constant(5 : isize) ;, local(v10) = constant(false) ;]
                                        places_live_on_exit: {}
                                        result: {}
                                    └─ loans_in_statement_respected: (assign) at nll.rs:315
                                           env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           statement: local(v1) = constant(5 : u16) ;
                                           places_live_on_exit: {}
                                           result: {}
                                       └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                              env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              value: constant(5 : u16)
                                              places_live_on_exit: {}
                                              result: {}
                                       └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                              env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              assumptions: {}
                                              loans_live_before_access: {}
                                              access: access(write, local(v1))
                                              places_live_after_access: {}
                                              result: ()
                                    └─ loans_in_statements_respected: (cons) at nll.rs:284
                                           env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           statements: [local(v2) = constant(5 : u32) ;, local(v3) = constant(5 : u64) ;, local(v4) = constant(5 : usize) ;, local(v5) = constant(5 : i8) ;, local(v6) = constant(5 : i16) ;, local(v7) = constant(5 : i32) ;, local(v8) = constant(5 : i64) ;, local(v9) = constant(5 : isize) ;, local(v10) = constant(false) ;]
                                           places_live_on_exit: {}
                                           result: {}
                                       └─ loans_in_statement_respected: (assign) at nll.rs:315
                                              env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              statement: local(v2) = constant(5 : u32) ;
                                              places_live_on_exit: {}
                                              result: {}
                                          └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                                 env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                 fn_assumptions: {}
                                                 loans_live_on_entry: {}
                                                 value: constant(5 : u32)
                                                 places_live_on_exit: {}
                                                 result: {}
                                          └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                                 env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                 assumptions: {}
                                                 loans_live_before_access: {}
                                                 access: access(write, local(v2))
                                                 places_live_after_access: {}
                                                 result: ()
                                       └─ loans_in_statements_respected: (cons) at nll.rs:284
                                              env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              statements: [local(v3) = constant(5 : u64) ;, local(v4) = constant(5 : usize) ;, local(v5) = constant(5 : i8) ;, local(v6) = constant(5 : i16) ;, local(v7) = constant(5 : i32) ;, local(v8) = constant(5 : i64) ;, local(v9) = constant(5 : isize) ;, local(v10) = constant(false) ;]
                                              places_live_on_exit: {}
                                              result: {}
                                          └─ loans_in_statement_respected: (assign) at nll.rs:315
                                                 env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                 fn_assumptions: {}
                                                 loans_live_on_entry: {}
                                                 statement: local(v3) = constant(5 : u64) ;
                                                 places_live_on_exit: {}
                                                 result: {}
                                             └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                                    env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                    fn_assumptions: {}
                                                    loans_live_on_entry: {}
                                                    value: constant(5 : u64)
                                                    places_live_on_exit: {}
                                                    result: {}
                                             └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                                    env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                    assumptions: {}
                                                    loans_live_before_access: {}
                                                    access: access(write, local(v3))
                                                    places_live_after_access: {}
                                                    result: ()
                                          └─ loans_in_statements_respected: (cons) at nll.rs:284
                                                 env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                 fn_assumptions: {}
                                                 loans_live_on_entry: {}
                                                 statements: [local(v4) = constant(5 : usize) ;, local(v5) = constant(5 : i8) ;, local(v6) = constant(5 : i16) ;, local(v7) = constant(5 : i32) ;, local(v8) = constant(5 : i64) ;, local(v9) = constant(5 : isize) ;, local(v10) = constant(false) ;]
                                                 places_live_on_exit: {}
                                                 result: {}
                                             └─ loans_in_statement_respected: (assign) at nll.rs:315
                                                    env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                    fn_assumptions: {}
                                                    loans_live_on_entry: {}
                                                    statement: local(v4) = constant(5 : usize) ;
                                                    places_live_on_exit: {}
                                                    result: {}
                                                └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                                       env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                       fn_assumptions: {}
                                                       loans_live_on_entry: {}
                                                       value: constant(5 : usize)
                                                       places_live_on_exit: {}
                                                       result: {}
                                                └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                                       env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                       assumptions: {}
                                                       loans_live_before_access: {}
                                                       access: access(write, local(v4))
                                                       places_live_after_access: {}
                                                       result: ()
                                             └─ loans_in_statements_respected: (cons) at nll.rs:284
                                                    env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                    fn_assumptions: {}
                                                    loans_live_on_entry: {}
                                                    statements: [local(v5) = constant(5 : i8) ;, local(v6) = constant(5 : i16) ;, local(v7) = constant(5 : i32) ;, local(v8) = constant(5 : i64) ;, local(v9) = constant(5 : isize) ;, local(v10) = constant(false) ;]
                                                    places_live_on_exit: {}
                                                    result: {}
                                                └─ loans_in_statement_respected: (assign) at nll.rs:315
                                                       env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                       fn_assumptions: {}
                                                       loans_live_on_entry: {}
                                                       statement: local(v5) = constant(5 : i8) ;
                                                       places_live_on_exit: {}
                                                       result: {}
                                                   └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                                          env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                          fn_assumptions: {}
                                                          loans_live_on_entry: {}
                                                          value: constant(5 : i8)
                                                          places_live_on_exit: {}
                                                          result: {}
                                                   └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                                          env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                          assumptions: {}
                                                          loans_live_before_access: {}
                                                          access: access(write, local(v5))
                                                          places_live_after_access: {}
                                                          result: ()
                                                └─ loans_in_statements_respected: (cons) at nll.rs:284
                                                       env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                       fn_assumptions: {}
                                                       loans_live_on_entry: {}
                                                       statements: [local(v6) = constant(5 : i16) ;, local(v7) = constant(5 : i32) ;, local(v8) = constant(5 : i64) ;, local(v9) = constant(5 : isize) ;, local(v10) = constant(false) ;]
                                                       places_live_on_exit: {}
                                                       result: {}
                                                   └─ loans_in_statement_respected: (assign) at nll.rs:315
                                                          env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                          fn_assumptions: {}
                                                          loans_live_on_entry: {}
                                                          statement: local(v6) = constant(5 : i16) ;
                                                          places_live_on_exit: {}
                                                          result: {}
                                                      └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                                             env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                             fn_assumptions: {}
                                                             loans_live_on_entry: {}
                                                             value: constant(5 : i16)
                                                             places_live_on_exit: {}
                                                             result: {}
                                                      └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                                             env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                             assumptions: {}
                                                             loans_live_before_access: {}
                                                             access: access(write, local(v6))
                                                             places_live_after_access: {}
                                                             result: ()
                                                   └─ loans_in_statements_respected: (cons) at nll.rs:284
                                                          env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                          fn_assumptions: {}
                                                          loans_live_on_entry: {}
                                                          statements: [local(v7) = constant(5 : i32) ;, local(v8) = constant(5 : i64) ;, local(v9) = constant(5 : isize) ;, local(v10) = constant(false) ;]
                                                          places_live_on_exit: {}
                                                          result: {}
                                                      └─ loans_in_statement_respected: (assign) at nll.rs:315
                                                             env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                             fn_assumptions: {}
                                                             loans_live_on_entry: {}
                                                             statement: local(v7) = constant(5 : i32) ;
                                                             places_live_on_exit: {}
                                                             result: {}
                                                         └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                                                env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                                fn_assumptions: {}
                                                                loans_live_on_entry: {}
                                                                value: constant(5 : i32)
                                                                places_live_on_exit: {}
                                                                result: {}
                                                         └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                                                env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                                assumptions: {}
                                                                loans_live_before_access: {}
                                                                access: access(write, local(v7))
                                                                places_live_after_access: {}
                                                                result: ()
                                                      └─ loans_in_statements_respected: (cons) at nll.rs:284
                                                             env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                             fn_assumptions: {}
                                                             loans_live_on_entry: {}
                                                             statements: [local(v8) = constant(5 : i64) ;, local(v9) = constant(5 : isize) ;, local(v10) = constant(false) ;]
                                                             places_live_on_exit: {}
                                                             result: {}
                                                         └─ loans_in_statement_respected: (assign) at nll.rs:315
                                                                env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                                fn_assumptions: {}
                                                                loans_live_on_entry: {}
                                                                statement: local(v8) = constant(5 : i64) ;
                                                                places_live_on_exit: {}
                                                                result: {}
                                                            └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                                                   env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                                   fn_assumptions: {}
                                                                   loans_live_on_entry: {}
                                                                   value: constant(5 : i64)
                                                                   places_live_on_exit: {}
                                                                   result: {}
                                                            └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                                                   env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                                   assumptions: {}
                                                                   loans_live_before_access: {}
                                                                   access: access(write, local(v8))
                                                                   places_live_after_access: {}
                                                                   result: ()
                                                         └─ loans_in_statements_respected: (cons) at nll.rs:284
                                                                env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                                fn_assumptions: {}
                                                                loans_live_on_entry: {}
                                                                statements: [local(v9) = constant(5 : isize) ;, local(v10) = constant(false) ;]
                                                                places_live_on_exit: {}
                                                                result: {}
                                                            └─ loans_in_statement_respected: (assign) at nll.rs:315
                                                                   env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                                   fn_assumptions: {}
                                                                   loans_live_on_entry: {}
                                                                   statement: local(v9) = constant(5 : isize) ;
                                                                   places_live_on_exit: {}
                                                                   result: {}
                                                               └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                                                      env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                                      fn_assumptions: {}
                                                                      loans_live_on_entry: {}
                                                                      value: constant(5 : isize)
                                                                      places_live_on_exit: {}
                                                                      result: {}
                                                               └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                                                      env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                                      assumptions: {}
                                                                      loans_live_before_access: {}
                                                                      access: access(write, local(v9))
                                                                      places_live_after_access: {}
                                                                      result: ()
                                                            └─ loans_in_statements_respected: (cons) at nll.rs:284
                                                                   env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                                   fn_assumptions: {}
                                                                   loans_live_on_entry: {}
                                                                   statements: [local(v10) = constant(false) ;]
                                                                   places_live_on_exit: {}
                                                                   result: {}
                                                               └─ loans_in_statement_respected: (assign) at nll.rs:315
                                                                      env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                                      fn_assumptions: {}
                                                                      loans_live_on_entry: {}
                                                                      statement: local(v10) = constant(false) ;
                                                                      places_live_on_exit: {}
                                                                      result: {}
                                                                  └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                                                         env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                                         fn_assumptions: {}
                                                                         loans_live_on_entry: {}
                                                                         value: constant(false)
                                                                         places_live_on_exit: {}
                                                                         result: {}
                                                                  └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                                                         env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                                         assumptions: {}
                                                                         loans_live_before_access: {}
                                                                         access: access(write, local(v10))
                                                                         places_live_after_access: {}
                                                                         result: ()
                                                               └─ loans_in_statements_respected: (none) at nll.rs:284
                                                                      env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                                      fn_assumptions: {}
                                                                      loans_live_on_entry: {}
                                                                      statements: []
                                                                      places_live_on_exit: {}
                                                                      result: {}
                              └─ loans_in_terminator_respected: (return) at nll.rs:199
                                     env: TypeckEnv { program: [crate Foo { fn foo () -> u8 = minirust() -> v0 { let v0 : u8 ; exists { let v1 : u16 ; let v2 : u32 ; let v3 : u64 ; let v4 : usize ; let v5 : i8 ; let v6 : i16 ; let v7 : i32 ; let v8 : i64 ; let v9 : isize ; let v10 : bool ; bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u8, local_variables: {v0: u8, v1: u16, v10: bool, v2: u32, v3: u64, v4: usize, v5: i8, v6: i16, v7: i32, v8: i64, v9: isize}, blocks: [bb0 : { statements{ local(v0) = constant(5 : u8) ; local(v1) = constant(5 : u16) ; local(v2) = constant(5 : u32) ; local(v3) = constant(5 : u64) ; local(v4) = constant(5 : usize) ; local(v5) = constant(5 : i8) ; local(v6) = constant(5 : i16) ; local(v7) = constant(5 : i32) ; local(v8) = constant(5 : i64) ; local(v9) = constant(5 : isize) ; local(v10) = constant(false) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     terminator: return
                                     result: ()
                  └─ check_coherence(Foo): at coherence.rs:13
        "#]]
    )
}

// Test valid program with Terminator::Switch.
#[test]
fn test_switch_statment() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo () -> u32 = minirust() -> v0 {
                    let v0: u32;
                    exists {
                        let v1: u32;

                        bb0: {
                            statements {
                                local(v1) = constant(0: u32);
                            }
                            switch(load(local(v1))) -> [(0: bb1), (1: bb2)] otherwise: bb3;
                        }

                        bb1: {
                            statements {
                                local(v0) = constant(1: u32);
                            }
                            return;
                        }

                        bb2: {
                            statements {
                                local(v0) = constant(2: u32);
                            }
                            return;
                        }

                        bb3: {
                            statements {
                            }
                            return;
                        }
                    }
                };
            }
        ]
        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(Foo): at lib.rs:73
                  └─ check_fn(Foo, fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ;, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
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
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
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
                     └─ check_body: at mini_rust_check.rs:39
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32
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
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {u32 <: u32}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (subtype) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32 <: u32
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                        └─ check_block(bb0): at mini_rust_check.rs:135
                           └─ check_statement(local(v1) = constant(0 : u32) ;): at mini_rust_check.rs:156
                              └─ check_value(constant(0 : u32)): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u32 <: u32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u32 <: u32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_terminator(switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3): at mini_rust_check.rs:211
                              └─ check_value(load(local(v1))): at mini_rust_check.rs:421
                              └─ ty_is_int: (rigid_ty is int) at mini_rust_check.rs:817
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     ty: u32
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ IfThen { expression: "id.is_int()", id: u32 }: at mini_rust_check.rs:817
                        └─ check_block(bb1): at mini_rust_check.rs:135
                           └─ check_statement(local(v0) = constant(1 : u32) ;): at mini_rust_check.rs:156
                              └─ check_value(constant(1 : u32)): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u32 <: u32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u32 <: u32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_terminator(return): at mini_rust_check.rs:211
                        └─ check_block(bb2): at mini_rust_check.rs:135
                           └─ check_statement(local(v0) = constant(2 : u32) ;): at mini_rust_check.rs:156
                              └─ check_value(constant(2 : u32)): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u32 <: u32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u32 <: u32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_terminator(return): at mini_rust_check.rs:211
                        └─ check_block(bb3): at mini_rust_check.rs:135
                           └─ check_terminator(return): at mini_rust_check.rs:211
                        └─ borrow_check: at nll.rs:136
                           └─ loans_in_basic_block_respected: (basic block) at nll.rs:152
                                  env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                  fn_assumptions: {}
                                  loans_live_on_entry: {}
                                  bb_id: bb0
                                  result: ()
                              └─ BasicBlock { id: _, statements, terminator } = bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }: at nll.rs:152
                              └─ loans_in_statements_respected: (cons) at nll.rs:284
                                     env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     statements: [local(v1) = constant(0 : u32) ;]
                                     places_live_on_exit: {local(v1)}
                                     result: {}
                                 └─ loans_in_statement_respected: (assign) at nll.rs:315
                                        env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statement: local(v1) = constant(0 : u32) ;
                                        places_live_on_exit: {local(v1)}
                                        result: {}
                                    └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                           env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           value: constant(0 : u32)
                                           places_live_on_exit: {}
                                           result: {}
                                    └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                           env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           assumptions: {}
                                           loans_live_before_access: {}
                                           access: access(write, local(v1))
                                           places_live_after_access: {local(v1)}
                                           result: ()
                                 └─ loans_in_statements_respected: (none) at nll.rs:284
                                        env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statements: []
                                        places_live_on_exit: {local(v1)}
                                        result: {}
                              └─ loans_in_terminator_respected: (switch) at nll.rs:199
                                     env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     terminator: switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3
                                     result: ()
                                 └─ successors = [bb1, bb2, bb3]: at nll.rs:199
                                 └─ places_live = {}: at nll.rs:199
                                 └─ loans_in_value_expression_respected: (load) at nll.rs:449
                                        env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        value: load(local(v1))
                                        places_live_on_exit: {}
                                        result: {}
                                    └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                           env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           assumptions: {}
                                           loans_live_before_access: {}
                                           access: access(read, local(v1))
                                           places_live_after_access: {}
                                           result: ()
                                 └─ loans_in_basic_blocks_respected: (cons) at nll.rs:178
                                        env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        bb_ids: [bb1, bb2, bb3]
                                        result: ()
                                    └─ loans_in_basic_block_respected: (basic block) at nll.rs:152
                                           env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           bb_id: bb1
                                           result: ()
                                       └─ BasicBlock { id: _, statements, terminator } = bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }: at nll.rs:152
                                       └─ loans_in_statements_respected: (cons) at nll.rs:284
                                              env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              statements: [local(v0) = constant(1 : u32) ;]
                                              places_live_on_exit: {}
                                              result: {}
                                          └─ loans_in_statement_respected: (assign) at nll.rs:315
                                                 env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                 fn_assumptions: {}
                                                 loans_live_on_entry: {}
                                                 statement: local(v0) = constant(1 : u32) ;
                                                 places_live_on_exit: {}
                                                 result: {}
                                             └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                                    env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                    fn_assumptions: {}
                                                    loans_live_on_entry: {}
                                                    value: constant(1 : u32)
                                                    places_live_on_exit: {}
                                                    result: {}
                                             └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                                    env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                    assumptions: {}
                                                    loans_live_before_access: {}
                                                    access: access(write, local(v0))
                                                    places_live_after_access: {}
                                                    result: ()
                                          └─ loans_in_statements_respected: (none) at nll.rs:284
                                                 env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                 fn_assumptions: {}
                                                 loans_live_on_entry: {}
                                                 statements: []
                                                 places_live_on_exit: {}
                                                 result: {}
                                       └─ loans_in_terminator_respected: (return) at nll.rs:199
                                              env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              terminator: return
                                              result: ()
                                    └─ loans_in_basic_blocks_respected: (cons) at nll.rs:178
                                           env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           bb_ids: [bb2, bb3]
                                           result: ()
                                       └─ loans_in_basic_block_respected: (basic block) at nll.rs:152
                                              env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              bb_id: bb2
                                              result: ()
                                          └─ BasicBlock { id: _, statements, terminator } = bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }: at nll.rs:152
                                          └─ loans_in_statements_respected: (cons) at nll.rs:284
                                                 env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                 fn_assumptions: {}
                                                 loans_live_on_entry: {}
                                                 statements: [local(v0) = constant(2 : u32) ;]
                                                 places_live_on_exit: {}
                                                 result: {}
                                             └─ loans_in_statement_respected: (assign) at nll.rs:315
                                                    env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                    fn_assumptions: {}
                                                    loans_live_on_entry: {}
                                                    statement: local(v0) = constant(2 : u32) ;
                                                    places_live_on_exit: {}
                                                    result: {}
                                                └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                                       env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                       fn_assumptions: {}
                                                       loans_live_on_entry: {}
                                                       value: constant(2 : u32)
                                                       places_live_on_exit: {}
                                                       result: {}
                                                └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                                       env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                       assumptions: {}
                                                       loans_live_before_access: {}
                                                       access: access(write, local(v0))
                                                       places_live_after_access: {}
                                                       result: ()
                                             └─ loans_in_statements_respected: (none) at nll.rs:284
                                                    env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                    fn_assumptions: {}
                                                    loans_live_on_entry: {}
                                                    statements: []
                                                    places_live_on_exit: {}
                                                    result: {}
                                          └─ loans_in_terminator_respected: (return) at nll.rs:199
                                                 env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                 fn_assumptions: {}
                                                 loans_live_on_entry: {}
                                                 terminator: return
                                                 result: ()
                                       └─ loans_in_basic_blocks_respected: (cons) at nll.rs:178
                                              env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              bb_ids: [bb3]
                                              result: ()
                                          └─ loans_in_basic_block_respected: (basic block) at nll.rs:152
                                                 env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                 fn_assumptions: {}
                                                 loans_live_on_entry: {}
                                                 bb_id: bb3
                                                 result: ()
                                             └─ BasicBlock { id: _, statements, terminator } = bb3 : { statements{ } return ; }: at nll.rs:152
                                             └─ loans_in_statements_respected: (none) at nll.rs:284
                                                    env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                    fn_assumptions: {}
                                                    loans_live_on_entry: {}
                                                    statements: []
                                                    places_live_on_exit: {}
                                                    result: {}
                                             └─ loans_in_terminator_respected: (return) at nll.rs:199
                                                    env: TypeckEnv { program: [crate Foo { fn foo () -> u32 = minirust() -> v0 { let v0 : u32 ; exists { let v1 : u32 ; bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; } bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; } bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; } bb3 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : u32) ; } switch(load(local(v1))) -> [(0 : bb1), (1 : bb2)] otherwise : bb3 ; }, bb1 : { statements{ local(v0) = constant(1 : u32) ; } return ; }, bb2 : { statements{ local(v0) = constant(2 : u32) ; } return ; }, bb3 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                    fn_assumptions: {}
                                                    loans_live_on_entry: {}
                                                    terminator: return
                                                    result: ()
                                          └─ trivial, as bb_ids.is_empty() is true: (): at nll.rs:188
                  └─ check_coherence(Foo): at coherence.rs:13
        "#]]
    )
}

/// Test valid goto terminator.
#[test]
fn test_goto_terminator() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {}
                            goto bb1;
                        }

                        bb1: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };
            }
        ]
        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(Foo): at lib.rs:73
                  └─ check_fn(Foo, fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ } goto bb1 ; } bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
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
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
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
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
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
                     └─ check_body: at mini_rust_check.rs:39
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32
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
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32
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
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {u32 <: u32}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (subtype) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32 <: u32
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                        └─ check_block(bb0): at mini_rust_check.rs:135
                           └─ check_terminator(goto bb1): at mini_rust_check.rs:211
                        └─ check_block(bb1): at mini_rust_check.rs:135
                           └─ check_statement(local(v0) = load(local(v1)) ;): at mini_rust_check.rs:156
                              └─ check_value(load(local(v1))): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u32 <: u32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u32 <: u32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_terminator(return): at mini_rust_check.rs:211
                        └─ borrow_check: at nll.rs:136
                           └─ loans_in_basic_block_respected: (basic block) at nll.rs:152
                                  env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ } goto bb1 ; } bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ } goto bb1 ; }, bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                  fn_assumptions: {}
                                  loans_live_on_entry: {}
                                  bb_id: bb0
                                  result: ()
                              └─ BasicBlock { id: _, statements, terminator } = bb0 : { statements{ } goto bb1 ; }: at nll.rs:152
                              └─ loans_in_statements_respected: (none) at nll.rs:284
                                     env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ } goto bb1 ; } bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ } goto bb1 ; }, bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     statements: []
                                     places_live_on_exit: {local(v1)}
                                     result: {}
                              └─ loans_in_terminator_respected: (goto) at nll.rs:199
                                     env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ } goto bb1 ; } bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ } goto bb1 ; }, bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     terminator: goto bb1
                                     result: ()
                                 └─ loans_in_basic_block_respected: (basic block) at nll.rs:152
                                        env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ } goto bb1 ; } bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ } goto bb1 ; }, bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        bb_id: bb1
                                        result: ()
                                    └─ BasicBlock { id: _, statements, terminator } = bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; }: at nll.rs:152
                                    └─ loans_in_statements_respected: (cons) at nll.rs:284
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ } goto bb1 ; } bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ } goto bb1 ; }, bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           statements: [local(v0) = load(local(v1)) ;]
                                           places_live_on_exit: {}
                                           result: {}
                                       └─ loans_in_statement_respected: (assign) at nll.rs:315
                                              env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ } goto bb1 ; } bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ } goto bb1 ; }, bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              statement: local(v0) = load(local(v1)) ;
                                              places_live_on_exit: {}
                                              result: {}
                                          └─ loans_in_value_expression_respected: (load) at nll.rs:449
                                                 env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ } goto bb1 ; } bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ } goto bb1 ; }, bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                 fn_assumptions: {}
                                                 loans_live_on_entry: {}
                                                 value: load(local(v1))
                                                 places_live_on_exit: {}
                                                 result: {}
                                             └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                                    env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ } goto bb1 ; } bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ } goto bb1 ; }, bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                    assumptions: {}
                                                    loans_live_before_access: {}
                                                    access: access(read, local(v1))
                                                    places_live_after_access: {}
                                                    result: ()
                                          └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                                 env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ } goto bb1 ; } bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ } goto bb1 ; }, bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                 assumptions: {}
                                                 loans_live_before_access: {}
                                                 access: access(write, local(v0))
                                                 places_live_after_access: {}
                                                 result: ()
                                       └─ loans_in_statements_respected: (none) at nll.rs:284
                                              env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ } goto bb1 ; } bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ } goto bb1 ; }, bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              statements: []
                                              places_live_on_exit: {}
                                              result: {}
                                    └─ loans_in_terminator_respected: (return) at nll.rs:199
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ } goto bb1 ; } bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ } goto bb1 ; }, bb1 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           terminator: return
                                           result: ()
                  └─ check_coherence(Foo): at coherence.rs:13
        "#]]
    )
}

/// Test valid call terminator.
/// This is equivalent to:
/// ```
///    fn foo(v1: u32) -> u32 {
///      let v0: u32;
///      v0 = v1;
///      return v0;
///    }
///
///    fn bar(v1: u32) -> u32 {
///       v0 = v1;
///       let v0 = foo(v0);
///       return v0;
///    }
/// ```
#[test]
fn test_call_terminator() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo(u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };

                fn bar(u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            call fn_id foo (Move(local(v0))) -> local(v0) goto bb1;
                        }

                        bb1: {
                            statements {}
                            return;
                        }
                    }
                };
            }
        ]
        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(Foo): at lib.rs:73
                  └─ check_fn(Foo, fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
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
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
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
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
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
                     └─ check_body: at mini_rust_check.rs:39
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32
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
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32
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
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {u32 <: u32}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (subtype) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32 <: u32
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                        └─ check_block(bb0): at mini_rust_check.rs:135
                           └─ check_statement(local(v0) = load(local(v1)) ;): at mini_rust_check.rs:156
                              └─ check_value(load(local(v1))): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u32 <: u32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u32 <: u32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_terminator(return): at mini_rust_check.rs:211
                        └─ borrow_check: at nll.rs:136
                           └─ loans_in_basic_block_respected: (basic block) at nll.rs:152
                                  env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                  fn_assumptions: {}
                                  loans_live_on_entry: {}
                                  bb_id: bb0
                                  result: ()
                              └─ BasicBlock { id: _, statements, terminator } = bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }: at nll.rs:152
                              └─ loans_in_statements_respected: (cons) at nll.rs:284
                                     env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     statements: [local(v0) = load(local(v1)) ;]
                                     places_live_on_exit: {}
                                     result: {}
                                 └─ loans_in_statement_respected: (assign) at nll.rs:315
                                        env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statement: local(v0) = load(local(v1)) ;
                                        places_live_on_exit: {}
                                        result: {}
                                    └─ loans_in_value_expression_respected: (load) at nll.rs:449
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           value: load(local(v1))
                                           places_live_on_exit: {}
                                           result: {}
                                       └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                              env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              assumptions: {}
                                              loans_live_before_access: {}
                                              access: access(read, local(v1))
                                              places_live_after_access: {}
                                              result: ()
                                    └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           assumptions: {}
                                           loans_live_before_access: {}
                                           access: access(write, local(v0))
                                           places_live_after_access: {}
                                           result: ()
                                 └─ loans_in_statements_respected: (none) at nll.rs:284
                                        env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statements: []
                                        places_live_on_exit: {}
                                        result: {}
                              └─ loans_in_terminator_respected: (return) at nll.rs:199
                                     env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     terminator: return
                                     result: ()
                  └─ check_fn(Foo, fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ;, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
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
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
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
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
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
                     └─ check_body: at mini_rust_check.rs:39
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32
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
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32
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
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {u32 <: u32}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (subtype) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32 <: u32
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                        └─ check_block(bb0): at mini_rust_check.rs:135
                           └─ check_statement(local(v0) = load(local(v1)) ;): at mini_rust_check.rs:156
                              └─ check_value(load(local(v1))): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u32 <: u32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u32 <: u32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_terminator(call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1)): at mini_rust_check.rs:211
                              └─ check_value(fn_id foo): at mini_rust_check.rs:421
                              └─ check_argument_expression(Move(local(v0))): at mini_rust_check.rs:543
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u32 <: u32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u32 <: u32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u32 <: u32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u32 <: u32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                        └─ check_block(bb1): at mini_rust_check.rs:135
                           └─ check_terminator(return): at mini_rust_check.rs:211
                        └─ borrow_check: at nll.rs:136
                           └─ loans_in_basic_block_respected: (basic block) at nll.rs:152
                                  env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                  fn_assumptions: {}
                                  loans_live_on_entry: {}
                                  bb_id: bb0
                                  result: ()
                              └─ BasicBlock { id: _, statements, terminator } = bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }: at nll.rs:152
                              └─ loans_in_statements_respected: (cons) at nll.rs:284
                                     env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     statements: [local(v0) = load(local(v1)) ;]
                                     places_live_on_exit: {local(v0)}
                                     result: {}
                                 └─ loans_in_statement_respected: (assign) at nll.rs:315
                                        env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statement: local(v0) = load(local(v1)) ;
                                        places_live_on_exit: {local(v0)}
                                        result: {}
                                    └─ loans_in_value_expression_respected: (load) at nll.rs:449
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           value: load(local(v1))
                                           places_live_on_exit: {}
                                           result: {}
                                       └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                              env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              assumptions: {}
                                              loans_live_before_access: {}
                                              access: access(read, local(v1))
                                              places_live_after_access: {}
                                              result: ()
                                    └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           assumptions: {}
                                           loans_live_before_access: {}
                                           access: access(write, local(v0))
                                           places_live_after_access: {local(v0)}
                                           result: ()
                                 └─ loans_in_statements_respected: (none) at nll.rs:284
                                        env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statements: []
                                        places_live_on_exit: {local(v0)}
                                        result: {}
                              └─ loans_in_terminator_respected: (call) at nll.rs:199
                                     env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     terminator: call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1)
                                     result: ()
                                 └─ places_live = {}: at nll.rs:199
                                 └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                        env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        value: fn_id foo
                                        places_live_on_exit: {local(v0)}
                                        result: {}
                                 └─ loans_in_argument_expressions_respected: (cons) at nll.rs:367
                                        env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        values: [Move(local(v0))]
                                        places_live_on_exit: {}
                                        result: {}
                                    └─ loans_in_argument_expression_respected: (in-place) at nll.rs:392
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           value: Move(local(v0))
                                           places_live_on_exit: {}
                                           result: {}
                                       └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                              env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              assumptions: {}
                                              loans_live_before_access: {}
                                              access: access(move, local(v0))
                                              places_live_after_access: {}
                                              result: ()
                                    └─ loans_in_argument_expressions_respected: (nil) at nll.rs:367
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           values: []
                                           places_live_on_exit: {}
                                           result: {}
                                 └─ loans_in_next_block_respected: (Some(_)) at nll.rs:262
                                        env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        assumptions: {}
                                        loans_live_on_entry: {}
                                        next_block: Some(bb1)
                                        result: ()
                                    └─ loans_in_basic_block_respected: (basic block) at nll.rs:152
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           bb_id: bb1
                                           result: ()
                                       └─ BasicBlock { id: _, statements, terminator } = bb1 : { statements{ } return ; }: at nll.rs:152
                                       └─ loans_in_statements_respected: (none) at nll.rs:284
                                              env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              statements: []
                                              places_live_on_exit: {}
                                              result: {}
                                       └─ loans_in_terminator_respected: (return) at nll.rs:199
                                              env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ; fn bar (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; } bb1 : { statements{ } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; } call fn_id foo (Move(local(v0))) -> local(v0) goto Some(bb1) ; }, bb1 : { statements{ } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {foo: (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ local(v0) = load(local(v1)) ; } return ; } } } ;}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              terminator: return
                                              result: ()
                  └─ check_coherence(Foo): at coherence.rs:13
        "#]]
    )
}

/// Test valid place mention statement.
/// This is equivalent to:
/// ```
///    fn foo(v1: u32) -> u32 {
///      let v0: u32;
///      v0;
///      v0 = v1;
///      return v0;
///    }
///
/// ```
#[test]
fn test_place_mention_statement() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                place_mention(local(v0));
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };
            }
        ]
        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(Foo): at lib.rs:73
                  └─ check_fn(Foo, fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; } } } ;, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
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
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
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
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
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
                     └─ check_body: at mini_rust_check.rs:39
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32
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
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32
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
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {u32 <: u32}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (subtype) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32 <: u32
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                        └─ check_block(bb0): at mini_rust_check.rs:135
                           └─ check_statement(place_mention(local(v0)) ;): at mini_rust_check.rs:156
                           └─ check_statement(local(v0) = load(local(v1)) ;): at mini_rust_check.rs:156
                              └─ check_value(load(local(v1))): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u32 <: u32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u32 <: u32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_terminator(return): at mini_rust_check.rs:211
                        └─ borrow_check: at nll.rs:136
                           └─ loans_in_basic_block_respected: (basic block) at nll.rs:152
                                  env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                  fn_assumptions: {}
                                  loans_live_on_entry: {}
                                  bb_id: bb0
                                  result: ()
                              └─ BasicBlock { id: _, statements, terminator } = bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; }: at nll.rs:152
                              └─ loans_in_statements_respected: (cons) at nll.rs:284
                                     env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     statements: [place_mention(local(v0)) ;, local(v0) = load(local(v1)) ;]
                                     places_live_on_exit: {}
                                     result: {}
                                 └─ loans_in_statement_respected: (place-mention) at nll.rs:315
                                        env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statement: place_mention(local(v0)) ;
                                        places_live_on_exit: {local(v1)}
                                        result: {}
                                    └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           assumptions: {}
                                           loans_live_before_access: {}
                                           access: access(read, local(v0))
                                           places_live_after_access: {local(v1)}
                                           result: ()
                                 └─ loans_in_statements_respected: (cons) at nll.rs:284
                                        env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statements: [local(v0) = load(local(v1)) ;]
                                        places_live_on_exit: {}
                                        result: {}
                                    └─ loans_in_statement_respected: (assign) at nll.rs:315
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           statement: local(v0) = load(local(v1)) ;
                                           places_live_on_exit: {}
                                           result: {}
                                       └─ loans_in_value_expression_respected: (load) at nll.rs:449
                                              env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              value: load(local(v1))
                                              places_live_on_exit: {}
                                              result: {}
                                          └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                                 env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                                 assumptions: {}
                                                 loans_live_before_access: {}
                                                 access: access(read, local(v1))
                                                 places_live_after_access: {}
                                                 result: ()
                                       └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                              env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              assumptions: {}
                                              loans_live_before_access: {}
                                              access: access(write, local(v0))
                                              places_live_after_access: {}
                                              result: ()
                                    └─ loans_in_statements_respected: (none) at nll.rs:284
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           statements: []
                                           places_live_on_exit: {}
                                           result: {}
                              └─ loans_in_terminator_respected: (return) at nll.rs:199
                                     env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32}, blocks: [bb0 : { statements{ place_mention(local(v0)) ; local(v0) = load(local(v1)) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     terminator: return
                                     result: ()
                  └─ check_coherence(Foo): at coherence.rs:13
        "#]]
    )
}

/// Test valid StorageLive and StorageDead statements.
#[test]
fn test_storage_live_dead() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        let v2: u32;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                                StorageLive(v2);
                                StorageDead(v2);
                            }
                            return;
                        }
                    }
                };
            }
        ]
        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(Foo): at lib.rs:73
                  └─ check_fn(Foo, fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : u32 ; bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; } } } ;, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
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
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
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
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [], {}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
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
                     └─ check_body: at mini_rust_check.rs:39
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32
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
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32
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
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [], {}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {u32 <: u32}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (subtype) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [], {}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32 <: u32
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                        └─ check_block(bb0): at mini_rust_check.rs:135
                           └─ check_statement(local(v0) = load(local(v1)) ;): at mini_rust_check.rs:156
                              └─ check_value(load(local(v1))): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [], {}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u32 <: u32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [], {}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u32 <: u32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
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
                           └─ check_statement(StorageLive(v2) ;): at mini_rust_check.rs:156
                           └─ check_statement(StorageDead(v2) ;): at mini_rust_check.rs:156
                           └─ check_terminator(return): at mini_rust_check.rs:211
                        └─ borrow_check: at nll.rs:136
                           └─ loans_in_basic_block_respected: (basic block) at nll.rs:152
                                  env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : u32 ; bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                  fn_assumptions: {}
                                  loans_live_on_entry: {}
                                  bb_id: bb0
                                  result: ()
                              └─ BasicBlock { id: _, statements, terminator } = bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; }: at nll.rs:152
                              └─ loans_in_statements_respected: (cons) at nll.rs:284
                                     env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : u32 ; bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     statements: [local(v0) = load(local(v1)) ;, StorageLive(v2) ;, StorageDead(v2) ;]
                                     places_live_on_exit: {}
                                     result: {}
                                 └─ loans_in_statement_respected: (assign) at nll.rs:315
                                        env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : u32 ; bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statement: local(v0) = load(local(v1)) ;
                                        places_live_on_exit: {}
                                        result: {}
                                    └─ loans_in_value_expression_respected: (load) at nll.rs:449
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : u32 ; bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           value: load(local(v1))
                                           places_live_on_exit: {}
                                           result: {}
                                       └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                              env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : u32 ; bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              assumptions: {}
                                              loans_live_before_access: {}
                                              access: access(read, local(v1))
                                              places_live_after_access: {}
                                              result: ()
                                    └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : u32 ; bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           assumptions: {}
                                           loans_live_before_access: {}
                                           access: access(write, local(v0))
                                           places_live_after_access: {}
                                           result: ()
                                 └─ loans_in_statements_respected: (cons) at nll.rs:284
                                        env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : u32 ; bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statements: [StorageLive(v2) ;, StorageDead(v2) ;]
                                        places_live_on_exit: {}
                                        result: {}
                                    └─ loans_in_statement_respected: (storage-live) at nll.rs:315
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : u32 ; bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           statement: StorageLive(v2) ;
                                           places_live_on_exit: {}
                                           result: {}
                                    └─ loans_in_statements_respected: (cons) at nll.rs:284
                                           env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : u32 ; bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           statements: [StorageDead(v2) ;]
                                           places_live_on_exit: {}
                                           result: {}
                                       └─ loans_in_statement_respected: (storage-dead) at nll.rs:315
                                              env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : u32 ; bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              statement: StorageDead(v2) ;
                                              places_live_on_exit: {}
                                              result: {}
                                       └─ loans_in_statements_respected: (none) at nll.rs:284
                                              env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : u32 ; bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              statements: []
                                              places_live_on_exit: {}
                                              result: {}
                              └─ loans_in_terminator_respected: (return) at nll.rs:199
                                     env: TypeckEnv { program: [crate Foo { fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : u32 ; bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: u32}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; StorageLive(v2) ; StorageDead(v2) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [], {}, {}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     terminator: return
                                     result: ()
                  └─ check_coherence(Foo): at coherence.rs:13
        "#]]
    )
}

/// Test valid program that uses struct.
#[test]
fn test_struct() {
    crate::assert_ok!(
        [
            crate Foo {
                struct Dummy {
                    value: u32,
                    is_true: bool,
                }

                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        let v2: Dummy;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                                local(v2) = struct { constant(1: u32), constant(false)} as Dummy;
                                local(v2).0 = constant(2: u32);
                            }
                            return;
                        }
                    }
                };
            }
        ]
        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(Foo): at lib.rs:73
                  └─ check_adt(Dummy): at adts.rs:12
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(bool)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(bool)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: bool
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_fn(Foo, fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ;, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                  _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ check_body: at mini_rust_check.rs:39
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {@ wf(u32)}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: @ wf(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                     _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goal: u32
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc_list: (some) at prove_wc_list.rs:11
                               _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goals: {u32 <: u32}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wc: (subtype) at prove_wc.rs:22
                                  _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: u32 <: u32
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ check_block(bb0): at mini_rust_check.rs:135
                           └─ check_statement(local(v0) = load(local(v1)) ;): at mini_rust_check.rs:156
                              └─ check_value(load(local(v1))): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u32 <: u32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u32 <: u32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                        constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ check_statement(local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ;): at mini_rust_check.rs:156
                              └─ check_value(struct{ constant(1 : u32), constant(false) } as Dummy): at mini_rust_check.rs:421
                                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                        _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goals: {@ wf(Dummy)}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                                           _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {}
                                           goal: @ wf(Dummy)
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ prove_wf: (ADT) at prove_wf.rs:14
                                              _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {}
                                              goal: Dummy
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                                          └─ t = adt Dummy { struct { value : u32, is_true : bool } }: at prove_wf.rs:14
                                          └─ t = { struct { value : u32, is_true : bool } }: at prove_wf.rs:14
                                          └─ prove_after: (prove_after) at prove_after.rs:8
                                                 _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                                 constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                 assumptions: {}
                                                 goal: {}
                                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                             └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                    _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                                    env: Env { variables: [], bias: Soundness, pending: [] }
                                                    assumptions: {}
                                                    goals: {}
                                                    result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                           constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                           assumptions: {}
                                           goal: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                              _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {}
                                              goals: {}
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ check_value(constant(1 : u32)): at mini_rust_check.rs:421
                                 └─ check_value(constant(false)): at mini_rust_check.rs:421
                                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                        _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goals: {u32 <: u32, bool <: bool}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_wc: (subtype) at prove_wc.rs:22
                                           _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {}
                                           goal: u32 <: u32
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                           constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                           assumptions: {}
                                           goal: {bool <: bool}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (assumptions, goal) = ({}, {bool <: bool}): at prove_after.rs:8
                                       └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                              _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {}
                                              goals: {bool <: bool}
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ prove_wc: (subtype) at prove_wc.rs:22
                                                 _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                                 env: Env { variables: [], bias: Soundness, pending: [] }
                                                 assumptions: {}
                                                 goal: bool <: bool
                                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
                                          └─ prove_after: (prove_after) at prove_after.rs:8
                                                 _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                                 constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                 assumptions: {}
                                                 goal: {}
                                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                             └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                    _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                                    env: Env { variables: [], bias: Soundness, pending: [] }
                                                    assumptions: {}
                                                    goals: {}
                                                    result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {Dummy <: Dummy}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: Dummy <: Dummy
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                        constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ check_statement(local(v2) . 0 = constant(2 : u32) ;): at mini_rust_check.rs:156
                              └─ check_value(constant(2 : u32)): at mini_rust_check.rs:421
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {u32 <: u32}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (subtype) at prove_wc.rs:22
                                        _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: u32 <: u32
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_sub.rs:24
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                        constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ check_terminator(return): at mini_rust_check.rs:211
                        └─ borrow_check: at nll.rs:136
                           └─ loans_in_basic_block_respected: (basic block) at nll.rs:152
                                  env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                  fn_assumptions: {}
                                  loans_live_on_entry: {}
                                  bb_id: bb0
                                  result: ()
                              └─ BasicBlock { id: _, statements, terminator } = bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }: at nll.rs:152
                              └─ loans_in_statements_respected: (cons) at nll.rs:284
                                     env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     statements: [local(v0) = load(local(v1)) ;, local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ;, local(v2) . 0 = constant(2 : u32) ;]
                                     places_live_on_exit: {}
                                     result: {}
                                 └─ loans_in_statement_respected: (assign) at nll.rs:315
                                        env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statement: local(v0) = load(local(v1)) ;
                                        places_live_on_exit: {}
                                        result: {}
                                    └─ loans_in_value_expression_respected: (load) at nll.rs:449
                                           env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           value: load(local(v1))
                                           places_live_on_exit: {}
                                           result: {}
                                       └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                              env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                              assumptions: {}
                                              loans_live_before_access: {}
                                              access: access(read, local(v1))
                                              places_live_after_access: {}
                                              result: ()
                                    └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                           env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                           assumptions: {}
                                           loans_live_before_access: {}
                                           access: access(write, local(v0))
                                           places_live_after_access: {}
                                           result: ()
                                 └─ loans_in_statements_respected: (cons) at nll.rs:284
                                        env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                        fn_assumptions: {}
                                        loans_live_on_entry: {}
                                        statements: [local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ;, local(v2) . 0 = constant(2 : u32) ;]
                                        places_live_on_exit: {}
                                        result: {}
                                    └─ loans_in_statement_respected: (assign) at nll.rs:315
                                           env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           statement: local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ;
                                           places_live_on_exit: {}
                                           result: {}
                                       └─ loans_in_value_expression_respected: (struct) at nll.rs:449
                                              env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              value: struct{ constant(1 : u32), constant(false) } as Dummy
                                              places_live_on_exit: {}
                                              result: {}
                                          └─ loans_in_value_expressions_respected: (cons) at nll.rs:417
                                                 env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                                 fn_assumptions: {}
                                                 loans_live_on_entry: {}
                                                 values: [constant(1 : u32), constant(false)]
                                                 places_live_on_exit: {}
                                                 result: {}
                                             └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                                    env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                                    fn_assumptions: {}
                                                    loans_live_on_entry: {}
                                                    value: constant(1 : u32)
                                                    places_live_on_exit: {}
                                                    result: {}
                                             └─ loans_in_value_expressions_respected: (cons) at nll.rs:417
                                                    env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                                    fn_assumptions: {}
                                                    loans_live_on_entry: {}
                                                    values: [constant(false)]
                                                    places_live_on_exit: {}
                                                    result: {}
                                                └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                                       env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                                       fn_assumptions: {}
                                                       loans_live_on_entry: {}
                                                       value: constant(false)
                                                       places_live_on_exit: {}
                                                       result: {}
                                                └─ loans_in_value_expressions_respected: (nil) at nll.rs:417
                                                       env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                                       fn_assumptions: {}
                                                       loans_live_on_entry: {}
                                                       values: []
                                                       places_live_on_exit: {}
                                                       result: {}
                                       └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                              env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                              assumptions: {}
                                              loans_live_before_access: {}
                                              access: access(write, local(v2))
                                              places_live_after_access: {}
                                              result: ()
                                    └─ loans_in_statements_respected: (cons) at nll.rs:284
                                           env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                           fn_assumptions: {}
                                           loans_live_on_entry: {}
                                           statements: [local(v2) . 0 = constant(2 : u32) ;]
                                           places_live_on_exit: {}
                                           result: {}
                                       └─ loans_in_statement_respected: (assign) at nll.rs:315
                                              env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              statement: local(v2) . 0 = constant(2 : u32) ;
                                              places_live_on_exit: {}
                                              result: {}
                                          └─ loans_in_value_expression_respected: (constant-or-fn) at nll.rs:449
                                                 env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                                 fn_assumptions: {}
                                                 loans_live_on_entry: {}
                                                 value: constant(2 : u32)
                                                 places_live_on_exit: {}
                                                 result: {}
                                          └─ access_permitted_by_loans: (no loans) at nll.rs:489
                                                 env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                                 assumptions: {}
                                                 loans_live_before_access: {}
                                                 access: access(write, local(v2) . 0)
                                                 places_live_after_access: {}
                                                 result: ()
                                       └─ loans_in_statements_respected: (none) at nll.rs:284
                                              env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                              fn_assumptions: {}
                                              loans_live_on_entry: {}
                                              statements: []
                                              places_live_on_exit: {}
                                              result: {}
                              └─ loans_in_terminator_respected: (return) at nll.rs:199
                                     env: TypeckEnv { program: [crate Foo { struct Dummy { value : u32, is_true : bool } fn foo (u32) -> u32 = minirust(v1) -> v0 { let v0 : u32 ; let v1 : u32 ; exists { let v2 : Dummy ; bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; } } } ; }], env: Env { variables: [], bias: Soundness, pending: [] }, output_ty: u32, local_variables: {v0: u32, v1: u32, v2: Dummy}, blocks: [bb0 : { statements{ local(v0) = load(local(v1)) ; local(v2) = struct{ constant(1 : u32), constant(false) } as Dummy ; local(v2) . 0 = constant(2 : u32) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [u32], callee_input_tys: {}, crate_id: Foo, fn_args: [v1], pending_outlives: [], decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32, is_true : bool } }], {}, {Dummy}) }
                                     fn_assumptions: {}
                                     loans_live_on_entry: {}
                                     terminator: return
                                     result: ()
                  └─ check_coherence(Foo): at coherence.rs:13
        "#]]
    )
}

// Test what will happen if the next block does not exist for Terminator::Call.
#[test]
#[ignore] // FIXME: proof tree changes broke this - was passing at af3a087
fn test_no_next_bb_for_call_terminator() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo(u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };

                fn bar() -> u32 = minirust() -> v0 {
                    let v0: u32;
                    exists {
                        let v1: u32;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            call fn_id foo (Move(local(v1))) -> local(v0);
                        }
                    }
                };
            }
        ]
        expect_test::expect![["()"]]
    )
}

/// Test invalid assign statement with local at rhs.
/// This is equivalent to:
/// ```
///    fn foo(v1: u32) -> () {
///      let v0: ();
///      v0 = v1;
///      return v0;
///    }
///
/// ```
#[test]
fn test_invalid_assign_statement() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (u32) -> () = minirust(v1) -> v0 {
                    let v0: ();
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect![[r#"
            judgment `prove { goal: {u32 <: ()}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
              failed at (src/file.rs:LL:CC) because
                judgment `prove_wc_list { goals: {u32 <: ()}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: u32 <: (), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "subtype" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_sub { a: u32, b: (), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                          the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_normalize { p: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }`
                          the rule "normalize-r" failed at step #0 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_normalize { p: (), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }`"#]]
    )
}

// Test invalid assign statement with constant at rhs.
#[test]
fn test_invalid_assign_constant() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo () -> usize = minirust() -> v0 {
                    let v0: usize;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = constant(5: u32);
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect![[r#"
            judgment `prove { goal: {u32 <: usize}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
              failed at (src/file.rs:LL:CC) because
                judgment `prove_wc_list { goals: {u32 <: usize}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: u32 <: usize, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "subtype" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_sub { a: u32, b: usize, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                          the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_normalize { p: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }`
                          the rule "normalize-r" failed at step #0 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_normalize { p: usize, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }`"#]]
    )
}

// Test the behaviour of having invalid local name in place mention.
#[test]
fn test_invalid_local_in_place_mention() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                place_mention(local(v2));
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["PlaceExpression::Local: unknown local name `v2`"]
    )
}

// Test the behavior of having undeclared local_id in function argument.
#[test]
fn test_undeclared_local_in_function_arg() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    exists {
                        bb0: {
                            statements {
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["Function argument v1 is not declared, consider declaring them with `let v1: type;`"]
    )
}

// Test the behavior of having undeclared local_id in return place.
#[test]
fn test_undeclared_local_in_return_place() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo () -> u32 = minirust() -> v0 {
                    exists {
                        bb0: {
                            statements {
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["return variable v0 is not declared, consider declaring them with `let v0: type;`"]
    )
}

// Test the behaviour of having invalid bb_id in goto terminator.
#[test]
fn test_invalid_goto_bbid() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {}
                            goto bb1;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect![[r#"
            Basic block bb1 does not exist"#]]
    )
}

// Test the behaviour of calling a function that does not exist .
#[test]
fn test_call_invalid_fn() {
    crate::assert_err!(
        [
            crate Foo {
                fn bar() -> u32 = minirust() -> v0 {
                    let v0: u32;
                    exists {
                        let v1: u32;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            call fn_id foo (Move(local(v1))) -> local(v0);
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect![[r#"
            The function called is not declared in current crate"#]]
    )
}

#[test]
// Test what will happen if the type of arguments passed in is not subtype of what is expected.
fn test_pass_non_subtype_arg() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo(u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };

                fn bar(()) -> () = minirust(v1) -> v0 {
                    let v0: ();
                    let v1: ();
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            call fn_id foo (Move(local(v1))) -> local(v0) goto bb1;
                        }

                        bb1: {
                            statements {}
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect![[r#"
            judgment `prove { goal: {() <: u32}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
              failed at (src/file.rs:LL:CC) because
                judgment `prove_wc_list { goals: {() <: u32}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: () <: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "subtype" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_sub { a: (), b: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                          the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_normalize { p: (), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }`
                          the rule "normalize-r" failed at step #0 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_normalize { p: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }`"#]]
    )
}

// Test the behaviour of having invalid next bbid Terminator::Call.
#[test]
fn test_invalid_next_bbid_for_call_terminator() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo(u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };

                fn bar() -> u32 = minirust() -> v0 {
                    let v0: u32;
                    exists {
                        let v1: u32;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            call fn_id foo (Move(local(v1))) -> local(v0) goto bb1;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["Basic block bb1 does not exist"]
    )
}

/// Test what will happen if the declared and actual return type are not compatible.
/// This is equivalent to:
/// ```
/// fn foo(v1: ()) -> u32 {
///     let v0: ();
///     v0 = v1;
///     return v0;
/// }
/// ```
#[test]
fn test_incompatible_return_type() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (()) -> u32 = minirust(v1) -> v0 {
                    let v0: ();
                    let v1: ();
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };
            }
        ]

        []

        expect_test::expect![[r#"
            judgment `prove { goal: {() <: u32}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
              failed at (src/file.rs:LL:CC) because
                judgment `prove_wc_list { goals: {() <: u32}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: () <: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "subtype" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_sub { a: (), b: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                          the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_normalize { p: (), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }`
                          the rule "normalize-r" failed at step #0 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_normalize { p: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }`"#]]
    )
}

#[test]
fn test_function_arg_number_mismatch() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo () -> () = minirust(v1) -> v0 {
                    let v0: ();
                    let v1: ();
                    exists {
                    }
                };
            }
        ]

        []

        expect_test::expect!["Function argument number mismatch: expected 0 arguments, but found 1"]
    )
}

// Test the behaviour of having unitialised return local variable.
#[test]
fn test_uninitialised_return_type() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo () -> u32 = minirust() -> v0 {
                    let v0: u32;
                    exists {
                        bb0: {
                            statements {
                            }
                            return;
                        }
                    }
                };
            }
        ]

        []

        expect_test::expect![[r#"
            The return local variable has not been initialized."#]]
    )
}

/// Test switch terminator with invalid type in Terminator::Switch.
#[test]
fn test_invalid_value_in_switch_terminator() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo () -> bool = minirust() -> v0 {
                    let v0: bool;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = constant(false);
                            }
                            switch(load(local(v0))) -> [(0: bb1), (1: bb2)] otherwise: bb3;
                        }

                        bb1: {
                            statements {
                            }
                            return;
                        }

                        bb2: {
                            statements {
                            }
                            return;
                        }

                        bb3: {
                            statements {
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect![[r#"
            judgment `ty_is_int { assumptions: {}, ty: bool, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
              the rule "rigid_ty is int" failed at step #0 (src/file.rs:LL:CC) because
                condition evaluted to false: `id.is_int()`
                  id = bool"#]]
    )
}

/// Test the behaviour of having return place in StorageDead.
#[test]
fn test_ret_place_storage_dead() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                StorageDead(v1);
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["Statement::StorageDead: trying to mark function arguments or return local as dead"]
    )
}

/// Test the behaviour of having function argument in StorageDead.
#[test]
fn test_fn_arg_storage_dead() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                StorageDead(v0);
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["Statement::StorageDead: trying to mark function arguments or return local as dead"]
    )
}

/// Test the behaviour of using invalid index for the struct field.
#[test]
fn test_invalid_struct_field() {
    crate::assert_err!(
        [
            crate Foo {
                struct Dummy {
                    value: u32,
                }

                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        let v2: Dummy;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                                local(v2) = struct { constant(1: u32) } as Dummy;
                                local(v2).1 = constant(2: u32);
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["The field index used in PlaceExpression::Field is invalid."]
    )
}

/// Test the behaviour of using non-adt local for field projection.
#[test]
fn test_field_projection_root_non_adt() {
    crate::assert_err!(
        [
            crate Foo {
                struct Dummy {
                    value: u32,
                }

                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        let v2: Dummy;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                                local(v2) = struct { constant(1: u32) } as Dummy;
                                local(v1).1 = constant(2: u32);
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["The local used for field projection is not adt."]
    )
}

/// Test the behaviour of initialising the struct with wrong type.
#[test]
fn test_struct_wrong_type_in_initialisation() {
    crate::assert_err!(
        [
            crate Foo {
                struct Dummy {
                    value: u32,
                }

                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        let v2: Dummy;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                                local(v2) = struct { constant(false) } as Dummy;
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect![[r#"
            judgment `prove { goal: {bool <: u32}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32 } }], {}, {Dummy}) }` failed at the following rule(s):
              failed at (src/file.rs:LL:CC) because
                judgment `prove_wc_list { goals: {bool <: u32}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: bool <: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                      the rule "subtype" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_sub { a: bool, b: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                          the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_normalize { p: bool, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }`
                          the rule "normalize-r" failed at step #0 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_normalize { p: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [] } }`"#]]
    )
}

/// Test the behaviour of having non-adt as the type for ValueExpression::Struct.
#[test]
fn test_non_adt_ty_for_struct() {
    crate::assert_err!(
        [
            crate Foo {

                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        let v2: u32;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                                local(v2) = struct { constant(false) } as u32;
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["The type used in ValueExpression::Struct must be adt"]
    )
}

/// Basic pass test for lifetime.
///
/// The test is equivalent to:
/// ```rust,ignore
/// fn pick<'a>(v1: &'a u32) -> &'a u32 {
///     let v2 = v1;
///     v2
/// }
/// ```
#[test]
#[ignore] // FIXME: proof tree changes broke this - was passing at af3a087
fn test_ref_identity() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo<lt a>(&a u32) -> &a u32 = minirust(v1) -> v0 {
                    let v0: &a u32;
                    let v1: &a u32;

                    exists<lt r0> {
                        let v2: &r0 u32;

                        bb0: {
                            statements {
                                local(v2) = load(local(v1));
                                local(v0) = load(local(v2));
                            }
                            return;
                        }
                    }
                };
            }
        ]

        [
            // FIXME: we haven't written the code to deal with outlives relations yet
            "unproven outlives relationships found",
        ]

        expect_test::expect![[r#"
            unproven outlives relationships found: [
                PendingOutlives {
                    location: Location,
                    a: Lt(
                        Lt {
                            data: Variable(
                                !lt_1,
                            ),
                        },
                    ),
                    b: Lt(
                        Lt {
                            data: Variable(
                                ?lt_2,
                            ),
                        },
                    ),
                },
                PendingOutlives {
                    location: Location,
                    a: Lt(
                        Lt {
                            data: Variable(
                                ?lt_2,
                            ),
                        },
                    ),
                    b: Lt(
                        Lt {
                            data: Variable(
                                !lt_1,
                            ),
                        },
                    ),
                },
            ]"#]]
    )
}

/// Basic pass test for lifetime.
///
/// The test is equivalent to:
/// ```rust,ignore
/// fn mutate() -> i32 {
///     let mut i = 0;
///     let j = &i;
///     i = 1; // <-- ERROR
///     *j
/// }
/// ```
#[test]
fn test_borrow_check_basic() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> i32 = minirust() -> v0 {
                    let v0: i32;

                    exists<lt r0, lt r1> {
                        let v1: i32;
                        let v2: &r0 i32;

                        bb0: {
                            statements {
                                local(v1) = constant(0: i32);
                                local(v2) = &r1 local(v1);

                                // This should result in an error
                                local(v1) = constant(1: i32);

                                local(v0) = load(*(local(v2)));
                            }
                            return;
                        }
                    }
                };
            }
        ]

        [
        ]

        expect_test::expect![[r#"
            judgment `loans_in_basic_block_respected { loans_live_on_entry: {}, bb_id: bb0, fn_assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> i32 = minirust() -> v0 { let v0 : i32 ; exists <lt, lt> { let v1 : i32 ; let v2 : &^lt0_0 i32 ; bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ^lt0_1 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; } } } ; }], env: Env { variables: [?lt_1, ?lt_2], bias: Soundness, pending: [] }, output_ty: i32, local_variables: {v0: i32, v1: i32, v2: &?lt_1 i32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ?lt_2 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [PendingOutlives { location: Location, a: ?lt_2, b: ?lt_1 }], decls: decls(222, [], [], [], [], [], [], {}, {}) } }` failed at the following rule(s):
              the rule "basic block" failed at step #1 (src/file.rs:LL:CC) because
                judgment `loans_in_statements_respected { loans_live_on_entry: {}, statements: [local(v1) = constant(0 : i32) ;, local(v2) = & ?lt_2 local(v1) ;, local(v1) = constant(1 : i32) ;, local(v0) = load(*(local(v2))) ;], places_live_on_exit: {}, fn_assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> i32 = minirust() -> v0 { let v0 : i32 ; exists <lt, lt> { let v1 : i32 ; let v2 : &^lt0_0 i32 ; bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ^lt0_1 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; } } } ; }], env: Env { variables: [?lt_1, ?lt_2], bias: Soundness, pending: [] }, output_ty: i32, local_variables: {v0: i32, v1: i32, v2: &?lt_1 i32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ?lt_2 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [PendingOutlives { location: Location, a: ?lt_2, b: ?lt_1 }], decls: decls(222, [], [], [], [], [], [], {}, {}) } }` failed at the following rule(s):
                  the rule "cons" failed at step #1 (src/file.rs:LL:CC) because
                    judgment `loans_in_statements_respected { loans_live_on_entry: {}, statements: [local(v2) = & ?lt_2 local(v1) ;, local(v1) = constant(1 : i32) ;, local(v0) = load(*(local(v2))) ;], places_live_on_exit: {}, fn_assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> i32 = minirust() -> v0 { let v0 : i32 ; exists <lt, lt> { let v1 : i32 ; let v2 : &^lt0_0 i32 ; bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ^lt0_1 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; } } } ; }], env: Env { variables: [?lt_1, ?lt_2], bias: Soundness, pending: [] }, output_ty: i32, local_variables: {v0: i32, v1: i32, v2: &?lt_1 i32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ?lt_2 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [PendingOutlives { location: Location, a: ?lt_2, b: ?lt_1 }], decls: decls(222, [], [], [], [], [], [], {}, {}) } }` failed at the following rule(s):
                      the rule "cons" failed at step #1 (src/file.rs:LL:CC) because
                        judgment `loans_in_statements_respected { loans_live_on_entry: {loan(?lt_2, local(v1), shared)}, statements: [local(v1) = constant(1 : i32) ;, local(v0) = load(*(local(v2))) ;], places_live_on_exit: {}, fn_assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> i32 = minirust() -> v0 { let v0 : i32 ; exists <lt, lt> { let v1 : i32 ; let v2 : &^lt0_0 i32 ; bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ^lt0_1 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; } } } ; }], env: Env { variables: [?lt_1, ?lt_2], bias: Soundness, pending: [] }, output_ty: i32, local_variables: {v0: i32, v1: i32, v2: &?lt_1 i32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ?lt_2 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [PendingOutlives { location: Location, a: ?lt_2, b: ?lt_1 }], decls: decls(222, [], [], [], [], [], [], {}, {}) } }` failed at the following rule(s):
                          the rule "cons" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `loans_in_statement_respected { loans_live_on_entry: {loan(?lt_2, local(v1), shared)}, statement: local(v1) = constant(1 : i32) ;, places_live_on_exit: {*(local(v2))}, fn_assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> i32 = minirust() -> v0 { let v0 : i32 ; exists <lt, lt> { let v1 : i32 ; let v2 : &^lt0_0 i32 ; bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ^lt0_1 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; } } } ; }], env: Env { variables: [?lt_1, ?lt_2], bias: Soundness, pending: [] }, output_ty: i32, local_variables: {v0: i32, v1: i32, v2: &?lt_1 i32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ?lt_2 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [PendingOutlives { location: Location, a: ?lt_2, b: ?lt_1 }], decls: decls(222, [], [], [], [], [], [], {}, {}) } }` failed at the following rule(s):
                              the rule "assign" failed at step #1 (src/file.rs:LL:CC) because
                                judgment `access_permitted_by_loans { loans_live_before_access: {loan(?lt_2, local(v1), shared)}, access: access(write, local(v1)), places_live_after_access: {*(local(v2))}, assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> i32 = minirust() -> v0 { let v0 : i32 ; exists <lt, lt> { let v1 : i32 ; let v2 : &^lt0_0 i32 ; bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ^lt0_1 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; } } } ; }], env: Env { variables: [?lt_1, ?lt_2], bias: Soundness, pending: [] }, output_ty: i32, local_variables: {v0: i32, v1: i32, v2: &?lt_1 i32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ?lt_2 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [PendingOutlives { location: Location, a: ?lt_2, b: ?lt_1 }], decls: decls(222, [], [], [], [], [], [], {}, {}) } }` failed at the following rule(s):
                                  the rule "cons" failed at step #0 (src/file.rs:LL:CC) because
                                    judgment `access_permitted_by_loan { loan: loan(?lt_2, local(v1), shared), access: access(write, local(v1)), places_live_after_access: {*(local(v2))}, assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> i32 = minirust() -> v0 { let v0 : i32 ; exists <lt, lt> { let v1 : i32 ; let v2 : &^lt0_0 i32 ; bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ^lt0_1 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; } } } ; }], env: Env { variables: [?lt_1, ?lt_2], bias: Soundness, pending: [] }, output_ty: i32, local_variables: {v0: i32, v1: i32, v2: &?lt_1 i32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ?lt_2 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [PendingOutlives { location: Location, a: ?lt_2, b: ?lt_1 }], decls: decls(222, [], [], [], [], [], [], {}, {}) } }` failed at the following rule(s):
                                      the rule "borrow of disjoint places" failed at step #0 (src/file.rs:LL:CC) because
                                        condition evaluted to false: `place_disjoint_from_place(&loan.place, &access.place)`
                                          &loan.place = local(v1)
                                          &access.place = local(v1)
                                      the rule "borrows of disjoint places" failed at step #1 (src/file.rs:LL:CC) because
                                        judgment `loan_not_required_by_live_places { loan: loan(?lt_2, local(v1), shared), places_live_after_access: {*(local(v2))}, assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> i32 = minirust() -> v0 { let v0 : i32 ; exists <lt, lt> { let v1 : i32 ; let v2 : &^lt0_0 i32 ; bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ^lt0_1 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; } } } ; }], env: Env { variables: [?lt_1, ?lt_2], bias: Soundness, pending: [] }, output_ty: i32, local_variables: {v0: i32, v1: i32, v2: &?lt_1 i32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ?lt_2 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [PendingOutlives { location: Location, a: ?lt_2, b: ?lt_1 }], decls: decls(222, [], [], [], [], [], [], {}, {}) } }` failed at the following rule(s):
                                          the rule "borrow not live -- live place" failed at step #0 (src/file.rs:LL:CC) because
                                            judgment `loan_not_required_by_live_place { loan: loan(?lt_2, local(v1), shared), live_place: *(local(v2)), assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> i32 = minirust() -> v0 { let v0 : i32 ; exists <lt, lt> { let v1 : i32 ; let v2 : &^lt0_0 i32 ; bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ^lt0_1 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; } } } ; }], env: Env { variables: [?lt_1, ?lt_2], bias: Soundness, pending: [] }, output_ty: i32, local_variables: {v0: i32, v1: i32, v2: &?lt_1 i32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ?lt_2 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [PendingOutlives { location: Location, a: ?lt_2, b: ?lt_1 }], decls: decls(222, [], [], [], [], [], [], {}, {}) } }` failed at the following rule(s):
                                              the rule "loan is not required by type" failed at step #2 (src/file.rs:LL:CC) because
                                                judgment `loan_not_required_by_live_place_prefix { loan: loan(?lt_2, local(v1), shared), live_place: *(local(v2)), assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> i32 = minirust() -> v0 { let v0 : i32 ; exists <lt, lt> { let v1 : i32 ; let v2 : &^lt0_0 i32 ; bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ^lt0_1 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; } } } ; }], env: Env { variables: [?lt_1, ?lt_2], bias: Soundness, pending: [] }, output_ty: i32, local_variables: {v0: i32, v1: i32, v2: &?lt_1 i32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ?lt_2 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [PendingOutlives { location: Location, a: ?lt_2, b: ?lt_1 }], decls: decls(222, [], [], [], [], [], [], {}, {}) } }` failed at the following rule(s):
                                                  the rule "field" failed at step #0 (src/file.rs:LL:CC) because
                                                    judgment `loan_not_required_by_live_place { loan: loan(?lt_2, local(v1), shared), live_place: local(v2), assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> i32 = minirust() -> v0 { let v0 : i32 ; exists <lt, lt> { let v1 : i32 ; let v2 : &^lt0_0 i32 ; bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ^lt0_1 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; } } } ; }], env: Env { variables: [?lt_1, ?lt_2], bias: Soundness, pending: [] }, output_ty: i32, local_variables: {v0: i32, v1: i32, v2: &?lt_1 i32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ?lt_2 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [PendingOutlives { location: Location, a: ?lt_2, b: ?lt_1 }], decls: decls(222, [], [], [], [], [], [], {}, {}) } }` failed at the following rule(s):
                                                      the rule "loan is not required by type" failed at step #1 (src/file.rs:LL:CC) because
                                                        judgment `loan_not_required_by_parameter { loan: loan(?lt_2, local(v1), shared), live_parameter: &?lt_1 i32, assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> i32 = minirust() -> v0 { let v0 : i32 ; exists <lt, lt> { let v1 : i32 ; let v2 : &^lt0_0 i32 ; bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ^lt0_1 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; } } } ; }], env: Env { variables: [?lt_1, ?lt_2], bias: Soundness, pending: [] }, output_ty: i32, local_variables: {v0: i32, v1: i32, v2: &?lt_1 i32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ?lt_2 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [PendingOutlives { location: Location, a: ?lt_2, b: ?lt_1 }], decls: decls(222, [], [], [], [], [], [], {}, {}) } }` failed at the following rule(s):
                                                          the rule "rigid-ty" failed at step #0 (src/file.rs:LL:CC) because
                                                            judgment `loan_not_required_by_parameters { loan: loan(?lt_2, local(v1), shared), live_parameters: [?lt_1, i32], assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> i32 = minirust() -> v0 { let v0 : i32 ; exists <lt, lt> { let v1 : i32 ; let v2 : &^lt0_0 i32 ; bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ^lt0_1 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; } } } ; }], env: Env { variables: [?lt_1, ?lt_2], bias: Soundness, pending: [] }, output_ty: i32, local_variables: {v0: i32, v1: i32, v2: &?lt_1 i32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ?lt_2 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [PendingOutlives { location: Location, a: ?lt_2, b: ?lt_1 }], decls: decls(222, [], [], [], [], [], [], {}, {}) } }` failed at the following rule(s):
                                                              the rule "cons" failed at step #0 (src/file.rs:LL:CC) because
                                                                judgment `loan_not_required_by_parameter { loan: loan(?lt_2, local(v1), shared), live_parameter: ?lt_1, assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> i32 = minirust() -> v0 { let v0 : i32 ; exists <lt, lt> { let v1 : i32 ; let v2 : &^lt0_0 i32 ; bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ^lt0_1 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; } } } ; }], env: Env { variables: [?lt_1, ?lt_2], bias: Soundness, pending: [] }, output_ty: i32, local_variables: {v0: i32, v1: i32, v2: &?lt_1 i32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ?lt_2 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [PendingOutlives { location: Location, a: ?lt_2, b: ?lt_1 }], decls: decls(222, [], [], [], [], [], [], {}, {}) } }` failed at the following rule(s):
                                                                  the rule "universal-variable" failed at step #0 (src/file.rs:LL:CC) because
                                                                    judgment `loan_cannot_outlive { loan: loan(?lt_2, local(v1), shared), lifetime: ?lt_1, assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> i32 = minirust() -> v0 { let v0 : i32 ; exists <lt, lt> { let v1 : i32 ; let v2 : &^lt0_0 i32 ; bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ^lt0_1 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; } } } ; }], env: Env { variables: [?lt_1, ?lt_2], bias: Soundness, pending: [] }, output_ty: i32, local_variables: {v0: i32, v1: i32, v2: &?lt_1 i32}, blocks: [bb0 : { statements{ local(v1) = constant(0 : i32) ; local(v2) = & ?lt_2 local(v1) ; local(v1) = constant(1 : i32) ; local(v0) = load(*(local(v2))) ; } return ; }], ret_id: v0, ret_place_is_initialised: true, declared_input_tys: [], callee_input_tys: {}, crate_id: Foo, fn_args: [], pending_outlives: [PendingOutlives { location: Location, a: ?lt_2, b: ?lt_1 }], decls: decls(222, [], [], [], [], [], [], {}, {}) } }` failed at the following rule(s):
                                                                      the rule "loan_cannot_outlive" failed at step #1 (src/file.rs:LL:CC) because
                                                                        condition evaluted to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                                                                          outlived_by_loan = {?lt_1, ?lt_2}
                                                                          &lifetime.upcast() = ?lt_1"#]]
    )
}

/// FIXME(tiif): this should not pass, I think Relation::sub does not consider lifetime yet?
/// Basic fail test for lifetime.
///
/// The test is equivalent to:
/// ```rust,ignore
/// fn pick<'a, 'b>(v1: &'a u32) -> &'b u32 {
///     let v2 = v1;
///     v2
/// }
/// ```
#[formality_core::test]
#[ignore] // FIXME: proof tree changes broke this - was passing at af3a087
fn test_ref_not_subtype() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo<lt a, lt b>(&a u32) -> &b u32 = minirust(v1) -> v0 {
                    let v0: &b u32;
                    let v1: &a u32;

                    exists<lt r0> {
                        let v2: &r0 u32;

                        bb0: {
                            statements {
                                local(v2) = load(local(v1));
                                local(v0) = load(local(v2));
                            }
                            return;
                        }
                    }
                };
            }
        ]

        [
            // FIXME: we haven't written the code to deal with outlives relations yet
            "unproven outlives relationships found",
        ]

        expect_test::expect![[r#"
            unproven outlives relationships found: [
                PendingOutlives {
                    location: Location,
                    a: Lt(
                        Lt {
                            data: Variable(
                                !lt_1,
                            ),
                        },
                    ),
                    b: Lt(
                        Lt {
                            data: Variable(
                                ?lt_3,
                            ),
                        },
                    ),
                },
                PendingOutlives {
                    location: Location,
                    a: Lt(
                        Lt {
                            data: Variable(
                                ?lt_3,
                            ),
                        },
                    ),
                    b: Lt(
                        Lt {
                            data: Variable(
                                !lt_2,
                            ),
                        },
                    ),
                },
            ]"#]]
    )
}

/// Test ref and deref
/// FIXME(tiif): This is not implemented yet
#[test]
#[ignore]
fn test_ref_deref() {
    crate::assert_ok!(
        [
            crate Foo {

                fn foo () -> u32 = minirust() -> v0 {
                    let v0: u32;
                    exists<lt a> {
                        let v1: u32;
                        let v2: &a u32;
                        let v3: u32;

                        bb0: {
                            statements {
                                local(v1) = constant(3: u32);
                                local(v2) = &(local(v1));
                                local(v3) = load(*(load(local(v2))));
                            }
                            return;
                        }
                    }
                };
            }
        ]
        expect_test::expect![["()"]]
    )
}
