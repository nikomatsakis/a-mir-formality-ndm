#![allow(non_snake_case)]

mod coherence_orphan;
mod coherence_overlap;
mod consts;
mod decl_safety;
mod functions;
mod mir_fn_bodies;
mod well_formed_trait_ref;

#[test]
fn parser() {
    crate::assert_err!(
        [
            crate Foo {
                trait Baz where  cake  {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            expected `:`

            Caused by:
                0: {} }]
                1: failed to parse [crate Foo { trait Baz where cake {} }]"#]]
    )
}

#[test]
fn hello_world_fail() {
    crate::assert_err!(
        [
            crate Foo {
                trait Foo<ty T> where T: Bar<Self> {}

                trait Bar<ty T> where T: Baz {}

                trait Baz {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_trait(Foo)

            Caused by:
                0: prove_where_clauses_well_formed([!ty_2 : Bar <!ty_1>])
                1: judgment `prove { goal: {@ WellFormedTraitRef(Bar(!ty_0, !ty_1))}, assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [], [], [], [], [], {Bar, Baz, Foo}, {}) }` failed at the following rule(s):
                     failed at (src/file.rs:LL:CC) because
                       judgment `prove_wc_list { goals: {@ WellFormedTraitRef(Bar(!ty_0, !ty_1))}, assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                         the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                           judgment `prove_wc { goal: @ WellFormedTraitRef(Bar(!ty_0, !ty_1)), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                             the rule "trait well formed" failed at step #3 (src/file.rs:LL:CC) because
                               judgment `prove_after { constraints: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }, goal: {Baz(!ty_1)}, assumptions: {Bar(!ty_0, !ty_1)} }` failed at the following rule(s):
                                 the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                                   judgment `prove { goal: {Baz(!ty_1)}, assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [], [], [], [], [], {Bar, Baz, Foo}, {}) }` failed at the following rule(s):
                                     failed at (src/file.rs:LL:CC) because
                                       judgment `prove_wc_list { goals: {Baz(!ty_1)}, assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                         the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                           judgment `prove_wc { goal: Baz(!ty_1), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                             the rule "trait implied bound" failed at step #0 (src/file.rs:LL:CC) because
                                               expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn hello_world() {
    crate::assert_ok!(
        //@check-pass
        [
            crate Foo {
                trait Foo<ty T> where T: Bar<Self>, Self: Baz {}

                trait Bar<ty T> where T: Baz {}

                trait Baz {}

                impl Baz for u32 {}

                impl Bar<u32> for u32 {}
                impl<ty T> Bar<T> for () where T: Baz {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(Foo): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                            env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                            assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                            goals: {@ WellFormedTraitRef(Bar(!ty_0, !ty_1)), @ WellFormedTraitRef(Baz(!ty_1))}
                            result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                               assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                               goal: @ WellFormedTraitRef(Bar(!ty_0, !ty_1))
                               result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ for_all: at combinators.rs:73
                              └─ prove_wf: (universal variables) at prove_wf.rs:14
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                     goal: !ty_0
                                     result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ for_all: at combinators.rs:73
                                 └─ prove_wf: (universal variables) at prove_wf.rs:14
                                        _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                        env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                                        assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                        goal: !ty_1
                                        result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                           └─ t = trait Bar <ty, ty> where {Baz(^ty0_1)}: at prove_wc.rs:22
                           └─ t = where {Baz(!ty_1)}: at prove_wc.rs:22
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                  goal: {Baz(!ty_1)}
                                  result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Bar(!ty_0, !ty_1), Baz(!ty_1)}, {Baz(!ty_1)}): at prove_after.rs:8
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                     goals: {Baz(!ty_1)}
                                     result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (assumption - predicate) at prove_wc.rs:22
                                        _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                        env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                                        assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                        goal: Baz(!ty_1)
                                        result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ item = Baz(!ty_1): at prove_wc.rs:22
                                    └─ prove_via: (predicate-congruence-axiom) at prove_via.rs:9
                                           _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                           env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                                           assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                           via: Baz(!ty_1)
                                           goal: Baz(!ty_1)
                                           result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (skel_c, parameters_c) = (is_implemented(Baz), [!ty_1]): at prove_via.rs:9
                                       └─ (skel_g, parameters_g) = (is_implemented(Baz), [!ty_1]): at prove_via.rs:9
                                       └─ IfThen { expression: "skel_c == skel_g", skel_c: is_implemented(Baz), skel_g: is_implemented(Baz) }: at prove_via.rs:9
                                       └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                              env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                                              assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                              goals: {!ty_1 = !ty_1}
                                              result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ prove_wc: (eq) at prove_wc.rs:22
                                                 _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                                 env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                                                 assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                                 goal: !ty_1 = !ty_1
                                                 result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ trivial, as a == b is true: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                          └─ prove_after: (prove_after) at prove_after.rs:8
                                                 _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                                 constraints: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                 assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                                 goal: {}
                                                 result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ (assumptions, goal) = ({Bar(!ty_0, !ty_1), Baz(!ty_1)}, {}): at prove_after.rs:8
                                             └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                    _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                                    env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                                                    assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                                    goals: {}
                                                    result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                        constraints: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({Bar(!ty_0, !ty_1), Baz(!ty_1)}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                           env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                                           assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               constraints: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                               goal: {@ WellFormedTraitRef(Baz(!ty_1))}
                               result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Bar(!ty_0, !ty_1), Baz(!ty_1)}, {@ WellFormedTraitRef(Baz(!ty_1))}): at prove_after.rs:8
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                  goals: {@ WellFormedTraitRef(Baz(!ty_1))}
                                  result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (trait well formed) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                     goal: @ WellFormedTraitRef(Baz(!ty_1))
                                     result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ for_all: at combinators.rs:73
                                    └─ prove_wf: (universal variables) at prove_wf.rs:14
                                           _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                           env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                                           assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                           goal: !ty_1
                                           result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                                 └─ t = trait Baz <ty> : at prove_wc.rs:22
                                 └─ t = : at prove_wc.rs:22
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                        constraints: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({Bar(!ty_0, !ty_1), Baz(!ty_1)}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                           env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                                           assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     constraints: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({Bar(!ty_0, !ty_1), Baz(!ty_1)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                        env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }
                                        assumptions: {Bar(!ty_0, !ty_1), Baz(!ty_1)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {Baz(!ty_0)}
                            goals: {@ WellFormedTraitRef(Baz(!ty_0))}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {Baz(!ty_0)}
                               goal: @ WellFormedTraitRef(Baz(!ty_0))
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ for_all: at combinators.rs:73
                              └─ prove_wf: (universal variables) at prove_wf.rs:14
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Baz(!ty_0)}
                                     goal: !ty_0
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                           └─ t = trait Baz <ty> : at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Baz(!ty_0)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Baz(!ty_0)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Baz(!ty_0)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Baz(!ty_0)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Baz(!ty_0)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Baz(!ty_0)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {Baz(u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: Baz(u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl Baz(u32): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [], bias: Soundness, pending: [] }, []): at prove_wc.rs:22
                           └─ i = Baz(u32): at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ co_assumptions = ({}, Baz(u32)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {Baz(u32)}
                                  goals: {u32 = u32}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Baz(u32)}
                                     goal: u32 = u32
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Baz(u32)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({Baz(u32)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Baz(u32)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Baz(u32)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Baz(u32)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Baz(u32)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Baz(u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Baz(u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Baz(u32), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {Bar(u32, u32)}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: Bar(u32, u32)
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl Bar(u32, u32): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [], bias: Soundness, pending: [] }, []): at prove_wc.rs:22
                           └─ i = Bar(u32, u32): at prove_wc.rs:22
                           └─ t = where {Baz(u32)}: at prove_wc.rs:22
                           └─ co_assumptions = ({}, Bar(u32, u32)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {Bar(u32, u32)}
                                  goals: {u32 = u32}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Bar(u32, u32)}
                                     goal: u32 = u32
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Bar(u32, u32)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({Bar(u32, u32)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {Bar(u32, u32)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Bar(u32, u32)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Bar(u32, u32)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {Bar(u32, u32)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {}
                                  goal: {Baz(u32)}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({}, {Baz(u32)}): at prove_after.rs:8
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {Baz(u32)}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (positive impl) at prove_wc.rs:22
                                        _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                        env: Env { variables: [], bias: Soundness, pending: [] }
                                        assumptions: {}
                                        goal: Baz(u32)
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ item = impl Baz(u32): at prove_wc.rs:22
                                    └─ (env, subst) = (Env { variables: [], bias: Soundness, pending: [] }, []): at prove_wc.rs:22
                                    └─ i = Baz(u32): at prove_wc.rs:22
                                    └─ t = : at prove_wc.rs:22
                                    └─ co_assumptions = ({}, Baz(u32)): at prove_wc.rs:22
                                    └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {Baz(u32)}
                                           goals: {u32 = u32}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ prove_wc: (eq) at prove_wc.rs:22
                                              _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {Baz(u32)}
                                              goal: u32 = u32
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ trivial, as a == b is true: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                       └─ prove_after: (prove_after) at prove_after.rs:8
                                              _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                              constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                              assumptions: {Baz(u32)}
                                              goal: {}
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ (assumptions, goal) = ({Baz(u32)}, {}): at prove_after.rs:8
                                          └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                 _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                                 env: Env { variables: [], bias: Soundness, pending: [] }
                                                 assumptions: {Baz(u32)}
                                                 goals: {}
                                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                           constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                           assumptions: {Baz(u32)}
                                           goal: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (assumptions, goal) = ({Baz(u32)}, {}): at prove_after.rs:8
                                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {Baz(u32)}
                                              goals: {}
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                           constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                           assumptions: {}
                                           goal: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {}
                                              goals: {}
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                        constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                           env: Env { variables: [], bias: Soundness, pending: [] }
                                           assumptions: {}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Bar(u32, u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Bar(u32, u32)}, assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Bar(u32, u32), assumptions: {}, env: Env { variables: [], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {Baz(!ty_0)}
                            goals: {@ WellFormedTraitRef(Baz(!ty_0))}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {Baz(!ty_0)}
                               goal: @ WellFormedTraitRef(Baz(!ty_0))
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ for_all: at combinators.rs:73
                              └─ prove_wf: (universal variables) at prove_wf.rs:14
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Baz(!ty_0)}
                                     goal: !ty_0
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                           └─ t = trait Baz <ty> : at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {Baz(!ty_0)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({Baz(!ty_0)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Baz(!ty_0)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Baz(!ty_0)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Baz(!ty_0)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Baz(!ty_0)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {Baz(!ty_0)}
                            goals: {Bar((), !ty_0)}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {Baz(!ty_0)}
                               goal: Bar((), !ty_0)
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}: at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, [?ty_1]): at prove_wc.rs:22
                           └─ i = Bar((), ?ty_1) where {Baz(?ty_1)}: at prove_wc.rs:22
                           └─ t = where {Baz(?ty_1)}: at prove_wc.rs:22
                           └─ co_assumptions = ({Baz(!ty_0)}, Bar((), !ty_0)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                  assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                  goals: {() = (), !ty_0 = ?ty_1}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                     assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                     goal: () = ()
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ trivial, as a == b is true: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                     goal: {!ty_0 = ?ty_1}
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                 └─ (assumptions, goal) = ({Bar((), !ty_0), Baz(!ty_0)}, {!ty_0 = ?ty_1}): at prove_after.rs:8
                                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                        env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                        assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                        goals: {!ty_0 = ?ty_1}
                                        result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                    └─ prove_wc: (eq) at prove_wc.rs:22
                                           _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                           env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                           assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                           goal: !ty_0 = ?ty_1
                                           result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                       └─ prove_eq: (symmetric) at prove_eq.rs:23
                                              _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                              env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                              assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                              a: !ty_0
                                              b: ?ty_1
                                              result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                          └─ prove_eq: (existential) at prove_eq.rs:23
                                                 _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                                 env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                                 assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                                 a: ?ty_1
                                                 b: !ty_0
                                                 result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                             └─ prove_existential_var_eq: (existential-universal) at prove_eq.rs:76
                                                    _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                                    env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                                    assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                                    v: ?ty_1
                                                    b: !ty_0
                                                    result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                                └─ IfThen { expression: "env.universe(p) < env.universe(v)" }: at prove_eq.rs:76
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                           constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                           assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                           goal: {}
                                           result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                       └─ (assumptions, goal) = ({Bar((), !ty_0), Baz(!ty_0)}, {}): at prove_after.rs:8
                                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                              env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                              assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                              goals: {}
                                              result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                  assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                  goal: {Baz(?ty_1)}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ (assumptions, goal) = ({Bar((), !ty_0), Baz(!ty_0)}, {Baz(!ty_0)}): at prove_after.rs:8
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                     goals: {Baz(!ty_0)}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (assumption - predicate) at prove_wc.rs:22
                                        _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                        assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                        goal: Baz(!ty_0)
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ item = Baz(!ty_0): at prove_wc.rs:22
                                    └─ prove_via: (predicate-congruence-axiom) at prove_via.rs:9
                                           _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                           env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                           assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                           via: Baz(!ty_0)
                                           goal: Baz(!ty_0)
                                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (skel_c, parameters_c) = (is_implemented(Baz), [!ty_0]): at prove_via.rs:9
                                       └─ (skel_g, parameters_g) = (is_implemented(Baz), [!ty_0]): at prove_via.rs:9
                                       └─ IfThen { expression: "skel_c == skel_g", skel_c: is_implemented(Baz), skel_g: is_implemented(Baz) }: at prove_via.rs:9
                                       └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                              env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                              assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                              goals: {!ty_0 = !ty_0}
                                              result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ prove_wc: (eq) at prove_wc.rs:22
                                                 _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                                 env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                 assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                                 goal: !ty_0 = !ty_0
                                                 result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ trivial, as a == b is true: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                          └─ prove_after: (prove_after) at prove_after.rs:8
                                                 _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                                 constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                 assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                                 goal: {}
                                                 result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ (assumptions, goal) = ({Bar((), !ty_0), Baz(!ty_0)}, {}): at prove_after.rs:8
                                             └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                    _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                                    env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                    assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                                    goals: {}
                                                    result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                        constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({Bar((), !ty_0), Baz(!ty_0)}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                           env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                           assumptions: {Bar((), !ty_0), Baz(!ty_0)}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                  assumptions: {Baz(!ty_0)}
                                  goal: {Baz(?ty_1)}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ (assumptions, goal) = ({Baz(!ty_0)}, {Baz(!ty_0)}): at prove_after.rs:8
                              └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {Baz(!ty_0)}
                                     goals: {Baz(!ty_0)}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_wc: (assumption - predicate) at prove_wc.rs:22
                                        _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                        assumptions: {Baz(!ty_0)}
                                        goal: Baz(!ty_0)
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ item = Baz(!ty_0): at prove_wc.rs:22
                                    └─ prove_via: (predicate-congruence-axiom) at prove_via.rs:9
                                           _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                           env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                           assumptions: {Baz(!ty_0)}
                                           via: Baz(!ty_0)
                                           goal: Baz(!ty_0)
                                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (skel_c, parameters_c) = (is_implemented(Baz), [!ty_0]): at prove_via.rs:9
                                       └─ (skel_g, parameters_g) = (is_implemented(Baz), [!ty_0]): at prove_via.rs:9
                                       └─ IfThen { expression: "skel_c == skel_g", skel_c: is_implemented(Baz), skel_g: is_implemented(Baz) }: at prove_via.rs:9
                                       └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                              env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                              assumptions: {Baz(!ty_0)}
                                              goals: {!ty_0 = !ty_0}
                                              result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                          └─ prove_wc: (eq) at prove_wc.rs:22
                                                 _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                                 env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                 assumptions: {Baz(!ty_0)}
                                                 goal: !ty_0 = !ty_0
                                                 result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ trivial, as a == b is true: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at prove_eq.rs:35
                                          └─ prove_after: (prove_after) at prove_after.rs:8
                                                 _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                                 constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                                 assumptions: {Baz(!ty_0)}
                                                 goal: {}
                                                 result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                             └─ (assumptions, goal) = ({Baz(!ty_0)}, {}): at prove_after.rs:8
                                             └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                    _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                                    env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                    assumptions: {Baz(!ty_0)}
                                                    goals: {}
                                                    result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ prove_after: (prove_after) at prove_after.rs:8
                                        _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                        constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                        assumptions: {Baz(!ty_0)}
                                        goal: {}
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ (assumptions, goal) = ({Baz(!ty_0)}, {}): at prove_after.rs:8
                                    └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                           _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                           env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                           assumptions: {Baz(!ty_0)}
                                           goals: {}
                                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Baz(!ty_0)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Baz(!ty_0)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Baz(!ty_0)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! Bar((), ?ty_0)}, assumptions: {Baz(?ty_0)}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! Bar((), ?ty_0)}, assumptions: {Baz(?ty_0)}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! Bar((), ?ty_0), assumptions: {Baz(?ty_0)}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_coherence(Foo): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(Baz(u32))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(Baz(u32))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: Baz(u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {}), &goal.trait_id: Baz }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(Bar(u32, u32))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(Bar(u32, u32))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: Bar(u32, u32)
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {}), &goal.trait_id: Bar }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {Baz(!ty_0)}
                            goals: {@ IsLocal(Bar((), !ty_0))}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {Baz(!ty_0)}
                               goal: @ IsLocal(Bar((), !ty_0))
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Baz(!ty_0)}
                                  goal: Bar((), !ty_0)
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {}), &goal.trait_id: Bar }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {Baz(!ty_0)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({Baz(!ty_0)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {Baz(!ty_0)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ overlap_check: (not_goal) at coherence.rs:127
                        └─ negation succeeded: judgment `prove { goal: {u32 = (), u32 = ?ty_0, Baz(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {u32 = (), u32 = ?ty_0, Baz(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: u32 = (), assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "eq" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:126:14) because
                        judgment `prove_eq { a: u32, b: (), assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                          the rule "normalize-l" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:68:14) because
                            judgment had no applicable rules: `prove_normalize { p: u32, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }`
                          the rule "symmetric" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:38:14) because
                            judgment `prove_eq { a: (), b: u32, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                              the rule "normalize-l" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:68:14) because
                                judgment had no applicable rules: `prove_normalize { p: (), assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }`
                              the rule "symmetric" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:38:14) because
                                cyclic proof attempt: `prove_eq { a: u32, b: (), assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }`: at negation.rs:125
                     └─ overlap_check: (not_goal) at coherence.rs:127
                        └─ negation succeeded: judgment `prove { goal: {() = u32, ?ty_0 = u32, Baz(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0), Baz(^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [impl Baz(u32), impl Bar(u32, u32), impl <ty> Bar((), ^ty0_0) where {Baz(^ty0_0)}], [], [], [], [], {Bar, Baz, Foo}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {() = u32, ?ty_0 = u32, Baz(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: () = u32, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "eq" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:126:14) because
                        judgment `prove_eq { a: (), b: u32, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                          the rule "normalize-l" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:68:14) because
                            judgment had no applicable rules: `prove_normalize { p: (), assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }`
                          the rule "symmetric" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:38:14) because
                            judgment `prove_eq { a: u32, b: (), assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                              the rule "normalize-l" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:68:14) because
                                judgment had no applicable rules: `prove_normalize { p: u32, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }`
                              the rule "symmetric" failed at step #0 (crates/formality-prove/src/prove/prove_eq.rs:38:14) because
                                cyclic proof attempt: `prove_eq { a: (), b: u32, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }`: at negation.rs:125
        "#]]
    )
}

#[test]
fn basic_where_clauses_pass() {
    crate::assert_ok!(
        //@check-pass
        [
            crate core {
                trait A<ty T> where T: B { }

                trait B { }

                trait WellFormed where for<ty T> u32: A<T> { }

                impl <ty T> B for T {}
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(core): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {B(!ty_0)}
                            goals: {@ WellFormedTraitRef(B(!ty_0))}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {B(!ty_0)}
                               goal: @ WellFormedTraitRef(B(!ty_0))
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ for_all: at combinators.rs:73
                              └─ prove_wf: (universal variables) at prove_wf.rs:14
                                     _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {B(!ty_0)}
                                     goal: !ty_0
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                           └─ t = trait B <ty> : at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                  assumptions: {B(!ty_0)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ (assumptions, goal) = ({B(!ty_0)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {B(!ty_0)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {B(!ty_0)}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({B(!ty_0)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {B(!ty_0)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {for <ty> A(u32, ^ty0_0)}
                            goals: {for <ty> @ WellFormedTraitRef(A(u32, ^ty0_0))}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (forall) at prove_wc.rs:22
                               _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {for <ty> A(u32, ^ty0_0)}
                               goal: for <ty> @ WellFormedTraitRef(A(u32, ^ty0_0))
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (env, subst) = (Env { variables: [!ty_1], bias: Soundness, pending: [] }, [!ty_1]): at prove_wc.rs:22
                           └─ p1 = @ WellFormedTraitRef(A(u32, !ty_1)): at prove_wc.rs:22
                           └─ prove_wc: (trait well formed) at prove_wc.rs:22
                                  _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                  env: Env { variables: [!ty_1], bias: Soundness, pending: [] }
                                  assumptions: {for <ty> A(u32, ^ty0_0)}
                                  goal: @ WellFormedTraitRef(A(u32, !ty_1))
                                  result: Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ for_all: at combinators.rs:73
                                 └─ prove_wf: (integers and booleans) at prove_wf.rs:14
                                        _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                        env: Env { variables: [!ty_1], bias: Soundness, pending: [] }
                                        assumptions: {for <ty> A(u32, ^ty0_0)}
                                        goal: u32
                                        result: Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                                 └─ for_all: at combinators.rs:73
                                    └─ prove_wf: (universal variables) at prove_wf.rs:14
                                           _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                           env: Env { variables: [!ty_1], bias: Soundness, pending: [] }
                                           assumptions: {for <ty> A(u32, ^ty0_0)}
                                           goal: !ty_1
                                           result: Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                              └─ t = trait A <ty, ty> where {B(^ty0_1)}: at prove_wc.rs:22
                              └─ t = where {B(!ty_1)}: at prove_wc.rs:22
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                     constraints: Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                     assumptions: {for <ty> A(u32, ^ty0_0)}
                                     goal: {B(!ty_1)}
                                     result: Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                 └─ (assumptions, goal) = ({for <ty> A(u32, ^ty0_0)}, {B(!ty_1)}): at prove_after.rs:8
                                 └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                        assumptions: {for <ty> A(u32, ^ty0_0)}
                                        goals: {B(!ty_0)}
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_wc: (positive impl) at prove_wc.rs:22
                                           _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                           env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                           assumptions: {for <ty> A(u32, ^ty0_0)}
                                           goal: B(!ty_0)
                                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ item = impl <ty> B(^ty0_0): at prove_wc.rs:22
                                       └─ (env, subst) = (Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, [?ty_1]): at prove_wc.rs:22
                                       └─ i = B(?ty_1): at prove_wc.rs:22
                                       └─ t = : at prove_wc.rs:22
                                       └─ co_assumptions = ({for <ty> A(u32, ^ty0_0)}, B(!ty_0)): at prove_wc.rs:22
                                       └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                              env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                              assumptions: {B(!ty_0), for <ty> A(u32, ^ty0_0)}
                                              goals: {!ty_0 = ?ty_1}
                                              result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                          └─ prove_wc: (eq) at prove_wc.rs:22
                                                 _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                                 env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                                 assumptions: {B(!ty_0), for <ty> A(u32, ^ty0_0)}
                                                 goal: !ty_0 = ?ty_1
                                                 result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                             └─ prove_eq: (symmetric) at prove_eq.rs:23
                                                    _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                                    env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                                    assumptions: {B(!ty_0), for <ty> A(u32, ^ty0_0)}
                                                    a: !ty_0
                                                    b: ?ty_1
                                                    result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                                └─ prove_eq: (existential) at prove_eq.rs:23
                                                       _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                                       env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                                       assumptions: {B(!ty_0), for <ty> A(u32, ^ty0_0)}
                                                       a: ?ty_1
                                                       b: !ty_0
                                                       result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                                   └─ prove_existential_var_eq: (existential-universal) at prove_eq.rs:76
                                                          _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                                          env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                                          assumptions: {B(!ty_0), for <ty> A(u32, ^ty0_0)}
                                                          v: ?ty_1
                                                          b: !ty_0
                                                          result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                                      └─ IfThen { expression: "env.universe(p) < env.universe(v)" }: at prove_eq.rs:76
                                          └─ prove_after: (prove_after) at prove_after.rs:8
                                                 _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                                 constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                                 assumptions: {B(!ty_0), for <ty> A(u32, ^ty0_0)}
                                                 goal: {}
                                                 result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                             └─ (assumptions, goal) = ({B(!ty_0), for <ty> A(u32, ^ty0_0)}, {}): at prove_after.rs:8
                                             └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                    _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                                    env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                    assumptions: {B(!ty_0), for <ty> A(u32, ^ty0_0)}
                                                    goals: {}
                                                    result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ prove_after: (prove_after) at prove_after.rs:8
                                              _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                              constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                              assumptions: {B(!ty_0), for <ty> A(u32, ^ty0_0)}
                                              goal: {}
                                              result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                          └─ (assumptions, goal) = ({B(!ty_0), for <ty> A(u32, ^ty0_0)}, {}): at prove_after.rs:8
                                          └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                 _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                                 env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                                 assumptions: {B(!ty_0), for <ty> A(u32, ^ty0_0)}
                                                 goals: {}
                                                 result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ prove_after: (prove_after) at prove_after.rs:8
                                              _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                              constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                              assumptions: {for <ty> A(u32, ^ty0_0)}
                                              goal: {}
                                              result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                          └─ (assumptions, goal) = ({for <ty> A(u32, ^ty0_0)}, {}): at prove_after.rs:8
                                          └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                                 _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                                 env: Env { variables: [], bias: Soundness, pending: [] }
                                                 assumptions: {for <ty> A(u32, ^ty0_0)}
                                                 goals: {}
                                                 result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                    └─ prove_after: (prove_after) at prove_after.rs:8
                                           _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                           constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                           assumptions: {for <ty> A(u32, ^ty0_0)}
                                           goal: {}
                                           result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                                       └─ (assumptions, goal) = ({for <ty> A(u32, ^ty0_0)}, {}): at prove_after.rs:8
                                       └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                              _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                              env: Env { variables: [], bias: Soundness, pending: [] }
                                              assumptions: {for <ty> A(u32, ^ty0_0)}
                                              goals: {}
                                              result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {for <ty> A(u32, ^ty0_0)}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({for <ty> A(u32, ^ty0_0)}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {for <ty> A(u32, ^ty0_0)}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_trait_impl: at impls.rs:27
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {B(!ty_0)}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (positive impl) at prove_wc.rs:22
                               _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: B(!ty_0)
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ item = impl <ty> B(^ty0_0): at prove_wc.rs:22
                           └─ (env, subst) = (Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, [?ty_1]): at prove_wc.rs:22
                           └─ i = B(?ty_1): at prove_wc.rs:22
                           └─ t = : at prove_wc.rs:22
                           └─ co_assumptions = ({}, B(!ty_0)): at prove_wc.rs:22
                           └─ prove_wc_list: (some) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                  env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                  assumptions: {B(!ty_0)}
                                  goals: {!ty_0 = ?ty_1}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ prove_wc: (eq) at prove_wc.rs:22
                                     _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                     env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                     assumptions: {B(!ty_0)}
                                     goal: !ty_0 = ?ty_1
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                 └─ prove_eq: (symmetric) at prove_eq.rs:23
                                        _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                        env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                        assumptions: {B(!ty_0)}
                                        a: !ty_0
                                        b: ?ty_1
                                        result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                    └─ prove_eq: (existential) at prove_eq.rs:23
                                           _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                           env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                           assumptions: {B(!ty_0)}
                                           a: ?ty_1
                                           b: !ty_0
                                           result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                       └─ prove_existential_var_eq: (existential-universal) at prove_eq.rs:76
                                              _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                              env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }
                                              assumptions: {B(!ty_0)}
                                              v: ?ty_1
                                              b: !ty_0
                                              result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                          └─ IfThen { expression: "env.universe(p) < env.universe(v)" }: at prove_eq.rs:76
                              └─ prove_after: (prove_after) at prove_after.rs:8
                                     _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                     constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                     assumptions: {B(!ty_0)}
                                     goal: {}
                                     result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                 └─ (assumptions, goal) = ({B(!ty_0)}, {}): at prove_after.rs:8
                                 └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                        _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                        env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                        assumptions: {B(!ty_0)}
                                        goals: {}
                                        result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                  assumptions: {B(!ty_0)}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ (assumptions, goal) = ({B(!ty_0)}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                     env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                     assumptions: {B(!ty_0)}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_after: (prove_after) at prove_after.rs:8
                                  _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                  constraints: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                                  assumptions: {}
                                  goal: {}
                                  result: Constraints { env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {?ty_1 => !ty_0} }
                              └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                              └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                     _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                     env: Env { variables: [], bias: Soundness, pending: [] }
                                     assumptions: {}
                                     goals: {}
                                     result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ negation succeeded: judgment `prove { goal: {! B(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] }, decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {}) }` failed at the following rule(s):
              failed at (/Users/nikomat/dev/a-mir-formality/crates/formality-prove/src/prove.rs:89:45) because
                judgment `prove_wc_list { goals: {! B(?ty_0)}, assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (crates/formality-prove/src/prove/prove_wc_list.rs:29:14) because
                    judgment `prove_wc { goal: ! B(?ty_0), assumptions: {}, env: Env { variables: [?ty_0], bias: Completeness, pending: [] } }` failed at the following rule(s):
                      the rule "negative impl" failed at step #0 (crates/formality-prove/src/prove/prove_wc.rs:100:14) because
                        expression evaluated to an empty collection: `decls.neg_impl_decls(&trait_ref.trait_id)`: at negation.rs:125
                     └─ safety_matches(safe, safe): at impls.rs:122
                  └─ check_coherence(core): at coherence.rs:13
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                            env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ IsLocal(B(!ty_0))}
                            result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (trait ref is local) at prove_wc.rs:22
                               _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                               env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ IsLocal(B(!ty_0))
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ is_local_trait_ref: (local trait) at is_local.rs:199
                                  _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                  env: Env { variables: [!ty_0], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: B(!ty_0)
                                  result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ IfThen { expression: "decls.is_local_trait_id(&goal.trait_id)", decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {}), &goal.trait_id: B }: at is_local.rs:199
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                               constraints: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [impl <ty> B(^ty0_0)], [], [], [], [], {A, B, WellFormed}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
        "#]]
    )
}

#[test]
fn basic_where_clauses_fail() {
    crate::assert_err!(
        [
            crate core {
                trait A<ty T> where T: B { }

                trait B { }

                trait WellFormed where for<ty T> u32: A<T> { }
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_trait(WellFormed)

            Caused by:
                0: prove_where_clauses_well_formed([for <ty> u32 : A <^ty0_0>])
                1: judgment `prove { goal: {for <ty> @ WellFormedTraitRef(A(u32, ^ty0_0))}, assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [], bias: Soundness, pending: [] }, decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [], [], [], [], [], {A, B, WellFormed}, {}) }` failed at the following rule(s):
                     failed at (src/file.rs:LL:CC) because
                       judgment `prove_wc_list { goals: {for <ty> @ WellFormedTraitRef(A(u32, ^ty0_0))}, assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                         the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                           judgment `prove_wc { goal: for <ty> @ WellFormedTraitRef(A(u32, ^ty0_0)), assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [], bias: Soundness, pending: [] } }` failed at the following rule(s):
                             the rule "forall" failed at step #2 (src/file.rs:LL:CC) because
                               judgment `prove_wc { goal: @ WellFormedTraitRef(A(u32, !ty_1)), assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [!ty_1], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                 the rule "trait well formed" failed at step #3 (src/file.rs:LL:CC) because
                                   judgment `prove_after { constraints: Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [] }, known_true: true, substitution: {} }, goal: {B(!ty_1)}, assumptions: {for <ty> A(u32, ^ty0_0)} }` failed at the following rule(s):
                                     the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                                       judgment `prove { goal: {B(!ty_0)}, assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [] }, decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [], [], [], [], [], {A, B, WellFormed}, {}) }` failed at the following rule(s):
                                         failed at (src/file.rs:LL:CC) because
                                           judgment `prove_wc_list { goals: {B(!ty_0)}, assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                             the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                               judgment `prove_wc { goal: B(!ty_0), assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [] } }` failed at the following rule(s):
                                                 the rule "trait implied bound" failed at step #0 (src/file.rs:LL:CC) because
                                                   expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn basic_adt_variant_dup() {
    crate::assert_err!(
        [
            crate Foo {
                enum Bar {
                    Baz{},
                    Baz{},
                }
            }
        ]

        [ r#"variant "Baz" defined multiple times"#, ]

        expect_test::expect![[r#"variant "Baz" defined multiple times"#]]
    )
}

#[test]
fn basic_adt_field_dup() {
    crate::assert_err!(
        [
            crate Foo {
                struct Bar {
                    baz: (),
                    baz: (),
                }
            }
        ]

        [ r#"field "baz" of variant "struct" defined multiple times"#, ]

        expect_test::expect![[r#"field "baz" of variant "struct" defined multiple times"#]]
    )
}

#[test]
fn trait_items_with_duplicate_fn_names() {
    crate::assert_err!(
        [
            crate core {
                trait A {
                    fn a() -> ();
                    fn a() -> ();
                }
            }
        ]

        ["the function name `a` is defined multiple times",]

        expect_test::expect![[r#"
            check_trait(A)

            Caused by:
                the function name `a` is defined multiple times"#]]

    );
}

#[test]
fn trait_items_with_duplicate_associated_type_names() {
    crate::assert_err!(
        [
            crate core {
                trait A {
                    type Assoc : [];
                    type Assoc : [];
                }
            }
        ]

        ["the associated type name `Assoc` is defined multiple times",]

        expect_test::expect![[r#"
            check_trait(A)

            Caused by:
                the associated type name `Assoc` is defined multiple times"#]]
    );
}

#[test]
fn crate_with_duplicate_item_names() {
    crate::assert_err!(
        [
            crate core {
                struct A {}

                enum A {}
            }
        ]

        ["the item name `A` is defined multiple times",]

        expect_test::expect![[r#"the item name `A` is defined multiple times"#]]
    );

    crate::assert_err!(
        [
            crate core {
                trait a {}

                trait a {}
            }
        ]

        ["the trait name `a` is defined multiple times",]

        expect_test::expect![[r#"the trait name `a` is defined multiple times"#]]
    );

    crate::assert_err!(
        [
            crate core {
                fn a() -> () { trusted }

                fn a() -> () { trusted }
            }
        ]

        ["the function name `a` is defined multiple times",]

        expect_test::expect![[r#"the function name `a` is defined multiple times"#]]
    );

    crate::assert_ok!(
        //@check-pass
        [
            crate core {
                trait a {}

                fn a() -> () { trusted }
            }
        ]

        expect_test::expect![[r#"
            └─ check_all_crates: at lib.rs:27
               └─ check_current_crate(core): at lib.rs:73
                  └─ check_trait: at traits.rs:13
                     └─ check_trait_items_have_unique_names: at traits.rs:71
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait a <ty> ], [], [], [], [], [], {a}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_fn(core, fn a () -> () { trusted}, {}, Env { variables: [], bias: Soundness, pending: [] }): at fns.rs:33
                     └─ prove_wc_list: (none) at prove_wc_list.rs:11
                            _decls: decls(222, [trait a <ty> ], [], [], [], [], [], {a}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                     └─ prove_wc_list: (some) at prove_wc_list.rs:11
                            _decls: decls(222, [trait a <ty> ], [], [], [], [], [], {a}, {})
                            env: Env { variables: [], bias: Soundness, pending: [] }
                            assumptions: {}
                            goals: {@ wf(())}
                            result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                        └─ prove_wc: (parameter well formed) at prove_wc.rs:22
                               _decls: decls(222, [trait a <ty> ], [], [], [], [], [], {a}, {})
                               env: Env { variables: [], bias: Soundness, pending: [] }
                               assumptions: {}
                               goal: @ wf(())
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ prove_wf: (tuples) at prove_wf.rs:14
                                  _decls: decls(222, [trait a <ty> ], [], [], [], [], [], {a}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goal: ()
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                              └─ Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }: at combinators.rs:61
                        └─ prove_after: (prove_after) at prove_after.rs:8
                               _decls: decls(222, [trait a <ty> ], [], [], [], [], [], {a}, {})
                               constraints: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                               assumptions: {}
                               goal: {}
                               result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                           └─ (assumptions, goal) = ({}, {}): at prove_after.rs:8
                           └─ prove_wc_list: (none) at prove_wc_list.rs:11
                                  _decls: decls(222, [trait a <ty> ], [], [], [], [], [], {a}, {})
                                  env: Env { variables: [], bias: Soundness, pending: [] }
                                  assumptions: {}
                                  goals: {}
                                  result: Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }
                  └─ check_coherence(core): at coherence.rs:13
        "#]]
    );
}
