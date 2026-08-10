use a_mir_formality::{crates, FormalityTest};

const EMPTY_PROGRAM: &str = crates![crate test {}];

const FOO_FOR_VEC: &str = crates![crate test {
    struct Vec<T> {}

    trait Foo {}

    impl<T> Foo for Vec<T> {}
}];

const RECURSIVE_DEBUG: &str = crates![crate test {
    struct Vec<T> {}

    trait Debug {}

    impl<T> Debug for Vec<T>
    where
        T: Debug,
    {}
}];

#[test]
fn trait_goal_infers_impl_argument() {
    FormalityTest::new(FOO_FOR_VEC)
        .prove("exists<U> {} => {U: Foo}")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [?ty_2, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => Vec<?ty_2>} }}"]);
}

#[test]
fn direct_occurs_check_cycle_is_rejected() {
    FormalityTest::new(FOO_FOR_VEC)
        .prove("exists<A> {} => {A = Vec<A>}")
        .assert_err(expect_test::expect![[r#"
            the rule "assumption" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `assumptions`

            failed at (proven_set.rs) because
              `?ty_0` occurs in `Vec<?ty_0>`

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:53:1: no applicable rules for prove_normalize { p: ?ty_0, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:53:1: no applicable rules for prove_normalize { p: Vec<?ty_0>, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}

#[test]
fn existential_variable_equals_rigid_type() {
    FormalityTest::new(FOO_FOR_VEC)
        .prove("exists<X, Y> {} => {X = Vec<Y>}")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [?ty_3, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => Vec<?ty_3>, ?ty_2 => ?ty_3} }}"]);
}

#[test]
fn rigid_type_equals_existential_variable() {
    FormalityTest::new(FOO_FOR_VEC)
        .prove("exists<X, Y> {} => {Vec<Y> = X}")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [?ty_3, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => Vec<?ty_3>, ?ty_2 => ?ty_3} }}"]);
}

#[test]
fn indirect_occurs_check_cycle_is_rejected() {
    FormalityTest::new(FOO_FOR_VEC)
        .prove("exists<A, B> {} => {A = Vec<B>, B = A}")
        .assert_err(expect_test::expect![[r#"
            the rule "assumption" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `assumptions`

            failed at (proven_set.rs) because
              `?ty_0` occurs in `Vec<?ty_0>`

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:53:1: no applicable rules for prove_normalize { p: ?ty_0, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:53:1: no applicable rules for prove_normalize { p: Vec<?ty_0>, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}

#[test]
fn reordered_indirect_occurs_check_cycle_is_rejected() {
    FormalityTest::new(FOO_FOR_VEC)
        .prove("exists<A, B> {} => {B = A, A = Vec<B>}")
        .assert_err(expect_test::expect![[r#"
            the rule "assumption" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `assumptions`

            failed at (proven_set.rs) because
              `?ty_0` occurs in `Vec<?ty_0>`

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:53:1: no applicable rules for prove_normalize { p: ?ty_0, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:53:1: no applicable rules for prove_normalize { p: Vec<?ty_0>, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}

#[test]
fn equality_assumptions_are_applied_transitively() {
    FormalityTest::new(EMPTY_PROGRAM)
        .prove("{} => {for<T, U> if {T = u32, U = Vec<T>} U = Vec<u32>}")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

#[test]
fn equality_assumptions_constrain_an_existential() {
    FormalityTest::new(EMPTY_PROGRAM)
        .prove("exists<A> {} => {for<T, U> if {T = u32, U = Vec<T>} A = U}")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [?ty_2, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => Vec<u32>, ?ty_2 => u32} }}"]);
}

#[test]
fn existential_cannot_name_a_later_universal_in_alias_assumption() {
    FormalityTest::new(EMPTY_PROGRAM)
        .prove("exists<A> {} => {for<T> if { <T as Iterator>::Item = u32 } <A as Iterator>::Item = u32}")
        .assert_err(expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:9:1: no applicable rules for prove_via_assumption { goal: <?ty_0 as Iterator>::Item = u32, via: <!ty_1 as Iterator>::Item = u32, assumptions: {<!ty_1 as Iterator>::Item = u32}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "existential-nonvar" at (prove_eq.rs) failed because
              pattern `None` did not match value `Some(!ty_1)`

            the rule "existential-universal" at (prove_eq.rs) failed because
              condition evaluated to false: `env.universe(p) < env.universe(v)`

            the rule "existential-nonvar" at (prove_eq.rs) failed because
              pattern `None` did not match value `Some(!ty_1)`

            the rule "existential-universal" at (prove_eq.rs) failed because
              condition evaluated to false: `env.universe(p) < env.universe(v)`

            the rule "normalize-via-impl" at (prove_normalize.rs) failed because
              expression evaluated to an empty collection: `decls.raw_trait_impls_for(trait_id)`

            crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:9:1: no applicable rules for prove_via_assumption { goal: <!ty_0 as Iterator>::Item = <?ty_1 as Iterator>::Item, via: <!ty_0 as Iterator>::Item = u32, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:9:1: no applicable rules for prove_via_assumption { goal: !ty_0 = ?ty_1, via: <!ty_0 as Iterator>::Item = u32, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "existential-nonvar" at (prove_eq.rs) failed because
              pattern `None` did not match value `Some(!ty_0)`

            the rule "existential-universal" at (prove_eq.rs) failed because
              condition evaluated to false: `env.universe(p) < env.universe(v)`

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:305:1: no applicable rules for prove_normalize_via_eq { goal: ?ty_1, left: <!ty_0 as Iterator>::Item, right: u32, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:9:1: no applicable rules for prove_via_assumption { goal: u32 = <?ty_1 as Iterator>::Item, via: <!ty_0 as Iterator>::Item = u32, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "existential-nonvar" at (prove_eq.rs) failed because
              pattern `None` did not match value `Some(!ty_0)`

            the rule "existential-universal" at (prove_eq.rs) failed because
              condition evaluated to false: `env.universe(p) < env.universe(v)`

            the rule "existential-nonvar" at (prove_eq.rs) failed because
              pattern `None` did not match value `Some(!ty_0)`

            the rule "existential-universal" at (prove_eq.rs) failed because
              condition evaluated to false: `env.universe(p) < env.universe(v)`

            the rule "normalize-via-impl" at (prove_normalize.rs) failed because
              expression evaluated to an empty collection: `decls.raw_trait_impls_for(trait_id)`

            crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:9:1: no applicable rules for prove_via_assumption { goal: ?ty_1 = !ty_0, via: <!ty_0 as Iterator>::Item = u32, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "existential-nonvar" at (prove_eq.rs) failed because
              pattern `None` did not match value `Some(!ty_0)`

            the rule "existential-universal" at (prove_eq.rs) failed because
              condition evaluated to false: `env.universe(p) < env.universe(v)`

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:305:1: no applicable rules for prove_normalize_via_eq { goal: ?ty_1, left: <!ty_0 as Iterator>::Item, right: u32, assumptions: {<!ty_0 as Iterator>::Item = u32}, env: Env { variables: [?ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "existential-nonvar" at (prove_eq.rs) failed because
              pattern `None` did not match value `Some(!ty_0)`

            the rule "existential-universal" at (prove_eq.rs) failed because
              condition evaluated to false: `env.universe(p) < env.universe(v)`

            the rule "existential-nonvar" at (prove_eq.rs) failed because
              pattern `None` did not match value `Some(!ty_0)`

            the rule "existential-universal" at (prove_eq.rs) failed because
              condition evaluated to false: `env.universe(p) < env.universe(v)`

            the rule "normalize-via-impl" at (prove_normalize.rs) failed because
              expression evaluated to an empty collection: `decls.raw_trait_impls_for(trait_id)`"#]]);
}

#[test]
fn existential_can_name_an_earlier_universal_in_alias_assumption() {
    FormalityTest::new(EMPTY_PROGRAM)
        .prove(
            "forall<T> exists<A> { <T as Iterator>::Item = u32 } => { <A as Iterator>::Item = u32 }",
        )
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_2 => !ty_1} }}"]);
}

#[test]
fn recursive_blanket_impl_yields_an_ambiguous_answer() {
    FormalityTest::new(RECURSIVE_DEBUG)
        .prove_with_max_size("exists<T> {} => {T: Debug}", 10)
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: false, substitution: {} }}"]);
}

#[test]
fn existential_cannot_equal_every_later_universal() {
    FormalityTest::new(EMPTY_PROGRAM)
        .prove("exists<U> {} => {for<T> T = U}")
        .assert_err(expect_test::expect![[r#"
            the rule "assumption" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `assumptions`

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:53:1: no applicable rules for prove_normalize { p: !ty_1, assumptions: {}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "existential-nonvar" at (prove_eq.rs) failed because
              pattern `None` did not match value `Some(!ty_1)`

            the rule "existential-universal" at (prove_eq.rs) failed because
              condition evaluated to false: `env.universe(p) < env.universe(v)`

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:53:1: no applicable rules for prove_normalize { p: ?ty_0, assumptions: {}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}
