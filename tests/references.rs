use a_mir_formality::{crates, FormalityTest};

/// See <https://github.com/rust-lang/a-mir-formality/issues/312> for more information.
#[test]
fn recursive_reference_validity() {
    FormalityTest::new(crates![crate core {
                trait Trait {}
                struct A<X> where X: Trait {}
                fn invalid<'a, X>(x: &'a A<X>) -> ()
                where
                    X: 'a,
                {}
            }]).err(expect_test::expect![[r#"
                crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:7:1: no applicable rules for prove_via_assumption { goal: @ wf(&!lt_1 A<!ty_0>), via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:7:1: no applicable rules for prove_via_assumption { goal: @ wf(A<!ty_0>), via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:7:1: no applicable rules for prove_via_assumption { goal: Trait(!ty_0), via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}

#[test]
fn reference_validity() {
    FormalityTest::new(crates![crate core {
        trait Trait {}
        struct A<X> where X: Trait {}
        fn valid<'a, X>(x: &'a A<X>) -> ()
        where
            X: 'a,
            X: Trait,
        {}
    }])
    .skip_execute()
    .ok();
}
