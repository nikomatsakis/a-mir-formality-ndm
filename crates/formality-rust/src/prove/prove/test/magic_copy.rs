use crate::rust::term;
use expect_test::expect;
use formality_macros::test;
use std::sync::Arc;

use crate::prove::prove::decls::Program;

use crate::prove::prove::test_util::test_prove;

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Program {
    Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Copy where {}"),
            term("trait Magic where Self : Copy {}"),
            term("impl<T> Magic for T where T : Magic {}"),
            term("impl Copy for u32 {}"),
        ])),
        ..Program::empty()
    }
}

#[test]
fn all_t_is_not_magic_without_copy_for_all_t() {
    // The recursive `Magic(T)` prerequisite cannot validate the missing `Copy(T)` supertrait.
    test_prove(decls(), term("{} => {for<T> Magic(T)}")).assert_err(expect![[r#"
        the rule "assumption - predicate" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:7:1: no applicable rules for prove_via_assumption { goal: Copy(!ty_0), via: validate(a, Magic(!ty_0)), assumptions: {validate(a, Magic(!ty_0))}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via_impl.rs:50:1: no applicable rules for prove_via_impl { _requested_trait_ref: Copy(!ty_0), _candidate: ImplCandidate { id: ImplId { crate_index: 0, item_index: 3 }, trait_impl: impl Copy for u32 { } }, _assumptions: {validate(a, Magic(!ty_0))}, _env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}

#[test]
fn invalid_magic_impl_does_not_imply_copy_for_all_t() {
    // In particular, the invalid recursive `Magic` impl must not expose its unproven `Copy`
    // supertrait to ordinary implied-bound reasoning.
    test_prove(decls(), term("{} => {for<T> Copy(T)}")).assert_err(expect![[r#"
        the rule "assumption - predicate" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        crates/formality-rust/src/prove/prove/prove/prove_via_impl.rs:50:1: no applicable rules for prove_via_impl { _requested_trait_ref: Copy(!ty_1), _candidate: ImplCandidate { id: ImplId { crate_index: 0, item_index: 3 }, trait_impl: impl Copy for u32 { } }, _assumptions: {}, _env: Env { variables: [!ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}
