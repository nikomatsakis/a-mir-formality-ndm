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
    // The recursive `T: Magic` prerequisite cannot validate the missing `T: Copy` supertrait.
    test_prove(decls(), term("{} => {for<T> T: Magic}")).assert_err(expect![[r#"
        the rule "assumption" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        crates/formality-rust/src/prove/prove/prove/prove_via_impl.rs:45:1: no applicable rules for prove_via_impl { requested_trait_ref: !ty_1: Magic, candidate: ImplCandidate { id: ImplId { crate_index: 0, item_index: 2 }, trait_impl: impl <ty> Magic for ^ty0_0 where ^ty0_0 : Magic { } }, assumptions: {}, env: Env { variables: [!ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}

#[test]
fn invalid_magic_impl_does_not_imply_copy_for_all_t() {
    // In particular, the invalid recursive `Magic` impl must not expose its unproven `Copy`
    // supertrait to ordinary implied-bound reasoning.
    test_prove(decls(), term("{} => {for<T> T: Copy}")).assert_err(expect![[r#"
        the rule "assumption" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        crates/formality-rust/src/prove/prove/prove/prove_via_impl.rs:45:1: no applicable rules for prove_via_impl { requested_trait_ref: !ty_1: Copy, candidate: ImplCandidate { id: ImplId { crate_index: 0, item_index: 3 }, trait_impl: impl Copy for u32 { } }, assumptions: {}, env: Env { variables: [!ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}
