use crate::grammar::{Parameter, Relation, Wcs};
use crate::rust::term;
use expect_test::expect;
use formality_core::test;
use std::sync::Arc;

use crate::prove::prove::{decls::Program, prove::prove, Env};

fn decls() -> Program {
    Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Foo where {}"),
            term("impl Foo for u32 {}"),
            term("struct X<T> where T : Foo {}"),
        ])),
        ..Program::empty()
    }
}

#[test]
fn well_formed_adt() {
    let assumptions: Wcs = Wcs::t();
    let goal: Parameter = term("X<u32>");
    let constraints = prove(
        decls(),
        Env::default(),
        assumptions,
        Relation::WellFormed(goal),
    );
    constraints.assert_ok(
    expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

#[test]
fn not_well_formed_adt() {
    let assumptions: Wcs = Wcs::t();
    let goal: Parameter = term("X<u64>");
    prove(
        decls(),
        Env::default(),
        assumptions,
        Relation::WellFormed(goal),
    )
    .assert_err(expect![[r#"
        the rule "assumption" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        the rule "assumption" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        crates/formality-rust/src/prove/prove/prove/prove_via_impl.rs:46:1: no applicable rules for prove_via_impl { _requested_trait_ref: Foo(u64), _candidate: ImplCandidate { id: ImplId { crate_index: 0, item_index: 1 }, trait_impl: impl Foo for u32 { } }, _assumptions: {}, _env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}
