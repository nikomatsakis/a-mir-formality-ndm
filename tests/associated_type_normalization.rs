use a_mir_formality::{crates, test_where_clause, FormalityTest};
use formality_core::test;

const MIRROR: &str = "[
    crate core {
        trait Mirror {
            type Assoc : [];
        }

        impl<T> Mirror for T {
            type Assoc = T;
        }
    }
]";

#[test]
fn test_mirror_normalizes_u32_to_u32() {
    test_where_clause(MIRROR, "exists<T> {} => {<u32 as Mirror>::Assoc = T}")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => u32} }, Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => <u32 as Mirror>::Assoc} }}"]);
}

#[test]
fn exact_alias_cycle_has_no_codegen_normal_form() {
    FormalityTest::new(crates![crate test {
        trait Family {
            type Output : [];
        }

        impl Family for () {
            type Output = <() as Family>::Output;
        }

        fn main() -> () {
            let value: <() as Family>::Output;
        }
    }])
    .codegen_err(expect_test::expect![[r#"
        the rule "function" at (mod.rs) failed because
          the rule "alias" at (prove_fully_normalize.rs) failed because
            cyclic proof attempt: `prove_fully_normalize_ty { ty: <() as Family>::Output, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }`"#]])
    .ok();
}

#[test]
fn mutual_alias_cycle_has_no_codegen_normal_form() {
    FormalityTest::new(crates![crate test {
        trait First {
            type Output : [];
        }

        trait Second {
            type Output : [];
        }

        impl First for ()
        where
            (): Second,
        {
            type Output = <() as Second>::Output;
        }

        impl Second for ()
        where
            (): First,
        {
            type Output = <() as First>::Output;
        }

        fn main() -> () {
            let value: <() as First>::Output;
        }
    }])
    .codegen_err(expect_test::expect![[r#"
        the rule "function" at (mod.rs) failed because
          the rule "alias" at (prove_fully_normalize.rs) failed because
            cyclic proof attempt: `prove_fully_normalize_ty { ty: <() as First>::Output, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }`"#]])
    .ok();
}
