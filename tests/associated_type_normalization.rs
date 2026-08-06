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
fn normalization_distinguishes_impl_and_gat_parameters() {
    // Header matching determines `T`, the impl where-clause determines `U`, and the projection
    // directly supplies `V`. Normalization must apply all three before reading the associated
    // value from the matched impl.
    FormalityTest::new(crates![crate test {
        trait Witness<T> {}
        impl Witness<i32> for () {}

        struct Triple<T, U, V> {}
        struct Source {}

        trait Family<T> {
            type Assoc<V> : [];
        }

        impl<T, U> Family<T> for Source
        where
            (): Witness<U>,
        {
            type Assoc<V> = Triple<T, U, V>;
        }

        test {
            prove(<Source as Family<u32>>::Assoc<bool> => Triple<u32, i32, bool>)
        }
    }])
    .skip_execute()
    .ok();
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

        // Each impl explicitly supplies the other dictionary needed to validate its
        // associated value. This makes both impls well-formed without giving the
        // resulting aliases a finite codegen normal form.
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
