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
