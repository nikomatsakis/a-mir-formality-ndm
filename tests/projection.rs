use a_mir_formality::test_where_clause;

const NORMALIZE_BASIC: &str = "[
    crate test {
        trait Iterator {
            type Item : [];
        }

        struct Vec<T> {}

        struct Foo {}

        impl<T> Iterator for Vec<T> {
            type Item = T;
        }
    }
]";

#[test]
fn normalize_basic() {
    test_where_clause(
        NORMALIZE_BASIC,
        "forall<T> exists<U> {} => { <Vec<T> as Iterator>::Item = U }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_2 => <Vec<!ty_1> as Iterator>::Item} }, Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_2 => !ty_1} }}"]);

    test_where_clause(
        NORMALIZE_BASIC,
        "forall<T> {} => { Vec<T>: Iterator, <Vec<T> as Iterator>::Item = T }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);

    test_where_clause(
        NORMALIZE_BASIC,
        "forall<T> { T: Iterator, <T as Iterator>::Item = Foo } => { <T as Iterator>::Item = Foo }",
    ).assert_ok(
        expect_test::expect!["{Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]
    );

    test_where_clause(
        NORMALIZE_BASIC,
        "forall<T> exists<U> { T: Iterator } => { <T as Iterator>::Item = U }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_2 => <!ty_1 as Iterator>::Item} }}"]);

    test_where_clause(
        NORMALIZE_BASIC,
        "forall<T> { T: Iterator } => { <T as Iterator>::Item = <T as Iterator>::Item }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);

    // Besides `U = T`, selecting `Iterator for Vec<X>` yields the valid solution
    // `U = Vec<<T as Iterator>::Item>`.
    test_where_clause(
        NORMALIZE_BASIC,
        "forall<T> exists<U> { T: Iterator } => { <T as Iterator>::Item = <U as Iterator>::Item }",
    ).assert_ok(
    expect_test::expect!["{Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_2 => !ty_1} }, Constraints { env: Env { variables: [!ty_1, ?ty_3, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_2 => Vec<<!ty_1 as Iterator>::Item>, ?ty_3 => <!ty_1 as Iterator>::Item} }}"]);
}

const NORMALIZE_INTO_ITERATOR: &str = "[
    crate test {
        trait IntoIterator {
            type Item : [];
        }

        trait Iterator {
            type Item : [];
        }

        struct Vec<T> {}

        struct Foo {}

        impl<T> IntoIterator for Vec<T> {
            type Item = T;
        }

        impl<T> IntoIterator for T where  T: Iterator  {
            type Item = <T as Iterator>::Item;
        }
    }
]";

#[test]
fn normalize_into_iterator() {
    // The blanket impl cannot contribute a value: its own `Vec<T>: Iterator` condition is not
    // known. In particular, applicability cannot be borrowed from the direct `Vec<T>` impl.
    test_where_clause(
        NORMALIZE_INTO_ITERATOR,
        "forall<T> exists<U> {} => { <Vec<T> as IntoIterator>::Item = U }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_2 => <Vec<!ty_1> as IntoIterator>::Item} }, Constraints { env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_2 => !ty_1} }}"]);
}

const PROJECTION_EQUALITY: &str = "[
    crate test {
        trait Trait1<> {
            type Type : [];
        }
        trait Trait2<T> {}
        impl<T, U> Trait2<T> for U where  U: Trait1<>, <S as Trait1>::Type => T  {}
        struct S {}
        impl Trait1<> for S {
            type Type = u32;
        }
    }
]";

#[test]
fn projection_equality() {
    test_where_clause(
        PROJECTION_EQUALITY,
        "exists<U> {} => { S: Trait1, <S as Trait1<>>::Type = U }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => u32} }, Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => <S as Trait1>::Type} }}"]);

    test_where_clause(PROJECTION_EQUALITY, "exists<U> {} => { S: Trait2<U> }")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => u32} }, Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => <S as Trait1>::Type} }}"]);
}
