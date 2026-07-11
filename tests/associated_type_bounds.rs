use a_mir_formality::{crates, FormalityTest};

#[test]
fn associated_type_bound_is_implied_by_trait_assumption() {
    FormalityTest::new(crates![crate test {
        trait Ord {}

        trait Foo {
            type Bar : [Ord];
        }

        trait UsesOrd {
            type Value : [Ord];
        }

        impl<T> UsesOrd for T
        where
            T: Foo,
        {
            type Value = <T as Foo>::Bar;
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn associated_type_bound_cannot_validate_its_own_impl() {
    FormalityTest::new(crates![crate test {
        trait Ord {}

        struct X {}
        struct Bad {}

        trait Foo {
            type Bar : [Ord];
        }

        impl Foo for X {
            type Bar = Bad;
        }
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "trait requirement" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `decls.trait_requirements()`"#]]);
}

#[test]
fn recursive_associated_type_bound_is_valid() {
    FormalityTest::new(crates![crate test {
        trait Foo {
            type Bar : [Foo];
        }

        impl Foo for u32 {
            type Bar = u32;
        }
    }])
    .skip_execute()
    .ok();
}
