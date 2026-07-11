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
    // FIXME(ndm): unsound
    //
    // This program ought to be rejected because nothing proves `Bad: Ord`. While checking the
    // `Foo for X` impl, however, the ordinary trait-requirement rule uses
    //
    //     Foo(T) => Ord(<T as Foo>::Bar)
    //
    // to prove `Bad: Ord`: it normalizes `<X as Foo>::Bar` to `Bad` using this impl and then
    // proves `X: Foo` using the same impl. The impl therefore validates its own associated-type
    // requirement. Once requirement validation is separated from ordinary proving, this test
    // should expect an error again.
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
    .ok();
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
