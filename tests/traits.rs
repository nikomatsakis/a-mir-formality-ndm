use a_mir_formality::{crates, FormalityTest};

#[test]
fn trait_with_valid_fn() {
    FormalityTest::new(crates![
        crate core {
            trait A {
                fn a() -> ();
            }
        }
    ])
    .skip_execute()
    .ok();
}

#[test]
fn trait_with_valid_associated_type() {
    FormalityTest::new(crates![
        crate core {
            trait A {
                type Assoc : [];
            }
        }
    ])
    .skip_execute()
    .ok();
}

#[test]
#[should_panic(expected = "but no impl provides that trait-ref")]
fn bare_test_trait_ref_requires_an_impl_application() {
    FormalityTest::new(crates![crate test {
        trait Foo {}

        test<T>
        where
            T: Foo,
        {
            T: Foo
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn prove_test_goal_does_not_require_an_impl_application() {
    FormalityTest::new(crates![crate test {
        trait Foo {}

        test<T>
        where
            T: Foo,
        {
            prove(T: Foo)
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
#[ignore = "ensures bounds WF check not yet implemented, see FIXME(#228)"]
fn trait_with_ill_formed_where_clause() {
    FormalityTest::new(crates![
        crate core {
            trait A<T> where T: B {}
            trait B {}
            trait C {
                type Assoc : [ A<u32> ];
            }
        }
    ])
    .err(expect_test::expect![[r#"
            the rule "trait requirement" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_requirements()`"#]]);
}
