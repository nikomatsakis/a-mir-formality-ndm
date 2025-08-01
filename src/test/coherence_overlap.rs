#![allow(non_snake_case)]

use formality_core::test;

#[test]
fn u32_not_u32_impls() {
    crate::assert_err!(
        // Test that a positive and negative impl for the same type (`u32`, here) is rejected.
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl !Foo for u32 {}
            }
        ]

        [
            "check_trait_impl",
            "impl Foo for u32",
        ]

        expect_test::expect![[r#"
            check_trait_impl(impl Foo for u32 { })

            Caused by:
                judgment `negation_via_failure` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    found an unconditionally true solution Constraints { env: Env { variables: [], bias: Completeness, pending: [] }, known_true: true, substitution: {} }"#]]
    )
}

#[test]
fn neg_CoreTrait_for_CoreStruct_implies_no_overlap() {
    crate::assert_ok!(
        //@check-pass
        // Variant of foo_crate_cannot_assume_CoreStruct_does_not_impl_CoreTrait
        // where there is a negative impl, so it is accepted.
        [
            crate core {
                trait CoreTrait {}
                struct CoreStruct {}
                impl !CoreTrait for CoreStruct {}
            },
            crate foo {
                trait FooTrait {}
                impl<ty T> FooTrait for T where T: CoreTrait {}
                impl FooTrait for CoreStruct {}
            }
        ]

        expect_test::expect!["()"]
    )
}

#[test]
fn foo_crate_cannot_assume_CoreStruct_does_not_impl_CoreTrait() {
    crate::assert_err!(
        [
            crate core {
                trait CoreTrait {}
                struct CoreStruct {}
            },
            crate foo {
                trait FooTrait {}
                impl<ty T> FooTrait for T where T: CoreTrait {}
                impl FooTrait for CoreStruct {}
            }
        ]

        [
            "impls may overlap",
        ]

        expect_test::expect![[r#"
            impls may overlap:
            impl <ty> FooTrait for ^ty0_0 where ^ty0_0 : CoreTrait { }
            impl FooTrait for CoreStruct { }"#]]
    )
}

#[test]
fn T_where_Foo_not_u32_impls() {
    crate::assert_err!(
        // Test positive impl that has a where-clause which checks for itself,
        // i.e., `T: Foo where T: Foo`. This `T: Foo` where-clause isn't harmful
        // in the coinductive interpretation of trait matching, it actually
        // doesn't change the meaning of the impl at all. However, this formulation
        // was erroneously accepted by an earlier variant of negative impls.
        [
            crate core {
                trait Foo {}
                impl<ty T> Foo for T where T: Foo {}
                impl !Foo for u32 {}
            }
        ]

        [
            "check_trait_impl",
            "Foo for ^ty0_0",
        ]

        expect_test::expect![[r#"
            check_trait_impl(impl <ty> Foo for ^ty0_0 where ^ty0_0 : Foo { })

            Caused by:
                failed to prove {! Foo(!ty_1)} given {Foo(!ty_1)}, got {Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [] }, known_true: false, substitution: {} }}"#]]
    )
}

#[test]
fn u32_T_where_T_Is_impls() {
    crate::assert_err!(
        // Test that we detect "indirect" overlap -- here `Foo` is implemented for `u32`
        // and also all `T: Is`, and `u32: Is`.
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl<ty T> Foo for T where T: Is {}

                trait Is {}
                impl Is for u32 {}
            }
        ]

        [
            "impls may overlap",
        ]

        expect_test::expect![[r#"
            impls may overlap:
            impl Foo for u32 { }
            impl <ty> Foo for ^ty0_0 where ^ty0_0 : Is { }"#]]
    )
}

#[test]
fn u32_T_where_T_Not_impls() {
    crate::assert_ok!(
        //@check-pass

        // Test that, within a crate, we are able to rely on the fact
        // that `u32: Not` is not implemented.
        //
        // See also test_foo_crate_cannot_assume_CoreStruct_does_not_impl_CoreTrait
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl<ty T> Foo for T where T: Not {}

                trait Not {}
            }
        ]

        expect_test::expect!["()"]
    )
}

#[test]
fn u32_u32_impls() {
    crate::assert_err!(
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl Foo for u32 {}
            }
        ]

        [
            "duplicate impl",
        ]

        expect_test::expect!["duplicate impl in current crate: impl Foo for u32 { }"]
    )
}

#[test]
fn u32_i32_impls() {
    crate::assert_ok!(
        //@check-pass
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl Foo for i32 {}
            }
        ]

        expect_test::expect!["()"]
    )
}

#[test]
fn u32_T_impls() {
    crate::assert_err!(
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl<ty T> Foo for T {}
            }
        ]

        [
            "impls may overlap",
        ]

        expect_test::expect![[r#"
            impls may overlap:
            impl Foo for u32 { }
            impl <ty> Foo for ^ty0_0 { }"#]]
    )
}

#[test]
fn T_and_T_bar() {
    crate::assert_err! {
        [
            crate core {
                trait Foo { }

                trait Bar { }

                impl<ty T> Foo for T { }

                impl<ty T> Foo for T where T: Bar { }
            }
        ]

        [
            "impls may overlap",
        ]

        expect_test::expect![[r#"
            impls may overlap:
            impl <ty> Foo for ^ty0_0 { }
            impl <ty> Foo for ^ty0_0 where ^ty0_0 : Bar { }"#]]
    }
}

#[test]
fn T_and_Local_Bar_T() {
    crate::assert_err! {
        [
            crate core {
                trait Foo { }

                trait Bar<ty U> { }

                impl<ty T> Foo for T { }

                impl<ty T> Foo for T where LocalType: Bar<T> { }

                struct LocalType { }
            }
        ]

        [
            "impls may overlap",
        ]

        expect_test::expect![[r#"
            impls may overlap:
            impl <ty> Foo for ^ty0_0 { }
            impl <ty> Foo for ^ty0_0 where LocalType : Bar <^ty0_0> { }"#]]
    }
}

#[test]
fn is_local_unknowable_trait_ref() {
    crate::assert_ok! {
        [
            crate core {
                trait Project {
                    type Assoc: [];
                }

                impl<ty T> Project for T {
                    type Assoc = T;
                }

                trait Foo<ty U> { }
            },
            crate foo {
                struct LocalType {}

                trait Overlap<ty U> {}
                impl<ty T, ty U> Overlap<U> for T
                where
                    <T as Project>::Assoc: Foo<U> {}
                impl<ty T> Overlap<LocalType> for () {}
            }
        ]

        expect_test::expect!["()"]
    }
}

#[test]
fn is_local_with_unconstrained_self_ty_blanket_impl() {
    // TODO: this test should pass imho
    crate::assert_err! {
        [
            crate core {
                trait Project {
                    type Assoc: [];
                }

                impl<ty T> Project for T {
                    type Assoc = ();
                }

                trait Foo<ty U> { }
            },
            crate foo {
                struct LocalType {}
                impl Foo<LocalType> for () {}

                trait Overlap<ty U> {}
                impl<ty T, ty U> Overlap<U> for T
                where
                    <T as Project>::Assoc: Foo<U> {}
                impl<ty T> Overlap<LocalType> for T {}
            }
        ]

        [        ]

        expect_test::expect![[r#"
            impls may overlap:
            impl <ty, ty> Overlap <^ty0_1> for ^ty0_0 where <^ty0_0 as Project>::Assoc : Foo <^ty0_1> { }
            impl <ty> Overlap <LocalType> for ^ty0_0 { }"#]]
    }
}
