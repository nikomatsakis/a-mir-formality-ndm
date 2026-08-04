use a_mir_formality::{crates, FormalityTest};
use formality_core::test;

#[test]
fn associated_type_bound_is_implied_by_trait_assumption() {
    FormalityTest::new(crates![crate test {
        #![formality(max_size = 222)]

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

        struct Source {}
        struct Value {}

        impl Ord for Value {}

        impl Foo for Source {
            type Bar = Value;
        }

        test {
            Source: UsesOrd
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn associated_type_bound_cannot_validate_its_own_impl() {
    // This program ought to be rejected because nothing proves `Bad: Ord`. While checking the
    // `Foo for X` impl, the ordinary trait-requirement rule could otherwise use
    //
    //     Foo(T) => Ord(<T as Foo>::Bar)
    //
    // to prove `Bad: Ord` from the very impl being validated. During validation, `Foo(X)` is
    // available only as `Validate(Foo(X))`, so its implied associated-type bound is not available.
    // The call in `main` exhibits how accepting the impl would expose that false proof to outside
    // code.
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

        fn require_ord<T>(value: T) -> ()
        where
            T: Ord,
        {
            trusted
        }

        fn main() -> () {
            let bad: Bad = Bad {};
            require_ord::<Bad>(bad);
        }
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "assumption" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`"#]]);
}

#[test]
fn conditional_associated_type_bound_cannot_validate_its_own_impl() {
    // This is the conditional form of the same exploit. If `MyTrait for X` were accepted, its
    // own `X: MyTrait` condition could be used to obtain the associated-type requirement
    //
    //     Ord(<X as MyTrait>::Gat)
    //
    // and hence prove the otherwise false `Bad: Ord` obligation in `main`.
    FormalityTest::new(crates![crate test {
        trait Ord {}

        struct X {}
        struct Bad {}

        trait MyTrait {
            type Gat : [Ord]
            where
                Self: MyTrait;
        }

        impl MyTrait for X {
            type Gat = Bad
            where
                X: MyTrait;
        }

        fn require_ord<T>(value: T) -> ()
        where
            T: Ord,
        {
            trusted
        }

        fn main() -> () {
            let bad: Bad = Bad {};
            require_ord::<Bad>(bad);
        }
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "assumption" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`"#]]);
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

        test {
            u32: Foo
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn mutually_recursive_impl_requirements_are_valid() {
    FormalityTest::new(crates![crate test {
        trait Foo {}

        trait Bar {
            type Baz : [Foo];
        }

        impl<T> Foo for T
        where
            T: Bar,
        {}

        impl Bar for u32 {
            type Baz = u32;
        }

        test {
            u32: Bar
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn impl_header_substitution_is_applied_before_requirement_validation() {
    // Header matching fixes the impl's `T` to `u32`. Associated-type validation must apply that
    // constraint before checking that the concrete `NeedsRequired<T>` value is well formed.
    FormalityTest::new(crates![crate test {
        trait Required {}
        impl Required for u32 {}

        struct NeedsRequired<T>
        where
            T: Required,
        {}

        trait Family<T> {
            type Output : [];
        }

        impl<T> Family<T> for () {
            type Output = NeedsRequired<T>;
        }

        test {
            (): Family<u32>
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn associated_type_value_wf_may_use_validation_assumption_after_validation() {
    FormalityTest::new(crates![crate test {
        trait Foo {
            type Bar : [];
        }

        struct NeedsFoo<T>
        where
            T: Foo,
        {}

        impl Foo for u32 {
            type Bar = NeedsFoo<u32>;
        }

        test {
            u32: Foo
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn associated_type_value_must_be_well_formed() {
    // Selecting `Foo for ()` must verify the concrete associated type value. Although clients can
    // treat `<() as Foo>::Bar` as well formed without normalizing it, `NeedsRequired<Ground>` is
    // well formed only if `Ground: Required`, and no such impl exists.
    FormalityTest::new(crates![crate test {
        trait Required {}

        struct NeedsRequired<T>
        where
            T: Required,
        {}

        struct Ground {}

        trait Foo {
            type Bar : [];
        }

        impl Foo for () {
            type Bar = NeedsRequired<Ground>;
        }

        fn require_foo<T>() -> ()
        where
            T: Foo,
        {
            trusted
        }

        fn main() -> () {
            require_foo::<()>();
        }
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "assumption" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:7:1: no applicable rules for prove_via_assumption { goal: @ wf(NeedsRequired<Ground>), via: validate(a, Foo(())), assumptions: {validate(a, Foo(()))}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }

        crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:7:1: no applicable rules for prove_via_assumption { goal: Required(Ground), via: validate(a, Foo(())), assumptions: {validate(a, Foo(()))}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }

        crates/formality-rust/src/prove/prove/prove/prove_validate.rs:14:1: no applicable rules for prove_validate { validation_state: a, validate_goal: Required(Ground), assumptions: {validate(a, Foo(()))}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]]);
}

#[test]
fn unused_invalid_associated_type_impl_is_not_eagerly_rejected() {
    // Associated-value semantics are checked when the impl is selected. Merely declaring an impl
    // whose value misses a bound does not select it.
    FormalityTest::new(crates![crate test {
        trait Required {}

        struct Bad {}

        trait Family {
            type Output : [Required];
        }

        impl Family for () {
            type Output = Bad;
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn associated_type_projection_requires_a_valid_impl() {
    // `Foo for X` is not valid because its concrete `Bar` does not implement `Required`.
    // Alias reduction must therefore establish `X: Foo`, rather than obtaining the equation
    // directly from the impl declaration. Otherwise the call in `main` could normalize the
    // projection to `Bad` without validating the impl.
    FormalityTest::new(crates![crate test {
        trait Required {}

        struct X {}
        struct Bad {}

        trait Foo {
            type Bar : [Required];
        }

        impl Foo for X {
            type Bar = Bad;
        }

        fn take_bar(value: <X as Foo>::Bar) -> () {
            trusted
        }

        fn main() -> () {
            let bad: Bad = Bad {};
            take_bar(bad);
        }
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "assumption" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        the rule "assumption" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        crates/formality-rust/src/prove/prove/prove/prove_validate.rs:14:1: no applicable rules for prove_validate { validation_state: a, validate_goal: Required(Bad), assumptions: {validate(a, Foo(X))}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}

#[test]
fn conditional_associated_bound_elaborates_staged_validation_assumption() {
    // The associated type's caller-supplied `T: Sub` condition is stage-B validation evidence,
    // so it can provide its `T: Super` supertrait while validating the associated value. This is
    // distinct from the stage-A evidence for an impl whose dictionary is still being constructed.
    FormalityTest::new(crates![crate test {
        trait Super {}

        trait Sub
        where
            Self: Super,
        {}

        trait Family {
            type Item<T> : [Super]
            where
                T: Sub;
        }

        impl Family for u32 {
            type Item<T> = T
            where
                T: Sub;
        }

        fn require_family<T>() -> ()
        where
            T: Family,
        {
            trusted
        }

        fn main() -> () {
            require_family::<u32>();
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn conditional_associated_bound_accepts_explicit_validation_assumption() {
    // Spelling out the otherwise redundant `T: Super` bound supplies the exact validation
    // assumption needed to validate the associated type requirement.
    FormalityTest::new(crates![crate test {
        trait Super {}

        trait Sub
        where
            Self: Super,
        {}

        trait Family {
            type Item<T> : [Super]
            where
                T: Sub,
                T: Super;
        }

        impl Family for u32 {
            type Item<T> = T
            where
                T: Sub,
                T: Super;
        }

        test {
            u32: Family
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn validation_antecedent_does_not_leak_to_sibling_requirement() {
    // Validating `AIntroduce` temporarily assumes `Bad: Required`, but that implication
    // antecedent is scoped to this requirement. It must not remain available while validating
    // the concrete value of `ZConsume`.
    FormalityTest::new(crates![crate test {
        trait Required {}

        struct Bad {}

        trait Family {
            type AIntroduce : []
            where
                Bad: Required;

            type ZConsume : [Required];
        }

        impl Family for () {
            type AIntroduce = ();
            type ZConsume = Bad;
        }

        fn require_family<T>() -> ()
        where
            T: Family,
        {
            trusted
        }

        fn main() -> () {
            require_family::<()>();
        }
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "assumption" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        crates/formality-rust/src/prove/prove/prove/prove_validate.rs:14:1: no applicable rules for prove_validate { validation_state: a, validate_goal: Required(Bad), assumptions: {validate(a, Family(()))}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]]);
}

#[test]
fn validation_antecedent_does_not_leak_to_impl_where_clause() {
    // The associated type condition validates the value under `Bad: Required`. The same
    // requirement on the impl itself is proven only after validation and must not inherit that
    // requirement-local antecedent.
    FormalityTest::new(crates![crate test {
        trait Required {}

        struct Bad {}

        trait Family {
            type Introduce : []
            where
                Bad: Required;
        }

        impl Family for ()
        where
            Bad: Required,
        {
            type Introduce = ();
        }

        fn require_family<T>() -> ()
        where
            T: Family,
        {
            trusted
        }

        fn main() -> () {
            require_family::<()>();
        }
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "assumption" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:7:1: no applicable rules for prove_via_assumption { goal: Required(Bad), via: Family(()), assumptions: {Family(())}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]]);
}

#[test]
fn impl_header_alias_normalization_enters_post_validation() {
    // Proving `Bar: Target<X>` requires matching `Bar` with `<X as Family>::Out`. Normalizing
    // that alias selects the `Family for X` impl, whose where-clause is the original goal. Alias
    // reduction enters the post-validation phase, so the cycle is guarded and succeeds.
    FormalityTest::new(crates![crate test {
        trait Family {
            type Out : [];
        }

        trait Target<T> {}

        struct X {}
        struct Bar {}

        impl Family for X
        where
            Bar: Target<X>,
        {
            type Out = Bar;
        }

        impl<T> Target<T> for <T as Family>::Out
        where
            T: Family,
        {}

        test {
            Bar: Target<X>
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn impl_header_rigid_type_matches_normalized_goal_projection() {
    // Matching the self type determines `T = X`, but the trait argument still compares the rigid
    // type `Set<X>` from the impl header with an associated-type projection from the goal. The
    // projection has to normalize to expose the matching `Set` constructor.
    FormalityTest::new(crates![crate test {
        #![formality(max_size = 222)]

        trait Iterator {
            type Item : [];
        }

        trait Foo<T> {}

        struct Set<T> {}
        struct IntoIter<T> {}
        struct X {}

        impl<T> Iterator for IntoIter<T> {
            type Item = T;
        }

        impl<T> Foo<Set<T>> for IntoIter<Set<T>> {}

        test {
            IntoIter<Set<X>>: Foo<<IntoIter<Set<X>> as Iterator>::Item>
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn impl_header_normalization_may_infer_binder_from_candidate_cycle() {
    // Here the `Foo` impl's `T` cannot be inferred from its self type. It is learned only after
    // normalizing `Iterator::Item` to `Set<X>`. Normalization in turn requires the exact `Foo`
    // goal whose candidate header is being matched, so the inferred binder value escapes from a
    // guarded evidence cycle.
    FormalityTest::new(crates![crate test {
        #![formality(max_size = 222)]

        trait Iterator {
            type Item : [];
        }

        trait Foo<T> {}

        struct Set<T> {}
        struct IntoIter<T> {}
        struct X {}

        struct Ground {}

        impl<T> Iterator for IntoIter<Set<T>>
        where
            Ground: Foo<<IntoIter<Set<T>> as Iterator>::Item>,
        {
            type Item = Set<T>;
        }

        impl<T> Foo<Set<T>> for Ground {}

        test {
            Ground: Foo<<IntoIter<Set<X>> as Iterator>::Item>
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn post_validation_alias_normalization_still_checks_the_normalized_type() {
    // The same guarded cycle cannot prove a mismatched header: normalization yields `Wrong`, not
    // `Bar`, so the equality required to apply the `Target` impl fails.
    FormalityTest::new(crates![crate test {
        trait Family {
            type Out : [];
        }

        trait Target<T> {}

        struct X {}
        struct Bar {}
        struct Wrong {}

        impl Family for X
        where
            Bar: Target<X>,
        {
            type Out = Wrong;
        }

        impl<T> Target<T> for <T as Family>::Out
        where
            T: Family,
        {}

        test {
            Bar: Target<X>
        }
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "assumption" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        crates/formality-rust/src/prove/prove/prove/prove_via_impl.rs:50:1: no applicable rules for prove_via_impl { _requested_trait_ref: Target(Bar, X), _candidate: ImplCandidate { id: ImplId { crate_index: 1, item_index: 6 }, trait_impl: impl <ty> Target <^ty0_0> for <^ty0_0 as Family>::Out where ^ty0_0 : Family { } }, _assumptions: {}, _env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}

#[test]
fn gat_argument_bound_uses_post_validation_normalization_cycle() {
    // Applying `Unpin for UnpinMe<()>` requires normalizing
    // `<() as Foo>::Assoc<UnpinMe<()>>`. The GAT argument bound is the original `Unpin` goal,
    // but normalization observes it after validation and yields `()`, which implements `Foo`
    // independently.
    FormalityTest::new(crates![crate test {
        trait Unpin {}

        struct UnpinMe<T> {
            value: T,
        }

        trait Foo {
            type Assoc<T> : []
            where
                T: Unpin;
        }

        impl<T> Unpin for UnpinMe<T>
        where
            T: Foo,
            <T as Foo>::Assoc<UnpinMe<T>>: Foo,
        {}

        impl Foo for () {
            type Assoc<T> = ()
            where
                T: Unpin;
        }

        test {
            UnpinMe<()>: Unpin
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn gat_value_may_project_from_validation_argument_bound() {
    FormalityTest::new(crates![crate test {
        trait Iterator {
            type Item : [];
        }

        trait Family {
            type Assoc<T> : []
            where
                T: Iterator;
        }

        impl Family for () {
            type Assoc<T> = <T as Iterator>::Item
            where
                T: Iterator;
        }

        test {
            (): Family
        }
    }])
    .skip_execute()
    .ok();
}
