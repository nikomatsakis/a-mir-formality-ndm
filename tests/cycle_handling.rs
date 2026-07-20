use a_mir_formality::{crates, FormalityTest};

#[test]
fn method_where_clause_is_not_a_trait_requirement() {
    FormalityTest::new(crates![crate test {
        trait Send {}

        trait Foo {
            fn send(value: Self) -> ()
            where
                Self: Send;
        }

        struct NotSend {}

        impl Foo for NotSend {
            fn send(value: NotSend) -> ()
            where
                NotSend: Send,
            {
                trusted
            }
        }

        test {
            NotSend: Foo
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn impl_condition_may_use_supertrait_associated_type_as_gat_argument() {
    FormalityTest::new(crates![crate test {
        trait Bound {}

        trait PrivSuper {
            type IsU32 : [];
        }

        trait Trait
        where
            Self: PrivSuper,
        {}

        trait Foo {
            type Assoc<T> : []
            where
                T: Bound;
        }

        impl Foo for i32 {
            type Assoc<T> = i32
            where
                T: Bound;
        }

        impl<T> PrivSuper for T {
            type IsU32 = u32;
        }

        impl Bound for u32 {}
        impl Bound for i32 {}

        impl<T> Trait for T
        where
            T: Foo,
            <T as Foo>::Assoc<<T as PrivSuper>::IsU32>: Bound,
        {}

        test {
            i32: Trait
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn impl_condition_may_project_from_the_dictionary_being_constructed() {
    // Declaring this impl is valid: its where-clauses are prerequisites supplied by its caller.
    // Applying it to `i32`, however, requires inspecting the `Trait<i32>` dictionary that the same
    // application is meant to construct. In dictionary-passing form, the recursive dictionary
    // therefore occurs in the type of one of its own inputs rather than behind a constructed
    // output field.
    FormalityTest::new(crates![crate test {
        trait Bound {}

        trait Foo {
            type Assoc<T> : []
            where
                T: Bound;
        }

        trait Trait {
            type IsU32 : [];
        }

        impl Foo for i32 {
            type Assoc<T> = i32
            where
                T: Bound;
        }

        impl Bound for u32 {}
        impl Bound for i32 {}

        impl<T> Trait for T
        where
            T: Foo,
            <T as Foo>::Assoc<<T as Trait>::IsU32>: Bound,
        {
            type IsU32 = u32;
        }

        fn require_trait<T>() -> ()
        where
            T: Trait,
        {
            trusted
        }

        fn main() -> () {
            require_trait::<i32>();
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn post_validation_cycle_cannot_invent_impl_for_ground_type() {
    // `Wrapper<Ground>` has a matching impl, so that goal is recorded as validation evidence.
    // Applying the impl still requires `Ground: Trait`, however, and the exact validation evidence
    // for `Wrapper<Ground>` must not be generalized into an impl for `Ground`.
    FormalityTest::new(crates![crate test {
        trait Trait {}

        struct Ground {}

        struct Wrapper<T> {
            value: T,
        }

        impl<T> Trait for Wrapper<T>
        where
            T: Trait,
        {}

        fn require_trait<T>() -> ()
        where
            T: Trait,
        {
            trusted
        }

        fn main() -> () {
            require_trait::<Wrapper<Ground>>();
        }
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "assumption - predicate" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:7:1: no applicable rules for prove_via_assumption { goal: Wrapper<Ground> = Ground, via: Trait(Wrapper<Ground>), assumptions: {Trait(Wrapper<Ground>)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:91:1: no applicable rules for prove_normalize_via { goal: Wrapper<Ground>, via: Trait(Wrapper<Ground>), assumptions: {Trait(Wrapper<Ground>)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:91:1: no applicable rules for prove_normalize_via { goal: Ground, via: Trait(Wrapper<Ground>), assumptions: {Trait(Wrapper<Ground>)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }

        crates/formality-rust/src/prove/prove/prove/prove_via_impl.rs:48:1: no applicable rules for prove_via_impl { _requested_trait_ref: Trait(Ground), _candidate: ImplCandidate { id: ImplId { crate_index: 1, item_index: 3 }, trait_impl: impl <ty> Trait for Wrapper<^ty0_0> where ^ty0_0 : Trait { } }, _assumptions: {Trait(Wrapper<Ground>)}, _env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]]);
}

#[test]
fn candidate_cannot_validate_its_own_missing_supertrait() {
    // `Ground: Sub` is not a valid dictionary because constructing it requires a `Ground: Super`
    // dictionary, and no such impl exists. In particular, the provisional `Validate(Sub(Ground))`
    // assumption must not use the `Sub => Super` trait requirement to validate that very
    // supertrait obligation. If it could, `main` would observe the nonexistent `Super` dictionary.
    FormalityTest::new(crates![crate test {
        trait Super {}

        trait Sub
        where
            Self: Super,
        {}

        struct Ground {}

        impl Sub for Ground {}

        fn require_super<T>() -> ()
        where
            T: Super,
        {
            trusted
        }

        fn main() -> () {
            require_super::<Ground>();
        }
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "assumption - predicate" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`"#]]);
}

#[test]
fn impl_where_clause_cannot_justify_its_matching_supertrait() {
    // Constructing `Magic(Ground)` requires `Prerequisite(Ground)`, and no such impl exists.
    // Validation must not treat the impl prerequisite as provisional evidence for the matching
    // supertrait:
    //
    //     Validate(Prerequisite(Ground) => Prerequisite(Ground))
    //
    // combined with the post-validation `Magic(Ground) => Prerequisite(Ground)` implied bound
    // would let the prerequisite and supertrait justify one another.
    FormalityTest::new(crates![crate test {
        trait Prerequisite {}

        trait Magic
        where
            Self: Prerequisite,
        {}

        struct Ground {}

        impl<T> Magic for T
        where
            T: Prerequisite,
        {}

        fn require_magic<T>() -> ()
        where
            T: Magic,
        {
            trusted
        }

        fn main() -> () {
            require_magic::<Ground>();
        }
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "assumption - predicate" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:7:1: no applicable rules for prove_via { goal: Prerequisite(Ground), via: validate(Magic(Ground)), assumptions: {validate(Magic(Ground))}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]]);
}

#[test]
fn gat_value_may_delegate_to_ground_trait_impl() {
    // The `u32` GAT delegates to the GAT of its argument. For the ground argument `i32`, that
    // projection has a matching impl and bottoms out at `()`.
    FormalityTest::new(crates![crate test {
        trait Trait {
            type Assoc<T> : []
            where
                T: Trait;
        }

        impl Trait for u32 {
            type Assoc<T> = <T as Trait>::Assoc<T>
            where
                T: Trait;
        }

        impl Trait for i32 {
            type Assoc<T> = ()
            where
                T: Trait;
        }

        test {
            <u32 as Trait>::Assoc<i32> => ()
        }
    }])
    .skip_execute()
    .ok();
}

#[test]
fn infinitely_recursive_associated_type_value_is_currently_accepted() {
    // FIXME: This impl should be rejected because its associated type has no finite, alias-free
    // normal form. For now, the impl requirements check that the value is well formed but do not
    // require it to normalize transitively.
    FormalityTest::new(crates![crate test {
        trait Trait {
            type Assoc : []
            where
                Self: Trait;
        }

        impl Trait for () {
            type Assoc = <() as Trait>::Assoc
            where
                (): Trait;
        }

        test {
            (): Trait
        }
    }])
    .skip_execute()
    .ok();
}
