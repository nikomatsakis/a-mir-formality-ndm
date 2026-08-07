use a_mir_formality::{crates, test_program_ok, FormalityTest};
use formality_rust::{codegen::codegen_program, grammar::Crates, rust::try_term};

fn assert_monomorphizes_if_accepted(input: &str) {
    let crates: Crates = try_term(input).expect("test program should parse");

    if test_program_ok(input).is_ok() {
        codegen_program(&crates)
            .expect("type checking accepted evidence that codegen could not monomorphize");
    }
}

fn assert_accepted_and_monomorphizes(input: &str) {
    let crates: Crates = try_term(input).expect("test program should parse");

    let _ = test_program_ok(input).expect("test program should type check");
    codegen_program(&crates)
        .expect("type checking accepted evidence that codegen could not monomorphize");
}

#[test]
fn coinductive_impl_evidence_monomorphizes() {
    FormalityTest::new(crates![crate test {
        trait Value {
            fn value() -> i32;
        }

        struct Ground {}

        impl<T> Value for T
        where
            T: Value,
        {
            fn value() -> i32 {
                return 22 _ i32;
            }
        }

        fn main() -> () {
            println!(<Ground as Value>::value());
        }
    }])
    .expect_output("22\n")
    .ok();
}

#[test]
fn coinductive_impl_with_an_associated_value_monomorphizes() {
    FormalityTest::new(crates![crate test {
        trait Value {
            type Output : [];

            fn value() -> i32;
        }

        struct Ground {}

        impl<T> Value for T
        where
            T: Value,
        {
            type Output = u32;

            fn value() -> i32 {
                return 22 _ i32;
            }
        }

        fn main() -> () {
            println!(<Ground as Value>::value());
        }
    }])
    .expect_output("22\n")
    .ok();
}

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
    assert_accepted_and_monomorphizes(crates![crate test {
        #![formality(max_size = 222)]

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
        }

        fn main() -> () {
            require_trait::<i32>();
        }
    }]);
}

#[test]
fn rooted_validation_cycle_cannot_invent_impl_for_ground_type() {
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
        the rule "assumption" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:9:1: no applicable rules for prove_via_assumption { goal: Ground: Trait, via: Supertraits[Trait](Wrapper<Ground>: Trait), assumptions: {Supertraits[Trait](Wrapper<Ground>: Trait)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }

        crates/formality-rust/src/prove/prove/prove/prove_via_impl.rs:45:1: no applicable rules for prove_via_impl { requested_trait_ref: Ground: Trait, candidate: ImplCandidate { id: ImplId { crate_index: 1, item_index: 3 }, trait_impl: impl <ty> Trait for Wrapper<^ty0_0> where ^ty0_0 : Trait { } }, assumptions: {Supertraits[Trait](Wrapper<Ground>: Trait)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]]);
}

#[test]
fn candidate_cannot_validate_its_own_missing_supertrait() {
    // `Ground: Sub` is not a valid dictionary because constructing it requires a `Ground: Super`
    // dictionary, and no such impl exists. In particular, the provisional
    // `Supertraits[Sub](Ground: Sub)`
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
    .err(expect_test::expect!["crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:9:1: no applicable rules for prove_via_assumption { goal: Ground: Super, via: Supertraits[Sub](Ground: Sub), assumptions: {Supertraits[Sub](Ground: Sub)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }"]);
}

#[test]
fn impl_where_clause_cannot_justify_its_matching_supertrait() {
    // Constructing `Ground: Magic` requires `Ground: Prerequisite`, and no such impl exists.
    // Validation must not treat the impl prerequisite as provisional evidence for the matching
    // supertrait:
    //
    //     Supertraits[Magic](Ground: Prerequisite => Ground: Prerequisite)
    //
    // combined with unrestricted `Ground: Magic => Ground: Prerequisite` implied-bound
    // elaboration would let the prerequisite and supertrait justify one another.
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
        the rule "assumption" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `assumptions`

        crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:9:1: no applicable rules for prove_via_assumption { goal: Ground: Prerequisite, via: Supertraits[Magic](Ground: Magic), assumptions: {Supertraits[Magic](Ground: Magic)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]]);
}

macro_rules! grounded_supertrait_chain_program {
    ($ty:ident) => {
        crates![crate test {
            trait Base {}
            trait Debug {}

            trait A
            where
                Self: Base,
            {}

            trait B
            where
                Self: Base,
            {}

            impl<T> A for T
            where
                T: B,
            {}

            impl<T> B for T
            where
                T: Debug,
            {}

            impl<T> Base for T
            where
                T: Debug,
            {}

            struct Foo {}
            struct Bar {}

            impl Debug for Foo {}

            test {
                $ty: A
            }
        }]
    };
}

#[test]
fn grounded_supertrait_chain_accepts_type_with_debug_impl() {
    // `A` does not repeat the seemingly redundant `T: Base` condition. For `Foo`, the `B` and
    // `Base` evidence is ultimately grounded by its `Debug` impl.
    FormalityTest::new(grounded_supertrait_chain_program!(Foo))
        .skip_execute()
        .ok();
}

#[test]
fn grounded_supertrait_chain_rejects_type_without_debug_impl() {
    // `Bar` has no `Debug` impl, so neither `B` nor `A` can be constructed for it.
    FormalityTest::new(grounded_supertrait_chain_program!(Bar))
        .skip_execute()
        .err(expect_test::expect![[r#"
            the rule "assumption" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `assumptions`

            crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:9:1: no applicable rules for prove_via_assumption { goal: Bar: B, via: Supertraits[A](Bar: A), assumptions: {Supertraits[A](Bar: A)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:9:1: no applicable rules for prove_via_assumption { goal: Bar: Debug, via: Supertraits[A](Bar: A), assumptions: {Supertraits[A](Bar: A), Supertraits[B](Bar: B)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via_assumption.rs:9:1: no applicable rules for prove_via_assumption { goal: Bar: Debug, via: Supertraits[B](Bar: B), assumptions: {Supertraits[A](Bar: A), Supertraits[B](Bar: B)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via_impl.rs:45:1: no applicable rules for prove_via_impl { requested_trait_ref: Bar: Debug, candidate: ImplCandidate { id: ImplId { crate_index: 1, item_index: 9 }, trait_impl: impl Debug for Foo { } }, assumptions: {Supertraits[A](Bar: A), Supertraits[B](Bar: B)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
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
            prove(<u32 as Trait>::Assoc<i32> => ())
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

#[test]
fn associated_type_ensures_cycle_never_produces_unmonomorphizable_evidence() {
    // The `Ord for Bad` candidate has only a `Supertraits[Ord]` view while checking its
    // `PartialOrd` field. Its associated-type where-clause must not turn that provisional evidence
    // into the missing `Bad: PartialOrd` dictionary. Rejecting this program is fine; if proof
    // search accepts it, codegen must be able to select concrete evidence for the call.
    assert_monomorphizes_if_accepted(crates![crate test {
        trait PartialOrd {
            fn probe(value: Self) -> i32;
        }

        trait Ord
        where
            Self: PartialOrd,
        {}

        trait Family {
            type Gat<T>: [PartialOrd]
            where
                T: Ord;
        }

        impl Family for u32 {
            type Gat<T> = T
            where
                T: Ord;
        }

        struct Bad {}

        impl Ord for Bad
        where
            <u32 as Family>::Gat<Bad>: PartialOrd,
        {}

        fn call_probe<T>(value: T) -> i32
        where
            T: Ord,
        {
            return <T as PartialOrd>::probe(value);
        }

        fn main() -> () {
            let bad: Bad = Bad {};
            println!(call_probe::<Bad>(bad));
        }
    }]);
}

#[test]
fn associated_type_supertrait_cycle_never_produces_unmonomorphizable_evidence() {
    assert_monomorphizes_if_accepted(crates![crate test {
        #![formality(max_size = 32)]

        trait Target {
            fn probe() -> ();
        }

        trait Bridge
        where
            Self: Target,
        {}

        trait Entry
        where
            Self: Bridge,
        {}

        trait Family {
            type Item<T>: [Target]
            where
                T: Entry;
        }

        impl Family for () {
            type Item<T> = T
            where
                T: Entry;
        }

        struct Ground {}

        impl Entry for Ground
        where
            <() as Family>::Item<Ground>: Target,
        {}

        impl Bridge for Ground
        where
            Ground: Target,
        {}

        fn require_entry<T>() -> ()
        where
            T: Entry,
        {
            return <T as Target>::probe();
        }

        fn main() -> () {
            require_entry::<Ground>();
        }
    }]);
}
