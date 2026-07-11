use a_mir_formality::{crates, FormalityTest};

#[test]
fn hello_world() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            println!(22 _ i32);
        }
    }])
    .expect_output("22\n")
    .ok()
}

#[test]
fn booleans_and_multiple_prints() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            println!(1 _ i32);
            println!(true);
            println!(false);
        }
    }])
    .expect_output("1\ntrue\nfalse\n")
    .ok()
}

#[test]
fn let_bindings() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 42 _ i32;
            println!(x);
        }
    }])
    .expect_output("42\n")
    .ok()
}

#[test]
fn assignment() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 1 _ i32;
            x = 2 _ i32;
            println!(x);
        }
    }])
    .expect_output("2\n")
    .ok()
}

#[test]
fn function_calls() {
    FormalityTest::new(crates![crate test {
        fn add_one(x: i32) -> i32 {
            return x; // FIXME: this should probably be `x + 1`
        }
        fn main() -> () {
            let y: i32 = add_one(1 _ i32);
            println!(y);
        }
    }])
    .expect_output("1\n")
    .ok()
}

#[test]
fn generic_function_calls() {
    FormalityTest::new(crates![crate test {
        fn identity<T>(x: T) -> T {
            return x;
        }
        fn main() -> () {
            let y: i32 = identity::<i32>(42 _ i32);
            println!(y);
        }
    }])
    .expect_output("42\n")
    .ok()
}

#[test]
fn aliases_in_free_function_keys_are_fully_normalized() {
    FormalityTest::new(crates![crate test {
        trait Family {
            type Output : [];
        }

        impl Family for () {
            type Output = i32;
        }

        fn identity<T>(value: T) -> T {
            return value;
        }

        fn main() -> () {
            let first: i32 = identity::<<() as Family>::Output>(22 _ i32);
            let second: i32 = identity::<i32>(44 _ i32);
            println!(first);
            println!(second);
        }
    }])
    .rustc_ok()
    .expect_output("22\n44\n")
    .ok()
}

#[test]
fn qualified_trait_call_executes_impl_method() {
    FormalityTest::new(crates![crate test {
        trait Bar {
            fn bar() -> u32;
        }

        struct Ground {}

        impl Bar for Ground {
            fn bar() -> u32 {
                return 22 _ u32;
            }
        }

        fn main() -> () {
            println!(<Ground as Bar>::bar());
        }
    }])
    .rustc_ok()
    .expect_output("22\n")
    .ok()
}

#[test]
fn qualified_trait_call_without_impl_is_rejected() {
    FormalityTest::new(crates![crate test {
        trait Bar {
            fn bar() -> u32;
        }

        struct Ground {}

        fn main() -> () {
            println!(<Ground as Bar>::bar());
        }
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove/prove/prove_wc.rs:26:1: no applicable rules for prove_wc { goal: Bar(Ground), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]])
}

#[test]
fn qualified_trait_call_in_generic_body_executes_after_monomorphization() {
    FormalityTest::new(crates![crate test {
        trait Bar {
            fn bar() -> u32;
        }

        struct Ground {}

        impl Bar for Ground {
            fn bar() -> u32 {
                return 22 _ u32;
            }
        }

        fn foo<T>() -> u32
        where
            T: Bar,
        {
            return <T as Bar>::bar();
        }

        fn main() -> () {
            println!(foo::<Ground>());
        }
    }])
    .rustc_ok()
    .expect_output("22\n")
    .ok()
}

#[test]
fn qualified_trait_call_preserves_trait_arguments() {
    FormalityTest::new(crates![crate test {
        trait Convert<T> {
            fn convert(value: T) -> T;
        }

        impl Convert<i32> for () {
            fn convert(value: i32) -> i32 {
                return value;
            }
        }

        fn main() -> () {
            println!(<() as Convert<i32>>::convert(22 _ i32));
        }
    }])
    .rustc_ok()
    .expect_output("22\n")
    .ok()
}

#[test]
fn qualified_trait_call_infers_generic_impl_arguments() {
    FormalityTest::new(crates![crate test {
        struct Wrapper<T> {
            value: T,
        }

        trait Get<T> {
            fn get(value: Self) -> T;
        }

        impl<T> Get<T> for Wrapper<T> {
            fn get(value: Wrapper<T>) -> T {
                return value.value;
            }
        }

        fn main() -> () {
            let wrapper: Wrapper<i32> = Wrapper::<i32> { value: 22 _ i32 };
            println!(<Wrapper<i32> as Get<i32>>::get(wrapper));
        }
    }])
    .rustc_ok()
    .expect_output("22\n")
    .ok()
}

#[test]
fn qualified_trait_call_instantiates_method_arguments_last() {
    FormalityTest::new(crates![crate test {
        trait Identity {
            fn identity<T>(value: T) -> T;
        }

        impl Identity for () {
            fn identity<T>(value: T) -> T {
                return value;
            }
        }

        fn main() -> () {
            println!(<() as Identity>::identity::<i32>(22 _ i32));
        }
    }])
    .rustc_ok()
    .expect_output("22\n")
    .ok()
}

#[test]
fn qualified_trait_call_through_associated_type_bound_executes() {
    FormalityTest::new(crates![crate test {
        trait Baz {
            fn baz() -> u32;
        }

        struct Value {}

        impl Baz for Value {
            fn baz() -> u32 {
                return 22 _ u32;
            }
        }

        trait Bar {
            type Ty: [Baz];
        }

        struct Ground {}

        impl Bar for Ground {
            type Ty = Value;
        }

        fn foo<T>() -> u32
        where
            T: Bar,
        {
            return <<T as Bar>::Ty as Baz>::baz();
        }

        fn main() -> () {
            println!(foo::<Ground>());
        }
    }])
    .rustc_ok()
    .expect_output("22\n")
    .ok()
}

#[test]
fn alias_in_trait_method_self_type_is_fully_normalized() {
    FormalityTest::new(crates![crate test {
        trait Family {
            type Output : [];
        }

        impl Family for () {
            type Output = i32;
        }

        trait Identity {
            fn identity(value: Self) -> Self;
        }

        impl Identity for i32 {
            fn identity(value: i32) -> i32 {
                return value;
            }
        }

        fn main() -> () {
            let value: <() as Family>::Output = 22 _ i32;
            println!(<<() as Family>::Output as Identity>::identity(value));
        }
    }])
    .rustc_ok()
    .expect_output("22\n")
    .ok()
}

#[test]
fn alias_in_trait_argument_is_fully_normalized() {
    FormalityTest::new(crates![crate test {
        trait Family {
            type Output : [];
        }

        impl Family for () {
            type Output = i32;
        }

        trait Convert<T> {
            fn convert(value: T) -> T;
        }

        impl Convert<i32> for () {
            fn convert(value: i32) -> i32 {
                return value;
            }
        }

        fn main() -> () {
            println!(<() as Convert<<() as Family>::Output>>::convert(22 _ i32));
        }
    }])
    .rustc_ok()
    .expect_output("22\n")
    .ok()
}

#[test]
fn alias_in_method_argument_is_fully_normalized() {
    FormalityTest::new(crates![crate test {
        trait Family {
            type Output : [];
        }

        impl Family for () {
            type Output = i32;
        }

        trait Identity {
            fn identity<T>(value: T) -> T;
        }

        impl Identity for () {
            fn identity<T>(value: T) -> T {
                return value;
            }
        }

        fn main() -> () {
            println!(<() as Identity>::identity::<<() as Family>::Output>(22 _ i32));
        }
    }])
    .rustc_ok()
    .expect_output("22\n")
    .ok()
}

#[test]
fn nested_alias_in_trait_method_self_type_is_fully_normalized() {
    FormalityTest::new(crates![crate test {
        trait Family {
            type Output : [];
        }

        impl Family for () {
            type Output = i32;
        }

        struct Wrapper<T> {
            value: T,
        }

        trait Identity {
            fn identity(value: Self) -> Self;
        }

        impl<T> Identity for Wrapper<T> {
            fn identity(value: Wrapper<T>) -> Wrapper<T> {
                return value;
            }
        }

        fn main() -> () {
            let wrapper: Wrapper<<() as Family>::Output> =
                Wrapper::<<() as Family>::Output> { value: 22 _ i32 };
            let result: Wrapper<<() as Family>::Output> =
                <Wrapper<<() as Family>::Output> as Identity>::identity(wrapper);
            println!(result.value);
        }
    }])
    .rustc_ok()
    .expect_output("22\n")
    .ok()
}

#[test]
fn repeated_trait_method_calls_share_monomorphization_work() {
    FormalityTest::new(crates![crate test {
        trait Answer {
            fn answer() -> i32;
        }

        impl Answer for () {
            fn answer() -> i32 {
                return 22 _ i32;
            }
        }

        fn main() -> () {
            println!(<() as Answer>::answer());
            println!(<() as Answer>::answer());
        }
    }])
    .rustc_ok()
    .expect_output("22\n22\n")
    .ok()
}

#[test]
fn recursive_trait_method_reuses_preallocated_worklist_entry() {
    FormalityTest::new(crates![crate test {
        trait Answer {
            fn answer() -> i32;
        }

        impl Answer for () {
            fn answer() -> i32 {
                if false {
                    return <() as Answer>::answer();
                } else {
                    return 22 _ i32;
                }
            }
        }

        fn main() -> () {
            println!(<() as Answer>::answer());
        }
    }])
    .rustc_ok()
    .expect_output("22\n")
    .ok()
}

#[test]
fn trait_default_is_used_only_when_impl_has_no_override() {
    FormalityTest::new(crates![crate test {
        trait Answer {
            fn answer() -> i32 {
                return 1 _ i32;
            }
        }

        struct UsesDefault {}
        struct Overrides {}

        impl Answer for UsesDefault {}

        impl Answer for Overrides {
            fn answer() -> i32 {
                return 2 _ i32;
            }
        }

        fn main() -> () {
            println!(<UsesDefault as Answer>::answer());
            println!(<Overrides as Answer>::answer());
        }
    }])
    .rustc_ok()
    .expect_output("1\n2\n")
    .ok()
}

#[test]
fn trait_default_body_is_normalized_after_method_substitution() {
    FormalityTest::new(crates![crate test {
        trait Family {
            type Output : [];
        }

        impl Family for () {
            type Output = i32;
        }

        trait Identity {
            fn identity<T>(value: T) -> T {
                return value;
            }
        }

        impl Identity for () {}

        fn main() -> () {
            println!(<() as Identity>::identity::<<() as Family>::Output>(22 _ i32));
        }
    }])
    .rustc_ok()
    .expect_output("22\n")
    .ok()
}

#[test]
fn if_statements() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 1 _ i32;
            if true {
                println!(x);
            } else {
                println!(0 _ i32);
            }
        }
    }])
    .expect_output("1\n")
    .ok()
}

#[test]
fn loops_break_continue() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 0 _ i32;
            'a: loop {
                println!(x);
                break 'a;
            }
        }
    }])
    .expect_output("0\n")
    .ok()
}

#[test]
fn nested_blocks_and_exists() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            {
                let x: i32 = 99 _ i32;
                println!(x);
            }
            exists<'a> {
                println!(1 _ i32);
            }
        }
    }])
    .expect_output("99\n1\n")
    .ok()
}

#[test]
fn structs() {
    FormalityTest::new(crates![crate test {
        struct Pair<> where { x: i32, y: i32 }
        fn main() -> () {
            let p: Pair = Pair { x: 10 _ i32, y: 20 _ i32 };
            println!(p.x);
            println!(p.y);
        }
    }])
    .expect_output("10\n20\n")
    .ok()
}

#[test]
#[ignore = "codegen does not yet handle AliasTy from Derefable"]
fn references_and_deref() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 42 _ i32;
            exists<'a> {
                let r: & 'a i32 = & 'a x;
                println!(*r);
            }
        }
    }])
    .expect_output("42\n")
    .ok()
}

#[test]
fn usize_isize() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let a: usize = 100 _ usize;
            let b: isize = 200 _ isize;
            println!(a);
            println!(b);
        }
    }])
    .expect_output("100\n200\n")
    .ok()
}

#[test]
fn multiple_calls_in_sequence() {
    FormalityTest::new(crates![crate test {
        fn f(x: i32) -> i32 { return x; }
        fn main() -> () {
            let a: i32 = f(1 _ i32);
            let b: i32 = f(2 _ i32);
            let c: i32 = f(3 _ i32);
            println!(a);
            println!(b);
            println!(c);
        }
    }])
    .expect_output("1\n2\n3\n")
    .ok()
}

#[test]
fn nested_calls() {
    FormalityTest::new(crates![crate test {
        fn f(x: i32) -> i32 { return x; }
        fn main() -> () {
            let y: i32 = f(f(42 _ i32));
            println!(y);
        }
    }])
    .expect_output("42\n")
    .ok()
}

#[test]
fn functions_calling_other_functions() {
    FormalityTest::new(crates![crate test {
        fn inner(x: i32) -> i32 { return x; }
        fn outer(x: i32) -> i32 {
            let y: i32 = inner(x);
            return y;
        }
        fn main() -> () {
            let r: i32 = outer(7 _ i32);
            println!(r);
        }
    }])
    .expect_output("7\n")
    .ok()
}

#[test]
fn transitive_monomorphization() {
    FormalityTest::new(crates![crate test {
        fn c<T>(x: T) -> T { return x; }
        fn b<T>(x: T) -> T {
            let y: T = c::<T>(x);
            return y;
        }
        fn a() -> () {
            let r: i32 = b::<i32>(99 _ i32);
            println!(r);
        }
        fn main() -> () {
            a();
        }
    }])
    .expect_output("99\n")
    .ok()
}

#[test]
fn generic_function_multiple_type_params() {
    FormalityTest::new(crates![crate test {
        fn first<T, U>(x: T, y: U) -> T { return x; }
        fn main() -> () {
            let r: i32 = first::<i32, bool>(10 _ i32, true);
            println!(r);
        }
    }])
    .expect_output("10\n")
    .ok()
}

#[test]
fn generic_struct() {
    FormalityTest::new(crates![crate test {
        struct Wrapper<T> where { val: T }
        fn main() -> () {
            let w: Wrapper<i32> = Wrapper::<i32> { val: 42 _ i32 };
            println!(w.val);
        }
    }])
    .expect_output("42\n")
    .ok()
}

#[test]
#[ignore = "codegen does not yet handle AliasTy from Derefable"]
fn ref_to_struct_field() {
    FormalityTest::new(crates![crate test {
        struct Pair<> where { x: i32, y: i32 }
        fn main() -> () {
            let p: Pair = Pair { x: 10 _ i32, y: 20 _ i32 };
            exists<'a> {
                let r: & 'a i32 = & 'a p.x;
                println!(*r);
            }
        }
    }])
    .expect_output("10\n")
    .ok()
}

#[test]
#[ignore = "codegen does not yet handle AliasTy from Derefable"]
fn deref_through_ref_to_struct_field() {
    FormalityTest::new(crates![crate test {
        struct Pair<> where { x: i32, y: i32 }
        fn main() -> () {
            let p: Pair = Pair { x: 10 _ i32, y: 20 _ i32 };
            exists<'a> {
                let r: & 'a Pair = & 'a p;
                println!((*r).x);
                println!((*r).y);
            }
        }
    }])
    .expect_output("10\n20\n")
    .ok()
}

#[test]
fn continue_in_loop() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 0 _ i32;
            'a: loop {
                if true {
                    println!(x);
                    break 'a;
                } else {
                    continue 'a;
                }
            }
        }
    }])
    .expect_output("0\n")
    .ok()
}

#[test]
fn nested_loops_break_outer() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            'outer: loop {
                println!(1 _ i32);
                'inner: loop {
                    println!(2 _ i32);
                    break 'outer;
                }
            }
            println!(3 _ i32);
        }
    }])
    .expect_output("1\n2\n3\n")
    .ok()
}

#[test]
fn if_else_inside_loop_with_breaks() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            'a: loop {
                if true {
                    println!(1 _ i32);
                    break 'a;
                } else {
                    println!(2 _ i32);
                    break 'a;
                }
            }
            println!(3 _ i32);
        }
    }])
    .expect_output("1\n3\n")
    .ok()
}

#[test]
fn multiple_breaks_same_label() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 0 _ i32;
            'a: loop {
                if false {
                    x = 1 _ i32;
                    break 'a;
                } else {
                    x = 2 _ i32;
                    break 'a;
                }
            }
            println!(x);
        }
    }])
    .expect_output("2\n")
    .ok()
}

#[test]
#[ignore = "codegen does not yet handle AliasTy from Derefable"]
fn mut_reference() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 1 _ i32;
            exists<'a> {
                let r: &mut 'a i32 = &mut 'a x;
                *r = 2 _ i32;
                println!(*r);
            }
        }
    }])
    .expect_output("2\n")
    .ok()
}

#[test]
#[ignore = "codegen does not yet handle AliasTy from Derefable"]
fn reborrow_shared() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 42 _ i32;
            exists<'a> {
                let r1: & 'a i32 = & 'a x;
                let r2: & 'a i32 = & 'a *r1;
                println!(*r2);
            }
        }
    }])
    .expect_output("42\n")
    .ok()
}

#[test]
#[ignore = "codegen does not yet handle AliasTy from Derefable"]
fn exists_with_lifetime_parameterized_type() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 55 _ i32;
            exists<'a> {
                let r: & 'a i32 = & 'a x;
                println!(*r);
            }
        }
    }])
    .expect_output("55\n")
    .ok()
}

#[test]
fn return_from_inside_loop() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 0 _ i32;
            'a: loop {
                x = 77 _ i32;
                break 'a;
            }
            println!(x);
        }
    }])
    .expect_output("77\n")
    .ok()
}

#[test]
fn return_from_nested_block_inside_loop() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            let x: i32 = 0 _ i32;
            'a: loop {
                {
                    x = 88 _ i32;
                    break 'a;
                }
            }
            println!(x);
        }
    }])
    .expect_output("88\n")
    .ok()
}

#[test]
fn break_from_labeled_block() {
    FormalityTest::new(crates![crate test {
        fn main() -> () {
            'a: {
                println!(1 _ i32);
                break 'a;
                println!(2 _ i32);
            }
            println!(3 _ i32);
        }
    }])
    .expect_output("1\n3\n")
    .ok()
}
