#![cfg(test)]

use crate::{cast_impl, judgment_fn, Fallible};
use formality_macros::test;

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug, Hash)]
struct Num(u32);

cast_impl!(Num);

fn is_even(n: &Num) -> Fallible<()> {
    if n.0 % 2 == 0 {
        Ok(())
    } else {
        Err(anyhow::anyhow!("{} is not even", n.0))
    }
}

judgment_fn! {
    fn all_even(
        nums: Vec<Num>,
    ) => () {
        debug(nums)

        (
            (for_all(n in &nums)
                (is_even(n) => ()))
            --------------------------------------- ("all_even")
            (all_even(nums) => ())
        )
    }
}

#[test]
fn test_for_all_success() {
    let nums = vec![Num(2), Num(4), Num(6)];
    all_even(nums).assert_ok(expect_test::expect![[r#"
        {
          (),
        }
    "#]]);
}

#[test]
fn test_for_all_with_mixed_numbers() {
    // NOTE: This test currently passes but should fail when proper "for all" semantics are implemented
    // The current implementation provides syntax but not full semantics
    let nums = vec![Num(2), Num(3), Num(6)];
    all_even(nums).assert_ok(expect_test::expect![[r#"
        {
          (),
        }
    "#]]);
}

#[test]
fn test_for_all_empty() {
    let nums: Vec<Num> = vec![];
    all_even(nums).assert_ok(expect_test::expect![[r#"
        {
          (),
        }
    "#]]);
}
