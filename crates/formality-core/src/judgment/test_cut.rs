#![cfg(test)]

use std::cell::Cell;

use crate::judgment_fn;

thread_local! {
    static RULE_PASSES: Cell<usize> = const { Cell::new(0) };
    static RULES_AFTER_CUT: Cell<usize> = const { Cell::new(0) };
    static DELAYED_CUT_PASSES: Cell<usize> = const { Cell::new(0) };
}

fn is_one(value: &u32) -> bool {
    *value == 1
}

fn never_cut(_value: &u32) -> bool {
    false
}

judgment_fn! {
    fn cut_recursive_search() => u32 {
        debug()
        cut(is_one)

        // Count complete rule passes without producing an answer.
        (
            (let () = RULE_PASSES.set(RULE_PASSES.get() + 1))
            (if false)
            --- ("count pass")
            (cut_recursive_search() => 0)
        )

        // Ensure the terminal answer is discovered only after an active recursive call has
        // observed the initially empty approximation.
        (
            (cut_recursive_search() => value)
            --- ("recursive")
            (cut_recursive_search() => *value)
        )

        (
            --- ("terminal base")
            (cut_recursive_search() => 1)
        )

        // A terminal result skips subsequent rules in the same pass.
        (
            (let () = RULES_AFTER_CUT.set(RULES_AFTER_CUT.get() + 1))
            --- ("after cut")
            (cut_recursive_search() => 2)
        )
    }
}

judgment_fn! {
    fn cut_that_never_matches() => u32 {
        debug()
        cut(never_cut)

        (
            --- ("zero")
            (cut_that_never_matches() => 0)
        )

        (
            --- ("one")
            (cut_that_never_matches() => 1)
        )
    }
}

judgment_fn! {
    fn cut_discovered_after_a_nonterminal_answer() => u32 {
        debug()
        cut(is_one)

        (
            (let () = DELAYED_CUT_PASSES.set(DELAYED_CUT_PASSES.get() + 1))
            (if false)
            --- ("count pass")
            (cut_discovered_after_a_nonterminal_answer() => 99)
        )

        // This rule fails against the empty approximation in round one. Once the seed below
        // grows the approximation, round two produces the terminal answer.
        (
            (cut_discovered_after_a_nonterminal_answer() => 0)
            --- ("terminal from seed")
            (cut_discovered_after_a_nonterminal_answer() => 1)
        )

        (
            --- ("nonterminal seed")
            (cut_discovered_after_a_nonterminal_answer() => 0)
        )
    }
}

#[test]
fn terminal_answer_stops_rules_and_fixed_point_iteration() {
    RULE_PASSES.set(0);
    RULES_AFTER_CUT.set(0);

    let (value, _) = cut_recursive_search().into_singleton().unwrap();

    assert_eq!(value, 1);
    assert_eq!(RULE_PASSES.get(), 1);
    assert_eq!(RULES_AFTER_CUT.get(), 0);
}

#[test]
fn unmatched_cut_preserves_exhaustive_search() {
    let values = cut_that_never_matches()
        .iter()
        .map(|(value, _)| value)
        .collect::<Vec<_>>();

    assert_eq!(values, [0, 1]);
}

#[test]
fn terminal_answer_replaces_an_earlier_approximation() {
    DELAYED_CUT_PASSES.set(0);

    let (value, _) = cut_discovered_after_a_nonterminal_answer()
        .into_singleton()
        .unwrap();

    assert_eq!(value, 1);
    assert_eq!(DELAYED_CUT_PASSES.get(), 2);
}
