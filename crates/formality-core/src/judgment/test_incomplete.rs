#![cfg(test)]

use std::cell::Cell;

use anyhow::Context as _;

use crate::{
    cast_impl,
    judgment::{ProofTree, RuleFailureCause},
    judgment_fn, with_cutoff, Fallible, Size,
};

use super::IncompleteReason;

thread_local! {
    static MEMO_CHILD_EXECUTIONS: Cell<usize> = const { Cell::new(0) };
}

judgment_fn! {
    fn only_incomplete() => () {
        debug()

        (
            (incomplete) // INCOMPLETE_SPAN_MARKER
            --- ("stop")
            (only_incomplete() => ())
        )
    }
}

judgment_fn! {
    fn failure_and_incomplete() => () {
        debug()

        (
            (if false)
            --- ("failure")
            (failure_and_incomplete() => ())
        )

        (
            (incomplete)
            --- ("stop")
            (failure_and_incomplete() => ())
        )
    }
}

judgment_fn! {
    fn nested_then_incomplete() => () {
        debug()

        (
            (only_incomplete() => ())
            (incomplete)
            --- ("unreachable")
            (nested_then_incomplete() => ())
        )
    }
}

judgment_fn! {
    fn mixed_child_then_incomplete() => () {
        debug()

        (
            (proven_and_incomplete() => _value)
            (incomplete)
            --- ("stop after known answer")
            (mixed_child_then_incomplete() => ())
        )
    }
}

judgment_fn! {
    fn for_all_explicit_incomplete() => () {
        debug()

        (
            (let acc: u32 = 0)
            (for_all(_value in [0_u32]) with(acc)
                (if *acc == 0)
                (incomplete))
            --- ("loop")
            (for_all_explicit_incomplete() => ())
        )
    }
}

judgment_fn! {
    fn for_all_nested_incomplete() => () {
        debug()

        (
            (for_all(_value in [0_u32])
                (only_incomplete() => ()))
            --- ("loop")
            (for_all_nested_incomplete() => ())
        )
    }
}

fn erased_incomplete_proof() -> Fallible<ProofTree> {
    Ok(only_incomplete()
        .check_proven()
        .context("checking an incomplete proof")?)
}

fn erased_incomplete_value() -> Fallible<u32> {
    let (value, _) = proven_and_incomplete().into_singleton()?;
    Ok(value)
}

fn erased_failed_proof() -> Fallible<ProofTree> {
    Ok(ordinary_failure(1).check_proven()?)
}

fn erased_failed_value() -> Fallible<u32> {
    let (value, _) = ordinary_value_failure().into_singleton()?;
    Ok(value)
}

judgment_fn! {
    fn calls_erased_incomplete_proof() => () {
        debug()

        (
            (erased_incomplete_proof() => ())
            --- ("erased proof")
            (calls_erased_incomplete_proof() => ())
        )
    }
}

judgment_fn! {
    fn calls_erased_incomplete_value() => () {
        debug()

        (
            (let _value: u32 = erased_incomplete_value()?)
            --- ("erased value")
            (calls_erased_incomplete_value() => ())
        )
    }
}

judgment_fn! {
    fn calls_erased_failed_proof() => () {
        debug()

        (
            (erased_failed_proof() => ())
            --- ("erased proof failure")
            (calls_erased_failed_proof() => ())
        )
    }
}

judgment_fn! {
    fn calls_erased_failed_value() => () {
        debug()

        (
            (let _value: u32 = erased_failed_value()?)
            --- ("erased value failure")
            (calls_erased_failed_value() => ())
        )
    }
}

judgment_fn! {
    fn proven_and_incomplete() => u32 {
        debug()

        (
            --- ("proven")
            (proven_and_incomplete() => 22)
        )

        (
            (incomplete)
            --- ("stop")
            (proven_and_incomplete() => 44)
        )
    }
}

judgment_fn! {
    fn calls_incomplete() => () {
        debug()

        (
            (only_incomplete() => ())
            --- ("nested")
            (calls_incomplete() => ())
        )
    }
}

judgment_fn! {
    fn fails_before_incomplete(value: u32) => () {
        debug(value)

        (
            (if *value == 0)
            (incomplete)
            --- ("conditional")
            (fails_before_incomplete(value) => ())
        )
    }
}

judgment_fn! {
    fn multi_answer_incomplete() => u32 {
        debug()

        (
            (value in [0, 1])
            (if *value == 1)
            (incomplete)
            --- ("one binding stops")
            (multi_answer_incomplete() => *value)
        )
    }
}

judgment_fn! {
    fn ordinary_cycle(value: u32) => u32 {
        debug(value)

        (
            --- ("base")
            (ordinary_cycle(value) => *value)
        )

        (
            (ordinary_cycle(value) => result)
            --- ("cycle")
            (ordinary_cycle(value) => *result)
        )
    }
}

judgment_fn! {
    fn memoized_incomplete_child(value: u32) => u32 {
        debug(value)

        (
            (let () = MEMO_CHILD_EXECUTIONS.set(MEMO_CHILD_EXECUTIONS.get() + 1))
            --- ("proven")
            (memoized_incomplete_child(value) => *value)
        )

        (
            (incomplete)
            --- ("stop")
            (memoized_incomplete_child(_value) => 0)
        )
    }
}

judgment_fn! {
    fn calls_memoized_incomplete_child_twice(value: u32) => u32 {
        debug(value)

        (
            (memoized_incomplete_child(value) => first)
            (memoized_incomplete_child(value) => second)
            --- ("twice")
            (calls_memoized_incomplete_child_twice(value) => *first + *second)
        )
    }
}

judgment_fn! {
    fn memoized_incomplete_only_child(value: u32) => () {
        debug(value)

        (
            (let () = MEMO_CHILD_EXECUTIONS.set(MEMO_CHILD_EXECUTIONS.get() + 1))
            (incomplete)
            --- ("stop")
            (memoized_incomplete_only_child(_value) => ())
        )
    }
}

judgment_fn! {
    fn calls_incomplete_only_child_twice(value: u32) => () {
        debug(value)

        (
            (memoized_incomplete_only_child(value) => ())
            --- ("first")
            (calls_incomplete_only_child_twice(value) => ())
        )

        (
            (memoized_incomplete_only_child(value) => ())
            --- ("second")
            (calls_incomplete_only_child_twice(value) => ())
        )
    }
}

judgment_fn! {
    fn ordinary_failure(value: u32) => () {
        debug(value)

        (
            (if *value == 0)
            --- ("zero")
            (ordinary_failure(value) => ())
        )
    }
}

judgment_fn! {
    fn ordinary_value_failure() => u32 {
        debug()

        (
            (if false)
            --- ("never")
            (ordinary_value_failure() => 22)
        )
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Context(u32);

cast_impl!(Context);

impl Size for Context {
    fn size(&self) -> usize {
        0
    }
}

judgment_fn! {
    fn cutoff_input(context: Context, value: u32) => u32 {
        debug(context, value)

        (
            --- ("identity")
            (cutoff_input(_context, value) => *value)
        )
    }
}

judgment_fn! {
    fn two_sized_inputs(left: u32, right: u32) => u32 {
        debug(left, right)

        (
            --- ("sum")
            (two_sized_inputs(left, right) => *left + *right)
        )
    }
}

judgment_fn! {
    fn growing_input(values: Vec<u32>) => u32 {
        debug(values)

        (
            --- ("current")
            (growing_input(values) => values.len() as u32)
        )

        (
            (let next = {
                let mut next = values.clone();
                next.push(0);
                next
            })
            (growing_input(next) => result)
            --- ("grow")
            (growing_input(values) => *result)
        )
    }
}

fn assert_incomplete_without_proof<T>(result: &crate::ProvenSet<T>)
where
    T: Clone + std::fmt::Debug + Ord,
{
    assert!(result.is_incomplete());
    assert!(!result.is_proven());
    assert!(result.iter().next().is_none());
    assert!(!result.incomplete_frontiers().is_empty());
}

#[test]
fn explicit_incomplete_has_no_logical_output() {
    let result = only_incomplete();
    assert_incomplete_without_proof(&result);
    assert!(result
        .incomplete_frontiers()
        .iter()
        .all(|frontier| frontier.reason == IncompleteReason::Explicit));

    let marker_line = include_str!("test_incomplete.rs")
        .lines()
        .position(|line| line.contains("INCOMPLETE_SPAN_MARKER"))
        .expect("span marker must be present");
    let marker_text = include_str!("test_incomplete.rs")
        .lines()
        .nth(marker_line)
        .unwrap();
    let expected_column = marker_text.find("incomplete").unwrap() + 1;
    let frontier = result.incomplete_frontiers().iter().next().unwrap();
    assert!(frontier.file.ends_with("test_incomplete.rs"));
    assert_eq!(frontier.line as usize, marker_line + 1);
    assert_eq!(frontier.column as usize, expected_column);
}

#[test]
fn successful_alternative_is_retained_beside_incompleteness() {
    let result = proven_and_incomplete();
    assert!(result.is_proven());
    assert!(result.is_incomplete());
    assert_eq!(
        result.iter().map(|(value, _)| value).collect::<Vec<_>>(),
        [22]
    );

    let error = result
        .into_map()
        .expect_err("partial map must not look exhaustive");
    let result = error
        .into_incomplete()
        .expect("expected an incomplete result");
    assert_eq!(
        result.iter().map(|(value, _)| value).collect::<Vec<_>>(),
        [22]
    );
}

#[test]
fn nested_incompleteness_propagates() {
    assert_incomplete_without_proof(&calls_incomplete());
}

#[test]
fn incomplete_child_stops_before_later_incomplete_premise() {
    let result = nested_then_incomplete();
    assert_incomplete_without_proof(&result);
    assert_eq!(result.incomplete_frontiers().len(), 1);
    assert_eq!(
        result
            .incomplete_frontiers()
            .iter()
            .next()
            .unwrap()
            .judgment_name,
        "only_incomplete"
    );
}

#[test]
fn mixed_child_and_later_incomplete_both_propagate_without_output() {
    let result = mixed_child_then_incomplete();
    assert_incomplete_without_proof(&result);
    assert_eq!(result.incomplete_frontiers().len(), 2);
}

#[test]
fn incompleteness_propagates_from_for_all_bodies() {
    assert_incomplete_without_proof(&for_all_explicit_incomplete());
    assert_incomplete_without_proof(&for_all_nested_incomplete());
}

#[test]
fn incompleteness_survives_legacy_anyhow_adapters() {
    assert_incomplete_without_proof(&calls_erased_incomplete_proof());
    assert_incomplete_without_proof(&calls_erased_incomplete_value());
}

#[test]
fn complete_failures_remain_structured_through_legacy_anyhow_adapters() {
    for result in [calls_erased_failed_proof(), calls_erased_failed_value()] {
        let error = result
            .check_proven()
            .expect_err("the adapter must remain a complete failure");
        let failure = error
            .as_failed()
            .expect("a complete failure must retain its diagnostic tree");
        assert!(failure
            .failed_rules
            .iter()
            .any(|rule| matches!(&rule.cause, RuleFailureCause::FailedJudgment(_))));
    }
}

#[test]
fn extraction_errors_do_not_duplicate_their_source_diagnostics() {
    let failed = anyhow::Error::new(
        ordinary_failure(1)
            .check_proven()
            .expect_err("the judgment must fail"),
    );
    assert_eq!(
        format!("{failed:?}")
            .matches("condition evaluated to false")
            .count(),
        1
    );

    let incomplete = anyhow::Error::new(
        only_incomplete()
            .check_proven()
            .expect_err("the judgment must be incomplete"),
    );
    assert_eq!(
        format!("{incomplete:?}")
            .matches("reached `(incomplete)`")
            .count(),
        1
    );
}

#[test]
fn failed_premise_before_incomplete_is_complete_empty() {
    let result = fails_before_incomplete(1);
    assert!(result.is_complete());
    assert!(!result.is_proven());
}

#[test]
fn incomplete_multi_answer_continuation_manufactures_no_output() {
    assert_incomplete_without_proof(&multi_answer_incomplete());
}

#[test]
fn active_fixed_point_approximation_is_not_incomplete() {
    let result = ordinary_cycle(22);
    assert!(result.is_complete());
    assert_eq!(
        result.iter().map(|(value, _)| value).collect::<Vec<_>>(),
        [22]
    );
}

#[test]
fn memoized_result_preserves_proof_and_incompleteness() {
    MEMO_CHILD_EXECUTIONS.set(0);
    let result = calls_memoized_incomplete_child_twice(22);

    assert_eq!(MEMO_CHILD_EXECUTIONS.get(), 1);
    assert!(result.is_incomplete());
    assert_eq!(
        result.iter().map(|(value, _)| value).collect::<Vec<_>>(),
        [44]
    );
}

#[test]
fn memoized_incomplete_only_result_preserves_its_frontier() {
    MEMO_CHILD_EXECUTIONS.set(0);
    let result = calls_incomplete_only_child_twice(22);

    assert_eq!(MEMO_CHILD_EXECUTIONS.get(), 1);
    assert_incomplete_without_proof(&result);
    assert_eq!(result.incomplete_frontiers().len(), 1);
}

#[test]
fn ordinary_failure_retains_existing_behavior_and_diagnostics() {
    let result = ordinary_failure(1);
    assert!(result.is_complete());
    result.assert_err(expect_test::expect![[r#"
        the rule "zero" at (test_incomplete.rs) failed because
          condition evaluated to false: `*value == 0`"#]]);
}

#[test]
fn failure_diagnostics_remain_distinct_from_incompleteness() {
    let result = failure_and_incomplete();
    assert_incomplete_without_proof(&result);
    assert!(!result.is_failed());

    let rendered = result.to_string();
    assert!(rendered.contains("condition evaluated to false"));
    assert!(rendered.contains("\nincomplete frontiers:"));
}

#[test]
fn cutoff_counts_only_nonzero_input_contributions() {
    with_cutoff(1, || {
        cutoff_input(Context(999), 22).assert_ok(expect_test::expect!["{22}"]);
        assert_incomplete_without_proof(&two_sized_inputs(1, 2));
    });
}

#[test]
fn cutoff_is_inclusive_at_the_boundary() {
    with_cutoff(1, || {
        cutoff_input(Context(0), 22).assert_ok(expect_test::expect!["{22}"]);
    });
}

#[test]
fn cutoff_incompleteness_propagates_through_recursive_judgments() {
    let result = with_cutoff(2, || growing_input(Vec::<u32>::new()));
    assert!(result.is_incomplete());
    assert_eq!(
        result.iter().map(|(value, _)| value).collect::<Vec<_>>(),
        [0, 1, 2]
    );
    assert!(result
        .incomplete_frontiers()
        .iter()
        .any(|frontier| { frontier.reason == IncompleteReason::Size { size: 3, cutoff: 2 } }));
}
