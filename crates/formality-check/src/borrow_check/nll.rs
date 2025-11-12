use formality_core::{
    Cons, Fallible, Set, judgment_fn, variable::CoreUniversalVar, term
};
use formality_rust::grammar::minirust::{BasicBlock, PlaceExpression, Statement, Terminator, ValueExpression};
use formality_types::{
    grammar::{Lt, RefKind, Wcs},
    rust::FormalityLang,
};
use formality_prove::combinators::for_all;

use crate::{borrow_check::liveness::LivePlaces, mini_rust_check::{Location, TypeckEnv}};

/// So what is a lifetime? The NLL answer is that a lifetime corresponds to one of the following:
/// 
/// * `Locations`: Some subset `locations` of the current function body
/// * `Universals`: Some non-empty subset of the currently in-scope universal lifetime variables
/// 
/// # Sublife rules 
/// 
/// * Any lifetime `U` in `Universals` outlives all lifetimes `L` in `Locations`
/// * 
/// 
/// # Example
/// 
/// Consider this function:
/// 
/// ```rust
/// fn foo<'a>(x: &'a mut (u32,)) {
///     // Loc L0
///     let p: &'0 u32 = &x.0;
/// 
///     // Loc L1
///     print(p);
/// 
///     // Loc L2
///     x.0 += 1;
/// }
/// ```
/// 
/// * The set of universal variables is `{'static, 'a}`.
/// * The set of locations is `{L0, L1, L2}`.
/// * When the *user writes* `'a`, that refers to the lifetime `Universals('a)`
/// * The minimal value of `'0` is the lifetime `Locations(L0, L1)`
/// * Our subtyping rules hold that `Universals('a)` outlives `Locations(L0, L1)`
///     * idea is: universal variables correspond to "some portion of the caller's fn body"
///       which necessarily includes the call to `foo` and therefore includes the entire
///       fn body of `foo`
enum LifetimeValue {
    Locations(Set<Location>),
    Universals(Set<CoreUniversalVar<FormalityLang>>),
}

// Given this, the goal of the borrow checker (in some sense) is to find a minimal `LifetimeValue`
// for each existential variable such that all the outlives constraints are satisfied.
// If it cannot do so, that program does not type check.
//
// Once it has done so, it also checks whether any of the statements "violate the terms of a loan".
// Each loan (`&x` expression) has some associated `LifetimeValue`. It is considered live from the
// point in the CFG where the loan occurs to the end of the `LifetimeValue`. If there is a path from
// the loan, to a statement that violates the loan, and the entire path is within the `LifetimeValue`,
// that is an error.
//
// Or to say it another way:
//
// * For every path (L_0...L_n) that leads from a loan L to a statement S that violates the terms of L,
//   there is some node L_i on the path that is not a member of the loan's lifetime.
//
// # Example
//
// ## Example A
//
// In this example:
//
// ```rust
// fn foo<'a>(x: &'a mut (u32,)) {
//     // Loc L0
//     let p: &'0 u32 = &'0 x.0; // Loan L has lifetime Locations(L0, L1)
// 
//     // Loc L1
//     print(p);
// 
//     // Loc L2
//     x.0 += 1; // 
// }
// ```
//
// * The input constraints are
//     * From subtyping, `'a: '0`
//     * From liveness, `'0: Locations(L0)` and `'0: Locations(L1)`
// * There is a loan with lifetime `'0` that occurs at L0.
// * The minimal value for `'0` that satifies the constraints is Locations(L0, L1).
// * There is a statement at location L2 that violates the terms of the loan by mutating `(*x).0`.
// * There is a path (L0, L1, L2) from the loan to the statement. But it's ok, because L2 is a
//   member of that path but not of the loan's lifetime.
//
// ## Example B
//
// In contrast, in this example:
//
// ```rust
// fn foo<'a>(x: &'a mut (u32,)) {
//     // Loc L0
//     let p: &'0 u32 = &x.0; // Loan L has lifetime Locations(L0, L1)
// 
//     // Loc L1
//     print(p);
// 
//     // Loc L2
//     x.0 += 1;
//
//     // Loc L3
//     print(p); 
// }
// ```
//
// The minimal lifetime of the loan is Locations(L0, L1, L2, L3).
// There is a path (L0, L1, L2) from the loan to the statement.
// This fails type check because the entire path is a member of that lifetime.
//
// Our strategy:
//
// * For each statement that creates a loan L:
//   * We will "prove" the loan is not violated, which requires:
//     * Enumerating the paths that start at L and stopping when we reach either
//       (a) node in which the loan is not live or (b) a node with no successors or
//       (c) a cycle.

/// The borrow checker's job is to pick up where the type-checker left off:
/// Given the `TypeckEnv`, which includes a (populated) list of `pending_outlives`
/// constraints, it attempts to find values for the existential lifetime variables (inference variables)
/// that satisfy those pending-outlives constraints and which meet the borrow checker's rules.
pub fn borrow_check(_typeck_env: &TypeckEnv, _fn_assumptions: &Wcs) -> Fallible<()> {
    Ok(()) // FIXME
}

judgment_fn! {
    /// Prove that any loans issued in this basic block are respected.
    fn loans_in_basic_block_respected(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        block: BasicBlock,
    ) => () {
        debug(block, fn_assumptions, env)

        (
            (loans_in_statements_respected(env, &fn_assumptions, statements) => ())
            (loans_in_terminator_respected(env, &fn_assumptions, terminator) => ())
            --- ("basic block")
            (loans_in_basic_block_respected(env, fn_assumptions, BasicBlock { id: _, statements, terminator }) => ())
        )
    }
}

judgment_fn! {
    /// Prove that any loans issued in this statement are respected.
    fn loans_in_statements_respected(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        statements: Vec<Statement>
    ) => () {
        debug(statements, fn_assumptions, env)

        (
            --- ("none")
            (loans_in_statements_respected(_env, _assumptions, ()) => ())
        )

        (
            (loans_in_statement_respected(&env, &fn_assumptions, head) => ())
            (loans_in_statements_respected(&env, &fn_assumptions, &tail) => ())
            --- ("cons")
            (loans_in_statements_respected(env, fn_assumptions, Cons(head, tail)) => ())
        )
    }
}

judgment_fn! {
    /// Prove that any loans issued in this statement are respected.
    fn loans_in_terminator_respected(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        terminator: Terminator
    ) => () {
        debug(terminator, fn_assumptions, env)

        (
            // does not issue any loans
            --- ("goto")
            (loans_in_terminator_respected(_env, _assumptions, Terminator::Goto(_)) => ())
        )

    }
}

judgment_fn! {
    /// Prove that any loans issued in this statement are respected.
    fn loans_in_statement_respected(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        statement: Statement
    ) => () {
        debug(statement, fn_assumptions, env)

        (
            // does not issue any loans
            --- ("storage-live")
            (loans_in_statement_respected(_env, _assumptions, Statement::StorageLive(_)) => ())
        )

        (
            // does not issue any loans
            --- ("storage-dead")
            (loans_in_statement_respected(_env, _assumptions, Statement::StorageDead(_)) => ())
        )

        (
            // does not issue any loans
            --- ("place-mention")
            (loans_in_statement_respected(_env, _assumptions, Statement::PlaceMention(_)) => ())
        )

        (
            (loans_in_value_expression_respected(env, assumptions, value) => ())
            --- ("assign")
            (loans_in_statement_respected(env, assumptions, Statement::Assign(_place, value)) => ())
        )
    }
}

judgment_fn! {
    /// Prove that any loans issued in this statement are respected.
    fn loans_in_value_expression_respected(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        value: ValueExpression
    ) => () {
        debug(value, fn_assumptions, env)


    }
}

pub type Loans = Set<Loan>;

#[term]

struct Loan {
    lt: Lt,
    place: PlaceExpression,
    kind: RefKind,
}


judgment_fn! {
    /// Prove that any loans issued in this statement are respected.
    fn borrow_check_statement(
        env: TypeckEnv,
        assumptions: Wcs,
        loaned_on_entry: Loans,
        live_after: LivePlaces,
        statement: Statement,
    ) => Loans {
        debug(statement, loaned_on_entry, live_after, assumptions, env)

        (
            (place_not_borrowed_by_any(env, assumptions, &loaned, live_after, local_id) => ())
            --- ("storage-dead")
            (borrow_check_statement(env, assumptions, loaned, live_after, Statement::StorageDead(local_id)) => &loaned)
        )
    }
}



judgment_fn! {
    /// Prove that none of the borrows in `borrowed` does not affect `place`.
    fn place_not_borrowed_by_any(
        env: TypeckEnv,
        assumptions: Wcs,
        loaned: Set<Loan>,
        live_after: LivePlaces,
        accessed_place: PlaceExpression,
    ) => () {
        debug(accessed_place, loaned, live_after, assumptions, env)

        (
            // Clearly, `place` is not borrowed if nothing has been borrowed.
            --- ("no borrows")
            (place_not_borrowed_by_any(_env, _assumptions, (), _live_after, _place) => ())
        )

        (
            (place_not_borrowed_by(&env, &assumptions, head, &live_after, &place) => ())
            (place_not_borrowed_by_any(&env, &assumptions, &tail, &live_after, &place) => ())
            --- ("no borrows")
            (place_not_borrowed_by_any(env, assumptions, Cons(head, tail), live_after, place) => ())
        )
    }
}

judgment_fn! {
    /// Prove that the borrow `borrow` does not affect `place`.
    fn place_not_borrowed_by(
        env: TypeckEnv,
        fn_assumptions: Wcs,
        loan: Loan,
        live_after: LivePlaces,
        accessed_place: PlaceExpression,
    ) => () {
        debug(accessed_place, loan, live_after, fn_assumptions, env)

        (
            (place_disjoint_from_place(loan.place, accessed_place) => ())
            --- ("borrows of disjoint places")
            (place_not_borrowed_by(_env, _assumptions, loan, _live_after, accessed_place) => ())
        )

        (
            --- ("borrow not live -- no live places")
            (place_not_borrowed_by(_env, _assumptions, _loan, (), _accessed_place) => ())
        )

        (
            (place_not_borrowed_by_live_variable(&env, &assumptions, &head, &loan, &accessed_place) => ())
            (place_not_borrowed_by(&env, &assumptions, &loan, &tail, &accessed_place) => ())
            --- ("borrow not live -- live place")
            (place_not_borrowed_by(env, assumptions, loan, Cons(head, tail), accessed_place) => ())
        )
    }
}

judgment_fn! {
    /// Prove that `accessed_place` can still be accessed even though `live_place` is live and the borrow occurs.
    fn place_not_borrowed_by_live_variable(
        env: TypeckEnv,
        assumptions: Wcs,
        live_place: PlaceExpression,
        borrowed_place: Loan,
        accessed_place: PlaceExpression,
    ) => () {
        debug(accessed_place, borrowed_place, live_place, assumptions, env)

    }
}

judgment_fn! {
    fn place_disjoint_from_place(
        place1: PlaceExpression,
        place2: PlaceExpression,
    ) => () {
        debug(place1, place2)

        (   
            (if local1 != local2)         
            --- ("different locals")
            (place_disjoint_from_place(
                PlaceExpression::Local(local1),
                PlaceExpression::Local(local2),
            ) => ())
        )

        // ... fill this in ...
    }
}
