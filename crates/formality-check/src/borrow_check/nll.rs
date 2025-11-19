use std::future::Pending;

use formality_core::{
    Cons, Fallible, Set, judgment_fn, set, term, variable::{CoreUniversalVar, CoreVariable}
};
use formality_rust::grammar::minirust::{BasicBlock, FieldProjection, PlaceExpression, Statement, Terminator, ValueExpression};
use formality_types::{
    grammar::{Lt, Parameter, PredicateTy, RefKind, RigidTy, Ty, TyData, Wcs},
    rust::{FormalityLang, term},
};
use formality_prove::combinators::for_all;

use crate::{borrow_check::liveness::LivePlaces, mini_rust_check::{Location, PendingOutlives, TypeckEnv}};

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
// * For every path (L_0...L_n) that leads from a loan L to a statement (S that violates the terms of L),
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

/// A place that is being accessed and the way in which it is being accessed.
#[term]
struct Access {
    /// The kind of access
    kind: AccessKind,

    /// The place being accessed
    place: PlaceExpression
}

#[term]
enum AccessKind {
    /// Reading the value in the place
    Read,

    /// Writing a value to the place
    Write,

    /// Moving a value out of a place
    Move,
}

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
        loans_live_on_entry: Vec<Loan>,
        block: BasicBlock,
        places_live_on_exit: LivePlaces,
    ) => Vec<Loan> {
        debug(block, fn_assumptions, env)

        (
            (loans_in_statements_respected(&env, &fn_assumptions, statements) => ())
            (loans_in_terminator_respected(&env, &fn_assumptions, &terminator) => ())
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
        loans_live_on_entry: Vec<Loan>,
        statements: Vec<Statement>,
        places_live_on_exit: LivePlaces,
    ) => Vec<Loan> {
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
        loans_live_on_entry: Vec<Loan>,
        terminator: Terminator,
        places_live_on_exit: LivePlaces,
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
        loans_live_on_entry: Vec<Loan>,
        statement: Statement,
        places_live_on_exit: LivePlaces,
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
            
            // When you assign to a place, you need to know that
            //
            // * for every loan L that is live (tbd later), you need to know:
            //   * the place being assigned to is not the same place that was borrowed to create L
            //
            // But when is a loan live?
            //
            // * There is a path P in the CFG from the borrow expression that created the loan L in the region R_L
            // * There is a live variable whose type includes a region R
            // * Along the path P, there is some live variable that includes a region that is "descended from" R_L

            // Example:
            //
            // ```
            // let p: &'p u32 = &'b x.0; // Loan L with lifetime
            //                  ------- the borrow of `x.0`
            //                  ------- this expression has the type `&'b u32`
            //        ------- creates an outlive relation `'b: 'p`
            // ...
            // x.0 = 42; // must check that p does not borrow x.
            // read(p);  // `p` is live
            // ```

            // What we want in English:
            //
            // - For every loan L that is invalidated by assigning to Place:
            //   - For every live place Place_live:
            //     - For every region R_live appearing in the type of Place_live:
            //       - For every reachable nodes R_reachable in the outlives graph from L.lt
            //         - R_reachable ≠ R_live



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
        loans_live_on_entry: Vec<Loan>,
        value: ValueExpression,
        places_live_on_exit: LivePlaces,
    ) => () {
        debug(value, fn_assumptions, env)


    }
}

pub type Loans = Set<Loan>;

/// Represents a loan that resulted from executing a borrow expression like `&'0 place`.
#[term]

struct Loan {
    /// The region `'0` of the resulting reference from this borrow.
    lt: Lt,

    /// The place being borrowed.
    place: PlaceExpression,

    /// The kind of borrow (shared, mutable, etc).
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
    fn access_permitted_by_loans(
        env: TypeckEnv,
        assumptions: Wcs,
        loans_live_before_access: Set<Loan>,
        access: Access,
        places_live_after_access: LivePlaces,
    ) => () {
        debug(accessed_place, loaned, live_after, assumptions, env)

        (
            --- ("no loans")
            (access_permitted_by_loans(_env, _assumptions, (), _place, _places_live_after_access) => ())
        )

        (
            (access_permitted_by_loan(&env, &assumptions, loan_head, &access, &places_live_after_access) => ())
            (access_permitted_by_loans(&env, &assumptions, &loans_tail, &access, &places_live_after_access) => ())
            --- ("cons")
            (access_permitted_by_loans(env, assumptions, Cons(loan_head, loans_tail), access, places_live_after_access) => ())
        )
    }
}

judgment_fn! {
    /// Prove that the borrow `borrow` does not affect `place`.
    fn access_permitted_by_loan(
        env: TypeckEnv,
        assumptions: Wcs,
        loan: Loan,
        access: Access,
        places_live_after_access: LivePlaces,
    ) => () {
        debug(accessed_place, loan, live_after, assumptions, env)

        (
            (if place_disjoint_from_place(loan.place, accessed_place))
            --- ("borrows of disjoint places")
            (access_permitted_by_loan(_env, _assumptions, loan, accessed_place, _places_live_after_access) => ())
        )

        (
            --- ("borrow not live -- no live places")
            (access_permitted_by_loan(_env, _assumptions, _loan, _accessed_place, ()) => ())
        )

        (
            (access_permitted_by_loans_in_live_place(&env, &assumptions, &live_place_head, &loan, &accessed_place) => ())
            (place_not_borrowed_by(&env, &assumptions, &loan, &live_places_tail, &accessed_place) => ())
            --- ("borrow not live -- live place")
            (access_permitted_by_loan(env, assumptions, loan, accessed_place, Cons(live_place_head, live_places_tail)) => ())
        )
    }
}

judgment_fn! {
    /// Prove that `accessed_place` can still be accessed even though `live_place` is live and the borrow occurs.
    fn access_permitted_by_loans_in_live_place(
        env: TypeckEnv,
        assumptions: Wcs,
        loan: Loan,
        access: Access,
        live_place: PlaceExpression,
    ) => () {
        debug(loan, access, live_place, assumptions, env)

        (
            --- ("read-shared is ok")
            (access_permitted_by_loans_in_live_place(
                _env,
                _assumptions,
                Loan { kind: RefKind::Shared, .. },
                Access { kind: AccessKind::Read, .. },
                _live_place,
            ) => ())
        )

        (
            (let outlived_by_loan = transitively_outlived_by(&env, &loan.lt))
            (let live_place_ty = env.check_place(&assumptions, &live_place)?)
            (let live_in_place = live_regions_from_place_ty(&env, &live_place_ty))
            // For next time:
            // * think about universal variables
            // * check that the loan does not outlive any of the live regions in the place
            --- ("loan is not live")
            (access_permitted_by_loans_in_live_place(
                env,
                assumptions,
                Loan { kind: RefKind::Mut, .. },
                Access { kind: _, place: place_accessed },
                live_place,
            ) => ())
        )
    }
}

/// Given a region `r`, find a set of all regions `r1` where `r: r1` transitively
/// according to the `pending_outlives` in `env`.
fn transitively_outlived_by(
    env: &TypeckEnv,
    start_lt: &Lt,
) -> Set<Lt> {
    let mut reachable = Set::new();

    reachable.insert(start_lt.clone());
    let mut worklist = vec![start_lt.clone()];

    while let Some(current) = worklist.pop() {
        for PendingOutlives { location: _, a, b } in env.pending_outlives.iter() {
            if a == current {
                if reachable.insert(b.clone()) {
                    worklist.push(b.clone());
                }
            }
        }
    }

    reachable
}

/// Given a region `r`, find a set of all regions `r1` where `r: r1` transitively
/// according to the `pending_outlives` in `env`.
fn live_regions_from_place_ty(
    env: &TypeckEnv,
    ty: &Ty,
) -> Set<Lt> {
    match ty.data() {
        // Given a type like `Foo<'a, 'b, T>`, we would wind up with a set `{a, b}`.
        TyData::RigidTy(RigidTy { name: _, parameters }) => parameters.iter().flat_map(|parameter| match parameter {
            Parameter::Ty(ty_parameter) => live_regions_from_place_ty(env, ty_parameter),
            Parameter::Lt(lt_parameter) => set![lt_parameter.clone()],
            Parameter::Const(_) => set![], // FIXME: what *do* we do with const expressions *anyway*?
        }).collect(),

        TyData::AliasTy(_alias_ty) => todo!("oh crapola let's think about this later"),

        TyData::PredicateTy(predicate_ty) => match predicate_ty {
            // We can ignore binders like the `'a` here, so just peek over them.
            // The bound regions will show up as bound variables below.
            //
            // e.g., `for<'a> fn(&'a u32)`
            PredicateTy::ForAll(binder) => {
                live_regions_from_place_ty(env, &binder.peek())
            }
        },

        TyData::Variable(core_variable) => match core_variable {
            // a generic type variable like `T` in `fn foo<T>`
            CoreVariable::UniversalVar(_) => set![],

            // an inference variable like `_` -- we don't expect this
            CoreVariable::ExistentialVar(_) => panic!("do not expect existentials in borrow checker"),

            // e.g., the `'a' in `for<'a> fn(&'a u32)` -- this we can ignore because they don't represent a live borrow
            CoreVariable::BoundVar(_) => set![],
        },
    }
}

fn place_disjoint_from_place(
    place_a: &PlaceExpression,
    place_b: &PlaceExpression,
) -> bool {
    let prefixes_a = place_prefixes(place_a);
    let prefixes_b = place_prefixes(place_b);
    !prefixes_a.contains(place_b) && !prefixes_b.contains(place_a)
}

/// Returns a set containing `place` and all prefixes of `place`.
fn place_prefixes(
    place: &PlaceExpression,
) -> Set<PlaceExpression> {
    let mut prefixes = Set::new();
    let mut current = place;
    loop {
        prefixes.insert(current.clone());
        match current {
            PlaceExpression::Local(_) => break,
            PlaceExpression::Deref(place_expression) => current = place_expression,
            PlaceExpression::Field(FieldProjection { root, .. }) => current = root,
        }
    }
    prefixes
}

#[test]
fn test_locals_are_disjoint() {
    let place_a: PlaceExpression = term("local(a)");
    let place_b: PlaceExpression = term("local(b)");
    assert!(place_disjoint_from_place(&place_a, &place_b));
}

#[test]
fn test_local_plus_field() {
    let place_a: PlaceExpression = term("local(a)");
    let place_b: PlaceExpression = term("local(a).0");
    assert!(!place_disjoint_from_place(&place_a, &place_b));
}

#[test]
fn test_two_different_fields() {
    let place_a: PlaceExpression = term("local(a).0");
    let place_b: PlaceExpression = term("local(a).1");
    assert!(place_disjoint_from_place(&place_a, &place_b));
}