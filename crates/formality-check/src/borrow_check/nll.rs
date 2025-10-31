use formality_core::{
    variable::{
        CoreUniversalVar,
        CoreVariable::{BoundVar, ExistentialVar, UniversalVar},
    },
    Fallible, Set,
};
use formality_types::{
    grammar::{ParameterKind, Variable, Wc, WcData, Wcs},
    rust::FormalityLang,
};

use crate::mini_rust_check::{Location, TypeckEnv};

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
// XXX Niko's plan:
//
// * Find all paths `(L_0...L_n)` from a loan with lifetime `'0` to a statement violating that loan
// * Prove that each location `L_i \in (L_0..L_n)` along that path is not a member of `'0`
// * We further need to prove something about universals :) 


/// The borrow checker's job is to pick up where the type-checker left off:
/// Given the `TypeckEnv`, which includes a (populated) list of `pending_outlives`
/// constraints, it attempts to find values for the existential lifetime variables (inference variables)
/// that satisfy those pending-outlives constraints and which meet the borrow checker's rules.
pub fn borrow_check(typeck_env: &TypeckEnv<'_>, fn_assumptions: &Wcs) -> Fallible<()> {
    /*

        // Good:
        fn foo<'a>(x: &'a (String, String)) -> &'a String {
            &x.0

            // Coming in to borrow check we have two constraints to solve:
            //
            // * `'a: '0`
            // * `'0: 'a`
            //
            // For `'a: '0` to hold, we must prove that for all constraints `'0: 'X`, `'a: 'X`
            // We can prove `'a: 'a` (identity).
            // So we are happy.
        }

        // Bad:
        fn bar<'a,'b>(x: &'a (String, String)) -> &'b String {
            &x.0

            // Coming in to borrow check we have two constraints to solve:
            //
            // * `'a: '0`
            // * `'0: 'b`
            //
            // For `'a: '0` to hold, we must prove that for all constraints `'0: 'X`, `'a: 'X`.
            // But we cannot prove `'a: 'b`.
            // So we error.
        }

     */


    // Extract the list of inference variables.
    let inference_variables = typeck_env
        .env
        .variables()
        .iter()
        .filter_map(|v| match v {
            ExistentialVar(core_existential_var) => Some(core_existential_var.clone()),
            UniversalVar(_) | BoundVar(_) => None,
        })
        .collect::<Vec<_>>();

    // We expect to only find lifetime inference variables in the type-check environment.
    assert!(inference_variables
        .iter()
        .all(|v| v.kind == ParameterKind::Lt));

    // XXX: Next week -- can we reformulate this as a more declarative set of judgments, instead?

    // Figure out the outlives that we assume to be true
    fn_assumptions.iter().filter_map(to_outlives_assumption).collect();

    // For now, the "value" of an inference variable is
    let mut inference_variable_values = vec![LifetimeValue::Empty; inference_variables.len()];

    // Fixed point computation to infer values of the inference variables
    let mut changed = true;
    while !changed {
        changed = false;

        for p in &typeck_env.pending_outlives {
            // enforce this outlives requirement, modifying inference_variable_values to make it true
            // possible reporting an error
        }
    }

    Ok(())
}

fn to_outlives_assumption(
    wc: Wc,
) -> Option<OutlivesAssumption> {
    match wc.data() {
        WcData::Relation(relation) => todo!(),
        WcData::Predicate(predicate) => todo!(),
        WcData::ForAll(core_binder) => todo!(),
        WcData::Implies(wcs, wc) => todo!(),
    }
}

pub struct OutlivesAssumption {
    a: LifetimeValue,
    b: LifetimeValue,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LifetimeValue {
    Empty,
    Static,
    Placeholder(CoreUniversalVar<FormalityLang>),
}
