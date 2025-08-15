use formality_core::visit::CoreVisit;
use formality_core::{judgment_fn, Downcast, ProvenSet, Upcast};
use formality_core::{Deduplicate, Upcasted};
use formality_types::grammar::{
    AliasTy, ExistentialVar, Lt, LtData, Parameter, Relation, RigidTy, Substitution, TyData,
    UniversalVar, Variable, Wcs,
};

use crate::{
    decls::Decls,
    prove::{
        constraints::occurs_in, prove, prove_after::prove_after, prove_normalize::prove_normalize,
    },
};

use super::{constraints::Constraints, env::Env};

judgment_fn! {
    /// A *outlives* B if --
    ///
    /// * "as long as B is valid, A is valid"
    /// * "if A is invalidated, B *may* be invalidated"
    ///
    /// Outlives is "reflexive" -- `'a: 'a`.
    ///
    /// Examples:
    ///
    /// * `'static: 'a` -- true
    /// *
    ///
    /// Borrow check flow example
    ///
    /// ```rust
    /// fn main() {
    ///     let mut i = 22;
    ///     let p: &'?0 i32 = &i;
    ///     let q: &'?1 i32 = p;   // subtyping requires `&'?0 i32 <: &'?1: i32` requires `'?0: '?1`
    ///     if condition() {
    ///         i += 1;         // <- ok, `p` is dead
    ///     } else {
    ///         i += 1;         // <- error, `p` is live (via `q`)
    ///         println("{q}");
    ///     }
    /// }
    /// ```
    pub fn prove_outlives(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        a: Parameter,
        b: Parameter,
    ) => Constraints {
        debug(a, b, assumptions, env)

        trivial(a == b => Constraints::none(env))

        // 'static outlives us all
        (
            ----------------------------- ("static outlives everything")
            (prove_outlives(_decls, _env, _assumptions, LtData::Static, _b) => Constraints::none(env))
        )

        // assumptions
        //
        // `fn foo<'a, 'b>(x: &'a u32, y: &'b u32) where 'a: 'b`
        //
        // we don't know what 'a and 'b are, but we do know 'a outlives 'b
        //
        // FIXME: we do want to check the transitive case
        // FIXME: there is some logic in prove_wc though

        // FIXME: Niko and tiif to pick this up next week.
        // (
        //     // NB: We should capture assumptions, but that would not match compiler behavior, leave for later.
        //     //
        //     // Example:
        //     //
        //     // ```
        //     // trait Outlives<'a, 'b> where 'a: 'b { }
        //     // impl<'x, 'y> Outlives<'x, 'y> for () where 'x: 'y { }
        //     //
        //     // fn foo<'a, 'b, T>()
        //     // where
        //     //     T: Outlives<'a, 'b>, // implied bound: 'a: 'b
        //     // { }
        //     // ```
        //     //
        //     // now consider proving well-formedness of `for<'a, 'b> /* [where 'a: 'b] */ fn foo(impl Outlives<'a, 'b>)`
        //     //
        //     // anything this is complicated check out the relevant issues that I can't find right now. =)
        //     ----------------------------- ("defer existential lifetime variables")
        //     (prove_outlives(_decls, env, _assumptions,
        //         LtData::Variable(Variable::ExistentialVar(a)),
        //         LtData::Variable(Variable::ExistentialVar(b)),
        //     ) => Constraints::none(env.with_pending(Relation::outlives(a, b))))
        // )
    }
}

// test case
//
// fn foo<'a, 'b>(x: &'a u32, y: &'b u32) -> &'b u32 where 'a: 'b  { x } // OK
// fn foo<'a, 'b>(x: &'a u32, y: &'b u32) -> &'b u32 where 'a: 'b  { y } // ERROR
// fn foo<'a, 'b, 'c>(x: &'a u32, y: &'c u32) -> &'c u32 where 'a: 'b, 'b: 'c  { x } // OK
// fn foo<'b>(x: &'static u32, y: &'b u32) -> &'b u32 { x } // OK
// fn foo<'b>(x: &'b u32, y: &'static u32) -> &'b u32 { x } // ERROR
//
// What is going on here?
//
// (1) Two *universal* (lifetime) variables, 'a and 'b
// (2) Assumption: `Outlives('a, 'b)`
// (3) Goal:
// - `Sub(&'a u32 <: &'b u32)`
//   - `Outlives('a: 'b)``
