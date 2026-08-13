use crate::grammar::{Mode, Wc, Wcs};
use formality_core::judgment::{EachProof, FailedRule, FailureLocation, ProofTree};
use formality_core::visit::CoreVisit;
use formality_core::{judgment_fn, map, set, ProvenSet, Upcast};
use tracing::Level;

use crate::prove::prove::decls::Program;

use super::{constraints::Constraints, env::Env, minimize::minimize, prove_wc_list::prove_wc_list};

/// Measure the parts of a proof state whose structural growth can indicate divergence.
///
/// Modes are administrative proof-search metadata. Qualifying an existing atomic proposition
/// as `Mode(P)` does not make the proposition itself structurally larger, so charging the
/// mode against `max_size` makes otherwise finite nested impl selection hit
/// the overflow limit. We still count the wrapped proposition in full, except for opaque recursive
/// assumptions as described below: recursive impls that grow from `T` to `Vec<T>` therefore
/// continue to consume the budget as intended. `AtomicPredicate` is likewise a representational
/// wrapper introduced by the mode refactoring, so it does not consume the budget either.
fn proof_search_size(assumptions: &Wcs, goal: &Wcs) -> usize {
    (&assumptions, &goal).size()
        - mode_metadata_size(assumptions)
        - mode_metadata_size(goal)
        - atomic_metadata_size(assumptions)
        - atomic_metadata_size(goal)
        - opaque_assumption_size_discount(assumptions)
}

/// Return the size discount for opaque recursive handles in `assumptions`.
///
/// `Later(G)` is the guarded handle introduced while checking or applying the dictionary
/// constructor for atomic `G`. It can close that exact recursive occurrence, but no rule can
/// inspect `G` through the handle. Its payload is also already represented by the active
/// obligation that caused the handle to be introduced, so charge it less than an ordinary
/// obligation. It cannot be free, however:
/// accumulating distinct handles or growing the proposition inside one is proof-state growth and
/// must eventually reach `max_size`. We therefore discount half its logical size, leaving the
/// other half (rounded up) in the total. This discount applies only to assumptions: a
/// later-qualified goal still pays the full cost of the proposition it asks us to prove.
fn opaque_assumption_size_discount(assumptions: &Wcs) -> usize {
    assumptions
        .iter()
        .map(|assumption| match assumption {
            // `atomic.size()` is the logical `Wc` node plus the atomic payload once the
            // representational `AtomicPredicate` node has been discounted above.
            Wc::Mode(Mode::Later, atomic) => atomic.size() / 2,
            _ => 0,
        })
        .sum()
}

/// Count the representational `AtomicPredicate` node added by the mode refactoring.
///
/// Discounting this node preserves the meaning of `max_size` for both ordinary and mode-qualified
/// propositions: changing their representation must not cause previously finite searches to
/// overflow one node earlier per proposition.
fn atomic_metadata_size(wcs: &Wcs) -> usize {
    wcs.iter().map(|wc| atomic_metadata_size_wc(&wc)).sum()
}

fn atomic_metadata_size_wc(wc: &Wc) -> usize {
    match wc {
        Wc::Atomic(_) | Wc::Mode(_, _) => 1,
        Wc::ForAll(binder) => atomic_metadata_size_wc(binder.peek()),
        Wc::Implies(conditions, consequence) => {
            atomic_metadata_size(conditions) + atomic_metadata_size_wc(consequence)
        }
    }
}

fn mode_metadata_size(wcs: &Wcs) -> usize {
    wcs.iter().map(|wc| mode_metadata_size_wc(&wc)).sum()
}

fn mode_metadata_size_wc(wc: &Wc) -> usize {
    match wc {
        Wc::Atomic(_) => 0,
        Wc::ForAll(binder) => mode_metadata_size_wc(binder.peek()),
        Wc::Implies(conditions, consequence) => {
            mode_metadata_size(conditions) + mode_metadata_size_wc(consequence)
        }
        Wc::Mode(upto, _) => {
            // `Mode` replaces the ordinary `Atomic` `Wc` constructor, so only `Mode` is
            // administrative metadata. The `Wc` and atomic proposition remain logical size.
            upto.size()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::proof_search_size;
    use crate::{
        grammar::{TraitRef, Wc, Wcs},
        rust::term,
    };
    use formality_core::Upcast;

    #[test]
    fn overflow_size_discounts_but_still_charges_opaque_assumptions() {
        let proposition = term::<Wc>("Vec<u32>: Debug");
        let goal: Wcs = proposition.clone().upcast();
        let empty = Wcs::t();
        let later_assumption: Wcs = Wc::later(term::<TraitRef>("Vec<u32>: Debug")).upcast();
        let larger_later_assumption: Wcs =
            Wc::later(term::<TraitRef>("Vec<Vec<u32>>: Debug")).upcast();
        let two_later_assumptions: Wcs = (
            Wc::later(term::<TraitRef>("Vec<u32>: Debug")),
            Wc::later(term::<TraitRef>("Vec<Vec<u32>>: Debug")),
        )
            .upcast();
        let full_assumption: Wcs = proposition.upcast();
        let later_goal: Wcs = Wc::later(term::<TraitRef>("Vec<u32>: Debug")).upcast();

        let baseline = proof_search_size(&empty, &goal);
        let opaque_size = proof_search_size(&later_assumption, &goal);
        let larger_opaque_size = proof_search_size(&larger_later_assumption, &goal);
        let two_opaque_size = proof_search_size(&two_later_assumptions, &goal);
        let full_size = proof_search_size(&full_assumption, &goal);

        assert!(baseline < opaque_size);
        assert!(opaque_size < full_size);
        assert!(opaque_size < larger_opaque_size);
        assert!(larger_opaque_size < two_opaque_size);
        assert_eq!(proof_search_size(&empty, &later_goal), baseline);
    }

    #[test]
    fn overflow_size_still_observes_growth_inside_later() {
        let shallow: Wcs = Wc::later(term::<TraitRef>("u32: Debug")).upcast();
        let deep: Wcs = Wc::later(term::<TraitRef>("Vec<u32>: Debug")).upcast();

        assert!(proof_search_size(&Wcs::t(), &deep) > proof_search_size(&Wcs::t(), &shallow));
    }
}

judgment_fn! {
    pub fn prove_after(
        _decls: Program,
        constraints: Constraints,
        assumptions: Wcs,
        goal: Wcs,
    ) => Constraints {
        debug(constraints, goal, assumptions)
        cut(Constraints::unconditionally_true)

        (
            (let (assumptions, goal) = c1.substitution().apply((assumptions, goal)))
            (prove_substituted(decls, c1.env(), assumptions, goal) => c2)
            --- ("prove_after")
            (prove_after(decls, c1, assumptions, goal) => c1.seq(c2))
        )
    }
}

/// Prove goals in an environment where the accumulated substitution has been applied.
#[track_caller]
fn prove_substituted(
    decls: impl Upcast<Program>,
    env: impl Upcast<Env>,
    assumptions: impl Upcast<Wcs>,
    goal: impl Upcast<Wcs>,
) -> ProvenSet<Constraints> {
    let decls: Program = decls.upcast();
    let env: Env = env.upcast();
    let assumptions: Wcs = assumptions.upcast();
    let goal: Wcs = goal.upcast();

    // "Minimize" the env/assumptions/goals so that we better detect cycles.
    let (env, (assumptions, goal), min) = minimize(env, (assumptions, goal));

    // Establish context for debugging/tracing logs.
    let span = tracing::span!(Level::DEBUG, "prove", ?goal, ?assumptions, ?env, ?decls);
    let _guard = span.enter();

    // Fail if the terms are getting too large ("overflow detection").
    // This is meant to capture complex recursion cycles that will never terminate but also
    // never reach a (simple) cycle, e.g., proving `A: Foo` requires proving `Vec<A>: Foo`
    // requires proving `Vec<Vec<A>>: Foo` etc.
    //
    // In the compiler we use recursion depth instead. We avoid recursion depth because it requires
    // knowing the context in which the proof occurs.
    let term_in = (&assumptions, &goal);
    let proof_search_size = proof_search_size(&assumptions, &goal);
    if proof_search_size > decls.max_size {
        tracing::debug!(
            "term has size {} which exceeds max size of {}",
            proof_search_size,
            decls.max_size
        );
        let constraints = min.reconstitute(Constraints::none(env).ambiguous());
        return ProvenSet::singleton((constraints, ProofTree::leaf("max term size exceeded")));
    }

    // Assert the term we are trying to prove should not have any variables that are not in the environment.
    assert!(env.encloses(term_in));

    // Call `prove_wc_list` to do the real work.
    struct ProveFailureLabel(String);
    let label = ProveFailureLabel(format!(
        "prove {{ goal: {goal:?}, assumptions: {assumptions:?}, env: {env:?}, decls: {decls:?} }}"
    ));
    impl std::fmt::Debug for ProveFailureLabel {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(&self.0)
        }
    }
    let mut results = map![];
    let result_set = if let Err(e) =
        prove_wc_list(decls, &env, assumptions, goal).each_proof(|(result, proof_tree)| {
            results.insert(result, proof_tree);
        }) {
        ProvenSet::failed_rules(label, FailureLocation::caller(), set![FailedRule::new(e)])
    } else {
        ProvenSet::proven(results)
    };

    tracing::debug!(?result_set);

    // Map the results back to the "unminimized" form ("reconstitute").
    let maxified = result_set.map(|(r, proof_tree)| {
        assert!(r.is_valid_extension_of(&env));
        (min.reconstitute(r), proof_tree)
    });

    tracing::debug!(?maxified);

    maxified
}
