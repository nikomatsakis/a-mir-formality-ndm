use crate::grammar::Wcs;
use formality_core::judgment_fn;

use crate::prove::prove::decls::Program;

use super::{constraints::Constraints, prove_after::prove_after};

judgment_fn! {
    /// Prove `goals` after completing the current validation phase.
    ///
    /// The transition is local to this proof: apply the current substitution, remove exactly one
    /// outer `Validate` layer from each assumption, and continue with the ordinary solver.
    pub fn prove_after_validation(
        _decls: Program,
        constraints: Constraints,
        assumptions: Wcs,
        goals: Wcs,
    ) => Constraints {
        debug(constraints, goals, assumptions)

        (
            (let (assumptions, goals) = c.substitution().apply((assumptions, goals)))
            (let assumptions = assumptions.promote_validation())
            (prove_after(decls, c, assumptions, goals) => c)
            ----------------------------- ("after validation")
            (prove_after_validation(decls, c, assumptions, goals) => c)
        )
    }
}
