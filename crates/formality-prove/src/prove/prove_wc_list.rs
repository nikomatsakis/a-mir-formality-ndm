use formality_types::{grammar::Wcs, judgment_fn};

use crate::{
    decls::Decls,
    prove::{constraints::Constraints, prove_after::prove_after},
};

use super::{env::Env, prove_wc::prove_wc};

judgment_fn! {
    /// Prove that a list of `goals` is true given the program declarations `decls`,
    /// the environment `env`, and the list of `assumptions`.
    ///
    /// Returns the list of constraints under which the goals are true.
    pub fn prove_wc_list(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        goals: Wcs,
    ) => Constraints {
        debug(goals, assumptions, env, decls)

        assert(env.encloses((&assumptions, &goals)))

        (
            --- ("none")
            (prove_wc_list(_decls, env, _assumptions, ()) => Constraints::none(env))
        )

        (
            (prove_wc(&decls, env, &assumptions, wc0) => c)
            (prove_after(&decls, c, &assumptions, &wcs1) => c)
            --- ("some")
            (prove_wc_list(decls, env, assumptions, (wc0, wcs1)) => c)
        )
    }
}
