use formality_core::{
    variable::{
        CoreUniversalVar,
        CoreVariable::{BoundVar, ExistentialVar, UniversalVar},
    },
    Fallible,
};
use formality_types::{
    grammar::{ParameterKind, UniversalVar, Variable, Wc, WcData, Wcs},
    rust::FormalityLang,
};

use crate::mini_rust_check::TypeckEnv;

/// The borrow checker's job is to pick up where the type-checker left off:
/// Given the `TypeckEnv`, which includes a (populated) list of `pending_outlives`
/// constraints, it attempts to find values for the existential lifetime variables (inference variables)
/// that satisfy those pending-outlives constraints and which meet the borrow checker's rules.
pub fn borrow_check(typeck_env: &TypeckEnv<'_>, fn_assumptions: &Wcs) -> Fallible<()> {
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
