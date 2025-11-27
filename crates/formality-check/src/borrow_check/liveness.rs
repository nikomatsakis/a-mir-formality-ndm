use formality_core::{fixed_point, judgment_fn, Cons, Fallible, Set, SetExt, Union};
use formality_rust::grammar::minirust::{
    ArgumentExpression, BasicBlock, BbId, PlaceExpression, Statement, Terminator, ValueExpression,
};

use crate::mini_rust_check::TypeckEnv;

pub type LivePlaces = Set<PlaceExpression>;

/// Given a basic-block id, returns the places live on entry to the basic block.
#[formality_core::fixed_point]
fn places_live_before_basic_block(env: &TypeckEnv, bb_id: &BbId) -> LivePlaces {
    let BasicBlock {
        id: _,
        statements,
        terminator,
    } = env.basic_block(bb_id).expect("valid basic block id");

    let places = places_live_before_terminator(env, terminator);
    let places = places_live_before_statements(env, statements, places);
    places
}

/// Returns the places that are live before the terminator executes.
pub fn places_live_before_terminator(env: &TypeckEnv, terminator: &Terminator) -> LivePlaces {
    match terminator {
        Terminator::Goto(bb_id) => places_live_before_basic_block(env, bb_id),
        Terminator::Switch {
            switch_value,
            switch_targets,
            fallback,
        } => {
            let mut places = places_live_before_basic_block(env, fallback);
            for target in switch_targets {
                places = places.union_with(places_live_before_basic_block(env, &target.target));
            }
            places = places_live_before_value(env, switch_value, places);
            places
        }
        Terminator::Call {
            callee,
            generic_arguments: _,
            arguments,
            ret,
            next_block,
        } => {
            let mut places_live = match next_block {
                Some(bb_id) => places_live_before_basic_block(env, bb_id),
                None => LivePlaces::default(),
            };
            places_live = places_live_before_assigning(env, ret, places_live);
            for argument in arguments {
                places_live = places_live_before_argument(env, argument, places_live);
            }
            places_live = places_live_before_value(env, callee, places_live);
            places_live
        }
        Terminator::Return => LivePlaces::default(),
    }
}

/// Given a list of statements `statements` and a set of places `places_live` that are live after the statements,
/// returns the set of places live BEFORE the statements. This will include all places in `places_live`
/// minus any places overwritten by the statements, plus any places accessed by the statements.
pub fn places_live_before_statements(
    env: &TypeckEnv,
    statements: &[Statement],
    mut places_live: LivePlaces,
) -> LivePlaces {
    for statement in statements.iter().rev() {
        places_live = places_live_before_statement(env, statement, places_live);
    }
    places_live
}

/// Given a statement `statement` and a set of places `places_live` that are live after the statement,
/// returns the set of places live BEFORE the statement. This will include all places in `places_live`
/// minus any places overwritten by the statement, plus any places accessed.
pub fn places_live_before_statement(
    env: &TypeckEnv,
    statement: &Statement,
    mut places_live: LivePlaces,
) -> LivePlaces {
    match statement {
        Statement::Assign(place_expression, value_expression) => {
            places_live = places_live_before_assigning(env, place_expression, places_live);
            places_live = places_live_before_value(env, value_expression, places_live);
            places_live
        }
        Statement::PlaceMention(place_expression) => {
            places_live = places_live_before_accessing(env, place_expression, places_live);
            places_live
        }
        Statement::StorageLive(_) | Statement::StorageDead(_) => places_live,
    }
}

/// Given an argument expression `expr` and a set of places `places_live` that are live after the argument expression,
/// returns the set of places live BEFORE the argument expression. This will include all places in `places_live`
/// plus any additional places accessed by the argument expression.
fn places_live_before_argument(
    env: &TypeckEnv,
    expr: &ArgumentExpression,
    places_live: LivePlaces,
) -> LivePlaces {
    match expr {
        ArgumentExpression::ByValue(value_expression) => {
            places_live_before_value(env, value_expression, places_live)
        }
        ArgumentExpression::InPlace(place_expression) => {
            places_live_before_accessing(env, place_expression, places_live)
        }
    }
}

/// Given a value expression `expr` and a set of places `places_live` that are live after the value expression,
/// returns the set of places live BEFORE the value expression. This will include all places in `places_live`
/// plus any additional places accessed by the value expression.
fn places_live_before_value(
    env: &TypeckEnv,
    expr: &ValueExpression,
    mut places_live: LivePlaces,
) -> LivePlaces {
    match expr {
        ValueExpression::Constant(_) => places_live,
        ValueExpression::Fn(_) => places_live,
        ValueExpression::Struct(value_expressions, _ty) => {
            for value_expression in value_expressions {
                places_live = places_live_before_value(env, value_expression, places_live);
            }
            places_live
        }
        ValueExpression::Load(place_expression) => {
            places_live_before_accessing(env, place_expression, places_live)
        }
        ValueExpression::Ref(_lt, place_expression) => {
            places_live_before_accessing(env, place_expression, places_live)
        }
    }
}

/// Given a place expression `place_assigned` and a set of places `places_live` that are live after `place_assigned`
/// is assigned,  returns the set of places live BEFORE the assignment. This will include all places in `places_live`
/// except those that are overwritten by assigning to `place_assigned`.
///
/// This is because whatever value resides in `place_assigned` before the assignment is not relevant afterwards.
fn places_live_before_assigning(
    _env: &TypeckEnv,
    place_assigned: &PlaceExpression,
    mut places_live: LivePlaces,
) -> LivePlaces {
    places_live.retain(|p| !place_assigned.is_prefix_of(p));
    places_live
}

/// Given a place expression `place` and a set of places `places_live` that are live after `place`
/// is accessed, returns the set of places live BEFORE `place` is accessed. This will include all places in `places_live`
/// plus `place`.
fn places_live_before_accessing(
    _env: &TypeckEnv,
    place: &PlaceExpression,
    mut places_live: LivePlaces,
) -> LivePlaces {
    places_live.insert(place.clone());
    places_live
}

impl TypeckEnv {
    fn basic_block(&self, bb_id: &BbId) -> Fallible<&BasicBlock> {
        Ok(self
            .blocks
            .iter()
            .find(|bb| bb.id == *bb_id)
            .ok_or_else(|| anyhow::anyhow!("Basic block {:?} not found", bb_id))?)
    }
}
