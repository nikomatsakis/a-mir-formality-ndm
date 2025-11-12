use formality_core::{
    Cons, Fallible, Set, Union, judgment_fn
};
use formality_rust::grammar::minirust::{BasicBlock, BbId, PlaceExpression, Statement, Terminator, ValueExpression};

use crate::mini_rust_check::{TypeckEnv};

pub type LivePlaces = Set<PlaceExpression>;

judgment_fn! {
    /// Prove that any loans issued in this statement are respected.
    fn places_live_after_basic_block(
        env: TypeckEnv,
        bb_id: BbId,
    ) => LivePlaces {
        debug(bb_id, env)

        (
            (let BasicBlock { id: _, statements, terminator } = env.basic_block(bb_id)?)
            (places_live_after_statements(&env, statements) => places_after_statements)
            (places_live_after_terminator(&env, terminator) => places_after_terminator)
            --- ("lookup")
            (places_live_after_basic_block(env, bb_id) => Union((&places_after_statements, &places_after_terminator)))
        )
    }
}

judgment_fn! {
    fn places_live_after_statements(
        env: TypeckEnv,
        statements: Vec<Statement>,
   ) => LivePlaces {
        debug(statements, env)

        (
            --- ("lookup")
            (places_live_after_statements(_env, ()) => LivePlaces::default())
        )

        (
            (places_live_after_statement(&env, head) => places_after_head)
            (places_live_after_statements(&env, &tail) => places_after_tail)
            --- ("lookup")
            (places_live_after_statements(env, Cons(head, tail)) => Union((&places_after_head, &places_after_tail)))
        )
    }
}

judgment_fn! {
    fn places_live_after_statement(
        env: TypeckEnv,
        statement: Statement,
   ) => LivePlaces {
        debug(statement, env)

        // ...
    }
}

judgment_fn! {
    fn places_live_after_terminator(
        env: TypeckEnv,
        terminator: Terminator,
    ) => LivePlaces {
        debug(terminator, env)

        (
            (places_live_after_basic_block(env, bb_id) => places)
            --- ("goto")
            (places_live_after_terminator(env, Terminator::Goto(bb_id)) => places)
        )
    }
}


impl TypeckEnv {
    fn basic_block(&self, bb_id: BbId) -> Fallible<&BasicBlock> {
        Ok(self.blocks.iter()
            .find(|bb| bb.id == bb_id)
            .ok_or_else(|| anyhow::anyhow!("Basic block {:?} not found", bb_id))?)
    }
}