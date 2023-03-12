use formality_types::{cast::Upcast, collections::Set, set, term::Term};

use crate::program::Program;

use super::{Constraints, Env};

pub fn zip<A, B, C>(
    program: &Program,
    env: &Env,
    context: &C,
    mut a: Vec<A>,
    mut b: Vec<B>,
    op: &impl Fn(Program, Env, C, A, B) -> Set<Constraints>,
) -> Set<Constraints>
where
    A: Term,
    B: Term,
    C: Term,
{
    assert_eq!(a.len(), b.len());

    if a.is_empty() && b.is_empty() {
        return set![Constraints::none(env.upcast())];
    }

    let a0 = a.remove(0);
    let b0 = b.remove(0);
    op(program.clone(), env.clone(), context.clone(), a0, b0)
        .into_iter()
        .flat_map(|c1| {
            let context = c1.substitution().apply(context);
            let a = c1.substitution().apply(&a);
            let b = c1.substitution().apply(&b);
            zip(&program, c1.env(), &context, a, b, op)
                .into_iter()
                .map(move |c2| c1.seq(c2))
        })
        .collect()
}
