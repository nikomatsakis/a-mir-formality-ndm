use crate::prove::prove::decls::Program;
use crate::rust::Term;
use formality_core::judgment::ProofTree;
use formality_core::{ProvenSet, Upcast};

use super::{Constraints, Env};

pub fn zip<A, B, C>(
    decls: &Program,
    env: &Env,
    assumptions: &C,
    a: &Vec<A>,
    b: &Vec<B>,
    op: &impl Fn(Program, Env, C, A, B) -> ProvenSet<Constraints>,
) -> ProvenSet<Constraints>
where
    A: Term,
    B: Term,
    C: Term,
{
    let mut a: Vec<A> = a.upcast();
    let mut b: Vec<B> = b.upcast();

    assert_eq!(a.len(), b.len());

    if a.is_empty() && b.is_empty() {
        let constraints = Constraints::none(env);
        let leaf = ProofTree::leaf(format!("{:?}", constraints));
        return ProvenSet::singleton((constraints, leaf));
    }

    let a0 = a.remove(0);
    let b0 = b.remove(0);
    op(decls.upcast(), env.upcast(), assumptions.upcast(), a0, b0).flat_map(|(c1, tree1)| {
        let assumptions = c1.substitution().apply(assumptions);
        let a = c1.substitution().apply(&a);
        let b = c1.substitution().apply(&b);
        zip(decls, c1.env(), &assumptions, &a, &b, op).map(move |(c2, tree2)| {
            (
                c1.seq(c2),
                ProofTree::new("zip", None, vec![tree1.clone(), tree2.clone()]),
            )
        })
    })
}

pub fn for_all<A, C>(
    decls: &Program,
    env: &Env,
    assumptions: &C,
    a: &[A],
    op: &impl Fn(Program, Env, C, A) -> ProvenSet<Constraints>,
) -> ProvenSet<Constraints>
where
    A: Term,
    C: Term,
{
    if a.is_empty() {
        let constraints = Constraints::none(env);
        let leaf = ProofTree::leaf(format!("{:?}", constraints));
        return ProvenSet::singleton((constraints, leaf));
    }

    let a0: A = (&a[0]).upcast();
    let a_remaining: Vec<A> = (&a[1..]).upcast();
    op(decls.upcast(), env.upcast(), assumptions.upcast(), a0).flat_map(|(c1, tree1)| {
        let assumptions = c1.substitution().apply(assumptions);
        let a_remaining = c1.substitution().apply(&a_remaining);
        for_all(decls, c1.env(), &assumptions, &a_remaining, op).map(move |(c2, tree2)| {
            (
                c1.seq(c2),
                ProofTree::new("for_all", None, vec![tree1.clone(), tree2.clone()]),
            )
        })
    })
}
