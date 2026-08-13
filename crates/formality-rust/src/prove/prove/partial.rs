//! The input contract made available while constructing one impl dictionary.

use crate::grammar::{
    AtomicPredicate, Binder, BoundVar, Predicate, TraitId, TraitRef, Wc, WcData, Wcs,
};
use formality_core::{judgment_fn, Cons};

use super::{trait_less_than, Program};

fn is_trait_less_than(program: &Program, lower: &TraitId, upper: &TraitId) -> bool {
    trait_less_than(program, lower, upper).is_proven()
}

fn bind_each(variables: &[BoundVar], clauses: &Wcs) -> Wcs {
    clauses
        .iter()
        .map(|clause| Wc::for_all(Binder::new(variables, clause)))
        .collect()
}

fn imply_each(conditions: &Wcs, consequences: &Wcs) -> Wcs {
    consequences
        .iter()
        .map(|consequence| Wc::implies(conditions, consequence))
        .collect()
}

judgment_fn! {
    /// Translate `clauses` into the evidence contract available below `root`.
    ///
    /// Impl well-formedness assumes this contract; impl application proves the same contract.
    /// Consequently the translation is independent of whether its result is later placed in
    /// assumption or goal position.
    pub(crate) fn partial(
        program: Program,
        root: TraitId,
        clauses: Wcs,
    ) => Wcs {
        debug(root, clauses, program)

        (
            ----------------------------- ("empty")
            (partial(_program, _root, ()) => ())
        )

        (
            (partial_wc(program, root, clause) => translated)
            (partial(program, root, rest) => translated_rest)
            ----------------------------- ("clauses")
            (partial(program, root, Cons(clause, rest)) => (translated, translated_rest))
        )
    }
}

#[cfg(test)]
mod tests {
    use super::partial;
    use crate::grammar::{Crates, TraitId, TraitRef, Wc, Wcs};
    use crate::prove::prove::Program;
    use crate::rust::term;
    use formality_core::Upcast;

    fn program(source: &str) -> Program {
        term::<Crates>(source).to_prove_decls()
    }

    fn translated(program: &Program, root: &str, input: impl Upcast<Wcs>) -> Wcs {
        partial(program, term::<TraitId>(root), input)
            .into_singleton()
            .unwrap()
            .0
    }

    #[test]
    fn lower_trait_is_a_completed_input() {
        let program = program(
            "[
                crate test {
                    trait Base {}
                    trait Root where Self: Base {}
                }
            ]",
        );
        let input = term::<TraitRef>("u32: Base");

        assert_eq!(translated(&program, "Root", &input), input.upcast());
    }

    #[test]
    fn non_lower_trait_is_guarded() {
        let program = program(
            "[
                crate test {
                    trait Root {}
                    trait Other {}
                }
            ]",
        );
        let input = term::<TraitRef>("u32: Other");

        assert_eq!(
            translated(&program, "Root", &input),
            Wc::later(input).upcast(),
        );
    }

    #[test]
    fn implication_accepts_complete_inputs_and_returns_partial_output() {
        let program = program(
            "[
                crate test {
                    trait Root {}
                    trait Input {}
                    trait Output {}
                }
            ]",
        );
        let condition = term::<TraitRef>("u32: Input");
        let consequence = term::<TraitRef>("u32: Output");

        assert_eq!(
            translated(&program, "Root", Wc::implies(&condition, &consequence)),
            Wc::implies(condition, Wc::later(consequence)).upcast(),
        );
    }
}

judgment_fn! {
    /// Translate one where-clause into the partial evidence it represents.
    fn partial_wc(
        program: Program,
        root: TraitId,
        clause: Wc,
    ) => Wcs {
        debug(root, clause, program)

        // A dictionary strictly below the construction root is complete. Keeping it as one
        // ordinary proposition is important: splitting it into `Later(P)` plus the clauses in
        // `Requirements(P)` would let coinductive proof search cyclically combine those separate
        // pieces into a dictionary that no impl actually constructs.
        (
            (trait_less_than(program, trait_id, root) => ())!
            ----------------------------- ("completed lower trait")
            (partial_wc(
                program,
                root,
                trait_ref @ TraitRef {
                    trait_id,
                    parameters: _,
                },
            ) => trait_ref)
        )

        // At or outside the construction frontier, only the guarded promise of a dictionary is
        // available. None of its requirement fields may be inspected.
        (
            (if !is_trait_less_than(program, trait_id, root))!
            (let later = Wc::later(Predicate::is_implemented(trait_ref)))
            ----------------------------- ("opaque trait")
            (partial_wc(
                program,
                root,
                trait_ref @ TraitRef {
                    trait_id,
                    parameters: _,
                },
            ) => later)
        )

        // Non-dictionary facts have no partially initialized fields. They remain ordinary facts
        // in the constructor's input contract.
        (
            ----------------------------- ("predicate")
            (partial_wc(
                _program,
                _root,
                predicate @ (
                    Predicate::NotImplemented(_)
                    | Predicate::AliasEq(_, _)
                    | Predicate::WellFormedTraitRef(_)
                    | Predicate::IsLocal(_)
                    | Predicate::ConstHasType(_, _)
                ),
            ) => predicate)
        )

        (
            ----------------------------- ("relation")
            (partial_wc(_program, _root, AtomicPredicate::Relation(relation)) => relation)
        )

        // A conditional dictionary accepts complete inputs when invoked and returns partial
        // evidence at this construction frontier. The same conditional clauses can therefore be
        // assumed during ImplWF and proven during impl application.
        (
            (partial_wc(program, root, consequence) => consequences)
            (let implications = imply_each(conditions, consequences))
            ----------------------------- ("implication")
            (partial_wc(
                program,
                root,
                WcData::Implies(conditions, consequence),
            ) => implications)
        )

        (
            (let (variables, body) = binder.open())
            (partial_wc(program, root, body) => clauses)
            (let quantified = bind_each(variables, clauses))
            ----------------------------- ("forall")
            (partial_wc(program, root, WcData::ForAll(binder)) => quantified)
        )
    }
}
