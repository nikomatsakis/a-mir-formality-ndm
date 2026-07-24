//! Structured requirements declared by traits and validation of selected impls.

use crate::grammar::{
    AliasTy, AssociatedItemId, AssociatedTy, AssociatedTyBoundData, AssociatedTyValueBoundData,
    Binder, BoundVar, ImplItem, Parameter, ParameterKind, Predicate, Relation, Trait,
    TraitBoundData, TraitImplBoundData, TraitItem, TraitRef, Ty, ValidationState, Wc, WcData, Wcs,
};
use crate::prove::ToWcs;
use formality_core::{judgment_fn, set, Cons, Downcast, Set, Upcast};
use formality_macros::term;

use super::{
    prove::{prove_after, prove_via_assumption, Constraints, Env},
    Program,
};

/// A semantic requirement declared by a trait.
///
/// The outer binder binds `Self` and the trait's explicit parameters. Requirements retain their
/// category so impl validation does not have to recover it from a flattened where-clause.
#[term]
pub struct TraitRequirement {
    pub binder: Binder<TraitRequirementBoundData>,
}

#[term]
pub enum TraitRequirementBoundData {
    /// A supertrait of `Self`. The inner binder preserves higher-ranked parameters.
    Supertrait(Binder<TraitRef>),

    /// An outlives requirement on `Self`. The inner binder preserves higher-ranked parameters.
    Outlives(Binder<Relation>),

    #[cast]
    AssociatedTyRequirement(AssociatedTyRequirement),
}

/// Classification of one trait-header clause.
///
/// Only direct requirements on `Self` become selection-time trait requirements. Every other
/// clause remains an input well-formedness condition on impl headers.
#[term]
pub enum TraitHeaderClause {
    Supertrait(Binder<TraitRef>),
    Outlives(Binder<Relation>),
    InputWellFormed(Wc),
}

/// The bounds declared for one associated type.
#[term]
pub struct AssociatedTyRequirement {
    pub id: AssociatedItemId,

    /// The outer binder binds the associated type's parameters. The inner binder binds one type
    /// variable representing the associated type value. Instantiating that value variable with a
    /// projection yields implied bounds; instantiating it with an impl's concrete value yields
    /// validation obligations.
    pub binder: Binder<Binder<Wcs>>,
}

judgment_fn! {
    /// Use one structured requirement for ordinary implied-bound reasoning.
    pub fn prove_via_trait_requirement(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        trait_def: Trait,
        requirement: TraitRequirement,
        goal: Wc,
    ) => Constraints {
        debug(assumptions, trait_def, requirement, goal, env)

        (
            (let (env, trait_subst) =
                env.existential_substitution(&requirement.binder))
            (let requirement =
                requirement.binder.instantiate_with(trait_subst)?)
            (if let TraitRequirementBoundData::Supertrait(supertrait) = requirement)!
            (let required = quantified_goal(
                supertrait,
                |trait_ref| Predicate::is_implemented(trait_ref).upcast(),
            ))
            (prove_via_assumption(decls, env, assumptions, required, goal) => c)
            (let source = TraitRef::new(&trait_def.id, trait_subst))
            (prove_after(decls, c, assumptions, source) => c)
            ----------------------------- ("supertrait")
            (prove_via_trait_requirement(
                decls,
                env,
                assumptions,
                trait_def,
                requirement,
                goal,
            ) => c.pop_subst(trait_subst))
        )

        (
            (let (env, trait_subst) =
                env.existential_substitution(&requirement.binder))
            (let requirement =
                requirement.binder.instantiate_with(trait_subst)?)
            (if let TraitRequirementBoundData::AssociatedTyRequirement(associated) =
                requirement)
            (let (env, associated_subst) =
                env.existential_substitution(&associated.binder))
            (let value_template =
                associated.binder.instantiate_with(associated_subst)?)
            (let alias = AliasTy::associated_ty(
                &trait_def.id,
                &associated.id,
                associated_subst.len(),
                (trait_subst, associated_subst),
            ))
            (let value_bounds =
                value_template.instantiate_with(std::slice::from_ref(&alias))?)
            (required in value_bounds)!
            (prove_via_assumption(decls, env, assumptions, required, goal) => c)

            (let trait_data = trait_def.binder.instantiate_with(trait_subst)?)
            (let trait_associated_ty = trait_data
                .trait_items
                .iter()
                .find_map(|item| match item {
                    TraitItem::AssociatedTy(associated_ty)
                        if associated_ty.id == associated.id =>
                    {
                        Some(associated_ty)
                    }
                    _ => None,
                })
                .ok_or_else(|| anyhow::anyhow!(
                    "trait has no associated type {:?}",
                    associated.id,
                ))?)
            (let AssociatedTyBoundData {
                ensures: _,
                where_clauses,
            } = trait_associated_ty.binder.instantiate_with(associated_subst)?)
            (let source = TraitRef::new(&trait_def.id, trait_subst))
            (prove_after(decls, c, assumptions, where_clauses.to_wcs()) => c)
            (prove_after(decls, c, assumptions, source) => c)
            (let c = c.pop_subst(associated_subst))
            ----------------------------- ("associated type")
            (prove_via_trait_requirement(
                decls,
                env,
                assumptions,
                trait_def,
                requirement,
                goal,
            ) => c.pop_subst(trait_subst))
        )

    }
}

judgment_fn! {
    /// Generate all structured requirements from a trait declaration.
    ///
    /// Returning one set makes a trait with no requirements a successful empty result while
    /// preserving failures from classifying or constructing any individual requirement. Method
    /// where-clauses are intentionally not considered here.
    pub fn trait_requirement(
        trait_def: Trait,
    ) => Set<TraitRequirement> {
        debug(trait_def)

        (
            (let (variables, TraitBoundData { where_clauses, trait_items }) =
                trait_def.binder.open())
            (let self_parameter: Parameter = variables[0].upcast())
            (trait_header_requirements(
                variables,
                self_parameter,
                where_clauses.to_wcs(),
            ) => header_requirements)
            (associated_ty_requirements(variables, trait_items) => associated_ty_requirements)
            ----------------------------- ("trait requirements")
            (trait_requirement(trait_def) => (header_requirements, associated_ty_requirements))
        )
    }
}

judgment_fn! {
    /// Collect the requirements contributed by trait-header clauses.
    fn trait_header_requirements(
        trait_variables: Vec<BoundVar>,
        self_parameter: Parameter,
        clauses: Wcs,
    ) => Set<TraitRequirement> {
        debug(trait_variables, self_parameter, clauses)

        (
            ----------------------------- ("empty")
            (trait_header_requirements(_trait_variables, _self_parameter, ()) => ())
        )

        (
            (trait_header_clause(self_parameter, clause) => classification)
            (let requirement =
                trait_requirement_from_header_clause(trait_variables, classification))
            (trait_header_requirements(
                trait_variables,
                self_parameter,
                rest,
            ) => rest_requirements)
            ----------------------------- ("clause")
            (trait_header_requirements(
                trait_variables,
                self_parameter,
                Cons(clause, rest),
            ) => (requirement, rest_requirements))
        )
    }
}

judgment_fn! {
    /// Collect the requirements contributed by associated types.
    fn associated_ty_requirements(
        trait_variables: Vec<BoundVar>,
        trait_items: Vec<TraitItem>,
    ) => Set<TraitRequirement> {
        debug(trait_variables, trait_items)

        (
            ----------------------------- ("empty")
            (associated_ty_requirements(_trait_variables, ()) => ())
        )

        (
            (associated_ty_requirements(trait_variables, rest) => requirements)
            ----------------------------- ("function")
            (associated_ty_requirements(
                trait_variables,
                Cons(TraitItem::Fn(_function), rest),
            ) => requirements)
        )

        (
            (let (associated_variables, AssociatedTyBoundData { ensures, where_clauses: _ }) =
                binder.open())

            (let value_variable = BoundVar::fresh(ParameterKind::Ty))
            (let value_ty: Ty = value_variable.upcast())
            (let value_bounds: Wcs =
                ensures.iter().map(|ensure| ensure.to_wc(value_ty)).collect())
            (let value_binder: Binder<Wcs> =
                Binder::new(vec![value_variable], value_bounds.upcast()))
            (let associated_binder: Binder<Binder<Wcs>> =
                Binder::new(associated_variables, value_binder.upcast()))
            (let associated_requirement =
                AssociatedTyRequirement::new(id, associated_binder))
            (let requirement = TraitRequirement::new(Binder::new(
                trait_variables,
                TraitRequirementBoundData::associated_ty_requirement(associated_requirement),
            )))
            (associated_ty_requirements(trait_variables, rest) => rest_requirements)
            ----------------------------- ("associated type")
            (associated_ty_requirements(
                trait_variables,
                Cons(TraitItem::AssociatedTy(AssociatedTy { id, binder }), rest),
            ) => Cons(requirement, rest_requirements))
        )
    }
}

judgment_fn! {
    /// Classify one trait-header clause without erasing its semantic role.
    ///
    /// Only a direct predicate or outlives relation on `Self` is a selection-time requirement.
    /// Other surface where-clause forms are explicitly classified as input well-formedness
    /// conditions. `Validate` has no rule because it is internal to the solver and cannot occur in
    /// a trait declaration.
    pub fn trait_header_clause(
        self_parameter: Parameter,
        clause: Wc,
    ) => TraitHeaderClause {
        debug(self_parameter, clause)

        (
            (if let Predicate::IsImplemented(trait_ref) = predicate)
            (if trait_ref.parameters.first() == Some(self_parameter))!
            ----------------------------- ("supertrait")
            (trait_header_clause(
                self_parameter,
                WcData::Predicate(predicate),
            ) => TraitHeaderClause::supertrait(Binder::<TraitRef>::dummy(trait_ref.upcast())))
        )

        (
            (if source == self_parameter)!
            ----------------------------- ("outlives")
            (trait_header_clause(
                self_parameter,
                WcData::Relation(Relation::Outlives(source, target)),
            ) => TraitHeaderClause::outlives(Binder::dummy(Relation::outlives(source, target))))
        )

        (
            (let opened: (Vec<BoundVar>, Wc) = binder.open())
            (trait_header_clause(self_parameter, &opened.1) => classification)
            (let classification =
                rebind_trait_header_clause(&opened.0, classification, binder))
            ----------------------------- ("higher-ranked")
            (trait_header_clause(
                self_parameter,
                WcData::ForAll(binder),
            ) => classification)
        )

        (
            (if !matches!(
                predicate,
                Predicate::IsImplemented(trait_ref)
                    if trait_ref.parameters.first() == Some(self_parameter)
            ))!
            ----------------------------- ("input predicate")
            (trait_header_clause(
                self_parameter,
                WcData::Predicate(predicate),
            ) => TraitHeaderClause::input_well_formed(Wc::predicate(predicate)))
        )

        (
            (if !matches!(
                relation,
                Relation::Outlives(source, _) if source == self_parameter
            ))!
            ----------------------------- ("input relation")
            (trait_header_clause(
                self_parameter,
                WcData::Relation(relation),
            ) => TraitHeaderClause::input_well_formed(Wc::relation(relation)))
        )

        (
            ----------------------------- ("input implication")
            (trait_header_clause(
                _self_parameter,
                WcData::Implies(conditions, consequence),
            ) => TraitHeaderClause::input_well_formed(Wc::implies(conditions, consequence)))
        )
    }
}

fn rebind_trait_header_clause(
    outer_variables: &[BoundVar],
    classification: &TraitHeaderClause,
    original_binder: &Binder<Wc>,
) -> TraitHeaderClause {
    match classification {
        TraitHeaderClause::Supertrait(inner) => {
            let (inner_variables, trait_ref) = inner.open();
            TraitHeaderClause::supertrait(Binder::new(
                (outer_variables, &inner_variables),
                trait_ref,
            ))
        }

        TraitHeaderClause::Outlives(inner) => {
            let (inner_variables, relation) = inner.open();
            TraitHeaderClause::outlives(Binder::new((outer_variables, &inner_variables), relation))
        }

        TraitHeaderClause::InputWellFormed(_) => {
            TraitHeaderClause::input_well_formed(Wc::for_all(original_binder))
        }
    }
}

fn classify_trait_header_clause(
    self_parameter: &Parameter,
    clause: &Wc,
) -> anyhow::Result<TraitHeaderClause> {
    let proven = trait_header_clause(self_parameter, clause)
        .into_singleton()
        .map_err(|error| anyhow::anyhow!("{}", error.format_leaves()))?;
    Ok(proven.0)
}

fn trait_requirement_from_header_clause(
    trait_variables: &[BoundVar],
    classification: &TraitHeaderClause,
) -> Set<TraitRequirement> {
    let requirement_data = match classification {
        TraitHeaderClause::Supertrait(supertrait) => {
            TraitRequirementBoundData::supertrait(supertrait)
        }
        TraitHeaderClause::Outlives(outlives) => TraitRequirementBoundData::outlives(outlives),
        TraitHeaderClause::InputWellFormed(_) => return set![],
    };

    set![TraitRequirement::new(Binder::new(
        trait_variables,
        requirement_data,
    ))]
}

/// Requirements that make an impl header a well-formed input to the trait.
///
/// Supertraits and `Self` outlives bounds are validated when a positive impl is selected. All
/// other trait-header clauses retain their declaration-time input-well-formedness behavior.
pub fn trait_input_wf_requirements(trait_def: &Trait, trait_ref: &TraitRef) -> anyhow::Result<Wcs> {
    let (
        trait_variables,
        TraitBoundData {
            where_clauses,
            trait_items: _,
        },
    ) = trait_def.binder.open();
    let self_parameter: Parameter = trait_variables[0].upcast();

    let input_clauses: Wcs = where_clauses
        .to_wcs()
        .into_iter()
        .map(|clause| -> anyhow::Result<Option<Wc>> {
            Ok(
                match classify_trait_header_clause(&self_parameter, &clause)? {
                    TraitHeaderClause::InputWellFormed(clause) => Some(clause),
                    TraitHeaderClause::Supertrait(_) | TraitHeaderClause::Outlives(_) => None,
                },
            )
        })
        .collect::<anyhow::Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect();
    let input_clauses =
        Binder::new(&trait_variables, input_clauses).instantiate_with(&trait_ref.parameters)?;

    let parameter_requirements: Wcs = trait_ref
        .parameters
        .iter()
        .map(Relation::well_formed)
        .collect();

    Ok((parameter_requirements, input_clauses).upcast())
}

judgment_fn! {
    /// Validate all requirements of one selected impl, threading constraints between them.
    pub fn validate_impl(
        _decls: Program,
        constraints: Constraints,
        assumptions: Wcs,
        trait_impl: TraitImplBoundData,
    ) => Constraints {
        debug(constraints, assumptions, trait_impl)

        (
            (let trait_def = decls.trait_def(&trait_impl.trait_id))
            (trait_requirement(trait_def) => requirements)
            (for_all(requirement in requirements) with(c)
                // Header matching and earlier requirements may have constrained the selected
                // impl's existential parameters. Specialize all inputs with the current
                // accumulator, then instantiate the requirement's outer trait binder.
                (let (assumptions, (trait_impl, requirement)) =
                    c.substitution().apply((assumptions, (trait_impl, requirement))))
                (let trait_ref = trait_impl.trait_ref())
                (let requirement =
                    requirement.binder.instantiate_with(&trait_ref.parameters)?)
                (validate_impl_against_requirement(
                    decls,
                    c,
                    assumptions,
                    trait_impl,
                    requirement,
                ) => c))
            ----------------------------- ("all requirements")
            (validate_impl(decls, c, assumptions, trait_impl) => c)
        )
    }
}

judgment_fn! {
    /// Validate a selected impl against one instantiated trait requirement.
    pub fn validate_impl_against_requirement(
        _decls: Program,
        constraints: Constraints,
        assumptions: Wcs,
        trait_impl: TraitImplBoundData,
        requirement: TraitRequirementBoundData,
    ) => Constraints {
        debug(constraints, assumptions, trait_impl, requirement)

        (
            (let goal = quantified_goal(
                supertrait,
                |trait_ref| Predicate::is_implemented(trait_ref).upcast(),
            ))
            (let goal = Wc::validate(ValidationState::A, goal))
            (prove_after(decls, c, assumptions, goal) => c)
            ----------------------------- ("supertrait")
            (validate_impl_against_requirement(
                decls,
                c,
                assumptions,
                trait_impl,
                TraitRequirementBoundData::Supertrait(supertrait),
            ) => c)
        )

        (
            (let goal = quantified_goal(
                outlives,
                |relation| relation.upcast(),
            ))
            (let goal = Wc::validate(ValidationState::A, goal))
            (prove_after(decls, c, assumptions, goal) => c)
            ----------------------------- ("outlives")
            (validate_impl_against_requirement(
                decls,
                c,
                assumptions,
                trait_impl,
                TraitRequirementBoundData::Outlives(outlives),
            ) => c)
        )

        (
            (let goals =
                associated_ty_validation_goals(decls, trait_impl, associated)?)
            (prove_after(decls, c, assumptions, goals) => c)
            ----------------------------- ("associated type")
            (validate_impl_against_requirement(
                decls,
                c,
                assumptions,
                trait_impl,
                TraitRequirementBoundData::AssociatedTyRequirement(associated),
            ) => c)
        )
    }
}

pub(super) fn quantified_goal<T>(binder: &Binder<T>, to_wc: impl FnOnce(T) -> Wc) -> Wc
where
    T: crate::rust::Term,
{
    let (variables, value) = binder.open();
    Wc::for_all(Binder::new(&variables, to_wc(value)))
}

/// True if an assumption proves an atomic validation goal without introducing constraints.
///
/// Ordinary evidence can validate either stage. Stage-B evidence can additionally expose its
/// supertraits, whereas provisional stage-A evidence cannot.
pub(crate) fn has_unconditional_validation_supertrait_assumption(
    decls: &Program,
    assumptions: &Wcs,
    goal: &Wc,
) -> bool {
    let Wc::Predicate(Predicate::IsImplemented(goal)) = goal else {
        return false;
    };

    assumptions.iter().any(|assumption| match assumption {
        Wc::Predicate(Predicate::IsImplemented(source)) => {
            trait_ref_implies_via_supertraits(decls, source, goal)
        }

        Wc::Validate(ValidationState::B, source) => {
            let source: Wc = source.upcast();
            let Some(source) = source.downcast::<TraitRef>() else {
                return false;
            };

            trait_ref_implies_via_supertraits(decls, source, goal)
        }

        _ => false,
    })
}

fn trait_ref_implies_via_supertraits(decls: &Program, source: TraitRef, goal: &TraitRef) -> bool {
    let mut pending = vec![source];
    let mut visited: Set<TraitRef> = Set::new();

    while let Some(source) = pending.pop() {
        if &source == goal {
            return true;
        }

        if visited.contains(&source) {
            continue;
        }

        let Ok(trait_def) = decls.program().trait_named(&source.trait_id) else {
            continue;
        };
        let trait_def: Trait = trait_def.upcast();
        let Ok((requirements, _)) = trait_requirement(trait_def).into_singleton() else {
            continue;
        };

        let implied = requirements
            .iter()
            .filter_map(|requirement| {
                let requirement = requirement
                    .binder
                    .instantiate_with(&source.parameters)
                    .ok()?;
                let TraitRequirementBoundData::Supertrait(supertrait) = requirement else {
                    return None;
                };

                // Opening a higher-ranked binder would introduce fresh variables and may
                // constrain a caller goal. Leave those cases to the complete proof rules.
                supertrait
                    .is_empty()
                    .then(|| supertrait.instantiate_with(()).ok())
                    .flatten()
            })
            .collect::<Vec<_>>();
        visited.insert(source);
        pending.extend(implied);
    }

    false
}

fn associated_ty_validation_goals(
    decls: &Program,
    trait_impl: &TraitImplBoundData,
    requirement: &AssociatedTyRequirement,
) -> anyhow::Result<Wcs> {
    let impl_value = trait_impl
        .impl_items
        .iter()
        .find_map(|item| match item {
            ImplItem::AssociatedTyValue(value) if value.id == requirement.id => Some(value),
            _ => None,
        })
        .ok_or_else(|| anyhow::anyhow!("impl has no value for {:?}", requirement.id))?;

    if impl_value.binder.kinds() != requirement.binder.kinds() {
        anyhow::bail!(
            "distinct associated type binder kinds: impl {:?} vs trait {:?}",
            impl_value.binder.kinds(),
            requirement.binder.kinds(),
        );
    }

    let (
        associated_variables,
        AssociatedTyValueBoundData {
            where_clauses: _,
            ty,
        },
    ) = impl_value.binder.open();
    let value_template = requirement.binder.instantiate_with(&associated_variables)?;
    let value_bounds = value_template.instantiate_with(std::slice::from_ref(&ty))?;

    let trait_def = decls.trait_def(&trait_impl.trait_id);
    let trait_data = trait_def
        .binder
        .instantiate_with(&trait_impl.trait_ref().parameters)?;
    let trait_associated_ty = trait_data
        .trait_items
        .iter()
        .find_map(|item| match item {
            TraitItem::AssociatedTy(associated_ty) if associated_ty.id == requirement.id => {
                Some(associated_ty)
            }
            _ => None,
        })
        .ok_or_else(|| anyhow::anyhow!("trait has no associated type {:?}", requirement.id))?;
    let AssociatedTyBoundData {
        ensures: _,
        where_clauses,
    } = trait_associated_ty
        .binder
        .instantiate_with(&associated_variables)?;
    let conditions = (trait_impl.where_clauses.to_wcs(), where_clauses.to_wcs()).to_wcs();
    let validation_conditions = conditions.validated(ValidationState::B);

    let value_wf: Wc = Relation::well_formed(ty).upcast();
    Ok(std::iter::once(value_wf)
        .chain(value_bounds)
        .map(|goal| {
            Wc::for_all(Binder::new(
                &associated_variables,
                Wc::implies(
                    &validation_conditions,
                    Wc::validate(ValidationState::A, goal),
                ),
            ))
        })
        .collect())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::rust::term;
    use formality_macros::test;

    fn requirements_for(trait_def: impl Upcast<Trait>) -> Set<TraitRequirement> {
        trait_requirement(trait_def).into_singleton().unwrap().0
    }

    #[test]
    fn trait_without_requirements_produces_an_empty_set() {
        let trait_def: Trait = term("trait Empty where {}");
        assert!(requirements_for(trait_def).is_empty());
    }

    #[test]
    fn associated_type_requirements_preserve_item_and_binder_structure() {
        let trait_def: Trait = term(
            "trait Family<A> where {
                type Empty : [];
                type Item<T, U> : [Foo, Bar];
            }",
        );

        let requirements = requirements_for(trait_def);
        let mut shapes = requirements
            .iter()
            .filter_map(|requirement| {
                let TraitRequirementBoundData::AssociatedTyRequirement(associated) =
                    requirement.binder.peek()
                else {
                    return None;
                };

                // `Self` and `A` are bound by the trait requirement. The associated-type binder
                // then binds its GAT parameters, and the innermost binder binds exactly the
                // associated type value.
                assert_eq!(requirement.binder.len(), 2);
                assert_eq!(associated.binder.peek().len(), 1);

                Some((
                    associated.binder.len(),
                    associated.binder.peek().peek().iter().count(),
                ))
            })
            .collect::<Vec<_>>();

        shapes.sort();
        assert_eq!(shapes, vec![(0, 0), (2, 2)]);
    }

    #[test]
    fn associated_type_validation_generates_wf_and_one_goal_per_bound() {
        let program = Program {
            crates: Arc::new(Program::program_from_items(vec![
                term("trait Foo where {}"),
                term("trait Bar where {}"),
                term(
                    "trait Family where {
                        type Empty : [];
                        type Item<T, U> : [Foo, Bar];
                    }",
                ),
                term(
                    "impl Family for () {
                        type Empty = ();
                        type Item<T, U> = ();
                    }",
                ),
            ])),
            ..Program::empty()
        };
        let trait_def = program.trait_def(&crate::grammar::TraitId::new("Family"));
        let trait_impl = program
            .trait_impls_for(&trait_def.id)
            .into_iter()
            .next()
            .unwrap();
        let (_, trait_impl) = trait_impl.binder.open();
        let trait_ref = trait_impl.trait_ref();

        let mut goal_counts = requirements_for(&trait_def)
            .iter()
            .filter_map(|requirement| {
                let requirement = requirement
                    .binder
                    .instantiate_with(&trait_ref.parameters)
                    .unwrap();
                let TraitRequirementBoundData::AssociatedTyRequirement(associated) = requirement
                else {
                    return None;
                };
                Some(
                    associated_ty_validation_goals(&program, &trait_impl, &associated)
                        .unwrap()
                        .iter()
                        .count(),
                )
            })
            .collect::<Vec<_>>();

        goal_counts.sort();
        assert_eq!(goal_counts, vec![1, 3]);
    }

    #[test]
    fn associated_type_validation_uses_b_antecedents_and_a_consequences() {
        let program = Program {
            crates: Arc::new(Program::program_from_items(vec![
                term("trait Required where {}"),
                term(
                    "trait Family where {
                        type Item<T> : [Required]
                        where
                            T : Required;
                    }",
                ),
                term(
                    "impl Family for () where (): Required {
                        type Item<T> = T
                        where
                            T : Required;
                    }",
                ),
            ])),
            ..Program::empty()
        };
        let trait_def = program.trait_def(&crate::grammar::TraitId::new("Family"));
        let trait_impl = program
            .trait_impls_for(&trait_def.id)
            .into_iter()
            .next()
            .unwrap();
        let (_, trait_impl) = trait_impl.binder.open();
        let trait_ref = trait_impl.trait_ref();
        let associated = requirements_for(&trait_def)
            .iter()
            .find_map(|requirement| {
                let requirement = requirement
                    .binder
                    .instantiate_with(&trait_ref.parameters)
                    .unwrap();
                let TraitRequirementBoundData::AssociatedTyRequirement(associated) = requirement
                else {
                    return None;
                };
                Some(associated)
            })
            .unwrap();

        let goals = associated_ty_validation_goals(&program, &trait_impl, &associated).unwrap();
        assert_eq!(goals.iter().count(), 2);

        for goal in goals {
            let Wc::ForAll(binder) = goal else {
                panic!("expected quantified validation goal");
            };
            let (_, body) = binder.open();
            let Wc::Implies(conditions, consequence) = body else {
                panic!("expected conditional validation goal");
            };

            assert!(conditions
                .iter()
                .all(|condition| matches!(condition, Wc::Validate(ValidationState::B, _))));
            assert!(matches!(
                consequence.as_ref(),
                Wc::Validate(ValidationState::A, _)
            ));
        }
    }

    #[test]
    fn trait_header_requirements_preserve_categories_and_binders() {
        let trait_def: Trait = term(
            "trait Structured<'a, T>
            where
                Self : Super,
                Self : 'a,
                T : Extra,
            {
                fn method() -> () where Self : Extra;
            }",
        );
        let (trait_variables, trait_data) = trait_def.binder.open();
        let self_parameter: Parameter = trait_variables[0].upcast();
        let classifications = trait_data
            .where_clauses
            .to_wcs()
            .iter()
            .map(|clause| {
                trait_header_clause(&self_parameter, clause)
                    .into_singleton()
                    .unwrap()
                    .0
            })
            .collect::<Vec<_>>();
        assert_eq!(
            classifications
                .iter()
                .filter(|classification| matches!(
                    classification,
                    TraitHeaderClause::InputWellFormed(_)
                ))
                .count(),
            1,
        );

        let requirements = requirements_for(&trait_def);

        assert_eq!(requirements.len(), 2);
        assert!(requirements.iter().all(|requirement| {
            requirement.binder.len() == 3
                && match requirement.binder.peek() {
                    TraitRequirementBoundData::Supertrait(supertrait) => supertrait.is_empty(),
                    TraitRequirementBoundData::Outlives(outlives) => outlives.is_empty(),
                    TraitRequirementBoundData::AssociatedTyRequirement(_) => false,
                }
        }));

        let higher_ranked: Trait = term("trait HigherRanked where for<'a> Self : Super<'a> {}");
        let higher_ranked_requirements = requirements_for(higher_ranked);
        assert_eq!(higher_ranked_requirements.len(), 1);
        let requirement = higher_ranked_requirements.iter().next().unwrap();
        let TraitRequirementBoundData::Supertrait(supertrait) = requirement.binder.peek() else {
            panic!("expected a supertrait requirement");
        };
        assert_eq!(supertrait.len(), 1);

        let higher_ranked: Trait = term("trait HigherOutlives where for<'a> Self : 'a {}");
        let higher_ranked_requirements = requirements_for(higher_ranked);
        assert_eq!(higher_ranked_requirements.len(), 1);
        let requirement = higher_ranked_requirements.iter().next().unwrap();
        let TraitRequirementBoundData::Outlives(outlives) = requirement.binder.peek() else {
            panic!("expected an outlives requirement");
        };
        assert_eq!(outlives.len(), 1);
    }

    #[test]
    fn projection_trait_header_clause_is_input_well_formedness() {
        let trait_def: Trait = term("trait Projected where <Self as Family>::Item : Extra {}");
        let (trait_variables, trait_data) = trait_def.binder.open();
        let self_parameter: Parameter = trait_variables[0].upcast();
        let clauses = trait_data.where_clauses.to_wcs();
        let clause = clauses.iter().next().unwrap();
        let classification = trait_header_clause(self_parameter, clause)
            .into_singleton()
            .unwrap()
            .0;

        assert!(matches!(
            classification,
            TraitHeaderClause::InputWellFormed(_)
        ));
        assert!(requirements_for(trait_def).is_empty());
    }
}
