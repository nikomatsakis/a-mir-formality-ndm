//! Canonicalization of monomorphization keys before worklist insertion.

use std::sync::Arc;

use crate::grammar::{
    expr::{Block, Expr, FieldExpr, FnValue, Init, Stmt, Turbofish},
    Fallible, FnBody, FnBoundData, InputArg, Lt, MaybeFnBody, Parameter, TraitRef, Ty, Wcs,
};
use crate::prove::prove::{
    prove_fully_normalize_parameter, Constrained, Constraints, Env, Program,
};
use crate::rust::Fold;
use formality_core::{judgment_fn, Upcast};

use super::scope::{is_normalized_parameter, MonoKey};

judgment_fn! {
    pub(super) fn normalize_parameters(
        _program: Program,
        env: Env,
        assumptions: Wcs,
        parameters: Vec<Parameter>,
    ) => Constrained<Vec<Parameter>> {
        debug(parameters, assumptions, env)

        (
            (let c = Constraints::none(env))
            (let normalized: Vec<Parameter> = vec![])
            (for_all(parameter in parameters) with(c, normalized)
                (let assumptions = c.substitution().apply(&assumptions))
                (let parameter = c.substitution().apply(parameter))
                (prove_fully_normalize_parameter(program, c.env(), assumptions, parameter) => Constrained(parameter, c1))
                (let c = c.seq(c1))
                (let normalized = append_normalized(c, normalized.upcast(), parameter.upcast())))
            ---------------------------------------------------- ("parameters")
            (normalize_parameters(program, env, assumptions, parameters) => Constrained(normalized, c))
        )
    }
}

/// Fully normalize a closed list of codegen parameters and require one
/// unconditional normal form in the empty environment.
pub(super) fn normalize_ground_parameters(
    program: &Program,
    parameters: impl Upcast<Vec<Parameter>>,
) -> Fallible<Vec<Parameter>> {
    let initial_env = Env::default();
    let results = normalize_parameters(program, &initial_env, Wcs::t(), parameters)
        .into_map()
        .map_err(|error| anyhow::anyhow!("{}", error.format_leaves()))?;
    if results.len() != 1 {
        anyhow::bail!(
            "codegen requires exactly one normal form for parameters, found {}",
            results.len(),
        );
    }

    let (Constrained(parameters, constraints), _) = results.into_iter().next().unwrap();
    if constraints.env() != &initial_env || !constraints.unconditionally_true() {
        anyhow::bail!(
            "codegen requires an unconditional parameter normal form in the initial environment: {constraints:?}",
        );
    }
    if !parameters.iter().all(is_normalized_parameter) {
        anyhow::bail!(
            "codegen parameter normal form is not ground and recursively alias-free: {parameters:?}",
        );
    }
    Ok(parameters)
}

pub(super) fn normalize_fn_data(program: &Program, data: &FnBoundData) -> Fallible<FnBoundData> {
    Ok(FnBoundData {
        input_args: data
            .input_args
            .iter()
            .map(|arg| {
                Ok(InputArg {
                    id: (&arg.id).upcast(),
                    ty: normalize_ground_ty(program, &arg.ty)?,
                })
            })
            .collect::<Fallible<_>>()?,
        output_ty: normalize_ground_ty(program, &data.output_ty)?,
        where_clauses: (&data.where_clauses).upcast(),
        body: normalize_maybe_body(program, &data.body)?,
    })
}

pub(super) fn normalize_block(program: &Program, block: &Block) -> Fallible<Block> {
    Ok(Block {
        label: (&block.label).upcast(),
        stmts: block
            .stmts
            .iter()
            .map(|stmt| normalize_stmt(program, stmt))
            .collect::<Fallible<_>>()?,
    })
}

pub(super) fn normalize_ground_ty(program: &Program, ty: &Ty) -> Fallible<Ty> {
    let mut parameters = normalize_ground_parameters(program, vec![ty])?;
    let Parameter::Ty(ty) = parameters.pop().unwrap() else {
        unreachable!("normalizing a type changed its parameter kind")
    };
    Ok(ty.as_ref().upcast())
}

fn normalize_lt(program: &Program, lt: &Lt) -> Fallible<Lt> {
    let mut parameters = normalize_ground_parameters(program, vec![lt])?;
    let Parameter::Lt(lt) = parameters.pop().unwrap() else {
        unreachable!("normalizing a lifetime changed its parameter kind")
    };
    Ok(lt.as_ref().upcast())
}

fn normalize_maybe_body(program: &Program, body: &MaybeFnBody) -> Fallible<MaybeFnBody> {
    match body {
        MaybeFnBody::NoFnBody => Ok(MaybeFnBody::NoFnBody),
        MaybeFnBody::FnBody(FnBody::TrustedFnBody) => {
            Ok(MaybeFnBody::FnBody(FnBody::TrustedFnBody))
        }
        MaybeFnBody::FnBody(FnBody::Expr(block)) => Ok(MaybeFnBody::FnBody(FnBody::Expr(
            normalize_block(program, block)?,
        ))),
    }
}

fn normalize_stmt(program: &Program, stmt: &Stmt) -> Fallible<Stmt> {
    match stmt {
        Stmt::Let {
            label,
            id,
            ty,
            init,
        } => Ok(Stmt::Let {
            label: label.upcast(),
            id: id.upcast(),
            ty: normalize_ground_ty(program, ty)?,
            init: init
                .as_ref()
                .map(|init| {
                    Ok::<Init, anyhow::Error>(Init {
                        expr: normalize_expr(program, &init.expr)?,
                    })
                })
                .transpose()?,
        }),
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => Ok(Stmt::If {
            condition: normalize_expr(program, condition)?,
            then_block: normalize_block(program, then_block)?,
            else_block: normalize_block(program, else_block)?,
        }),
        Stmt::Expr { expr } => Ok(Stmt::Expr {
            expr: normalize_expr(program, expr)?,
        }),
        Stmt::Loop { label, body } => Ok(Stmt::Loop {
            label: label.upcast(),
            body: normalize_block(program, body)?,
        }),
        Stmt::Break { label } => Ok(Stmt::Break {
            label: label.upcast(),
        }),
        Stmt::Continue { label } => Ok(Stmt::Continue {
            label: label.upcast(),
        }),
        Stmt::Return { expr } => Ok(Stmt::Return {
            expr: normalize_expr(program, expr)?,
        }),
        Stmt::Block(block) => Ok(Stmt::Block(normalize_block(program, block)?)),
        // Normalize after codegen opens this binder and erases its supported
        // lifetime variables.
        Stmt::Exists { binder } => Ok(Stmt::Exists {
            binder: binder.upcast(),
        }),
        Stmt::Print { expr } => Ok(Stmt::Print {
            expr: normalize_expr(program, expr)?,
        }),
    }
}

fn normalize_expr(program: &Program, expr: &Expr) -> Fallible<Expr> {
    match expr {
        Expr::Assign { place, expr } => Ok(Expr::Assign {
            place: place.upcast(),
            expr: Arc::new(normalize_expr(program, expr)?),
        }),
        Expr::Call { callee, args } => Ok(Expr::Call {
            callee: Arc::new(normalize_expr(program, callee)?),
            args: args
                .iter()
                .map(|arg| normalize_expr(program, arg))
                .collect::<Fallible<_>>()?,
        }),
        Expr::Literal { value, ty } => Ok(Expr::Literal {
            value: *value,
            ty: ty.upcast(),
        }),
        Expr::True => Ok(Expr::True),
        Expr::False => Ok(Expr::False),
        Expr::Ref { kind, lt, place } => Ok(Expr::Ref {
            kind: kind.upcast(),
            lt: normalize_lt(program, lt)?,
            place: place.upcast(),
        }),
        Expr::Place(place) => Ok(Expr::Place(place.upcast())),
        Expr::FnValue(value) => Ok(Expr::FnValue(FnValue {
            name: (&value.name).upcast(),
            substitution: normalize_ground_parameters(program, &value.substitution)?,
        })),
        Expr::Struct {
            field_exprs,
            adt_id,
            turbofish,
        } => Ok(Expr::Struct {
            field_exprs: field_exprs
                .iter()
                .map(|field| {
                    Ok(FieldExpr {
                        name: (&field.name).upcast(),
                        value: normalize_expr(program, &field.value)?,
                    })
                })
                .collect::<Fallible<_>>()?,
            adt_id: adt_id.upcast(),
            turbofish: Turbofish {
                parameters: normalize_ground_parameters(program, &turbofish.parameters)?,
            },
        }),
    }
}

judgment_fn! {
    pub(super) fn normalize_mono_key(
        _program: Program,
        env: Env,
        assumptions: Wcs,
        key: MonoKey,
    ) => Constrained<MonoKey> {
        debug(key, assumptions, env)

        (
            (normalize_parameters(program, env, assumptions, fn_args) => Constrained(fn_args, c))
            (let key = MonoKey::free_fn(id, fn_args))
            ---------------------------------------------------- ("free function")
            (normalize_mono_key(
                program,
                env,
                assumptions,
                MonoKey::FreeFn { id, fn_args },
            ) => Constrained(key, c))
        )

        (
            (let trait_arity = trait_ref.parameters.len())
            (let parameters: Vec<Parameter> = trait_ref
                .parameters
                .iter()
                .chain(method_args)
                .map(|parameter| parameter.upcast())
                .collect())
            (normalize_parameters(program, env, assumptions, parameters) => Constrained(parameters, c))
            (let (trait_parameters, method_args) = split_parameters(parameters, *trait_arity))
            (let trait_ref = TraitRef {
                trait_id: (&trait_ref.trait_id).upcast(),
                parameters: trait_parameters.upcast(),
            })
            (let key = MonoKey::trait_method(trait_ref, method_id, method_args))
            ---------------------------------------------------- ("trait method")
            (normalize_mono_key(
                program,
                env,
                assumptions,
                MonoKey::TraitMethod {
                    trait_ref,
                    method_id,
                    method_args,
                },
            ) => Constrained(key, c))
        )
    }
}

fn split_parameters(
    parameters: &[Parameter],
    trait_arity: usize,
) -> (Vec<Parameter>, Vec<Parameter>) {
    let (trait_parameters, method_args) = parameters.split_at(trait_arity);
    (trait_parameters.upcast(), method_args.upcast())
}

fn append_normalized<T>(c: &Constraints, normalized: Vec<T>, value: T) -> Vec<T>
where
    T: Fold<Output = T> + Clone,
{
    let mut normalized = c.substitution().apply(normalized);
    normalized.push(c.substitution().apply(value));
    normalized
}
