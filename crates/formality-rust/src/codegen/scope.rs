//! CodegenGlobal, CodegenFn, and CodegenScope: the state threaded through codegen judgments.

use crate::check::borrow_check::env::TypeckEnv;
use crate::check::borrow_check::flow_state::FlowState;
use crate::grammar::{
    expr::{FnName, LabelId},
    Const, Crates, Fallible, Lt, Parameter, TraitRef, Ty, ValueId, Wcs,
};
use crate::prove::prove::{Constrained, Env, Program};
use formality_core::Upcast;
use libspecr::prelude::Map;
use minirust_rs::lang;

use super::code_block::CodeBlock;
use super::minirust::*;
use super::normalize::normalize_mono_key;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) enum MonoKey {
    FreeFn {
        id: ValueId,
        fn_args: Vec<Parameter>,
    },
    TraitMethod {
        trait_ref: TraitRef,
        method_id: ValueId,
        method_args: Vec<Parameter>,
    },
}

impl MonoKey {
    pub fn free_fn(id: impl Upcast<ValueId>, fn_args: impl Upcast<Vec<Parameter>>) -> Self {
        Self::FreeFn {
            id: id.upcast(),
            fn_args: fn_args.upcast(),
        }
    }

    pub fn trait_method(
        trait_ref: impl Upcast<TraitRef>,
        method_id: impl Upcast<ValueId>,
        method_args: impl Upcast<Vec<Parameter>>,
    ) -> Self {
        Self::TraitMethod {
            trait_ref: trait_ref.upcast(),
            method_id: method_id.upcast(),
            method_args: method_args.upcast(),
        }
    }

    /// Recover the structured codegen identity from a callable's nominal name
    /// and its ordered substitution.
    pub(super) fn from_callable(
        crates: &Crates,
        name: &FnName,
        substitution: &[Parameter],
    ) -> Fallible<Self> {
        match name {
            FnName::FreeId(id) => Ok(Self::free_fn(id, substitution)),
            FnName::QualifiedId { trait_id, id } => {
                let trait_decl = crates.trait_named(trait_id)?;
                let trait_arity = trait_decl.binder.explicit_binder.len();
                if substitution.len() < trait_arity || trait_arity == 0 {
                    anyhow::bail!(
                        "qualified function has {} parameters, but trait `{trait_id:?}` requires {trait_arity}",
                        substitution.len(),
                    );
                }

                let (trait_parameters, method_args) = substitution.split_at(trait_arity);
                Ok(Self::trait_method(
                    TraitRef {
                        trait_id: trait_id.upcast(),
                        parameters: trait_parameters.upcast(),
                    },
                    id,
                    method_args,
                ))
            }
        }
    }

    fn parameters(&self) -> impl Iterator<Item = &Parameter> {
        match self {
            MonoKey::FreeFn { fn_args, .. } => fn_args.iter().chain([].iter()),
            MonoKey::TraitMethod {
                trait_ref,
                method_args,
                ..
            } => trait_ref.parameters.iter().chain(method_args),
        }
    }
}

/// A monomorphization key that is ground and recursively free of aliases and
/// other type forms unsupported by code generation.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct NormalizedMonoKey(MonoKey);

impl NormalizedMonoKey {
    fn new(key: MonoKey) -> Fallible<Self> {
        if key.parameters().all(is_normalized_parameter) {
            Ok(Self(key))
        } else {
            anyhow::bail!("monomorphization key is not ground and recursively alias-free: {key:?}")
        }
    }
}

/// Cross-function state: tracks the set of monomorphized functions discovered
/// during codegen and allocates globally-unique function names.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct CodegenGlobal {
    /// Input program, including the solver declarations used to normalize keys.
    pub(super) program: Program,
    /// Monotonically increasing counter for generating unique function names.
    fn_counter: u32,
    /// Map from monomorphized call sites to MiniRust function names.
    /// Entries are added on first encounter; codegen loops until all are compiled.
    fn_map: Vec<(NormalizedMonoKey, MiniRustFn)>,
}

/// Per-function state: locals, basic-block counters, type environment.
/// Created fresh for each function and not shared across functions.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct CodegenFn {
    /// Input program (cloned from global for convenience).
    pub(super) crates: Crates,
    /// Used to resolve types, prove bounds, etc.
    pub(super) typeck_env: TypeckEnv,
    /// Where-clauses in scope for the current function being generated.
    pub(super) assumptions: Wcs,
    /// Monotonically increasing counter for generating unique local names.
    local_counter: u32,
    /// Monotonically increasing counter for generating unique basic-block names.
    bb_counter: u32,
    /// Locals declared in the current function.
    pub(super) locals: Vec<(MiniRustLocal, MiniRustType)>,
}

/// Per-function scope that wraps and extends `FlowState`.
///
/// `FlowState` tracks borrow-checker state: scopes, liveness, loans, and variable
/// types. `CodegenScope` adds the mapping from source `ValueId`s to MiniRust locals,
/// the loop label stack (for resolving `break`/`continue` to basic-block targets),
/// and the return-value local.
///
/// The variable-to-local mapping (`vars`) is duplicated with `FlowState::scopes`
/// in the sense that both know which variables are in scope and their types, but
/// `vars` additionally maps each variable to its MiniRust local name, which
/// `FlowState` doesn't track.
///
/// `FlowState` must be kept in sync as we codegen: when a new variable is introduced
/// (`push_var`), it is registered in both `vars` and `FlowState`. When we call into
/// the borrow-checker (e.g., `resolve_place`), we pass the current `flow_state` so
/// it can verify liveness and loan validity at this program point.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct CodegenScope {
    /// Source variable name → (MiniRust local, original Rust type).
    vars: Vec<(ValueId, MiniRustLocal, Ty)>,
    /// Stack of enclosing loops, used to resolve `break`/`continue` targets.
    label_scopes: Vec<LabelScope>,
    /// The local where the function's return value is written.
    pub(super) ret_local: MiniRustLocal,
    /// Borrow-checker flow state (liveness, loans) at this program point.
    pub(super) flow_state: FlowState,
}

/// One entry in the label scope stack, representing an enclosing loop.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct LabelScope {
    label: LabelId,
    /// Block to jump to on `continue`. `None` for non-loop labels (e.g., plain blocks).
    continue_target: Option<MiniRustBb>,
    /// Block to jump to on `break`.
    break_target: MiniRustBb,
}

formality_core::cast_impl!(CodegenGlobal);
formality_core::cast_impl!(CodegenFn);
formality_core::cast_impl!(CodegenScope);
formality_core::cast_impl!(MonoKey);

impl CodegenGlobal {
    /// Create a fresh global state for codegen over the given crates.
    pub(super) fn new(crates: &Crates) -> Self {
        CodegenGlobal {
            program: crates.to_prove_decls(),
            fn_counter: 0,
            fn_map: Vec::new(),
        }
    }

    /// Allocate a unique function name.
    pub(super) fn fresh_fn(&self) -> (lang::FnName, Self) {
        let mut this = self.clone();
        let n = this.fn_counter;
        this.fn_counter += 1;
        (lang::FnName(libspecr::Name::from_internal(n)), this)
    }

    /// Return the MiniRust function name for a monomorphized call site,
    /// allocating a new entry if this is the first time we've seen this key.
    fn ensure_normalized_fn(&self, key: NormalizedMonoKey) -> (lang::FnName, Self) {
        for (k, name) in &self.fn_map {
            if *k == key {
                return (name.0, self.clone());
            }
        }
        let (name, mut g) = self.fresh_fn();
        g.fn_map.push((key, MiniRustFn(name)));
        (name, g)
    }

    /// Normalize and classify a proposed key before it can enter the worklist.
    pub(super) fn ensure_monomorphized_fn(
        &self,
        key: impl Upcast<MonoKey>,
    ) -> Fallible<(lang::FnName, Self)> {
        let key = key.upcast();
        let initial_env = Env::default();
        let results = normalize_mono_key(&self.program, &initial_env, Wcs::t(), key)
            .into_map()
            .map_err(|error| anyhow::anyhow!("{}", error.format_leaves()))?;

        if results.len() != 1 {
            anyhow::bail!(
                "codegen requires exactly one normal form for a monomorphization key, found {}",
                results.len(),
            );
        }

        let (Constrained(key, constraints), _) = results.into_iter().next().unwrap();
        if constraints.env() != &initial_env || !constraints.unconditionally_true() {
            anyhow::bail!(
                "codegen requires an unconditional normal form in the initial environment: {constraints:?}"
            );
        }

        Ok(self.ensure_normalized_fn(NormalizedMonoKey::new(key)?))
    }

    /// Return the next function in `fn_map` that hasn't been compiled yet.
    pub(super) fn next_pending(
        &self,
        done: &Map<lang::FnName, lang::Function>,
    ) -> Option<(MonoKey, lang::FnName)> {
        self.fn_map
            .iter()
            .find(|(_, n)| !done.contains_key(n.0))
            .map(|(k, n)| ((&k.0).upcast(), n.0))
    }
}

pub(super) fn is_normalized_parameter(parameter: &Parameter) -> bool {
    match parameter {
        Parameter::Ty(ty) => match ty.as_ref() {
            Ty::RigidTy(rigid) => rigid.parameters.iter().all(is_normalized_parameter),
            Ty::AliasTy(_) | Ty::PredicateTy(_) | Ty::Variable(_) => false,
        },
        Parameter::Lt(lt) => matches!(lt.as_ref(), Lt::Static | Lt::Erased),
        Parameter::Const(constant) => is_normalized_const(constant),
    }
}

fn is_normalized_const(constant: &Const) -> bool {
    match constant {
        Const::Scalar(_) => true,
        Const::RigidValue(rigid) => {
            rigid.parameters.iter().all(is_normalized_parameter)
                && rigid.values.iter().all(is_normalized_const)
        }
        Const::Block(_) | Const::Variable(_) => false,
    }
}

impl CodegenFn {
    /// Create per-function state for compiling a function with the given return type.
    pub(super) fn new(crates: &Crates, output_ty: &Ty) -> Self {
        let program = crates.to_prove_decls();
        let typeck_env = TypeckEnv::for_fn_body(Env::default(), &program, output_ty);
        CodegenFn {
            crates: crates.clone(),
            typeck_env,
            assumptions: Wcs::t(),
            local_counter: 0,
            bb_counter: 0,
            locals: Vec::new(),
        }
    }

    /// Allocate a unique local name (does not register it in `locals`).
    pub(super) fn fresh_local(&self) -> (lang::LocalName, Self) {
        let mut this = self.clone();
        let n = this.local_counter;
        this.local_counter += 1;
        (lang::LocalName(libspecr::Name::from_internal(n)), this)
    }

    /// Allocate a unique basic-block name.
    pub(super) fn fresh_bb(&self) -> (lang::BbName, Self) {
        let mut this = self.clone();
        let n = this.bb_counter;
        this.bb_counter += 1;
        (lang::BbName(libspecr::Name::from_internal(n)), this)
    }

    /// Allocate a local and register it with its type in the locals list.
    pub(super) fn alloc_local(&self, ty: lang::Type) -> (lang::LocalName, Self) {
        let (name, mut f) = self.fresh_local();
        f.locals.push((MiniRustLocal(name), MiniRustType(ty)));
        (name, f)
    }

    /// Translate a formality-rust type to its MiniRust representation.
    pub(super) fn minirust_ty(&self, ty: &Ty) -> Fallible<lang::Type> {
        minirust_ty(&self.crates, ty)
    }

    /// Create a new empty anonymous `CodeBlock`.
    pub(super) fn fresh_code_block(&self) -> CodeBlock {
        CodeBlock::new()
    }

    /// Allocate a temporary local for the given Rust type.
    pub(super) fn alloc_temp(&self, ty: &Ty) -> Fallible<(MiniRustLocal, Self)> {
        let mr_ty = self.minirust_ty(ty)?;
        let (name, f) = self.alloc_local(mr_ty);
        Ok((MiniRustLocal(name), f))
    }
}

impl CodegenScope {
    pub(super) fn new(ret_local: lang::LocalName, flow_state: FlowState) -> Self {
        CodegenScope {
            vars: Vec::new(),
            label_scopes: Vec::new(),
            ret_local: MiniRustLocal(ret_local),
            flow_state,
        }
    }
    pub(super) fn lookup_var(&self, id: &ValueId) -> Fallible<(lang::LocalName, Ty)> {
        self.vars
            .iter()
            .rev()
            .find(|(n, _, _)| n == id)
            .map(|(_, l, t)| (l.0, t.clone()))
            .ok_or_else(|| anyhow::anyhow!("unbound variable `{id:?}`"))
    }
    pub(super) fn lookup_label(
        &self,
        label: &LabelId,
    ) -> Fallible<(Option<lang::BbName>, lang::BbName)> {
        self.label_scopes
            .iter()
            .rev()
            .find(|s| s.label == *label)
            .map(|s| (s.continue_target.map(|b| b.into()), s.break_target.into()))
            .ok_or_else(|| anyhow::anyhow!("no label `{label:?}` in scope"))
    }
    pub(super) fn push_var(
        &self,
        id: impl Upcast<ValueId>,
        local: impl Upcast<MiniRustLocal>,
        ty: impl Upcast<Ty>,
    ) -> Fallible<Self> {
        let id: ValueId = id.upcast();
        let local: MiniRustLocal = local.upcast();
        let ty: Ty = ty.upcast();
        let mut s = self.clone();
        s.flow_state = s
            .flow_state
            .with_local_in_scope(&Env::default(), &None, &id, &ty)?;
        s.vars.push((id, local, ty));
        Ok(s)
    }
    /// Add a variable mapping without updating FlowState.
    /// Used for input args that are already registered in FlowState via for_fn_body.
    pub(super) fn push_var_no_flow(mut self, id: ValueId, local: lang::LocalName, ty: Ty) -> Self {
        self.vars.push((id, MiniRustLocal(local), ty));
        self
    }
    pub(super) fn with_label(
        &self,
        label: impl Upcast<LabelId>,
        continue_target: impl Upcast<Option<MiniRustBb>>,
        break_target: impl Upcast<MiniRustBb>,
    ) -> Self {
        let label: LabelId = label.upcast();
        let continue_target: Option<MiniRustBb> = continue_target.upcast();
        let break_target: MiniRustBb = break_target.upcast();
        let mut s = self.clone();
        s.label_scopes.push(LabelScope {
            label,
            continue_target: continue_target.upcast(),
            break_target,
        });
        s
    }
}

#[cfg(test)]
mod tests {
    use super::{CodegenGlobal, MonoKey, NormalizedMonoKey};
    use crate::grammar::{Crates, Parameter, TraitRef, Ty, ValueId};
    use crate::rust::term;

    fn normalization_program() -> Crates {
        term(
            "[
                crate test {
                    trait Family { type Output : []; }
                    impl Family for () { type Output = i32; }
                    fn identity<T>(value: T) -> T { return value; }
                }
            ]",
        )
    }

    fn identity_key(ty: Ty) -> MonoKey {
        MonoKey::free_fn(term::<ValueId>("identity"), vec![ty])
    }

    #[test]
    fn raw_worklist_key_rejects_an_alias() {
        let key = identity_key(term("<() as Family>::Output"));
        assert!(NormalizedMonoKey::new(key).is_err());
    }

    #[test]
    fn alias_and_rigid_spelling_share_one_worklist_entry() {
        let global = CodegenGlobal::new(&normalization_program());
        let (alias_name, global) = global
            .ensure_monomorphized_fn(identity_key(term("<() as Family>::Output")))
            .unwrap();
        let (rigid_name, global) = global
            .ensure_monomorphized_fn(identity_key(term("i32")))
            .unwrap();

        assert_eq!(alias_name, rigid_name);
        assert_eq!(global.fn_map.len(), 1);
    }

    #[test]
    fn trait_arguments_are_part_of_trait_method_identity() {
        let global = CodegenGlobal::new(&normalization_program());
        let (i32_name, global) = global
            .ensure_monomorphized_fn(MonoKey::trait_method(
                term::<TraitRef>("Convert((), i32)"),
                term::<ValueId>("convert"),
                (),
            ))
            .unwrap();
        let (u32_name, global) = global
            .ensure_monomorphized_fn(MonoKey::trait_method(
                term::<TraitRef>("Convert((), u32)"),
                term::<ValueId>("convert"),
                (),
            ))
            .unwrap();

        assert_ne!(i32_name, u32_name);
        assert_eq!(global.fn_map.len(), 2);
    }

    #[test]
    fn method_arguments_are_part_of_trait_method_identity() {
        let global = CodegenGlobal::new(&normalization_program());
        let trait_ref: TraitRef = term("Identity(())");
        let (i32_name, global) = global
            .ensure_monomorphized_fn(MonoKey::trait_method(
                &trait_ref,
                term::<ValueId>("identity"),
                vec![term::<Parameter>("i32")],
            ))
            .unwrap();
        let (u32_name, global) = global
            .ensure_monomorphized_fn(MonoKey::trait_method(
                trait_ref,
                term::<ValueId>("identity"),
                vec![term::<Parameter>("u32")],
            ))
            .unwrap();

        assert_ne!(i32_name, u32_name);
        assert_eq!(global.fn_map.len(), 2);
    }

    #[test]
    fn alias_and_rigid_trait_method_keys_share_one_worklist_entry() {
        let global = CodegenGlobal::new(&normalization_program());
        let (alias_name, global) = global
            .ensure_monomorphized_fn(MonoKey::trait_method(
                term::<TraitRef>("Identity(<() as Family>::Output)"),
                term::<ValueId>("identity"),
                (),
            ))
            .unwrap();
        let (rigid_name, global) = global
            .ensure_monomorphized_fn(MonoKey::trait_method(
                term::<TraitRef>("Identity(i32)"),
                term::<ValueId>("identity"),
                (),
            ))
            .unwrap();

        assert_eq!(alias_name, rigid_name);
        assert_eq!(global.fn_map.len(), 1);
    }
}
