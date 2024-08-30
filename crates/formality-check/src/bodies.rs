use std::intrinsics::mir::BasicBlock;

use formality_core::Upcast;
use formality_prove::{Constraints, Decls, Env};
use formality_rust::grammar::{
    mir::{
        BasicBlockDecl, LocalDecl, LocalId, LocalsAndBlocks, MirFnBody, Operand, Place, Rvalue,
        Statement, Terminator,
    },
    FnBody,
};
use formality_types::{
    grammar::{Fallible, Ty, Wc, Wcs},
    rust::Term,
};

use crate::Check;

impl Check<'_> {
    pub(crate) fn check_fn_body(&self, env: &Env, assumptions: Wcs, f: &FnBody) -> Fallible<()> {
        match f {
            FnBody::TrustedFnBody => Ok(()),
            FnBody::MirFnBody(mir_fn_body) => self.check_mir_fn_body(env, assumptions, mir_fn_body),
        }
    }

    fn check_mir_fn_body(
        &self,
        env: &Env,
        assumptions: Wcs,
        mir_fn_body: &MirFnBody,
    ) -> Fallible<()> {
        let mut env = env.clone();
        let MirFnBody { binder } = mir_fn_body;
        let LocalsAndBlocks {
            local_decls,
            basic_block_decls,
        } = env.instantiate_existentially(binder);

        let mut type_check = TypeCheck::new(self.decls, &local_decls, env.clone(), assumptions);

        type_check.check_locals(&local_decls)?;

        Ok(())
    }

    fn local_decl_goal(&self, env: &Env, assumptions: Wcs, local_decl: &LocalDecl) -> Fallible<()> {
        let LocalDecl {
            ty,
            name: _,
            mutability: _,
        } = local_decl;
        self.prove_goal(env, assumptions, ty.well_formed())
    }
}

struct TypeCheck<'d> {
    decls: &'d Decls,
    local_decls: &'d [LocalDecl],
    assumptions: Wcs,
    constraints: Constraints,
}

impl<'d> TypeCheck<'d> {
    fn new(decls: &'d Decls, local_decls: &'d [LocalDecl], env: Env, assumptions: Wcs) -> Self {
        Self {
            decls,
            local_decls,
            constraints: Constraints::none(env),
            assumptions,
        }
    }

    fn env(&self) -> &Env {
        self.constraints.env()
    }

    fn update(&mut self, constraints: Constraints) {
        self.constraints = self.constraints.seq(constraints);
        self.assumptions = self.refresh(&self.assumptions);
    }

    fn refresh<T: Term>(&self, term: &T) -> T {
        self.constraints.substitution().apply(&term)
    }

    fn prove_goal(&mut self, goal: impl Upcast<Wc>) -> Fallible<()> {
        let goal: Wc = goal.upcast();
        let goal = self.refresh(&goal);
        let proven_set =
            formality_prove::prove(self.decls, self.env(), &self.assumptions, goal).into_set()?;

        if proven_set.len() == 1 {
            let constraints = proven_set.into_iter().next().unwrap();
            self.update(constraints);
            return Ok(());
        }

        if proven_set.iter().any(|c| c.unconditionally_true()) {
            return Ok(());
        }

        todo!("FIXME")
    }

    fn check_locals(&mut self, locals: &[LocalDecl]) -> Fallible<()> {
        for local in locals {
            self.check_local(local)?;
        }
        Ok(())
    }

    fn check_local(&mut self, local_decl: &LocalDecl) -> Fallible<()> {
        let LocalDecl {
            ty,
            name: _,
            mutability: _,
        } = local_decl;
        self.prove_goal(ty.well_formed())
    }

    fn check_basic_block(&mut self, basic_block: &BasicBlockDecl) -> Fallible<()> {
        let BasicBlockDecl {
            id: _,
            statements,
            terminator,
        } = basic_block;

        for statement in statements {
            self.check_statement(statement)?;
        }

        self.check_terminator(terminator)?;

        Ok(())
    }

    fn check_statement(&mut self, statement: &Statement) -> Fallible<()> {
        match statement {
            Statement::Assign(place, rvalue) => {
                self.check_place(place)?;
                self.check_rvalue(rvalue)
            }
            Statement::Noop => Ok(()),
            Statement::FakeRead(_) => todo!(),
        }
    }

    fn check_terminator(&mut self, terminator: &Terminator) -> Fallible<()> {
        match terminator {
            Terminator::Goto(_) => todo!(),
            Terminator::Resume => todo!(),
            Terminator::Abort => todo!(),
            Terminator::Return => todo!(),
            Terminator::Unreachable => todo!(),
            Terminator::Drop(_, _) => todo!(),
            Terminator::DropAndReplace(_, _) => todo!(),
            Terminator::Call(_, _, _, _) => todo!(),
        }
    }

    fn check_rvalue(&mut self, rvalue: &Rvalue) -> Fallible<Ty> {
        match rvalue {
            Rvalue::Use(operand) => todo!(),
            Rvalue::Repeat(_, _) => todo!(),
            Rvalue::Ref(_, _, _) => todo!(),
            Rvalue::AddrOf(_, _) => todo!(),
            Rvalue::Len(_) => todo!(),
            Rvalue::Apply(_, _, _) => todo!(),
            Rvalue::Checked(_, _, _) => todo!(),
            Rvalue::Aggregate(_, _) => todo!(),
            Rvalue::Cast(_, _) => todo!(),
        }
    }

    fn check_operand(&mut self, operand: &Operand) -> Fallible<Ty> {
        match operand {
            Operand::Move(place) => todo!(),
            Operand::Copy(place) => todo!(),
            Operand::Const(_) => todo!(),
        }
    }

    fn check_place(&mut self, place: &Place) -> Fallible<Ty> {
        match place {}
    }

    fn check_local(&mut self, local: LocalId) -> Fallible<Ty> {
        let Some(local_decl) = self.local_decls.find(|d| d.name == local) else {
            anyhow::bail!("local variable not found: {local:?}");
        };
    }
}
