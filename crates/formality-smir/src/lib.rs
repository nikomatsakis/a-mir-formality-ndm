// This is a modified version of project-stable-mir `test-drive`
// <https://github.com/rust-lang/project-stable-mir/blob/8ec26c61/tools/test-drive/src/main.rs>

//! This library provide different ways of scanning a crate.

#![feature(rustc_private)]

extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
#[macro_use]
extern crate rustc_smir;
extern crate stable_mir;

use rustc_middle::ty::TyCtxt;
use rustc_smir::{run_with_tcx, rustc_internal};
use std::ops::ControlFlow;
use std::path::Path;

use formality_types::grammar::Fallible;

pub fn check_rust_source(_input: &Path) -> Fallible<()> {
    let rustc_args = vec![];
    let _result = run_with_tcx!(rustc_args, |tcx| analyze_crate(tcx));
    anyhow::bail!("not implemented")
}

/// This function invoke the required analyses in the given order.
fn analyze_crate(_tcx: TyCtxt) -> ControlFlow<()> {
    ControlFlow::<()>::Continue(())
}
