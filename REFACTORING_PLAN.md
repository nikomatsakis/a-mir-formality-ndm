# Git History Refactoring Plan

## Overview

This plan organizes the git history cleanup for: This branch implements a "first draft" of the borrow checker. It's definitely incomplete (doesn't check moves, for example) but it is detecting basic failures. The branch ALSO does a bunch of other random things: generates proof-trees when the check is successful, so that you can tell why it succeeded; changes our error assertion macros to not have so much noise; adds comments into existing logic; introduces a new `(for_all (x in y) ...)` statement form to avoid extraneous judgments.

**Commits to create:**
* [x] Add .sparkle-space to .gitignore
* [x] Add ProofTree infrastructure and update function signatures
* [x] Simplify error assertion macros to reduce noise
* [x] Add new `for_all` statement form to avoid extraneous judgments  
* [x] Add comments and documentation to existing logic
* [x] Implement borrow checker liveness analysis
* [x] Implement core borrow checker with NLL
* [~] Integrate borrow checker with main checking pipeline (PARTIAL - delegated agent encountered file path issues)

## Agent Instructions

When processing each commit:
1. **FIRST**: Regenerate the diff to ensure it's current: `git diff living-large-cleanup-20251203-clean living-large-cleanup > full-changes.diff`
2. Examine `full-changes.diff` to understand all remaining changes
3. Copy the specified sections from `full-changes.diff` to `to-stage.diff`
4. Apply the patch: `git apply to-stage.diff` (or target specific files if needed)
5. Verify changes look correct with `git status` and `git diff`
6. Make small compilation fixes if needed for this commit to build independently
7. Create the commit with the specified message
8. For `expect_test::expect![]` tests: Run tests with `UPDATE_EXPECT=1 cargo test` instead of porting diffs directly
9. Clean up `to-stage.diff` file
10. Report results in `DELEGATED_TASK.md`

## Commit Details

### 1. Add .sparkle-space to .gitignore

**Complete commit message:**
```
gitignore: add .sparkle-space directory

Adds .sparkle-space to .gitignore to exclude Sparkle workspace
files from version control.
```

**Files to modify:**
- `.gitignore`

**Diff sections to include:**
- Single line addition: `.sparkle-space`

### 2. Add ProofTree infrastructure and update function signatures

**Complete commit message:**
```
core: add ProofTree infrastructure and update function signatures

Introduces ProofTree type and supporting infrastructure to track not just
whether proofs succeed, but why they succeeded. This foundational change
prepares for enhanced debugging and proof visualization capabilities.

Updates all core checking functions to return Fallible<ProofTree> instead
of Fallible<()>, enabling proof tree construction throughout the system.
Includes basic proof tree construction in existing functions.
```

**Files to modify:**
- `crates/formality-core/src/judgment.rs` - Add ProofTree exports
- `crates/formality-core/src/judgment/proven_set.rs` - Core ProofTree implementation
- `crates/formality-core/src/lib.rs` - Update exports
- `crates/formality-macros/src/fixed_point.rs` - Macro support for ProofTree (exclude for_all clauses)
- `crates/formality-check/src/adts.rs` - Update function signatures
- `crates/formality-check/src/coherence.rs` - Update function signatures
- `crates/formality-check/src/fns.rs` - Update function signatures
- `crates/formality-check/src/impls.rs` - Update function signatures
- `crates/formality-check/src/lib.rs` - Update function signatures
- `crates/formality-check/src/mini_rust_check.rs` - Update function signatures
- `crates/formality-check/src/traits.rs` - Update function signatures
- `crates/formality-check/src/where_clauses.rs` - Update function signatures
- `crates/formality-prove/src/lib.rs` - Update function signatures
- `crates/formality-prove/src/prove.rs` - Update function signatures
- All other prove/ files with ProofTree integration
- `src/lib.rs` - Update function signatures
- Test files that need updates for compilation

**Diff sections to include:**
- All ProofTree type definitions and implementations
- Changes to ProvenSet to work with ProofTree
- Macro changes to generate proof trees (EXCLUDE for_all related clauses)
- Export updates
- All function signature changes to return ProofTree
- Basic ProofTree construction in checking functions
- Import statement updates for ProofTree

**Important Note:** When porting changes to the `judgment_fn!` macro, exclude any clauses related to `for_all` - those belong in commit 3.

**Exclude from this commit:**
- Test file changes (except those needed for compilation)
- Borrow checker files
- for_all macro clauses

### 3. Simplify error assertion macros to reduce noise

**Complete commit message:**
```
test: simplify error assertion macros to reduce noise

Streamlines error messages in test assertions by removing verbose
file/line information and simplifying failure reporting format.
This makes test failures more readable and focused on the actual
error content rather than implementation details.
```

**Files to modify:**
- `crates/formality-core/src/judgment/test_fallible.rs`
- `crates/formality-core/src/judgment/test_filtered.rs`
- `crates/formality-core/src/judgment/test_reachable.rs`

**Diff sections to include:**
- All changes to test assertion error message formats
- Simplified expect! test outputs

**Note:** Use `UPDATE_EXPECT=1 cargo test` to regenerate expect test outputs instead of manually copying diffs.

### 4. Add new `for_all` statement form to avoid extraneous judgments

**Complete commit message:**
```
core: add `for_all` statement form to avoid extraneous judgments

Introduces new `(for_all (x in y) ...)` syntax in judgment macros to
streamline proof generation by avoiding creation of unnecessary
intermediate judgments. This improves both performance and proof
tree clarity.

Includes updates to existing code to use the new for_all syntax
where appropriate (excluding NLL module which comes later).
```

**Files to modify:**
- `crates/formality-core/src/judgment/test_for_all.rs` (new file)
- `crates/formality-core/src/judgment.rs` - Add test_for_all module
- Macro changes in `crates/formality-macros/src/fixed_point.rs` for for_all support
- Any existing code outside NLL module that uses the new for_all syntax

**Diff sections to include:**
- Complete new test_for_all.rs file
- Macro implementation for for_all syntax (the clauses excluded from commit 1)
- Module declaration updates
- Any usage of for_all syntax in existing code (excluding borrow checker files)

**Exclude:**
- NLL module usage of for_all (that comes in commit 6)

### 5. Add comments and documentation to existing logic

**Complete commit message:**
```
docs: add explanatory comments throughout existing logic

Adds comprehensive comments and documentation to complex algorithms
and logic flows throughout the codebase. Improves code readability
and maintainability without changing any behavior.
```

**Files to modify:**
- Various files with comment additions (identify from diff)

**Diff sections to include:**
- All comment-only additions
- Documentation improvements
- No functional changes

### 6. Implement borrow checker liveness analysis

**Complete commit message:**
```
borrow-check: implement liveness analysis foundation

Adds liveness analysis infrastructure for the borrow checker,
including computation of live places before basic blocks and
statements. This provides the foundation for non-lexical lifetimes
and borrow checking.
```

**Files to modify:**
- `crates/formality-check/src/borrow_check.rs` (new file)
- `crates/formality-check/src/borrow_check/liveness.rs` (new file)
- `crates/formality-check/src/lib.rs` - Add borrow_check module
- `crates/formality-rust/src/grammar/minirust.rs` - Grammar updates for borrow checker

**Diff sections to include:**
- Complete liveness.rs implementation
- Module structure setup
- Integration points with existing type checking

**Exclude:**
- NLL implementation (comes in next commit)
- Integration with main checking pipeline

### 7. Implement core borrow checker with NLL

**Complete commit message:**
```
borrow-check: implement non-lexical lifetimes borrow checker

Implements the core borrow checking algorithm using non-lexical
lifetimes. This is a first draft that detects basic borrow checking
failures but doesn't yet handle moves or other advanced cases.

The implementation includes constraint solving for lifetime variables
and integration with the existing proof tree infrastructure.
```

**Files to modify:**
- `crates/formality-check/src/borrow_check/nll.rs` (new file)
- `crates/formality-prove/src/prove/prove_outlives.rs` (new file)
- `crates/formality-types/src/grammar/wc.rs` - Updates needed for NLL
- Test files with borrow checker test cases

**Diff sections to include:**
- Complete NLL implementation
- Outlives proving logic
- Core borrow checking algorithms

### 8. Integrate borrow checker with main checking pipeline

**Complete commit message:**
```
check: integrate borrow checker with main checking pipeline

Integrates the borrow checker into the main type checking pipeline,
adding borrow checking calls to function body checking and connecting
the liveness analysis with constraint solving.

This completes the borrow checker integration, enabling detection of
basic borrow checking failures in function bodies.
```

**Files to modify:**
- Updates to `mini_rust_check.rs` to call borrow checker
- Any remaining integration points not covered in commit 1
- Remaining test files that need expect test updates

**Diff sections to include:**
- Borrow checker integration calls
- Any remaining ProofTree construction not in commit 1
- Pipeline integration logic
- Test expectation updates using `UPDATE_EXPECT=1 cargo test`

**Note:** Most function signature changes should already be handled in commit 1.


