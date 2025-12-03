# Delegated Task: Implement borrow checker liveness analysis

## Context

**Overall PR Goal:** This branch implements a "first draft" of the borrow checker. It's definitely incomplete (doesn't check moves, for example) but it is detecting basic failures. The branch ALSO does a bunch of other random things: generates proof-trees when the check is successful, so that you can tell why it succeeded; changes our error assertion macros to not have so much noise; adds comments into existing logic; introduces a new `(for_all (x in y) ...)` statement form to avoid extraneous judgments.

**Current Step:** 6 of 8 - Implement borrow checker liveness analysis

**Previous Commits:** 
- ✅ Commit 1: Added .sparkle-space to .gitignore
- ✅ Commit 2: Added ProofTree infrastructure and updated function signatures
- ✅ Commit 3: Simplified error assertion macros to reduce noise
- ✅ Commit 4: Added new `for_all` statement form to avoid extraneous judgments
- ✅ Commit 5: Added comments and documentation to existing logic

## Your Mission

Create one commit that accomplishes: Add liveness analysis infrastructure for the borrow checker, including computation of live places before basic blocks and statements.

**Target commit message:**
```
borrow-check: implement liveness analysis foundation

Adds liveness analysis infrastructure for the borrow checker,
including computation of live places before basic blocks and
statements. This provides the foundation for non-lexical lifetimes
and borrow checking.
```

## Current State

**Available files:**
- `full-changes.diff` - Complete diff of all changes from original branch
- Current working directory on clean branch `living-large-cleanup-20251203-clean`

**Your task:** Copy relevant sections from `full-changes.diff` to `to-stage.diff`, apply, and commit

## Expected Changes

**Files to modify:**
- `crates/formality-check/src/borrow_check.rs` (new file)
- `crates/formality-check/src/borrow_check/liveness.rs` (new file)
- `crates/formality-check/src/lib.rs` - Add borrow_check module
- `crates/formality-rust/src/grammar/minirust.rs` - Grammar updates for borrow checker

**Diff sections to include:**
- Complete liveness.rs implementation
- Module structure setup
- Integration points with existing type checking
- Grammar updates needed for liveness analysis

**EXCLUDE:**
- NLL implementation (comes in next commit)
- Integration with main checking pipeline (comes in final commit)

## Process

1. **FIRST**: Regenerate the diff to ensure it's current: `git diff living-large-cleanup-20251203-clean living-large-cleanup > full-changes.diff`
2. Examine `full-changes.diff` to understand liveness analysis changes
3. Copy the specified sections from `full-changes.diff` to `to-stage.diff`
4. Apply the patch: `git apply to-stage.diff`
5. Verify the changes look correct with `git status` and `git diff --name-only`
6. Create the commit with the specified message
7. Test that the commit builds correctly: `cargo check`
8. Clean up `to-stage.diff` file
9. Report results below

## Troubleshooting

- **Focus on liveness**: Only include liveness analysis, not full NLL implementation
- **New files**: Make sure to create the new borrow_check directory structure
- **Grammar updates**: Include any minirust.rs changes needed for liveness
- If `git apply` fails, try applying to specific files: `git apply to-stage.diff -- path/to/file.rs`
- If build fails, you may need to include additional dependencies
- When in doubt, ask for help rather than making assumptions

## RESULTS

**Status:** [SUCCESS/NEED_HELP/BLOCKED - fill this in when complete]

**Commits Made:** [List any commits created]

**Issues Encountered:** [Describe any problems]

**Remaining Staged Changes:** [What's left for future commits]

**Suggested Plan Changes:** [Any recommendations for revising the plan]
