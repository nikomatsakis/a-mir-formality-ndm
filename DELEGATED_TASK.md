# Delegated Task: Complete final integration and test updates

## Context

**Current Status:** ProofTree function signatures are now complete! The remaining changes are primarily:
1. Macro system integration (judgment.rs, proven_set.rs)
2. Test file updates that need expect test regeneration
3. Final cleanup

**Your Mission:** Complete the final integration work and update all test expectations.

## Expected Changes

**Primary focus:**
- Complete macro system integration in `judgment.rs` and `proven_set.rs`
- Update all test files using `UPDATE_EXPECT=1 cargo test`
- Apply any remaining infrastructure changes
- Clean up temporary files

**Key remaining files:**
- `crates/formality-core/src/judgment.rs` (482 lines) - macro system
- `crates/formality-core/src/judgment/proven_set.rs` (519 lines) - ProofTree implementation
- All test files - need expect test regeneration

## Process

1. **Apply remaining infrastructure changes**:
   - Focus on `judgment.rs` and `proven_set.rs` macro system changes
   - Apply other core infrastructure updates
2. **Regenerate all test expectations**:
   - Run `UPDATE_EXPECT=1 cargo test` to update all expect tests
   - Add all updated test files to git
3. **Build and test**:
   - Ensure `cargo check` passes
   - Ensure `cargo test` passes (or at least runs without panics)
4. **Clean up temporary files**:
   - Remove `DELEGATED_TASK.md`, `REFACTORING_PLAN.md`, `*.log`, `*.diff` files
5. **Final commit**

**Target commit message:**
```
test: complete final integration and update all test expectations

Completes the final integration work including macro system updates
and regenerates all test expectations to work with the new ProofTree
infrastructure and borrow checker implementation.

This completes the git history cleanup, transforming the original
messy development history into a clean, reviewable story.
```

## RESULTS

**Status:** [SUCCESS/NEED_HELP/BLOCKED - fill this in when complete]

**Commits Made:** [List any commits created]

**Build Status:** [Does `cargo check` and `cargo test` work?]

**Final Cleanup:** [Were temporary files removed?]
