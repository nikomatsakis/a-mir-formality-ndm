use crate::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::prove::decls::Program;

use crate::prove::prove::test_util::test_prove;

#[test]
fn test_normalize_assoc_ty() {
    test_prove(
        Program::empty(),
        term("{} => {for<T> if { <T as Iterator>::Item = u32 } <T as Iterator>::Item = u32}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}
