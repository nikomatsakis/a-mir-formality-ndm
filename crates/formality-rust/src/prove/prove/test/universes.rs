use crate::rust::term;
use expect_test::expect;
use formality_macros::test;
use std::sync::Arc;

use crate::prove::prove::decls::Program;

use crate::prove::prove::test_util::test_prove;

/// There is U that is equal to some T.
#[test]
fn for_t_exists_u() {
    let decls = Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Test<T> where {}"),
            term("impl<X> Test<X> for X {}"),
        ])),
        ..Program::empty()
    };

    test_prove(decls, term("{} => {for<T> Test(T, T)}")).assert_ok(expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}
