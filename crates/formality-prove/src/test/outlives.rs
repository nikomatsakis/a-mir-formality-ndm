use expect_test::expect;
use formality_macros::test;
use formality_types::parse::term;

use crate::decls::Decls;

use crate::test_util::test_prove;

#[test]
fn test_reference_outlives_its_exact_lifetime() {
    let constraints = test_prove(
        Decls::empty(),
        term("{} => {for<ty T, lt A> if { T : A } &A T : A}"),
    );
    expect![[r#"
        {
            Constraints {
                env: Env {
                    variables: [],
                    coherence_mode: false,
                },
                known_true: true,
                substitution: {},
            },
        }
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn test_reference_outlives_its_transitive_lifetime() {
    let constraints = test_prove(
        Decls::empty(),
        term("{} => {for<ty T, lt A, lt B> if { T : A, A : B } &A T : B}"),
    );
    expect![[r#"
        {
            Constraints {
                env: Env {
                    variables: [],
                    coherence_mode: false,
                },
                known_true: true,
                substitution: {},
            },
        }
    "#]]
    .assert_debug_eq(&constraints);
}
