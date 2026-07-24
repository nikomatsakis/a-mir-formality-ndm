//! Structured mutation fuzzer for trait-validation cycles.
//!
//! This is deliberately not a source-text fuzzer. [`CycleWorld`] describes a
//! small, well-kinded family of programs with a concrete trait-method call,
//! and [`Mutation`] changes relationships within that family. A generated
//! program may be rejected, but if type checking accepts it then codegen must
//! be able to select every impl promised by the proof.

use std::any::Any;
use std::collections::BTreeSet;
use std::fmt::Write;
use std::panic::{catch_unwind, AssertUnwindSafe};

use a_mir_formality::test_program_ok;
use clap::Parser;
use formality_rust::grammar::Crates;
use formality_rust::rust::try_term;

#[derive(Debug, Parser)]
#[command(about = "Run structured mutation fuzzing against a-mir-formality")]
struct Args {
    /// Maximum number of mutations applied to one generated world.
    #[arg(long, default_value_t = 3)]
    max_mutations: usize,
}

/// The semantic knobs in the generated trait-cycle program.
///
/// The default world is the smallest interesting cycle: constructing
/// `Entry(Ground)` requires a projected `Target` bound, while the projection's
/// GAT condition requires `Entry(Ground)`. The mutations add real base facts,
/// redirect the projection, or insert extra relationships around that cycle.
#[derive(Clone, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
struct CycleWorld {
    supertrait_bridge: bool,
    helper_gat_condition: bool,
    associated_value_is_other: bool,
    target_for_ground: bool,
    target_for_other: bool,
    helper_for_ground: bool,
    irrelevant_impl: bool,
    reverse_impl_order: bool,
}

/// One relationship-changing operation supported by the initial engine.
#[derive(Clone, Copy, Debug)]
enum Mutation {
    AddSupertraitBridge,
    GateGatOnHelper,
    RedirectAssociatedValueToOther,
    AddTargetImplForGround,
    AddTargetImplForOther,
    AddHelperImplForGround,
    AddIrrelevantImpl,
    ReverseImplOrder,
}

impl Mutation {
    const ALL: [Self; 8] = [
        Self::AddSupertraitBridge,
        Self::GateGatOnHelper,
        Self::RedirectAssociatedValueToOther,
        Self::AddTargetImplForGround,
        Self::AddTargetImplForOther,
        Self::AddHelperImplForGround,
        Self::AddIrrelevantImpl,
        Self::ReverseImplOrder,
    ];

    /// Apply this mutation, returning false if it was already present.
    fn apply(self, world: &mut CycleWorld) -> bool {
        let slot = match self {
            Mutation::AddSupertraitBridge => &mut world.supertrait_bridge,
            Mutation::GateGatOnHelper => &mut world.helper_gat_condition,
            Mutation::RedirectAssociatedValueToOther => &mut world.associated_value_is_other,
            Mutation::AddTargetImplForGround => &mut world.target_for_ground,
            Mutation::AddTargetImplForOther => &mut world.target_for_other,
            Mutation::AddHelperImplForGround => &mut world.helper_for_ground,
            Mutation::AddIrrelevantImpl => &mut world.irrelevant_impl,
            Mutation::ReverseImplOrder => &mut world.reverse_impl_order,
        };

        !std::mem::replace(slot, true)
    }
}

#[derive(Clone, Debug)]
struct GeneratedCase {
    world: CycleWorld,
    mutations: Vec<Mutation>,
}

impl GeneratedCase {
    fn describe(&self) -> String {
        format!("mutations={:?}\nworld={:#?}", self.mutations, self.world)
    }
}

impl CycleWorld {
    /// Render a complete program whose `main` consumes the `Target(Ground)`
    /// dictionary implied by `Entry(Ground)`.
    fn render(&self) -> String {
        let mut source = String::from(
            r#"[crate test {
                trait Target {
                    fn witness(value: Self) -> i32;
                }

                trait Helper {}
                trait Noise {}

                struct Ground {}
                struct Other {}
"#,
        );

        if self.supertrait_bridge {
            source.push_str(
                r#"
                trait Bridge
                where
                    Self: Target,
                {}

                trait Entry
                where
                    Self: Bridge,
                {}
"#,
            );
        } else {
            source.push_str(
                r#"
                trait Entry
                where
                    Self: Target,
                {}
"#,
            );
        }

        let gat_condition = if self.helper_gat_condition {
            "Helper"
        } else {
            "Entry"
        };
        let associated_value = if self.associated_value_is_other {
            "Other"
        } else {
            "T"
        };

        writeln!(
            source,
            r#"
                trait Family {{
                    type Item<T>: [Target]
                    where
                        T: {gat_condition};
                }}
"#,
        )
        .unwrap();

        let mut impls = vec![format!(
            r#"
                impl Family for () {{
                    type Item<T> = {associated_value}
                    where
                        T: {gat_condition};
                }}
"#,
        )];

        impls.push(String::from(
            r#"
                impl Entry for Ground
                where
                    <() as Family>::Item<Ground>: Target,
                {}
"#,
        ));

        if self.supertrait_bridge {
            impls.push(String::from(
                r#"
                impl Bridge for Ground
                where
                    Ground: Target,
                {}
"#,
            ));
        }

        if self.target_for_ground {
            impls.push(String::from(
                r#"
                impl Target for Ground {
                    fn witness(value: Ground) -> i32 {
                        return 22 _ i32;
                    }
                }
"#,
            ));
        }

        if self.target_for_other {
            impls.push(String::from(
                r#"
                impl Target for Other {
                    fn witness(value: Other) -> i32 {
                        return 44 _ i32;
                    }
                }
"#,
            ));
        }

        if self.helper_for_ground {
            impls.push(String::from(
                r#"
                impl Helper for Ground {}
"#,
            ));
        }

        if self.irrelevant_impl {
            impls.push(String::from(
                r#"
                impl Noise for Other {}
"#,
            ));
        }

        if self.reverse_impl_order {
            impls.reverse();
        }
        source.extend(impls);

        source.push_str(
            r#"
                fn exercise<T>(value: T) -> i32
                where
                    T: Entry,
                {
                    return <T as Target>::witness(value);
                }

                fn main() -> () {
                    println!(exercise::<Ground>(Ground {}));
                }
            }]
"#,
        );

        source
    }
}

/// Enumerate every distinct world reachable by at most `max_mutations` edits.
fn generated_cases(max_mutations: usize) -> Vec<GeneratedCase> {
    let initial = GeneratedCase {
        world: CycleWorld::default(),
        mutations: vec![],
    };
    let mut cases = vec![initial.clone()];
    let mut frontier = vec![initial];
    let mut seen = BTreeSet::from([CycleWorld::default()]);

    for _ in 0..max_mutations {
        let mut next = vec![];
        for case in frontier {
            for mutation in Mutation::ALL {
                let mut mutated = case.clone();
                if !mutation.apply(&mut mutated.world) || !seen.insert(mutated.world.clone()) {
                    continue;
                }
                mutated.mutations.push(mutation);
                cases.push(mutated.clone());
                next.push(mutated);
            }
        }
        frontier = next;
    }

    cases
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum PipelineOutcome {
    Rejected,
    PassedCodegen,
}

/// Enforce the cross-phase contract for one generated program.
fn exercise_case(case: &GeneratedCase) -> PipelineOutcome {
    let source = case.world.render();
    let crates: Crates = try_term(&source).unwrap_or_else(|error| {
        panic!(
            "mutation engine generated invalid syntax\n{}\nerror={error:#}\nsource:\n{source}",
            case.describe(),
        )
    });

    let check =
        catch_unwind(AssertUnwindSafe(|| test_program_ok(&source))).unwrap_or_else(|payload| {
            panic!(
                "type checking panicked for a generated program\n{}\npanic={}\nsource:\n{source}",
                case.describe(),
                panic_payload(payload),
            )
        });
    if check.is_err() {
        return PipelineOutcome::Rejected;
    }

    let codegen = catch_unwind(AssertUnwindSafe(|| {
        formality_rust::codegen::codegen_program(&crates)
    }))
    .unwrap_or_else(|payload| {
        panic!(
            "type checking accepted a generated program that panicked in codegen\n{}\npanic={}\nsource:\n{source}",
            case.describe(),
            panic_payload(payload),
        )
    });
    codegen.unwrap_or_else(|error| {
        panic!(
            "type checking accepted a generated program that codegen rejected\n{}\nerror={error:#}\nsource:\n{source}",
            case.describe(),
        )
    });

    PipelineOutcome::PassedCodegen
}

fn panic_payload(payload: Box<dyn Any + Send>) -> String {
    match payload.downcast::<String>() {
        Ok(message) => *message,
        Err(payload) => match payload.downcast::<&'static str>() {
            Ok(message) => (*message).to_owned(),
            Err(_) => String::from("non-string panic payload"),
        },
    }
}

fn main() {
    let args = Args::parse();

    // Exploring deliberately cyclic goals is deeply recursive. Keep the larger
    // stack local to this tool instead of changing every Cargo process.
    let sweep = std::thread::Builder::new()
        .name(String::from("trait-cycle-mutation-sweep"))
        .stack_size(64 * 1024 * 1024)
        .spawn(move || run_bounded_trait_cycle_mutations(args.max_mutations))
        .expect("failed to spawn trait-cycle mutation sweep");

    if let Err(payload) = sweep.join() {
        std::panic::resume_unwind(payload);
    }
}

fn run_bounded_trait_cycle_mutations(max_mutations: usize) {
    let cases = generated_cases(max_mutations);
    let mut rejected = 0;
    let mut passed_codegen = 0;

    eprintln!(
        "running {} trait-cycle worlds (maximum mutation depth: {max_mutations})",
        cases.len()
    );

    for case in &cases {
        match exercise_case(case) {
            PipelineOutcome::Rejected => rejected += 1,
            PipelineOutcome::PassedCodegen => passed_codegen += 1,
        }
    }

    assert!(rejected > 0, "mutation engine did not exercise rejection");
    assert!(
        passed_codegen > 0,
        "mutation engine did not exercise successful codegen"
    );

    eprintln!(
        "completed {} worlds: {rejected} rejected, {passed_codegen} passed codegen",
        cases.len()
    );
}
