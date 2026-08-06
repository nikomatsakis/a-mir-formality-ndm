# Trait solving and cyclic evidence

Rust trait evidence is modeled as a dictionary. A completed dictionary contains the information
promised by a trait declaration: selected associated-type values, supertrait dictionaries,
outlives evidence, and dictionaries for associated-type bounds.

Trait solving is coinductive. An impl is therefore allowed to use the dictionary it is currently
constructing to close a recursive occurrence. This does **not** mean that every field of that
dictionary is immediately available. Doing so could construct a supertrait or associated-type
bound using the very field being constructed, producing evidence that code generation cannot
monomorphize.

The solver prevents that by tracking a *construction frontier* for provisional evidence.

## Soundness criterion

The intended criterion is not that every successful trait proof has a finite inductive evidence
tree. Recursive dictionaries are intentional. Instead:

> In a well-checked program, trait evidence accepted by the solver can be monomorphized.

An inductive cycle that does not expose any unavailable dictionary field can therefore be valid.
A cycle that manufactures a field from itself is rejected.

## Proof modes

An ordinary atomic proposition such as `T: Debug` denotes completed evidence. A mode-qualified
proposition denotes the observable portion of provisional evidence:

| Proposition | Meaning |
| --- | --- |
| `Zero(T: Trait)` | An opaque recursive handle. It exposes no dictionary fields. |
| `Supertraits[Root](T: Trait)` | The view available while constructing `Root`'s supertrait fields. |
| `GatBounds[Root](T: Trait)` | The later view available while constructing `Root`'s associated-type-bound fields. |

The frontier is indexed by `Root`, but its meaning also depends on the trait in the proposition.
For example, if `Unrelated` has no fields available below `Root`, then
`Supertraits[Root](T: Unrelated)` may require no more evidence than `Zero(T: Unrelated)`.

Modes attach only to atomic predicates. Applying a mode to a compound where-clause propagates it
through binders and implications. Goal and assumption positions are distinguished: when crossing
the premise of an implication, their roles reverse. This treats an implication as a hypothetical
judgment rather than assuming the generally invalid modal equivalence
`M(P -> Q) = (M(P) -> M(Q))`.

## Ordering traits

The construction frontier uses a syntactically derived strict partial order between traits. First,
the model builds a dependency graph. A trait has an edge to another trait when the latter appears
in a relevant declaration dependency, including:

* a positive predicate in the trait's where-clauses;
* an associated type's declared bounds or where-clauses;
* a companion blanket impl of the form `impl<T: Dependency> Trait for T` in the trait's defining
  crate; or
* a trait declared in a dependency crate.

The graph itself may contain cycles. The strict order uses asymmetric reachability:

```text
Lower < Upper
    if Upper can reach Lower
    and Lower cannot reach Upper
```

Traits in the same strongly connected component are incomparable. Consequently, recursive traits
cannot use the ordering to expose one another's provisional fields. Omitting a safe graph edge can
make the solver reject a valid impl, but it cannot create an invalid ordering fact; the edge policy
is intentionally conservative and can evolve independently of the construction rules.

## Checking an impl

Impl well-formedness is a closed judgment:

{judgment}`prove_impl_wf`

Its caller supplies no assumptions. The impl binder is instantiated universally, which prevents an
impl from proving its own well-formedness by selecting itself as ordinary completed evidence.

Conceptually, an impl is checked as a dictionary constructor. For an impl of `Trait`, its inputs are
the impl where-clauses viewed at the appropriate construction frontier:

* supertrait and outlives fields are checked using `Supertraits[Trait]` evidence;
* an associated value and its promised bound dictionaries are checked using
  `GatBounds[Trait]` evidence.

This distinction corresponds to dictionary construction order. Associated-type values are selected
by the impl and can be known before the dictionaries proving their declared bounds have been built.
Knowing the value does not expose those bound dictionaries.

For example:

```rust
trait Bound {}

trait Family {
    type Item: [Bound];
}

impl Family for Source {
    type Item = Value;
}
```

Selecting this impl determines that `<Source as Family>::Item` is `Value`. The impl is nevertheless
well-formed only if it can construct the separate evidence that `Value: Bound` at the GAT-bound
frontier.

## Applying an impl

Impl application begins with Löb-style recursion. While matching one candidate, the requested trait
reference is added as a branch-local `Zero` assumption. This can close an exact recursive request,
but it cannot project any fields from the dictionary being constructed.

After matching the candidate and inferring its binder substitution, the selected impl fixes its
associated-type values. The candidate's header is then available at its `Supertraits` frontier, and
the impl where-clauses are proven at that same frontier. Impl well-formedness established that these
validated inputs suffice to construct completed evidence.

Schematically, after applying the inferred substitution:

```text
ImplWF(I)
Zero(requested trait ref) |- match I
Supertraits[I.Trait](I.header) |- Supertraits[I.Trait](I.where-clauses)
------------------------------------------------------------------------
requested trait ref is implemented via I
```

Substitutions inferred while matching remain local to the candidate branch until its residual
obligations succeed.

## Normalizing associated types

Normalization also selects a concrete impl and infers its substitution. Selection reveals the
associated-type value because that value is part of the impl itself. Proving the declared bounds of
the value is a separate operation performed at the GAT-bound frontier.

This separation is important for early normalization: matching an impl header may need to know that
an alias has a particular rigid value even though the surrounding dictionary construction is not
yet complete. The normalized value may be used within that validation branch, but provisional
evidence must not escape as ordinary completed evidence.

## Locality and cycles

Consider two traits that share a base trait:

```rust
trait Base {}
trait A where Self: Base {}
trait B where Self: Base {}

impl<T> A for T
where
    T: B,
{}
```

This impl can be valid when the derived ordering makes the required `B` fields available while
constructing `A`. If `A` and `B` participate in a dependency cycle, they become incomparable and the
same provisional projection is rejected. In some incomparable cases an impl must state an
apparently redundant where-clause explicitly; this is an intentional completeness tradeoff of a
local, syntactic ordering policy.

Associated-type bounds need the same protection. A diverging associated type must not supply the
bound dictionary that would justify its own normalization. The regression derived from
[rust-lang/rust#135011](https://github.com/rust-lang/rust/issues/135011) exercises this boundary:
the declared bound of a non-terminating projection cannot be used to bootstrap arbitrary evidence.

The principal executable examples live in:

* `tests/cycle_handling.rs`;
* `tests/associated_type_bounds.rs`; and
* `crates/formality-rust/src/prove/prove/test/proof_modes.rs`.
