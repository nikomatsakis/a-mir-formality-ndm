# a-mir-formality

a-mir-formality is an experimental project that aims to be a complete, authoritative formal model of [Rust](https://rustc-dev-guide.rust-lang.org/mir/index.html). Presuming these experiments bear fruit, the intention is to bring this model into Rust as an RFC
and develop it as an official part of the language definition.

At least initially, the goal is to not to model surface Rust syntax but rather the internal "[MIR][]" used within the compiler. Modeling MIR means that we have a model of the trait and type system, but we exclude some parts of type inference and other aspects of Rust. Eventually we may extend the model to cover more and more of surface Rust.

[MIR]: https://rustc-dev-guide.rust-lang.org/mir/index.html

a-mir-formality does NOT define operational semantics. Those are the provenance (pun intended) of [MiniRust][].

[MiniRust]: mostly this seems to be “tesin
