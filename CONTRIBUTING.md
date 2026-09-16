# Contributing to CodeSleuth

Bug reports, feature requests, and pull requests are accepted.

## How to contribute

- [Issues](https://github.com/Cod-e-Codes/codesleuth/issues): include how to reproduce bugs.
- Pull requests: branch from `main`, pass tests and lints, describe the change.

## Coding style

- [Rust Style Guide](https://doc.rust-lang.org/stable/style-guide/)
- `cargo fmt` and `cargo clippy --all-targets -- -D warnings`

Use descriptive names. Comment non-obvious logic.

## Commit messages

Present tense. Short summary, then detail if needed. Reference issues when relevant (`Fixes #12`).

## Contact

Open an issue or email Cody Marsengill at [cod.e.codes.dev@gmail.com](mailto:cod.e.codes.dev@gmail.com).

## Setup

1. Rust 1.85+: https://rustup.rs/
2. Clone and build:

```sh
git clone https://github.com/Cod-e-Codes/codesleuth.git
cd codesleuth
cargo build --release
```

## Tests

```sh
cargo test
cargo clippy --all-targets -- -D warnings
cargo fmt --check
```

Product path on local public COBOL (not dummy files):

```sh
./target/release/codesleuth analyze test-cobol --workers 4 --output reports-verify
./target/release/codesleuth analyze test-cobol --workers 4 --benchmark
```

`test-cobol/` is gitignored. The COBOL Programming Course labs are CC-BY-4.0; IBM Z Open Editor samples in that tree keep their upstream licenses.

Load-test files only:

```sh
cargo run --bin gen_dummy -- --count=1000 --outdir=dummy-cobol
```

## Before committing

- `cargo fmt` and `cargo clippy --all-targets -- -D warnings`
