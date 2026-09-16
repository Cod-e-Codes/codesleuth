# Contributing to CodeSleuth

Bug reports, feature requests, and pull requests are accepted.

## How to contribute

- [Issues](https://github.com/Cod-e-Codes/codesleuth/issues): include reproduction steps for bugs.
- Pull requests: branch from `main`, pass tests and lints, describe the change.

## Coding style

- Rust: [Rust Style Guide](https://doc.rust-lang.org/1.0.0/style/)
- Go: [Effective Go](https://go.dev/doc/effective_go)

Use descriptive names. Comment non-obvious logic.

## Commit messages

Present tense. Short summary, then detail if needed. Reference issues when relevant (`Fixes #12`).

## Contact

Open an issue or email Cody Marsengill at [cod.e.codes.dev@gmail.com](mailto:cod.e.codes.dev@gmail.com).

## Setup

1. Rust 1.70+: https://rustup.rs/
2. Go 1.23+: https://go.dev/dl/
3. Clone and build:

```sh
git clone https://github.com/Cod-e-Codes/codesleuth.git
cd codesleuth
go mod tidy
cargo build --release --manifest-path codesleuth/Cargo.toml
cd cmd && go build -o codesleuth && cd ..
```

## Tests

```sh
go test ./...
cd codesleuth && cargo test
```

CLI:

```sh
./cmd/codesleuth analyze --workers=4 --benchmark dummy-cobol
```

Dummy files:

```sh
go run scripts/generate_dummy_cobol.go --count=1000 --outdir=dummy-cobol
```

## Before committing

- Rust: `cargo fmt` and `cargo clippy` in `codesleuth/`
- Go: `gofmt` and `go vet`
