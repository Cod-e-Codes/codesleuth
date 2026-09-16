# CodeSleuth

CLI that parses COBOL and writes Markdown summaries (program structure, dead code, call and control flow). The Go front end in `cmd/` runs the Rust parser and summarizer in `codesleuth/`. `--workers` analyzes files concurrently.

## Features

- Parse COBOL to IR JSON
- Summarize IR to Markdown
- Report unused paragraphs and variables
- Mermaid call and control-flow graphs
- Concurrent file analysis (`--workers`)

## Requirements

- Rust 1.70 or later: https://rustup.rs/
- Go 1.23 or later: https://go.dev/dl/

## Install

```bash
git clone https://github.com/Cod-e-Codes/codesleuth.git
cd codesleuth
cargo build --release --manifest-path codesleuth/Cargo.toml
cd cmd && go build -o codesleuth && cd ..
```

On Windows, name the Go binary `codesleuth.exe`. The Rust binary is `codesleuth/target/release/codesleuth` (`.exe` on Windows).

## Usage

```bash
./cmd/codesleuth analyze path/to/cobol
```

Flags:

- `--verbose` extra progress on stderr
- `--debug` internal trace on stderr
- `--workers N` concurrent workers (default: logical CPUs)
- `--benchmark` print wall-clock throughput after analysis
- `--rust-backend PATH` Rust binary (default: `../codesleuth/target/release/codesleuth` next to the Go binary)
- `--input PATH` input directory or file (overrides the positional path)
- `--output DIR` write one Markdown file per COBOL file

The Rust binary also accepts `parse`, `summarize`, and `analyze`.

## Limitations

The parser is not a complete COBOL implementation.

- COPY is recorded as an unresolved copybook name. REPLACING is not applied, and copybooks are not expanded.
- LINKAGE SECTION is not parsed.
- Quoted literals are split on spaces, so operands in the report may not match the source tokens.
- Missing AUTHOR is reported as UNKNOWN. Missing DATE-WRITTEN is filled with the analysis date.
- File Section and Input/Output headings are always printed; they say no entries when the source has none.

Report failures in [issues](https://github.com/Cod-e-Codes/codesleuth/issues).

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) and [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).

## Support

Open a GitHub issue.

## Maintainers

Cody Marsengill ([Cod-e-Codes](https://github.com/Cod-e-Codes))

## License

[MIT](LICENSE). For licensing questions, email [cod.e.codes.dev@gmail.com](mailto:cod.e.codes.dev@gmail.com).
