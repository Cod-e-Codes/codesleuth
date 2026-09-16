# CodeSleuth

CLI that parses COBOL and writes Markdown summaries (program structure, unused paragraphs, call and control flow).

## Requirements

- Rust 1.70 or later: https://rustup.rs/

## Install

```bash
git clone https://github.com/Cod-e-Codes/codesleuth.git
cd codesleuth
cargo build --release
```

The binary is `target/release/codesleuth` (`.exe` on Windows).

## Usage

```bash
./target/release/codesleuth analyze path/to/cobol
```

`analyze` accepts a file or a directory of `.cob` / `.cbl` / `.cobol` files.

Flags:

- `--verbose` extra progress on stderr
- `--debug` internal trace on stderr
- `--workers N` concurrent workers (default: available parallelism)
- `--benchmark` print wall-clock throughput after analysis (`FilesProcessed`, `TotalTimeSec`, `Throughput`)
- `--output PATH` Markdown file if the input is a file; directory of flattened `.md` names if the input is a directory

Flattened report names strip `:`, replace `/` and `\` with `_`, and drop the source extension.

Other subcommands:

```bash
./target/release/codesleuth parse file.cob
./target/release/codesleuth summarize -i ir.json -o report.md
```

`parse` writes IR JSON to stdout. `summarize` reads IR JSON (file or stdin) and writes Markdown.

Load-test file writer (not used as verification):

```bash
cargo run --bin gen_dummy -- --count 1000 --outdir dummy-cobol
```

## Limitations

The parser is not a complete COBOL implementation.

- COPY is recorded as an unresolved copybook name. REPLACING and REPLACE are not applied, and copybooks are not expanded.
- LINKAGE SECTION is not parsed.
- Quoted literals are split on spaces, so operands in the report may not match the source tokens.
- Missing AUTHOR is reported as UNKNOWN. Missing DATE-WRITTEN is filled with the UTC date of the run, so Markdown is not bit-stable across days.
- File Section and Input/Output headings are always printed; they say no entries when the source has none.
- Nested programs (`END PROGRAM`, inner IDENTIFICATION DIVISION) are not modeled. One PROGRAM-ID and one procedure walk per file.
- `EXEC SQL` ... `END-EXEC` is treated as COBOL statements, not SQL.
- Column-7 continuation (hyphen vs space, Area B) is not implemented.
- `--workers` isolates unwinding panics per file (`catch_unwind`). The process still exits if Rust is built with `panic = "abort"`, on a double panic, or on some stack overflows.

IR JSON shape: [docs/ir_schema.json](docs/ir_schema.json).

Continuous integration has two required jobs: `rust` (`cargo fmt --check`, `cargo test`, `clippy -D warnings`, `cargo build --release`) and `corpus` (clone [openmainframeproject/cobol-programming-course](https://github.com/openmainframeproject/cobol-programming-course) at `1f037d24326124f4f2c5cd2d31e5220ceb549580`, CC-BY-4.0, then `analyze --workers` and `analyze --benchmark`). Clone or analyze failure fails the corpus job.

Report failures in [issues](https://github.com/Cod-e-Codes/codesleuth/issues).

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) and [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).

## Support

Open a GitHub issue.

## Maintainers

Cody Marsengill ([Cod-e-Codes](https://github.com/Cod-e-Codes))

## License

[MIT](LICENSE). For licensing questions, email [cod.e.codes.dev@gmail.com](mailto:cod.e.codes.dev@gmail.com).
