# CodeSleuth

CLI that parses COBOL and writes Markdown summaries (program structure, unused paragraphs, call and control flow).

## Requirements

- Rust 1.85 or later: https://rustup.rs/

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
- `--benchmark` print wall-clock throughput after analysis (`Enabled`, `FilesProcessed`, `TotalTimeSec`, `Throughput`, `StartTime`, `EndTime`)
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

- COPY inserts library text when a matching `.cpy` / `.cbl` / `.cob` member is found next to the source or in `COPYBOOK`, `COPYLIB`, or `copybooks` directories walking up from the file. REPLACING matches whole text words (so `FOO` does not change `FOOBAR`) and dummy operands (`:TAG:`, `(TAG)`). `LEADING` / `TRAILING` match a prefix or suffix of one text word. REPLACE runs after COPY. If the member is not found, COPY remains a copybook data item.
- LINKAGE SECTION is parsed as names and descriptions. IBM: that data exists elsewhere; this tool does not reserve storage.
- Missing AUTHOR and DATE-WRITTEN are UNKNOWN. IBM: those paragraphs are optional comment-entries. Column-7 `*` lines in IDENTIFICATION are stored as comments (leading `*` only if it is the first character of the line). Banner separators such as `*-----------------------` are included.
- Working-Storage, File Section, Linkage, and Input/Output headings are omitted when the source has no entries. Call Graph and Control Flow Graph headings are omitted when the IR has no edges.
- Nested programs (`END PROGRAM`, inner IDENTIFICATION) are stored in `nested_programs` (same IR shape). Coverage is the inline fixture in `tests/corpus_shapes.rs`. The verification corpus (cobol-programming-course pin plus IBM Z Open Editor samples) has no nested programs, so that path is not exercised on those files.
- In PROCEDURE DIVISION, `EXEC SQL` ... `END-EXEC` is one statement (IBM: those delimiters are complete on one line). In DATA DIVISION, `EXEC SQL INCLUDE` is expanded like COPY when the member is found; other `EXEC SQL` text (DECLARE TABLE, DECLARE CURSOR, INCLUDE when the member is missing) is skipped and is not a data item. Not a Db2 coprocessor: no DBRM, no generated SQLCA.
- Column-7 hyphen joins without a space. For an unclosed alphanumeric or national literal, spaces through column 72 of the continued line are kept, and the continuation quote is not part of the value ([IBM 6.5 continuation lines](https://www.ibm.com/docs/en/cobol-zos/6.5.0?topic=b-continuation-lines), Language Reference SC27-8713-04).
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
