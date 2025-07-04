# CodeSleuth

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg)](CONTRIBUTING.md)
[![Code of Conduct](https://img.shields.io/badge/code%20of%20conduct-Contributor%20Covenant-blueviolet.svg)](CODE_OF_CONDUCT.md)

**CodeSleuth** is a code intelligence CLI tool for analyzing legacy COBOL codebases. It is built with Rust and Go, providing a modular architecture for parsing, analysis, and reporting.

---

## Features

- **COBOL Parsing:** Rust-based parser for extracting program structure and metadata
- **Analysis & Summarization:** Generates Markdown reports with program structure, data division, procedure logic, call/control flow, and I/O analysis
- **Dead Code Detection:** Identifies unused paragraphs and variables
- **Call & Control Flow Visualization:** Mermaid diagrams for call and control flow
- **Modern CLI:** Go-based command-line interface with verbose/debug options
- **Cross-platform:** Runs on Windows, macOS, and Linux
- **CI/CD Ready:** Automated testing and linting via GitHub Actions

---

## Architecture

- **Rust Parser** (`parser/`): Parses COBOL source files and outputs an intermediate representation (IR) as JSON
- **Rust Summarizer** (`summarizer/`): Consumes IR and generates Markdown reports
- **Go CLI** (`cmd/`): Orchestrates parsing and summarization, providing a user-facing CLI

---

## Installation

### Prerequisites

- **Rust** (1.70+): [Install via rustup.rs](https://rustup.rs/)
- **Go** (1.21+): [Download from golang.org](https://golang.org/dl/)
- **Optional:** [Graphviz](https://graphviz.gitlab.io/) for advanced graph rendering

### Build from Source

```bash
# Clone the repository
git clone https://github.com/Cod-e-Codes/codesleuth.git
cd codesleuth

# Build Rust components
cd parser && cargo build --release && cd ..
cd summarizer && cargo build --release && cd ..

# Build Go CLI
cd cmd && go build -o codesleuth && cd ..
```

---

## Usage

```bash
# Analyze a COBOL file or directory (basic usage)
./cmd/codesleuth analyze path/to/your/cobol/files

# Enable verbose output
./cmd/codesleuth analyze --verbose path/to/your/cobol/files

# Analyze a single file
./cmd/codesleuth analyze program.cbl

# Output analysis to a Markdown file
./cmd/codesleuth analyze path/to/your/cobol/files > analysis_report.md

# Analyze with debug information (verbose + debug)
./cmd/codesleuth analyze --verbose --debug complex-program.cbl
```

---

## Configuration

CodeSleuth works out-of-the-box. It automatically detects COBOL files (`.cob`, `.cbl`, `.cobol`) and generates analysis reports. No additional configuration is required for standard use cases.

---

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on code style, testing, pull requests, and issue reporting.

---

## Security

If you discover a security vulnerability, please see [SECURITY.md](SECURITY.md) for responsible disclosure guidelines.

---

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

---

## Links

- [GitHub Repository](https://github.com/Cod-e-Codes/codesleuth)
- [Rust Programming Language](https://www.rust-lang.org/)
- [Go Programming Language](https://golang.org/)

---

## Roadmap & Future Enhancements

- **Concurrency:** Planned support for parallel analysis of multiple files to improve performance on large codebases.
- **Configurable Output:** More options for customizing report formats and output destinations.
- **Plugin System:** Extensible architecture for custom analysis or reporting plugins.
- **Improved Error Handling:** More robust diagnostics and user feedback for parsing and analysis errors.
- **Performance Benchmarks:** Publish real-world benchmarks as the tool matures.
- **CI/CD Integration:** Enhanced support for integration with CI/CD pipelines and code quality gates.

---

*Built for maintainers and developers working with legacy COBOL systems.* 