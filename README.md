# CodeSleuth

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg)](CONTRIBUTING.md)
[![Code of Conduct](https://img.shields.io/badge/code%20of%20conduct-Contributor%20Covenant-blueviolet.svg)](CODE_OF_CONDUCT.md)
[![Feature: Concurrency Worker Pool](https://img.shields.io/badge/feature-concurrency--worker--pool-blueviolet)](https://github.com/Cod-e-Codes/codesleuth/tree/feat/concurrency-worker-pool)

**CodeSleuth** is a code intelligence CLI tool for analyzing legacy COBOL codebases. It is built with Rust and Go, providing a modular architecture for parsing, analysis, and reporting.

---

## 🚀 Why Use CodeSleuth?

Enterprise COBOL codebases are often large, undocumented, and hard to modernize. CodeSleuth automates structural analysis, dead code detection, and documentation — helping teams audit, refactor, or migrate legacy COBOL more confidently.

---

> **Note:**
> This is the `main` branch. It currently uses a single-threaded Go CLI and separate Rust parser/summarizer binaries. **Unified backend and concurrency are available on the [`feat/concurrency-worker-pool`](https://github.com/Cod-e-Codes/codesleuth/tree/feat/concurrency-worker-pool) branch and will be merged soon.**

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

> **Upcoming:**
> The next release will unify the Rust backend and add concurrent file analysis, as already available on the feature branch.

---

## 🔧 Project Status

This is a **work-in-progress alpha**. The `main` branch is stable for single-file and small-batch COBOL analysis. **Unified backend and concurrency are being actively tested on the feature branch and will be merged after further bug fixes.**

---

## 📄 Example Output

```markdown
# COBOL Program Summary

**Program Name:** PAYROLL01  
**Author:** Jane Doe  
**Working-Storage:** 28 variables  
**Dead Code Detected:** 3 paragraphs

## Control Flow

```

```mermaid
graph TD
  Start --> Validate
  Validate --> Process
  Process --> End
```

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
./cmd/codesleuth.exe analyze path/to/your/cobol/files

# Enable verbose output (high-level extra info, logs to stderr)
./cmd/codesleuth.exe analyze --verbose path/to/your/cobol/files

# Enable debug output (detailed trace/internal state, logs to stderr)
./cmd/codesleuth.exe analyze --debug path/to/your/cobol/files

# Analyze a single file
./cmd/codesleuth.exe analyze program.cbl

# Output analysis to a Markdown file
./cmd/codesleuth.exe analyze path/to/your/cobol/files > analysis_report.md

# Output per-file Markdown to a directory
./cmd/codesleuth.exe analyze --output ./summaries path/to/your/cobol/files

# Output JSON instead of Markdown
./cmd/codesleuth.exe analyze --json path/to/your/cobol/files
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

- **Unified Backend & Concurrency:** Planned for the next release (see feature branch for early access)
- **Configurable Output:** More options for customizing report formats and output destinations.
- **Plugin System:** Extensible architecture for custom analysis or reporting plugins.
- **Improved Error Handling:** More robust diagnostics and user feedback for parsing and analysis errors.
- **Performance Benchmarks:** Publish real-world benchmarks as the tool matures.
- **CI/CD Integration:** Enhanced support for integration with CI/CD pipelines and code quality gates.

---

*Built for maintainers and developers working with legacy COBOL systems.* 
