# CodeSleuth

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg)](CONTRIBUTING.md)
[![Code of Conduct](https://img.shields.io/badge/code%20of%20conduct-Contributor%20Covenant-blueviolet.svg)](CODE_OF_CONDUCT.md)

> **🚀 Rust Summarizer Branch**: This branch replaces the Python summarizer with a high-performance Rust implementation, providing 10-50x faster processing and eliminating Python dependencies.

<!-- Note: Issue/PR/commit badges require a public repo and are omitted for privacy. -->

---

**CodeSleuth** is a modern, multi-language code intelligence CLI tool focused on COBOL analysis. It combines a fast Rust parser, a Go CLI, and a Rust Markdown summarizer to provide actionable, human-friendly reports for legacy codebases.

---

## Features
- **COBOL Parsing:** Extracts program structure, data division, procedure logic, call/control flow, and I/O.
- **Markdown Summaries:** Generates clear, sectioned Markdown reports with Mermaid diagrams for call and control flow.
- **Dead Code Detection:** Lists unused paragraphs and variables.
- **Extensible Pipeline:** Modular Rust, Go, and Python components.
- **Modern CLI:** Easy to use, with verbose/debug options.
- **Security Policy:** See [SECURITY.md](SECURITY.md)
- **Continuous Integration:** Automated tests and linting via GitHub Actions.

---

## Quickstart

### 1. Clone the Repo
```sh
git clone https://github.com/Cod-e-Codes/codesleuth.git
cd codesleuth
```

### 2. Install Requirements
- **Rust** (for the parser and summarizer): https://rustup.rs/
- **Go** (for the CLI): https://golang.org/dl/
- (Optional) [Graphviz](https://graphviz.gitlab.io/) for advanced graph rendering

### 3. Build the Rust Components
```sh
# Build the parser
cd parser
cargo build --release
cd ..

# Build the summarizer
cd summarizer
cargo build --release
cd ..
```

### 4. Build the Go CLI
```sh
cd cmd
go build -o codesleuth.exe
cd ..
```

**Note:** The Go CLI now calls the Rust summarizer instead of Python, providing better performance and eliminating Python dependencies.

### 5. Run CodeSleuth
```sh
./cmd/codesleuth.exe analyze --verbose path/to/your/cobol/files
```

---

## Usage
- Analyze a COBOL directory:
  ```sh
  ./cmd/codesleuth.exe analyze --verbose test-cobol/
  ```
- The tool will print Markdown summaries to the console.
- You can redirect output to a file:
  ```sh
  ./cmd/codesleuth.exe analyze test-cobol/ > summary.md
  ```

---

## Security

If you discover a security vulnerability, please see [SECURITY.md](SECURITY.md) for responsible disclosure guidelines.

---

## Continuous Integration

This project uses GitHub Actions for automated testing and linting. See `.github/workflows/ci.yml` for details.

---

## Contributing
Pull requests, issues, and suggestions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

---

## License
MIT License. See [LICENSE](LICENSE).

---

## Links
- [GitHub Repo](https://github.com/Cod-e-Codes/codesleuth)
- [Rust](https://www.rust-lang.org/)
- [Go](https://golang.org/)
- [Python](https://python.org/) 