# CodeSleuth

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg)](CONTRIBUTING.md)
[![Code of Conduct](https://img.shields.io/badge/code%20of%20conduct-Contributor%20Covenant-blueviolet.svg)](CODE_OF_CONDUCT.md)
[![Issues](https://img.shields.io/github/issues/Cod-e-Codes/codesleuth.svg)](https://github.com/Cod-e-Codes/codesleuth/issues)
[![Pull Requests](https://img.shields.io/github/issues-pr/Cod-e-Codes/codesleuth.svg)](https://github.com/Cod-e-Codes/codesleuth/pulls)
[![Last Commit](https://img.shields.io/github/last-commit/Cod-e-Codes/codesleuth.svg)](https://github.com/Cod-e-Codes/codesleuth/commits/main)

---

**CodeSleuth** is a modern, multi-language code intelligence CLI tool focused on COBOL analysis. It combines a fast Rust parser, a Go CLI, and a Python Markdown summarizer to provide actionable, human-friendly reports for legacy codebases.

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
- **Rust** (for the parser): https://rustup.rs/
- **Go** (for the CLI): https://golang.org/dl/
- **Python 3.8+** (for the summarizer): https://python.org/
- (Optional) [Graphviz](https://graphviz.gitlab.io/) for advanced graph rendering

Install Python dependencies:
```sh
pip install -r requirements.txt
```

### 3. Build the Rust Parser
```sh
cd parser
cargo build --release
cd ..
```

### 4. Build the Go CLI
```sh
cd cmd
go build -o codesleuth.exe
cd ..
```

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