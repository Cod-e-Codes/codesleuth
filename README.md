# CodeSleuth

**CodeSleuth** is a modern, multi-language code intelligence CLI tool focused on COBOL analysis. It combines a fast Rust parser, a Go CLI, and a Python Markdown summarizer to provide actionable, human-friendly reports for legacy codebases.

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

---

## Features
- **COBOL Parsing:** Extracts program structure, data division, procedure logic, call/control flow, and I/O.
- **Markdown Summaries:** Generates clear, sectioned Markdown reports with Mermaid diagrams for call and control flow.
- **Dead Code Detection:** Lists unused paragraphs and variables.
- **Extensible Pipeline:** Modular Rust, Go, and Python components.
- **Modern CLI:** Easy to use, with verbose/debug options.

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
pip install -r requirements.txt  # (if you add one)
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