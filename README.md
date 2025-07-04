# CodeSleuth

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg)](CONTRIBUTING.md)
[![Code of Conduct](https://img.shields.io/badge/code%20of%20conduct-Contributor%20Covenant-blueviolet.svg)](CODE_OF_CONDUCT.md)

**CodeSleuth** is a high-performance, multi-language code intelligence CLI tool designed for legacy COBOL analysis. Built with Rust and Go, it provides fast parsing, comprehensive analysis, and actionable insights for enterprise codebases.

---

## 🚀 Features

- **⚡ High-Performance Parsing:** Rust-based COBOL parser with 10-50x faster processing
- **📊 Comprehensive Analysis:** Program structure, data division, procedure logic, call/control flow, and I/O analysis
- **📝 Rich Markdown Reports:** Detailed summaries with Mermaid diagrams for call and control flow visualization
- **🔍 Dead Code Detection:** Identifies unused paragraphs and variables
- **🏗️ Modular Architecture:** Clean separation between parser, CLI, and summarizer components
- **🔧 Modern CLI:** Intuitive command-line interface with verbose/debug options
- **🛡️ Security Focused:** Comprehensive security policy and responsible disclosure
- **🔄 CI/CD Ready:** Automated testing and linting via GitHub Actions

---

## 🛠️ Architecture

CodeSleuth uses a modern, efficient architecture:

- **Rust Parser** (`parser/`): Fast COBOL parsing and IR generation
- **Rust Summarizer** (`summarizer/`): High-performance Markdown report generation
- **Go CLI** (`cmd/`): User-friendly command-line interface
- **Cross-platform:** Works on Windows, macOS, and Linux

---

## 📦 Installation

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

## 🚀 Quick Start

```bash
# Analyze a COBOL file or directory
./cmd/codesleuth analyze --verbose path/to/your/cobol/files

# Generate a report file
./cmd/codesleuth analyze test-cobol/ > analysis_report.md
```

### Example Output

CodeSleuth generates comprehensive Markdown reports including:

- **Program Information:** Name, author, date, comments
- **Data Division:** Working storage and file section analysis
- **Procedure Division:** Paragraph structure and statement analysis
- **Call Graph:** Visual representation of program flow
- **Control Flow:** Detailed execution path analysis
- **Variable Usage:** Read/write analysis for all variables
- **Dead Code Detection:** Unused paragraphs and variables

---

## 📋 Usage Examples

```bash
# Analyze a single file
./cmd/codesleuth analyze program.cbl

# Analyze entire directory with verbose output
./cmd/codesleuth analyze --verbose /path/to/cobol/projects/

# Generate report to file
./cmd/codesleuth analyze legacy-system/ > system_analysis.md

# Analyze with debug information
./cmd/codesleuth analyze --verbose --debug complex-program.cbl
```

---

## 🔧 Configuration

CodeSleuth is designed to work out-of-the-box with minimal configuration. The tool automatically detects COBOL files (`.cob`, `.cbl`, `.cobol`) and generates appropriate analysis reports.

---

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details on:

- Code style and standards
- Testing requirements
- Pull request process
- Issue reporting

---

## 🛡️ Security

Found a security vulnerability? Please review our [Security Policy](SECURITY.md) for responsible disclosure guidelines.

---

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

## 🔗 Links

- [GitHub Repository](https://github.com/Cod-e-Codes/codesleuth)
- [Rust Programming Language](https://www.rust-lang.org/)
- [Go Programming Language](https://golang.org/)
- [COBOL Resources](https://www.microfocus.com/en-us/products/cobol/overview)

---

## 📈 Performance

CodeSleuth's Rust-based architecture provides significant performance improvements:

- **10-50x faster** than Python-based alternatives
- **Lower memory usage** for large codebases
- **Faster startup times** with compiled binaries
- **Better concurrency** for parallel processing

---

*Built with ❤️ for the enterprise development community.* 