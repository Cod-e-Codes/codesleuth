# Contributing to CodeSleuth

Thank you for your interest in contributing to CodeSleuth! We welcome bug reports, feature requests, and pull requests.

## How to Contribute

- **Report Issues:**
  - Use [GitHub Issues](https://github.com/Cod-e-Codes/codesleuth/issues) to report bugs or request features.
  - Please provide as much detail as possible, including steps to reproduce bugs.

- **Submit Pull Requests:**
  - Fork the repository and create your branch from `main`.
  - Write clear, concise commit messages (see below).
  - Ensure your code passes all tests and lints.
  - Open a pull request with a clear description of your changes.

## Coding Style

- Follow idiomatic style for each language:
  - **Rust:** [Rust Style Guide](https://doc.rust-lang.org/1.0.0/style/)
  - **Go:** [Effective Go](https://golang.org/doc/effective_go.html)
  - **Python:** [PEP 8](https://www.python.org/dev/peps/pep-0008/)
- Use descriptive variable and function names.
- Write docstrings/comments for complex logic.

## Commit Messages

- Use the present tense ("Add feature" not "Added feature").
- Start with a short summary, followed by a blank line and more detail if needed.
- Reference issues or PRs when relevant (e.g., `Fixes #12`).

## Contact

For questions, open an issue or contact Cody Marsengill at cod.e.codes.dev@gmail.com.

## Setup Instructions

1. **Install Rust** (1.70+): https://rustup.rs/
2. **Install Go** (1.21+): https://golang.org/dl/
3. **Clone the repo:**
   ```sh
   git clone https://github.com/Cod-e-Codes/codesleuth.git
   cd codesleuth
   ```
4. **Install Go dependencies:**
   ```sh
   cd cmd && go mod tidy && cd ..
   ```
5. **Build Rust backend:**
   ```sh
   cargo build --release --manifest-path codesleuth/Cargo.toml
   ```
6. **Build Go CLI:**
   ```sh
   cd cmd && go build -o codesleuth.exe && cd ..
   ```

## Local Development & Test Workflow

- **Unit tests (Go):**
  ```sh
  cd cmd && go test ./...
  ```
- **Unit/integration tests (Rust):**
  ```sh
  cd codesleuth && cargo test
  ```
- **Run CLI locally:**
  ```sh
  ./cmd/codesleuth.exe analyze --workers=4 --benchmark test-cobol
  ```
- **Generate dummy COBOL files for benchmarking:**
  ```sh
  go run scripts/generate_dummy_cobol.go --count=1000 --outdir=bench-cobol
  ```

## Code Standards

- **Rust:**
  - Use `cargo fmt` and `cargo clippy` before committing.
  - Follow idiomatic Rust style and error handling.
- **Go:**
  - Use `gofmt` and `go vet` before committing.
  - Use clear, descriptive names and idiomatic Go patterns.
- **General:**
  - Write tests for new features and bugfixes.
  - Document complex logic with comments/docstrings.
  - Keep PRs focused and well-described.

---

Happy coding! 