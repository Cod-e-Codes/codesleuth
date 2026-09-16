use clap::{Parser as ClapParser, Subcommand, ValueHint};
use codesleuth::batch::{run_pool, Analyzed, FileResult};
use codesleuth::benchmark::BenchmarkSummary;
use codesleuth::discover::{default_workers, discover, report_path, worker_count};
use codesleuth::error::Error;
use codesleuth::parser::parse_cobol_file;
use codesleuth::report::{render, render_json};
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

#[derive(ClapParser)]
#[command(author, version, about = "Parse COBOL and write Markdown summaries")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Parse a COBOL file and write IR JSON to stdout
    Parse {
        /// Input COBOL file
        #[arg(value_name = "PATH", value_hint = ValueHint::FilePath)]
        input: PathBuf,
        /// Extra progress on stderr
        #[arg(long)]
        verbose: bool,
        /// Internal trace on stderr
        #[arg(long)]
        debug: bool,
    },
    /// Read IR JSON and write Markdown
    Summarize {
        /// Input IR JSON file (default: stdin)
        #[arg(short, long, value_name = "PATH", value_hint = ValueHint::FilePath)]
        input: Option<PathBuf>,
        /// Output Markdown file (default: stdout)
        #[arg(short, long, value_name = "PATH", value_hint = ValueHint::FilePath)]
        output: Option<PathBuf>,
        /// Extra progress on stderr
        #[arg(long)]
        verbose: bool,
        /// Internal trace on stderr
        #[arg(long)]
        debug: bool,
    },
    /// Analyze a COBOL file or directory (parse and summarize)
    ///
    /// If PATH is a file, --output is a Markdown file.
    /// If PATH is a directory, --output is a directory of flattened .md names.
    Analyze {
        /// Input file or directory
        #[arg(value_name = "PATH", value_hint = ValueHint::AnyPath)]
        input: PathBuf,
        /// Output path (file if input is a file; directory if input is a directory)
        #[arg(short, long, value_name = "PATH", value_hint = ValueHint::AnyPath)]
        output: Option<PathBuf>,
        /// Concurrent workers (default: available parallelism)
        #[arg(long)]
        workers: Option<usize>,
        /// Print wall-clock throughput after analysis
        #[arg(long)]
        benchmark: bool,
        /// Extra progress on stderr
        #[arg(long)]
        verbose: bool,
        /// Internal trace on stderr
        #[arg(long)]
        debug: bool,
    },
}

#[derive(Clone)]
struct AnalyzeJob {
    verbose: bool,
    debug: bool,
    output: Option<PathBuf>,
    input_is_dir: bool,
}

impl AnalyzeJob {
    fn run(&self, path: PathBuf) -> Result<Analyzed, Error> {
        let ir = parse_cobol_file(&path, self.verbose, self.debug)?;
        let md = render(&ir, self.verbose, self.debug)?;
        if let Some(out) = &self.output {
            let dest = if self.input_is_dir {
                report_path(out, &path)
            } else {
                out.clone()
            };
            if let Some(parent) = dest.parent() {
                if !parent.as_os_str().is_empty() {
                    std::fs::create_dir_all(parent).map_err(|source| Error::Io {
                        path: parent.to_path_buf(),
                        source,
                    })?;
                }
            }
            std::fs::write(&dest, &md).map_err(|source| Error::Io {
                path: dest.clone(),
                source,
            })?;
            Ok(Analyzed {
                markdown: md,
                report_path: Some(dest),
            })
        } else {
            Ok(Analyzed {
                markdown: md,
                report_path: None,
            })
        }
    }
}

fn print_results(results: &[FileResult]) -> bool {
    let mut failed = false;
    for r in results {
        match &r.outcome {
            Ok(analyzed) => {
                if let Some(dest) = &analyzed.report_path {
                    println!("Analyzed: {} -> {}", r.path.display(), dest.display());
                } else {
                    println!("Analyzed: {}\n{}", r.path.display(), analyzed.markdown);
                }
            }
            Err(e) => {
                failed = true;
                eprintln!("error analyzing {}: {e}", r.path.display());
            }
        }
    }
    failed
}

fn cmd_analyze(
    input: PathBuf,
    output: Option<PathBuf>,
    workers: Option<usize>,
    benchmark: bool,
    verbose: bool,
    debug: bool,
) -> ExitCode {
    let meta = match std::fs::metadata(&input) {
        Ok(m) => m,
        Err(source) => {
            eprintln!(
                "{}",
                Error::Io {
                    path: input,
                    source,
                }
            );
            return ExitCode::from(1);
        }
    };
    let input_is_dir = meta.is_dir();
    let files = match discover(&input) {
        Ok(f) => f,
        Err(source) => {
            eprintln!(
                "{}",
                Error::Io {
                    path: input,
                    source,
                }
            );
            return ExitCode::from(1);
        }
    };
    println!("Found {} COBOL files:", files.len());
    for f in &files {
        println!("{}", f.display());
    }
    if let Some(out) = &output {
        if input_is_dir {
            if let Err(source) = std::fs::create_dir_all(out) {
                eprintln!(
                    "{}",
                    Error::Io {
                        path: out.clone(),
                        source,
                    }
                );
                return ExitCode::from(1);
            }
        }
    }
    let requested = workers.unwrap_or_else(default_workers);
    let nworkers = worker_count(requested, files.len());
    let job = AnalyzeJob {
        verbose,
        debug,
        output,
        input_is_dir,
    };
    let nfiles = files.len();
    let run = || run_pool(files, nworkers, move |p| job.run(p));
    let failed = if benchmark {
        let (results, bm) = BenchmarkSummary::run(nfiles, run);
        let failed = print_results(&results);
        bm.print_table();
        bm.print_json();
        failed
    } else {
        let results = run();
        print_results(&results)
    };
    if failed {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    }
}

fn write_out(output: Option<&Path>, body: &str) -> Result<(), Error> {
    if let Some(path) = output {
        std::fs::write(path, body).map_err(|source| Error::Io {
            path: path.to_path_buf(),
            source,
        })
    } else {
        print!("{}", body);
        Ok(())
    }
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    match cli.command {
        Commands::Parse {
            input,
            verbose,
            debug,
        } => match parse_cobol_file(&input, verbose, debug) {
            Ok(ir) => match serde_json::to_string_pretty(&ir) {
                Ok(json) => {
                    println!("{}", json);
                    ExitCode::SUCCESS
                }
                Err(e) => {
                    eprintln!("{e}");
                    ExitCode::from(1)
                }
            },
            Err(e) => {
                eprintln!("{e}");
                ExitCode::from(1)
            }
        },
        Commands::Summarize {
            input,
            output,
            verbose,
            debug,
        } => {
            let ir_json = if let Some(input_path) = input {
                match std::fs::read_to_string(&input_path) {
                    Ok(s) => s,
                    Err(source) => {
                        eprintln!(
                            "{}",
                            Error::Io {
                                path: input_path,
                                source,
                            }
                        );
                        return ExitCode::from(1);
                    }
                }
            } else {
                let mut buf = String::new();
                if let Err(e) = io::stdin().read_to_string(&mut buf) {
                    eprintln!("{e}");
                    return ExitCode::from(1);
                }
                buf
            };
            match render_json(&ir_json, verbose, debug) {
                Ok(md) => match write_out(output.as_deref(), &md) {
                    Ok(()) => ExitCode::SUCCESS,
                    Err(e) => {
                        eprintln!("{e}");
                        ExitCode::from(1)
                    }
                },
                Err(e) => {
                    eprintln!("{e}");
                    ExitCode::from(1)
                }
            }
        }
        Commands::Analyze {
            input,
            output,
            workers,
            benchmark,
            verbose,
            debug,
        } => cmd_analyze(input, output, workers, benchmark, verbose, debug),
    }
}
