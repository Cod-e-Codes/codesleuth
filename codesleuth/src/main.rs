mod parser;
mod summarizer;

use clap::{Parser as ClapParser, Subcommand};
use std::io::{self, Read};

#[derive(ClapParser)]
#[command(author, version, about = "CodeSleuth: Unified COBOL parser and summarizer")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Parse a COBOL file and output IR JSON
    Parse {
        /// Input COBOL file
        input: String,
        /// Enable verbose output
        #[arg(long)]
        verbose: bool,
        /// Enable debug output
        #[arg(long)]
        debug: bool,
    },
    /// Summarize IR JSON and output Markdown
    Summarize {
        /// Input IR JSON file (default: stdin)
        #[arg(short, long)]
        input: Option<String>,
        /// Output Markdown file (default: stdout)
        #[arg(short, long)]
        output: Option<String>,
        /// Enable verbose output
        #[arg(long)]
        verbose: bool,
        /// Enable debug output
        #[arg(long)]
        debug: bool,
    },
    /// Analyze a COBOL file (parse + summarize in-process)
    Analyze {
        /// Input COBOL file
        input: String,
        /// Output Markdown file (default: stdout)
        #[arg(short, long)]
        output: Option<String>,
        /// Enable verbose output
        #[arg(long)]
        verbose: bool,
        /// Enable debug output
        #[arg(long)]
        debug: bool,
    },
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Commands::Parse { input, verbose, debug } => {
            let ir = parser::parse_cobol_file(&input, verbose, debug).unwrap_or_else(|e| {
                eprintln!("[ERROR] {}", e);
                std::process::exit(1);
            });
            println!("{}", ir);
        }
        Commands::Summarize { input, output, verbose, debug } => {
            let mut ir_json = String::new();
            if let Some(input_path) = input {
                ir_json = std::fs::read_to_string(input_path).unwrap_or_else(|e| {
                    eprintln!("[ERROR] {}", e);
                    std::process::exit(1);
                });
            } else {
                io::stdin().read_to_string(&mut ir_json).unwrap();
            }
            let md = summarizer::summarize_ir(&ir_json, verbose, debug).unwrap_or_else(|e| {
                eprintln!("[ERROR] {}", e);
                std::process::exit(1);
            });
            if let Some(output_path) = output {
                std::fs::write(output_path, md).unwrap();
            } else {
                print!("{}", md);
            }
        }
        Commands::Analyze { input, output, verbose, debug } => {
            let ir = parser::parse_cobol_file(&input, verbose, debug).unwrap_or_else(|e| {
                eprintln!("[ERROR] {}", e);
                std::process::exit(1);
            });
            let md = summarizer::summarize_ir(&ir, verbose, debug).unwrap_or_else(|e| {
                eprintln!("[ERROR] {}", e);
                std::process::exit(1);
            });
            if let Some(output_path) = output {
                std::fs::write(output_path, md).unwrap();
            } else {
                print!("{}", md);
            }
        }
    }
}
