pub mod batch;
pub mod benchmark;
pub mod cobol;
pub mod discover;
pub mod error;
pub mod ir;
pub mod parser;
pub mod report;

pub use error::Error;
pub use ir::IR;
pub use parser::{parse_cobol_file, parse_cobol_source};
pub use report::{render, render_json};
