use std::path::PathBuf;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("failed to read {path}: {source}")]
    Io {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
    #[error("{0}")]
    Report(String),
    #[error("panic while analyzing: {0}")]
    Panic(String),
    #[error("{0}")]
    Json(#[from] serde_json::Error),
}
