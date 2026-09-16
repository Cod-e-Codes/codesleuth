use std::path::{Path, PathBuf};

const COBOL_EXTS: &[&str] = &[".cob", ".cbl", ".cobol"];

pub fn is_cobol_path(path: &Path) -> bool {
    path.extension()
        .and_then(|e| e.to_str())
        .map(|e| {
            COBOL_EXTS
                .iter()
                .any(|ext| e.eq_ignore_ascii_case(&ext[1..]))
        })
        .unwrap_or(false)
}

pub fn discover(root: &Path) -> Result<Vec<PathBuf>, std::io::Error> {
    let meta = std::fs::metadata(root)?;
    if meta.is_file() {
        if is_cobol_path(root) {
            return Ok(vec![root.to_path_buf()]);
        }
        return Ok(Vec::new());
    }
    let mut files = Vec::new();
    walk(root, &mut files)?;
    files.sort();
    Ok(files)
}

fn walk(dir: &Path, files: &mut Vec<PathBuf>) -> Result<(), std::io::Error> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        let ft = entry.file_type()?;
        if ft.is_dir() {
            walk(&path, files)?;
        } else if ft.is_file() && is_cobol_path(&path) {
            files.push(path);
        }
    }
    Ok(())
}

/// Flatten a COBOL path to a Markdown report name: strip `:`, replace `/` and `\` with `_`,
/// drop the source extension, append `.md`.
pub fn report_path(output_dir: &Path, cobol_path: &Path) -> PathBuf {
    let rel = cobol_path.to_string_lossy().replace('\\', "/");
    let stripped = rel.replace(':', "");
    let flattened = stripped.replace('/', "_");
    let without_ext = match flattened.rfind('.') {
        Some(i) if !flattened[i + 1..].contains('_') => flattened[..i].to_string(),
        _ => flattened,
    };
    output_dir.join(format!("{}.md", without_ext))
}

pub fn default_workers() -> usize {
    std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1)
}

pub fn worker_count(requested: usize, nfiles: usize) -> usize {
    let mut n = requested;
    if n < 1 {
        n = 1;
    }
    if nfiles > 0 && n > nfiles {
        n = nfiles;
    }
    n
}
