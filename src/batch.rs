use crate::error::Error;
use std::collections::VecDeque;
use std::panic::{catch_unwind, RefUnwindSafe, UnwindSafe};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::thread;

#[derive(Debug)]
pub struct FileResult {
    pub path: PathBuf,
    pub outcome: Result<Analyzed, Error>,
}

#[derive(Debug)]
pub struct Analyzed {
    pub markdown: String,
    pub report_path: Option<PathBuf>,
}

fn panic_to_string(payload: Box<dyn std::any::Any + Send>) -> String {
    match payload.downcast::<String>() {
        Ok(s) => *s,
        Err(payload) => match payload.downcast::<&'static str>() {
            Ok(s) => (*s).to_string(),
            Err(_) => "Box<dyn Any>".to_string(),
        },
    }
}

fn lock_mutex<T>(m: &Mutex<T>) -> std::sync::MutexGuard<'_, T> {
    match m.lock() {
        Ok(g) => g,
        Err(p) => p.into_inner(),
    }
}

pub fn run_pool<F>(files: Vec<PathBuf>, workers: usize, analyze: F) -> Vec<FileResult>
where
    F: Fn(PathBuf) -> Result<Analyzed, Error> + Send + Sync + UnwindSafe + RefUnwindSafe + 'static,
{
    let n = files.len();
    if n == 0 {
        return Vec::new();
    }
    let nworkers = workers.max(1).min(n);
    let files = Arc::new(files);
    let queue = Arc::new(Mutex::new((0..n).collect::<VecDeque<usize>>()));
    let slots: Arc<Mutex<Vec<Option<FileResult>>>> =
        Arc::new(Mutex::new((0..n).map(|_| None).collect()));
    let analyze = Arc::new(analyze);

    let mut handles = Vec::with_capacity(nworkers);
    for i in 0..nworkers {
        let queue = Arc::clone(&queue);
        let slots = Arc::clone(&slots);
        let files = Arc::clone(&files);
        let analyze = Arc::clone(&analyze);
        if let Ok(h) = thread::Builder::new()
            .name(format!("codesleuth-worker-{i}"))
            .spawn(move || loop {
                let idx = {
                    let mut q = lock_mutex(&queue);
                    match q.pop_front() {
                        Some(i) => i,
                        None => break,
                    }
                };
                let path = files[idx].clone();
                let path_for_result = path.clone();
                let analyze_fn = Arc::clone(&analyze);
                let outcome = match catch_unwind(move || analyze_fn(path)) {
                    Ok(r) => r,
                    Err(payload) => Err(Error::Panic(panic_to_string(payload))),
                };
                let mut slot_guard = lock_mutex(&slots);
                slot_guard[idx] = Some(FileResult {
                    path: path_for_result,
                    outcome,
                });
            })
        {
            handles.push(h);
        }
    }

    let mut join_panic: Option<String> = None;
    for handle in handles {
        if let Err(payload) = handle.join() {
            join_panic = Some(panic_to_string(payload));
        }
    }

    let fallback = join_panic.unwrap_or_else(|| "worker exited before finishing file".to_string());
    let mut slot_guard = lock_mutex(&slots);
    (0..n)
        .map(|i| {
            slot_guard[i].take().unwrap_or_else(|| FileResult {
                path: files[i].clone(),
                outcome: Err(Error::Panic(fallback.clone())),
            })
        })
        .collect()
}
