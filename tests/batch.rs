use codesleuth::batch::{run_pool, Analyzed};
use codesleuth::discover::{report_path, worker_count};
use codesleuth::error::Error;
use codesleuth::parser::{parse_cobol_file, parse_cobol_source};
use codesleuth::report::render;
use std::fs;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

const HELLO: &str = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD!'.
           GOBACK.
"#;

fn scratch(label: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "codesleuth-{}-{}-{}",
        label,
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&dir).unwrap();
    dir
}

#[test]
fn report_path_flattens_separators() {
    let out = Path::new("reports");
    let p = report_path(out, Path::new("test-cobol/foo/bar.cbl"));
    assert_eq!(p, PathBuf::from("reports/test-cobol_foo_bar.md"));
    let p = report_path(out, Path::new(r"test-cobol\foo\bar.cobol"));
    assert_eq!(p, PathBuf::from("reports/test-cobol_foo_bar.md"));
}

#[test]
fn worker_count_caps_at_file_count() {
    assert_eq!(worker_count(8, 3), 3);
    assert_eq!(worker_count(0, 3), 1);
    assert_eq!(worker_count(2, 10), 2);
}

#[test]
fn run_pool_isolates_panic_and_covers_every_path() {
    let files = vec![
        PathBuf::from("ok.cob"),
        PathBuf::from("boom.cob"),
        PathBuf::from("also-ok.cob"),
    ];
    let calls = Arc::new(AtomicUsize::new(0));
    let calls_w = Arc::clone(&calls);
    let results = run_pool(files.clone(), 2, move |p| {
        calls_w.fetch_add(1, Ordering::SeqCst);
        if p.file_name().unwrap() == "boom.cob" {
            panic!("parser exploded");
        }
        Ok(Analyzed {
            markdown: format!("md-{}", p.display()),
            report_path: None,
        })
    });
    assert_eq!(results.len(), 3);
    assert_eq!(calls.load(Ordering::SeqCst), 3);
    let boom = results
        .iter()
        .find(|r| r.path.ends_with("boom.cob"))
        .unwrap();
    match &boom.outcome {
        Err(Error::Panic(msg)) => assert!(msg.contains("parser exploded")),
        other => panic!("expected Panic, got {other:?}"),
    }
    assert!(results
        .iter()
        .find(|r| r.path.ends_with("ok.cob"))
        .unwrap()
        .outcome
        .is_ok());
    assert!(results
        .iter()
        .find(|r| r.path.ends_with("also-ok.cob"))
        .unwrap()
        .outcome
        .is_ok());
}

#[test]
fn parse_and_render_do_not_unwind_on_truncated_or_null() {
    let deep_ws = {
        let mut s = String::from(
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. DEEP.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n",
        );
        for level in (1..=49).rev() {
            s.push_str(&format!("       {:02} N{} PIC X.\n", level, level));
        }
        s.push_str("       PROCEDURE DIVISION.\n           STOP RUN.\n");
        s
    };
    let cases: Vec<(&str, &str)> = vec![
        ("empty", ""),
        ("null-byte", "\0"),
        ("truncated-id", "IDENTIFICATION DIVISION."),
        (
            "end-perform-only",
            "       PROCEDURE DIVISION.\n       END-PERFORM.\n",
        ),
        ("deep-levels", deep_ws.as_str()),
        ("hello", HELLO),
    ];
    for (label, src) in cases {
        let parsed = catch_unwind(AssertUnwindSafe(|| {
            parse_cobol_source(src, Path::new("garbage.cob"), false, false)
        }));
        let ir = parsed
            .unwrap_or_else(|_| panic!("parse_cobol_source unwound on {label}"))
            .unwrap_or_else(|e| panic!("parse_cobol_source returned Err on {label}: {e}"));
        let rendered = catch_unwind(AssertUnwindSafe(|| render(&ir, false, false)));
        rendered
            .unwrap_or_else(|_| panic!("render unwound on {label}"))
            .unwrap_or_else(|e| panic!("render returned Err on {label}: {e}"));
    }
}

#[test]
fn run_pool_isolates_panic_during_parse_and_render() {
    let dir = scratch("parse-render-panic");
    let ok1 = dir.join("ok1.cbl");
    let boom = dir.join("boom.cbl");
    let ok2 = dir.join("ok2.cbl");
    fs::write(&ok1, HELLO).unwrap();
    fs::write(&boom, HELLO).unwrap();
    fs::write(&ok2, HELLO).unwrap();
    let files = vec![ok1.clone(), boom.clone(), ok2.clone()];
    let results = run_pool(files, 2, move |p| {
        if p.file_name().unwrap() == "boom.cbl" {
            panic!("parser exploded");
        }
        let ir = parse_cobol_file(&p, false, false)?;
        let markdown = render(&ir, false, false)?;
        Ok(Analyzed {
            markdown,
            report_path: None,
        })
    });
    assert_eq!(results.len(), 3);
    match &results
        .iter()
        .find(|r| r.path.ends_with("boom.cbl"))
        .unwrap()
        .outcome
    {
        Err(Error::Panic(msg)) => assert!(msg.contains("parser exploded")),
        other => panic!("expected Panic, got {other:?}"),
    }
    for name in ["ok1.cbl", "ok2.cbl"] {
        let outcome = &results
            .iter()
            .find(|r| r.path.ends_with(name))
            .unwrap()
            .outcome;
        let analyzed = outcome.as_ref().unwrap_or_else(|e| panic!("{name}: {e}"));
        assert!(analyzed.markdown.contains("HELLO"));
    }
    let _ = fs::remove_dir_all(dir);
}

#[test]
fn unreadable_utf8_is_io_error_not_panic() {
    let dir = scratch("utf8");
    let path = dir.join("bad.cbl");
    fs::write(&path, [0xffu8, 0xfe, 0x00]).unwrap();
    let parsed = catch_unwind(AssertUnwindSafe(|| parse_cobol_file(&path, false, false)));
    match parsed {
        Ok(Err(Error::Io { .. })) => {}
        Ok(Ok(_)) => panic!("expected Io error for invalid UTF-8"),
        Ok(Err(other)) => panic!("expected Io, got {other:?}"),
        Err(_) => panic!("parse_cobol_file unwound on invalid UTF-8"),
    }
    let _ = fs::remove_dir_all(dir);
}
