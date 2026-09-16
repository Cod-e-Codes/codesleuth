use std::fs;
use std::path::PathBuf;
use std::process::Command;

const HELLO: &str = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD!'.
           GOBACK.
"#;

fn scratch(label: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "codesleuth-cli-{}-{}-{}",
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
fn analyze_cli_finishes_batch_when_one_file_is_unreadable() {
    let dir = scratch("mixed");
    let out = dir.join("reports");
    fs::write(dir.join("ok.cbl"), HELLO).unwrap();
    fs::write(dir.join("truncated.cbl"), "IDENTIFICATION DIVISION.").unwrap();
    fs::write(dir.join("bad-utf8.cbl"), [0xffu8, 0xfe, 0x00]).unwrap();

    let exe = env!("CARGO_BIN_EXE_codesleuth");
    let output = Command::new(exe)
        .args([
            "analyze",
            dir.to_str().unwrap(),
            "--workers",
            "2",
            "--output",
            out.to_str().unwrap(),
        ])
        .output()
        .expect("spawn codesleuth analyze");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(
        output.status.code(),
        Some(1),
        "stdout:\n{stdout}\nstderr:\n{stderr}"
    );
    assert!(
        stderr.contains("error analyzing") && stderr.contains("bad-utf8.cbl"),
        "stderr:\n{stderr}"
    );
    assert!(
        stdout.contains("ok.cbl") || stdout.contains("ok.md"),
        "stdout:\n{stdout}"
    );
    let reports: Vec<_> = fs::read_dir(&out)
        .unwrap()
        .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
        .collect();
    assert!(
        reports.iter().any(|n| n.ends_with("ok.md")),
        "reports: {reports:?}"
    );
    assert!(
        reports.iter().any(|n| n.ends_with("truncated.md")),
        "reports: {reports:?}"
    );
    let _ = fs::remove_dir_all(dir);
}
