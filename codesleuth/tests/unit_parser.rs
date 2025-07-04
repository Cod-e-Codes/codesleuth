use std::fs::File;
use std::io::Write;
use std::env;
use codesleuth::parser;

#[test]
fn test_parser() {
    let cobol = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       STOP RUN.
    "#;
    let tmp_dir = env::temp_dir();
    let tmp_path = tmp_dir.join("testprog.cob");
    let mut file = File::create(&tmp_path).unwrap();
    file.write_all(cobol.as_bytes()).unwrap();
    let ir_json = parser::parse_cobol_file(tmp_path.to_str().unwrap(), false, false).unwrap();
    assert!(ir_json.contains("TESTPROG"));
    assert!(ir_json.contains("program_name"));
    std::fs::remove_file(tmp_path).unwrap();
} 