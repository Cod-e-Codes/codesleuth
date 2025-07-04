use std::fs::File;
use std::io::{Write, Read};
use std::env;
use codesleuth::parser;
use codesleuth::summarizer;

#[test]
fn test_integration() {
    let cobol = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEGTEST.
       PROCEDURE DIVISION.
       STOP RUN.
    "#;
    let tmp_dir = env::temp_dir();
    let tmp_path = tmp_dir.join("integtest.cob");
    let mut file = File::create(&tmp_path).unwrap();
    file.write_all(cobol.as_bytes()).unwrap();
    let ir_json = parser::parse_cobol_file(tmp_path.to_str().unwrap(), false, false).unwrap();
    let md = summarizer::summarize_ir(&ir_json, false, false).unwrap();
    assert!(md.contains("INTEGTEST"));
    assert!(md.contains("COBOL Program Summary"));
    std::fs::remove_file(tmp_path).unwrap();
} 