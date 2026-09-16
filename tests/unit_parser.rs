use codesleuth::parser;
use std::path::Path;

#[test]
fn test_parser() {
    let cobol = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       STOP RUN.
    "#;
    let ir = parser::parse_cobol_source(cobol, Path::new("testprog.cob"), false, false).unwrap();
    let ir_json = serde_json::to_string(&ir).unwrap();
    assert!(ir_json.contains("TESTPROG"));
    assert!(ir_json.contains("program_name"));
    assert_eq!(ir.program_name, "TESTPROG");
}
