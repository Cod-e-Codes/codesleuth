use codesleuth::parser;
use codesleuth::report;
use std::path::Path;

#[test]
fn test_integration() {
    let cobol = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEGTEST.
       PROCEDURE DIVISION.
       STOP RUN.
    "#;
    let ir = parser::parse_cobol_source(cobol, Path::new("integtest.cob"), false, false).unwrap();
    let md = report::render(&ir, false, false).unwrap();
    assert!(md.contains("INTEGTEST"));
    assert!(md.contains("COBOL Program Summary"));
}
