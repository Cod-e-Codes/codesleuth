use codesleuth::parser;
use codesleuth::summarizer;
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

fn write_temp(name: &str, source: &str) -> PathBuf {
    let path = env::temp_dir().join(format!("codesleuth_{}_{}.cob", name, std::process::id()));
    let mut file = File::create(&path).unwrap();
    file.write_all(source.as_bytes()).unwrap();
    path
}

fn parse_md(name: &str, source: &str) -> (String, String) {
    let path = write_temp(name, source);
    let ir = parser::parse_cobol_file(path.to_str().unwrap(), false, false).unwrap();
    let md = summarizer::summarize_ir(&ir, false, false).unwrap();
    std::fs::remove_file(&path).unwrap();
    (ir, md)
}

#[test]
fn level_77_comments_and_unnamed_procedure() {
    let cobol = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROL00.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ****** Variables for the report
       77  WHO        PIC X(15).
       77  WHERE      PIC X(20).
       77  RATE       PIC 9(3).
       PROCEDURE DIVISION.
           MOVE  "Captain COBOL" TO WHO.
           DISPLAY "Name: " WHO.
           GOBACK.
"#;
    let (ir, md) = parse_md("payrol00_shape", cobol);
    assert!(ir.contains("\"name\": \"WHO\""));
    assert!(ir.contains("\"picture\": \"X(15)\""));
    assert!(ir.contains("\"name\": \"WHERE\""));
    assert!(ir.contains("MOVE"));
    assert!(ir.contains("DISPLAY"));
    assert!(md.contains("WHO"));
    assert!(md.contains("MOVE"));
    assert!(md.contains("DISPLAY"));
    assert!(!md.contains("No Procedure Division content found"));
}

#[test]
fn hello_display_and_goback() {
    let cobol = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD!'.
           GOBACK.
"#;
    let (_ir, md) = parse_md("hello_shape", cobol);
    assert!(md.contains("DISPLAY"));
    assert!(md.contains("GOBACK"));
    assert!(!md.contains("No Procedure Division content found"));
}

#[test]
fn split_program_id() {
    let cobol = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           ADDAMT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  KEYED-INPUT.
           05  CUST-NO-IN                  PIC X(15).
       PROCEDURE DIVISION.
       100-MAIN.
           GOBACK.
"#;
    let (ir, md) = parse_md("addamt_shape", cobol);
    assert!(ir.contains("\"program_name\": \"ADDAMT\""));
    assert!(!ir.contains("\"program_name\": \"UNKNOWN\""));
    assert!(md.contains("ADDAMT"));
    assert!(md.contains("KEYED-INPUT"));
}

#[test]
fn pic_comp3_value_and_continuation() {
    let cobol = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMP3T.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 REPORT-TOTALS.
        05 NUM-TRAN-RECS         PIC S9(9) COMP-3 VALUE +0.
       01 ERR-MSG-BAD-TRAN.
        05 FILLER                PIC X(31)
           VALUE 'Error Processing Transaction. '.
        05 ERR-MSG-DATA1         PIC X(35) VALUE SPACES.
       PROCEDURE DIVISION.
           GOBACK.
"#;
    let (ir, md) = parse_md("comp3_shape", cobol);
    assert!(ir.contains("\"name\": \"NUM-TRAN-RECS\""));
    assert!(ir.contains("\"picture\": \"S9(9)\""));
    assert!(ir.contains("\"comp3\": true"));
    assert!(ir.contains("\"value\": \"+0\""));
    assert!(ir.contains("ERR-MSG-DATA1"));
    assert!(md.contains("NUM-TRAN-RECS"));
    assert!(md.contains("COMP-3"));
}

#[test]
fn file_section_fd_and_picture() {
    let cobol = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL0001
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT ACCT-REC   ASSIGN TO ACCTREC.
       DATA DIVISION.
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC.
           05  ACCT-NO-O      PIC X(8).
           05  ACCT-LIMIT-O   PIC $$,$$$,$$9.99.
       FD  ACCT-REC RECORDING MODE F.
       01  ACCT-FIELDS.
           05  ACCT-LIMIT         PIC S9(7)V99 COMP-3.
       WORKING-STORAGE SECTION.
       01 FLAGS.
         05 LASTREC           PIC X VALUE SPACE.
       PROCEDURE DIVISION.
       OPEN-FILES.
           OPEN INPUT  ACCT-REC.
           GOBACK.
"#;
    let (ir, md) = parse_md("cbl0001_shape", cobol);
    assert!(ir.contains("\"name\": \"PRINT-REC\""));
    assert!(ir.contains("$$,$$$,$$9.99"));
    assert!(ir.contains("ACCT-FIELDS"));
    assert!(ir.contains("PRINT-LINE"));
    assert!(ir.contains("ACCT-REC"));
    assert!(md.contains("PRINT-REC"));
    assert!(!md.contains("No File Section entries found"));
    assert!(md.contains("PRINT-LINE"));
}

#[test]
fn comments_do_not_wipe_following_items() {
    let cobol = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CMTTST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * comment before items
       01 FLAGS.
         05 LASTREC           PIC X VALUE SPACE.
      * comment between groups
       77  WHO        PIC X(15).
       PROCEDURE DIVISION.
           STOP RUN.
"#;
    let (ir, md) = parse_md("comments_ws", cobol);
    assert!(ir.contains("\"name\": \"FLAGS\""));
    assert!(ir.contains("\"name\": \"WHO\""));
    assert!(md.contains("FLAGS"));
    assert!(md.contains("WHO"));
}

#[test]
fn copy_recorded_and_multiline_select() {
    let cobol = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAM1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO CUSTFILE
               ACCESS IS SEQUENTIAL
               FILE STATUS  IS  WS-CUSTFILE-STATUS.
           SELECT REPORT-FILE      ASSIGN TO CUSTRPT
                   FILE STATUS  IS  WS-REPORT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE
           RECORDING MODE IS V
           RECORD IS VARYING FROM 20 TO 596 CHARACTERS.
       COPY CUSTCOPY REPLACING ==:TAG:== BY ==CUST==.
       FD  REPORT-FILE
           RECORDING MODE IS F.
       01 REPORT-RECORD          PIC X(132).
       WORKING-STORAGE SECTION.
       COPY CUSTCOPY REPLACING ==:TAG:== BY ==WS-CUST==.
       PROCEDURE DIVISION.
       MAIN.
           CALL SAM2 USING WS-CUST.
           GOBACK.
"#;
    let (ir, md) = parse_md("copy_select", cobol);
    assert!(ir.contains("\"name\": \"CUSTCOPY\""));
    assert!(ir.contains("\"type\": \"copybook\""));
    assert!(ir.contains("CUSTOMER-FILE"));
    assert!(ir.contains("CUSTFILE"));
    assert!(ir.contains("REPORT-FILE"));
    assert!(md.contains("CUSTCOPY"));
    assert!(md.contains("CUSTOMER-FILE"));
    assert!(md.contains("REPORT-RECORD"));
}

#[test]
fn procedure_division_using_header() {
    let cobol = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAM2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIELDS.
        05 WS-UPDATE-NUM      PIC S9(9)V99 COMP-3 VALUE +0.
       PROCEDURE DIVISION USING CUST-REC,
                                TRANSACTION-RECORD,
                                TRAN-OK,
                                TRAN-MSG.
       000-MAIN.
           MOVE 'Y' TO TRAN-OK.
           GOBACK.
"#;
    let (ir, md) = parse_md("proc_using", cobol);
    assert!(ir.contains("\"program_name\": \"SAM2\""));
    assert!(ir.contains("000-MAIN"));
    assert!(ir.contains("MOVE"));
    assert!(md.contains("000-MAIN"));
    assert!(!md.contains("No Procedure Division content found"));
}
