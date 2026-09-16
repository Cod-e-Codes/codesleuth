use codesleuth::parser;
use codesleuth::report;
use codesleuth::IR;
use std::fs;
use std::path::{Path, PathBuf};

fn parse_md(name: &str, source: &str) -> (String, String) {
    let path = Path::new(name);
    let ir = parser::parse_cobol_source(source, path, false, false).unwrap();
    let md = report::render(&ir, false, false).unwrap();
    let ir_json = serde_json::to_string(&ir).unwrap();
    (ir_json, md)
}

fn parse_ir(name: &str, source: &str) -> IR {
    parser::parse_cobol_source(source, Path::new(name), false, false).unwrap()
}

fn scratch(label: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "codesleuth-shape-{}-{}-{}",
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

fn data_names(items: &[codesleuth::ir::DataItem]) -> Vec<String> {
    let mut names = Vec::new();
    fn walk(items: &[codesleuth::ir::DataItem], names: &mut Vec<String>) {
        for item in items {
            names.push(item.name.clone());
            walk(&item.children, names);
        }
    }
    walk(items, &mut names);
    names
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
    let (ir, md) = parse_md("payrol00_shape.cob", cobol);
    assert!(ir.contains("\"name\":\"WHO\"") || ir.contains("\"name\": \"WHO\""));
    assert!(ir.contains("\"picture\":\"X(15)\"") || ir.contains("\"picture\": \"X(15)\""));
    assert!(ir.contains("\"name\":\"WHERE\"") || ir.contains("\"name\": \"WHERE\""));
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
    let (_ir, md) = parse_md("hello_shape.cob", cobol);
    assert!(md.contains("DISPLAY"));
    assert!(md.contains("GOBACK"));
    assert!(!md.contains("No Procedure Division content found"));
    assert!(!md.contains("## Working-Storage Variables"));
    assert!(!md.contains("## Call Graph"));
    assert!(!md.contains("No call graph data"));
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
    let (ir, md) = parse_md("addamt_shape.cob", cobol);
    assert!(
        ir.contains("\"program_name\":\"ADDAMT\"") || ir.contains("\"program_name\": \"ADDAMT\"")
    );
    assert!(
        !ir.contains("\"program_name\":\"UNKNOWN\"")
            && !ir.contains("\"program_name\": \"UNKNOWN\"")
    );
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
    let (ir, md) = parse_md("comp3_shape.cob", cobol);
    assert!(ir.contains("NUM-TRAN-RECS"));
    assert!(ir.contains("S9(9)"));
    assert!(ir.contains("\"comp3\":true") || ir.contains("\"comp3\": true"));
    assert!(ir.contains("+0"));
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
    let (ir, md) = parse_md("cbl0001_shape.cob", cobol);
    assert!(ir.contains("PRINT-REC"));
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
    let (ir, md) = parse_md("comments_ws.cob", cobol);
    assert!(ir.contains("FLAGS"));
    assert!(ir.contains("WHO"));
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
    let (ir, md) = parse_md("copy_select.cob", cobol);
    assert!(ir.contains("CUSTCOPY"));
    assert!(ir.contains("copybook"));
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
    let (ir, md) = parse_md("proc_using.cob", cobol);
    assert!(ir.contains("\"program_name\":\"SAM2\"") || ir.contains("\"program_name\": \"SAM2\""));
    assert!(ir.contains("000-MAIN"));
    assert!(ir.contains("MOVE"));
    assert!(md.contains("000-MAIN"));
    assert!(!md.contains("No Procedure Division content found"));
}

#[test]
fn copy_replacing_dummy_tag_inserts_library_text() {
    let dir = scratch("copy");
    fs::write(
        dir.join("CUSTCOPY.cpy"),
        "       01 :TAG:-REC.\n        05 :TAG:-ID PIC X(5).\n",
    )
    .unwrap();
    let src = dir.join("sam.cob");
    fs::write(
        &src,
        r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAM1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CUSTCOPY REPLACING ==:TAG:== BY ==CUST==.
       PROCEDURE DIVISION.
           GOBACK.
"#,
    )
    .unwrap();
    let ir = parser::parse_cobol_file(&src, false, false).unwrap();
    let names = data_names(&ir.data_division.working_storage);
    assert!(names.iter().any(|n| n == "CUST-REC"), "{names:?}");
    assert!(names.iter().any(|n| n == "CUST-ID"), "{names:?}");
    assert!(!names.iter().any(|n| n.contains(":TAG:")), "{names:?}");
    let _ = fs::remove_dir_all(dir);
}

#[test]
fn copy_replacing_is_text_word_not_substring() {
    let dir = scratch("copy-word");
    fs::write(
        dir.join("WORDS.cpy"),
        "       01 FOO PIC X.\n       01 FOOBAR PIC X.\n       01 FOO-ID PIC X.\n",
    )
    .unwrap();
    let src = dir.join("word.cob");
    fs::write(
        &src,
        r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WORDT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY WORDS REPLACING FOO BY BAR.
       PROCEDURE DIVISION.
           GOBACK.
"#,
    )
    .unwrap();
    let ir = parser::parse_cobol_file(&src, false, false).unwrap();
    let names = data_names(&ir.data_division.working_storage);
    assert!(names.iter().any(|n| n == "BAR"), "{names:?}");
    assert!(
        names.iter().any(|n| n == "FOOBAR"),
        "FOO must not match inside FOOBAR: {names:?}"
    );
    assert!(
        !names.iter().any(|n| n == "BARBAR"),
        "substring replace would yield BARBAR: {names:?}"
    );
    assert!(
        names.iter().any(|n| n == "FOO-ID"),
        "FOO must not match inside FOO-ID: {names:?}"
    );
    let _ = fs::remove_dir_all(dir);
}

#[test]
fn copy_replacing_applies_every_operand_pair() {
    let dir = scratch("copy-multi");
    fs::write(
        dir.join("MULTI.cpy"),
        "       01 :TAG:-REC PIC X.\n       01 OLD-ITEM PIC X.\n",
    )
    .unwrap();
    let src = dir.join("multi.cob");
    fs::write(
        &src,
        r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTI.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY MULTI REPLACING ==:TAG:== BY ==CUST== OLD-ITEM BY NEW-ITEM.
       PROCEDURE DIVISION.
           GOBACK.
"#,
    )
    .unwrap();
    let ir = parser::parse_cobol_file(&src, false, false).unwrap();
    let names = data_names(&ir.data_division.working_storage);
    assert!(names.iter().any(|n| n == "CUST-REC"), "{names:?}");
    assert!(names.iter().any(|n| n == "NEW-ITEM"), "{names:?}");
    assert!(!names.iter().any(|n| n == "OLD-ITEM"), "{names:?}");
    let _ = fs::remove_dir_all(dir);
}

#[test]
fn nested_copy_expands_inner_member() {
    let dir = scratch("copy-nested");
    fs::write(dir.join("INNER.cpy"), "       01 NESTED-ITEM PIC X.\n").unwrap();
    fs::write(dir.join("OUTER.cpy"), "       COPY INNER.\n").unwrap();
    let src = dir.join("nestcopy.cob");
    fs::write(
        &src,
        r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NESTCPY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY OUTER.
       PROCEDURE DIVISION.
           GOBACK.
"#,
    )
    .unwrap();
    let ir = parser::parse_cobol_file(&src, false, false).unwrap();
    let names = data_names(&ir.data_division.working_storage);
    assert!(names.iter().any(|n| n == "NESTED-ITEM"), "{names:?}");
    assert!(!names.iter().any(|n| n == "OUTER"), "{names:?}");
    let _ = fs::remove_dir_all(dir);
}

#[test]
fn linkage_section_parsed_not_in_working_storage() {
    let ir = parse_ir(
        "link.cob",
        r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAM2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIELDS PIC X.
       LINKAGE SECTION.
       01 CUST-REC PIC X(20).
       01 TRAN-OK PIC X.
       PROCEDURE DIVISION USING CUST-REC TRAN-OK.
           GOBACK.
"#,
    );
    let ws = data_names(&ir.data_division.working_storage);
    let ls = data_names(&ir.data_division.linkage);
    assert!(ws.iter().any(|n| n == "WS-FIELDS"), "{ws:?}");
    assert!(!ws.iter().any(|n| n == "CUST-REC"), "{ws:?}");
    assert!(ls.iter().any(|n| n == "CUST-REC"), "{ls:?}");
    assert!(ls.iter().any(|n| n == "TRAN-OK"), "{ls:?}");
}

#[test]
fn quoted_literal_spaces_are_one_operand() {
    let ir = parse_ir(
        "hello_shape.cob",
        r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY "HELLO WORLD".
           GOBACK.
"#,
    );
    let stmts: Vec<_> = ir
        .procedure_division
        .sections
        .iter()
        .flat_map(|s| s.paragraphs.iter())
        .flat_map(|p| p.statements.iter())
        .filter(|s| s.r#type == "DISPLAY")
        .collect();
    assert_eq!(stmts.len(), 1);
    assert_eq!(stmts[0].operands, vec!["\"HELLO WORLD\"".to_string()]);
}

#[test]
fn missing_author_and_date_written_are_unknown() {
    let ir = parse_ir(
        "nodates.cob",
        r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NODATES.
       PROCEDURE DIVISION.
           GOBACK.
"#,
    );
    assert_eq!(ir.identification_division.author, "UNKNOWN");
    assert_eq!(ir.identification_division.date_written, "UNKNOWN");
}

#[test]
fn nested_program_is_separate_ir() {
    let ir = parse_ir(
        "nested.cob",
        r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OUTER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 OUTER-WS PIC X.
       PROCEDURE DIVISION.
       OUTER-PARA.
           GOBACK.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INNER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INNER-WS PIC X.
       PROCEDURE DIVISION.
       INNER-PARA.
           GOBACK.
       END PROGRAM INNER.
       END PROGRAM OUTER.
"#,
    );
    assert_eq!(ir.program_name, "OUTER");
    let outer_ws = data_names(&ir.data_division.working_storage);
    assert!(outer_ws.iter().any(|n| n == "OUTER-WS"), "{outer_ws:?}");
    assert!(!outer_ws.iter().any(|n| n == "INNER-WS"), "{outer_ws:?}");
    let outer_paras: Vec<_> = ir
        .paragraphs
        .iter()
        .chain(
            ir.procedure_division
                .sections
                .iter()
                .flat_map(|s| s.paragraphs.iter()),
        )
        .map(|p| p.name.clone())
        .collect();
    assert!(
        outer_paras.iter().any(|n| n == "OUTER-PARA"),
        "{outer_paras:?}"
    );
    assert!(
        !outer_paras.iter().any(|n| n == "INNER-PARA"),
        "{outer_paras:?}"
    );
    assert_eq!(
        ir.nested_programs.len(),
        1,
        "{:?}",
        ir.nested_programs.len()
    );
    let inner = &ir.nested_programs[0];
    assert_eq!(inner.program_name, "INNER");
    let inner_ws = data_names(&inner.data_division.working_storage);
    assert!(inner_ws.iter().any(|n| n == "INNER-WS"), "{inner_ws:?}");
    let inner_paras: Vec<_> = inner
        .paragraphs
        .iter()
        .chain(
            inner
                .procedure_division
                .sections
                .iter()
                .flat_map(|s| s.paragraphs.iter()),
        )
        .map(|p| p.name.clone())
        .collect();
    assert!(
        inner_paras.iter().any(|n| n == "INNER-PARA"),
        "{inner_paras:?}"
    );
    let md = report::render(&ir, false, false).unwrap();
    assert!(md.contains("## Nested Program: INNER"), "{md}");
    assert!(md.contains("INNER-WS"), "{md}");
    assert!(md.contains("INNER-PARA"), "{md}");
}

#[test]
fn exec_sql_is_one_statement() {
    let ir = parse_ir(
        "sql.cob",
        r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLDB21.
       PROCEDURE DIVISION.
           EXEC SQL SELECT ACCTNO FROM ACCOUNTS END-EXEC.
           GOBACK.
"#,
    );
    let stmts: Vec<_> = ir
        .procedure_division
        .sections
        .iter()
        .flat_map(|s| s.paragraphs.iter())
        .flat_map(|p| p.statements.iter())
        .collect();
    let sql: Vec<_> = stmts.iter().filter(|s| s.r#type == "EXEC SQL").collect();
    assert_eq!(sql.len(), 1, "{stmts:?}");
    assert!(
        sql[0].operands[0].to_uppercase().contains("SELECT"),
        "{:?}",
        sql[0].operands
    );
    assert!(!stmts.iter().any(|s| s.r#type == "EXEC"));
}

#[test]
fn padded_working_storage_header_still_parses() {
    let cobol = concat!(
        "       IDENTIFICATION DIVISION.\n",
        "       PROGRAM-ID. CBLDB21.\n",
        "       DATA DIVISION.\n",
        "       WORKING-STORAGE SECTION.                                         \n",
        "       01 CUSTOMER-RECORD PIC X(8).\n",
        "       PROCEDURE DIVISION.\n",
        "           GOBACK.\n",
    );
    let ir = parse_ir("pad72.cob", cobol);
    let names = data_names(&ir.data_division.working_storage);
    assert!(names.iter().any(|n| n == "CUSTOMER-RECORD"), "{names:?}");
}

#[test]
fn column_7_hyphen_joins_literal() {
    // IBM 6.5: unclosed alphanumeric literal keeps spaces through column 72;
    // the continuation quotation mark is not part of the value.
    // https://www.ibm.com/docs/en/cobol-zos/6.5.0?topic=b-continuation-lines
    let line1 = "           DISPLAY \"HELLO";
    let pad = 72 - line1.chars().count();
    let cobol = concat!(
        "       IDENTIFICATION DIVISION.\n",
        "       PROGRAM-ID. CONT7.\n",
        "       PROCEDURE DIVISION.\n",
        "           DISPLAY \"HELLO\n",
        "      -             \"WORLD\".\n",
        "           GOBACK.\n",
    );
    let ir = parse_ir("cont7.cob", cobol);
    let stmts: Vec<_> = ir
        .procedure_division
        .sections
        .iter()
        .flat_map(|s| s.paragraphs.iter())
        .flat_map(|p| p.statements.iter())
        .filter(|s| s.r#type == "DISPLAY")
        .collect();
    assert_eq!(stmts.len(), 1, "{stmts:?}");
    let expected = format!("\"HELLO{}WORLD\"", " ".repeat(pad));
    assert_eq!(stmts[0].operands, vec![expected]);
    assert!(
        !stmts[0].operands[0].contains("HELLO\"WORLD"),
        "continuation quote must be dropped: {:?}",
        stmts[0].operands
    );
}

#[test]
fn level_77_is_not_nested_under_a_group() {
    let ir = parse_ir(
        "lv77.cob",
        r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LV77.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ERROR-MESSAGE.
          02 ERROR-TEXT PIC X(132).
       77 ERROR-TEXT-LEN PIC S9(9).
       77 ERROR-TEXT-HBOUND PIC S9(9).
       PROCEDURE DIVISION.
           GOBACK.
"#,
    );
    let ws = &ir.data_division.working_storage;
    let names = data_names(ws);
    assert!(names.iter().any(|n| n == "ERROR-TEXT-LEN"), "{names:?}");
    let top: Vec<_> = ws.iter().map(|i| i.name.as_str()).collect();
    assert!(top.contains(&"ERROR-MESSAGE"), "{top:?}");
    assert!(top.contains(&"ERROR-TEXT-LEN"), "{top:?}");
    assert!(top.contains(&"ERROR-TEXT-HBOUND"), "{top:?}");
    let msg = ws.iter().find(|i| i.name == "ERROR-MESSAGE").unwrap();
    assert!(
        !data_names(&msg.children)
            .iter()
            .any(|n| n == "ERROR-TEXT-LEN"),
        "{:?}",
        msg.children
    );
}

#[test]
fn exec_sql_declare_in_working_storage_is_not_a_data_item() {
    let ir = parse_ir(
        "sqlws.cob",
        r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLWS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL DECLARE Z#####T TABLE
                   (ACCTNO CHAR(8) NOT NULL)
                   END-EXEC.
           EXEC SQL DECLARE CUR1 CURSOR FOR SELECT ACCTNO FROM Z#####T END-EXEC.
       01 CUSTOMER-RECORD.
          02 ACCT-NO PIC X(8).
       PROCEDURE DIVISION.
           EXEC SQL OPEN CUR1 END-EXEC.
           GOBACK.
"#,
    );
    let names = data_names(&ir.data_division.working_storage);
    assert!(names.iter().any(|n| n == "CUSTOMER-RECORD"), "{names:?}");
    assert!(names.iter().any(|n| n == "ACCT-NO"), "{names:?}");
    assert!(!names.iter().any(|n| n == "ACCTNO"), "{names:?}");
    assert!(!names.iter().any(|n| n.contains("CUR1")), "{names:?}");
    assert!(!names.iter().any(|n| n.contains("Z#####T")), "{names:?}");
    assert!(!names.iter().any(|n| n.contains("DECLARE")), "{names:?}");
    let stmts: Vec<_> = ir
        .procedure_division
        .sections
        .iter()
        .flat_map(|s| s.paragraphs.iter())
        .flat_map(|p| p.statements.iter())
        .filter(|s| s.r#type == "EXEC SQL")
        .collect();
    assert_eq!(stmts.len(), 1, "{stmts:?}");
    assert!(stmts[0].operands[0].to_uppercase().contains("OPEN"));
}
