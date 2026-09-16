use codesleuth::report;

#[test]
fn paragraphs_print_in_source_line_order() {
    let ir_json = r#"{
        "program_name": "ORDER",
        "source_file": "order.cob",
        "identification_division": {"author": "A", "date_written": "2024-01-01", "comments": []},
        "environment_division": {"input_output_section": {"files": []}},
        "data_division": {"working_storage": [], "file_section": []},
        "paragraphs": [],
        "procedure_division": {"sections": [{
            "name": "",
            "paragraphs": [
                {
                    "name": "ZEBRA",
                    "section": null,
                    "kind": "paragraph",
                    "line": 40,
                    "source_location": null,
                    "statements": [{"type": "GOBACK", "operands": [], "raw": "GOBACK.", "line": 41, "source_location": null}],
                    "variable_usage": [
                        {"name": "Z-VAR", "read": true, "written": false},
                        {"name": "A-VAR", "read": false, "written": true}
                    ]
                },
                {
                    "name": "ALPHA",
                    "section": null,
                    "kind": "paragraph",
                    "line": 20,
                    "source_location": null,
                    "statements": [{"type": "DISPLAY", "operands": ["HELLO"], "raw": "DISPLAY HELLO.", "line": 21, "source_location": null}],
                    "variable_usage": []
                }
            ]
        }]},
        "call_graph": [],
        "control_flow_graph": []
    }"#;
    let md = report::render_json(ir_json, false, false).unwrap();
    let alpha = md.find("#### Paragraph: **ALPHA** (line 20)").unwrap();
    let zebra = md.find("#### Paragraph: **ZEBRA** (line 40)").unwrap();
    assert!(
        alpha < zebra,
        "paragraphs must print by source line, not HashMap or name order"
    );
    let vars = md.split("#### Paragraph: **ZEBRA**").nth(1).unwrap();
    let a_var = vars.find("| **A-VAR**").unwrap();
    let z_var = vars.find("| **Z-VAR**").unwrap();
    assert!(a_var < z_var, "variable rows must print in name order");
}

#[test]
fn test_report() {
    let ir_json = r#"{
        "program_name": "TESTPROG",
        "source_file": "test.cob",
        "identification_division": {"author": "A", "date_written": "2024-01-01", "comments": []},
        "environment_division": {"input_output_section": {"files": []}},
        "data_division": {"working_storage": [], "file_section": []},
        "paragraphs": [],
        "procedure_division": {"sections": []},
        "call_graph": [],
        "control_flow_graph": []
    }"#;
    let md = report::render_json(ir_json, false, false).unwrap();
    assert!(md.contains("Program Name"));
    assert!(md.contains("Procedure Division") || md.contains("No Procedure Division"));
    assert!(md.contains("COBOL Program Summary"));
}

#[test]
fn empty_file_and_io_omit_headings() {
    let ir_json = r#"{
        "program_name": "TESTPROG",
        "source_file": "test.cob",
        "identification_division": {"author": "A", "date_written": "2024-01-01", "comments": []},
        "environment_division": {"input_output_section": {"files": []}},
        "data_division": {"working_storage": [], "file_section": [], "linkage": []},
        "paragraphs": [],
        "procedure_division": {"sections": []},
        "call_graph": [],
        "control_flow_graph": []
    }"#;
    let md = report::render_json(ir_json, false, false).unwrap();
    assert!(!md.contains("## Environment Division - Input/Output Section"));
    assert!(!md.contains("_No Input/Output files found._"));
    assert!(!md.contains("## File Section"));
    assert!(!md.contains("_No File Section entries found._"));
    assert!(!md.contains("## Linkage Section"));
}
