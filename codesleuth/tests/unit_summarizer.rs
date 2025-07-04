use codesleuth::summarizer;

#[test]
fn test_summarizer() {
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
    let md = summarizer::summarize_ir(ir_json, false, false).unwrap();
    assert!(md.contains("Program Name"));
    assert!(md.contains("Procedure Division"));
    assert!(md.contains("COBOL Program Summary"));
} 