use clap::Parser;
use serde::Serialize;
use std::fs;
use std::path::PathBuf;
use chrono::Utc;
use regex::Regex;
use std::collections::HashMap;
use once_cell::sync::Lazy;
use std::collections::HashSet;

#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    /// Input COBOL file
    input: PathBuf,
    /// Enable verbose debug output
    #[arg(long)]
    verbose: bool,
}

// static RE_IDENT_DIV: Lazy<Regex> = Lazy::new(|| Regex::new(r"(?i)^\s*IDENTIFICATION DIVISION\s*\.?$").unwrap());

static COBOL_KEYWORDS: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    [
        "MOVE", "PERFORM", "READ", "WRITE", "DISPLAY", "IF", "ELSE", "END-IF", "UNTIL", "AT", "END", "STOP",
        "CALL", "EXEC", "SQL", "OPEN", "CLOSE", "FETCH", "COMPUTE", "SET", "IS", "NOT", "EQUAL", "THEN",
        "USING", "FROM", "BY", "TO", "VARYING", "AND", "OR", ">", "<", "=", ".", "(", ")", "FUNCTION",
        "INTO", "AFTER", "ADVANCING", "END-EXEC", "RETURN", "RUN", "INPUT", "OUTPUT", "I-O", "EXTEND",
        "CURRENT-DATE", "GOBACK"
    ].iter().cloned().collect()
});

fn is_literal(name: &str) -> bool {
    let n = name.trim();
    if n.starts_with('"') && n.ends_with('"') { return true; }
    if n.starts_with('\'') && n.ends_with('\'') { return true; }
    if n.parse::<f64>().is_ok() { return true; }
    let upper = n.to_uppercase();
    matches!(upper.as_str(), "SPACES" | "ZERO" | "ZEROS" | "HIGH-VALUE" | "LOW-VALUE" | "QUOTE" | "QUOTES" | "NULL") ||
    n.starts_with('"') || n.ends_with('"') || n.starts_with('\'') || n.ends_with('\'')
}

#[derive(Serialize)]
struct IR {
    program_name: String,
    source_file: String,
    identification_division: IdentificationDivision,
    environment_division: EnvironmentDivision,
    data_division: DataDivision,
    paragraphs: Vec<Paragraph>,
    procedure_division: ProcedureDivision,
    call_graph: Vec<CallGraphEntry>,
    control_flow_graph: Vec<ControlFlowEdge>,
}

#[derive(Serialize)]
struct IdentificationDivision {
    author: String,
    date_written: String,
    comments: Vec<String>,
}

#[derive(Serialize)]
struct EnvironmentDivision {
    input_output_section: InputOutputSection,
}

#[derive(Serialize)]
struct InputOutputSection {
    files: Vec<IOFile>,
}

#[derive(Serialize)]
struct IOFile {
    name: String,
    r#type: String,
    description: String,
    record_name: Option<String>,
}

#[derive(Serialize, Debug, Clone)]
struct DataItem {
    name: String,
    level: i32,
    picture: Option<String>,
    r#type: Option<String>,
    value: Option<String>,
    occurs: Option<usize>,
    redefines: Option<String>,
    comp3: bool,
    section: Option<String>,
    children: Vec<DataItem>,
}

#[derive(Serialize)]
struct DataDivision {
    working_storage: Vec<DataItem>,
    file_section: Vec<DataItem>,
}

#[derive(Serialize, Clone)]
struct ProcedureSection {
    name: String,
    paragraphs: Vec<Paragraph>,
}

#[derive(Serialize, Clone)]
struct Paragraph {
    name: String,
    section: Option<String>,
    kind: String, // "paragraph"
    line: Option<usize>,
    source_location: Option<String>,
    statements: Vec<Statement>,
    variable_usage: Vec<VariableUsage>,
}

#[derive(Serialize, Clone)]
struct Statement {
    r#type: String,
    operands: Vec<String>,
    raw: String,
    line: Option<usize>,
    source_location: Option<String>,
}

#[derive(Serialize)]
struct CallGraphEntry {
    from: String,
    to: String,
    r#type: String,
    kind: String, // "edge"
    line: Option<usize>,
    section: Option<String>,
    source_location: Option<String>,
}

#[derive(Serialize, Clone)]
struct ProcedureDivision {
    sections: Vec<ProcedureSection>,
}

#[derive(Serialize, Clone)]
struct VariableUsage {
    name: String,
    read: bool,
    written: bool,
}

#[derive(Serialize)]
struct ControlFlowEdge {
    from: String,
    to: String,
    r#type: String,
}

// Enhanced type inference from PIC
fn infer_type_from_pic(pic: &str) -> String {
    let pic = pic.to_uppercase();
    if pic.contains("COMP-3") || pic.contains("PACKED") {
        return "packed-decimal (COMP-3)".to_string();
    }
    if pic.starts_with('X') || pic.contains("X(") {
        return "string".to_string();
    }
    if pic.contains('9') {
        if pic.contains('V') || pic.contains('.') {
            return "float".to_string();
        }
        return "numeric".to_string();
    }
    if pic.contains("COMP") {
        return "binary".to_string();
    }
    "unknown".to_string()
}

fn normalize_name(s: &str) -> String {
    s.trim().trim_end_matches('.').to_uppercase()
}

fn parse_identification_division(source: &str) -> (String, String, String, Vec<String>) {
    let mut program_name = String::new();
    let mut author = String::new();
    let mut date_written = String::new();
    let mut comments = Vec::new();
    let mut in_ident = false;

    // Regex patterns (case-insensitive, flexible spacing)
    let re_program = Regex::new(r"(?i)^\s*PROGRAM-ID\s*\.\s*(\S+)").unwrap();
    let re_author = Regex::new(r"(?i)^\s*AUTHOR\s*\.\s*(.+)").unwrap();
    let re_date = Regex::new(r"(?i)^\s*DATE-WRITTEN\s*\.\s*(.+)").unwrap();
    let re_ident_div = Regex::new(r"(?i)^\s*IDENTIFICATION DIVISION\s*\.?").unwrap();
    let re_end_div = Regex::new(r"(?i)^\s*\w+ DIVISION\s*\.?").unwrap();

    for line in source.lines() {
        let line = line.trim_end();
        if !in_ident && re_ident_div.is_match(line) {
            in_ident = true;
            continue;
        }
        if in_ident {
            if re_end_div.is_match(line) && !re_ident_div.is_match(line) {
                break; // End of identification division
            }
            if let Some(caps) = re_program.captures(line) {
                program_name = caps.get(1).map(|m| m.as_str().trim_end_matches('.').to_string()).unwrap_or_default();
            } else if let Some(caps) = re_author.captures(line) {
                author = caps.get(1).map(|m| m.as_str().trim_end_matches('.').to_string()).unwrap_or_default();
            } else if let Some(caps) = re_date.captures(line) {
                date_written = caps.get(1).map(|m| m.as_str().trim_end_matches('.').to_string()).unwrap_or_default();
            } else if line.trim_start().starts_with('*') {
                comments.push(line.trim_start_matches('*').trim().to_string());
            }
        }
    }
    (program_name, author, date_written, comments)
}

fn extract_section_lines<'a>(source: &'a str, section: &str) -> Vec<&'a str> {
    let re_start = Regex::new(&format!(r"(?i)^\s*{}\s*\.?$", section)).unwrap();
    let re_end = Regex::new(r"(?i)^\s*(WORKING-STORAGE SECTION|FILE SECTION|LINKAGE SECTION|PROCEDURE DIVISION|[A-Z-]+ DIVISION)\s*\.?$").unwrap();
    let mut lines = Vec::new();
    let mut in_section = false;
    for line in source.lines() {
        if !in_section && re_start.is_match(line) {
            in_section = true;
            continue;
        }
        if in_section {
            if re_end.is_match(line) && !re_start.is_match(line) {
                break;
            }
            lines.push(line);
        }
    }
    lines
}

fn parse_data_items(section_lines: &[&str], section_name: Option<&str>) -> Vec<DataItem> {
    let re_item = Regex::new(
        r"(?i)^\s*(\d{2})\s+([A-Z0-9-]+)(?:\s+REDEFINES\s+([A-Z0-9-]+))?(?:\s+OCCURS\s+(\d+))?(?:\s+PIC\s+([A-Z0-9\(\)V\.,$S-]+))?(?:\s+VALUE\s+([^\.]+))?\s*\.?$"
    ).unwrap();
    let mut stack: Vec<(i32, DataItem)> = Vec::new();
    let mut result: Vec<DataItem> = Vec::new();
    for line in section_lines {
        if let Some(caps) = re_item.captures(line) {
            let level = caps[1].parse::<i32>().unwrap_or(0);
            let name = caps[2].to_string();
            let redefines = caps.get(3).map(|m| m.as_str().to_string());
            let occurs = caps.get(4).map(|m| m.as_str().parse::<usize>().ok()).flatten();
            let picture = caps.get(5).map(|m| m.as_str().to_string());
            let value = caps.get(6).map(|m| m.as_str().trim().to_string());
            let comp3 = picture.as_ref().map_or(false, |pic| pic.to_uppercase().contains("COMP-3") || pic.to_uppercase().contains("PACKED"));
            let r#type = picture.as_ref().map(|pic| infer_type_from_pic(pic));
            let section = section_name.map(|s| s.to_string());
            eprintln!("[DATA-ITEM] level={} name={} pic={:?} occurs={:?} redefines={:?} value={:?} comp3={}", level, name, picture, occurs, redefines, value, comp3);
            let item = DataItem {
                name,
                level,
                picture,
                r#type,
                value,
                occurs,
                redefines,
                comp3,
                section,
                children: Vec::new(),
            };
            while let Some((parent_level, _)) = stack.last() {
                if *parent_level < level {
                    break;
                }
                let (_, completed) = stack.pop().unwrap();
                if let Some((_, parent)) = stack.last_mut() {
                    parent.children.push(completed);
                } else {
                    result.push(completed);
                }
            }
            stack.push((level, item));
        } else {
            eprintln!("[DATA-ITEM] No match for line: {}", line);
        }
    }
    while let Some((_, item)) = stack.pop() {
        if let Some((_, parent)) = stack.last_mut() {
            parent.children.push(item);
        } else {
            result.push(item);
        }
    }
    result.reverse();
    result
}

fn extract_variable_usage(statements: &[Statement], para_name_map: &HashMap<String, String>) -> Vec<VariableUsage> {
    let mut usage: HashMap<String, (bool, bool)> = HashMap::new();
    for stmt in statements {
        let stype = stmt.r#type.to_uppercase();
        let ops = &stmt.operands;
        let filtered_ops: Vec<_> = ops.iter()
            .filter(|op| {
                let opu = normalize_name(op);
                !COBOL_KEYWORDS.contains(opu.as_str()) && !is_literal(op) && !para_name_map.contains_key(&opu)
            })
            .cloned()
            .collect();
        match stype.as_str() {
            "MOVE" => {
                if filtered_ops.len() >= 2 {
                    usage.entry(filtered_ops[0].clone()).or_insert((true, false));
                    for op in &filtered_ops[1..] {
                        usage.entry(op.clone()).or_insert((false, true));
                    }
                }
            }
            "ADD" | "SUBTRACT" => {
                if filtered_ops.len() >= 2 {
                    for op in &filtered_ops[..filtered_ops.len() - 1] {
                        usage.entry(op.clone()).or_insert((true, false));
                    }
                    let last_op = &filtered_ops[filtered_ops.len() - 1];
                    usage.entry(last_op.clone()).or_insert((true, true));
                }
            }
            "READ" | "WRITE" | "OPEN" | "CLOSE" => {
                for op in &filtered_ops {
                    usage.entry(op.clone()).or_insert((true, false));
                }
            }
            _ => {
                for op in &filtered_ops {
                    usage.entry(op.clone()).or_insert((true, false));
                }
            }
        }
    }
    usage.into_iter().map(|(name, (read, written))| VariableUsage { name, read, written }).collect()
}

fn parse_procedure_division_and_call_graph(source: &str) -> (ProcedureDivision, Vec<CallGraphEntry>, Vec<ControlFlowEdge>, Vec<Paragraph>) {
    let re_proc_div = Regex::new(r"(?i)^\s*PROCEDURE DIVISION\s*\.?$").unwrap();
    let re_division = Regex::new(r"(?i)^\s*\w+ DIVISION\s*\.?$").unwrap();
    let re_section = Regex::new(r"^\s*([A-Z0-9-]+) SECTION\s*\.\s*$").unwrap();
    let re_paragraph = Regex::new(r"^\s*([A-Z0-9-]+)\.\s*$").unwrap();
    let skip_paragraphs = ["END-IF", "END-READ", "GOBACK"];
    let mut in_proc = false;
    let mut all_paragraphs: Vec<String> = Vec::new();
    let mut para_name_map: HashMap<String, String> = HashMap::new();
    // First pass: collect all paragraph names
    for (_i, line) in source.lines().enumerate() {
        let line = line.trim_end();
        if !in_proc && re_proc_div.is_match(line) {
            in_proc = true;
            continue;
        }
        if in_proc {
            if re_division.is_match(line) && !re_proc_div.is_match(line) {
                break;
            }
            if let Some(para_caps) = re_paragraph.captures(line) {
                let name = para_caps.get(1).map(|m| m.as_str().to_string()).unwrap_or_default();
                if skip_paragraphs.contains(&name.as_str()) { continue; }
                let norm_name = normalize_name(&name);
                all_paragraphs.push(norm_name.clone());
                para_name_map.insert(norm_name, name.clone());
            }
        }
    }
    // Second pass: parse statements and build graphs
    in_proc = false;
    let mut sections: Vec<ProcedureSection> = Vec::new();
    let mut current_section: Option<ProcedureSection> = None;
    let mut current_paragraph: Option<Paragraph> = None;
    let mut current_paragraph_name = String::new();
    let mut call_graph = Vec::new();
    let mut default_section_paragraphs: Vec<Paragraph> = Vec::new();
    let mut control_flow_graph = Vec::new();
    let mut all_paragraphs_flat: Vec<Paragraph> = Vec::new();
    for (i, line) in source.lines().enumerate() {
        let line = line.trim_end();
        if !in_proc && re_proc_div.is_match(line) {
            in_proc = true;
            continue;
        }
        if in_proc {
            if re_division.is_match(line) && !re_proc_div.is_match(line) {
                break;
            }
            if let Some(sec_caps) = re_section.captures(line) {
                if let Some(mut sec) = current_section.take() {
                    if let Some(mut p) = current_paragraph.take() {
                        p.variable_usage = extract_variable_usage(&p.statements, &para_name_map);
                        sec.paragraphs.push(p.clone());
                        all_paragraphs_flat.push(p);
                    }
                    sections.push(sec);
                }
                current_section = Some(ProcedureSection {
                    name: sec_caps.get(1).map(|m| m.as_str().to_string()).unwrap_or_default(),
                    paragraphs: Vec::new(),
                });
                current_paragraph = None;
                current_paragraph_name.clear();
            } else if let Some(para_caps) = re_paragraph.captures(line) {
                let name = para_caps.get(1).map(|m| m.as_str().to_string()).unwrap_or_default();
                if skip_paragraphs.contains(&name.as_str()) { continue; }
                if let Some(ref mut para) = current_paragraph {
                    para.variable_usage = extract_variable_usage(&para.statements, &para_name_map);
                    if let Some(ref mut sec) = current_section {
                        sec.paragraphs.push(para.clone());
                    } else {
                        default_section_paragraphs.push(para.clone());
                    }
                    all_paragraphs_flat.push(para.clone());
                }
                current_paragraph_name = name.clone();
                current_paragraph = Some(Paragraph {
                    name,
                    section: current_section.as_ref().map(|s| s.name.clone()),
                    kind: "paragraph".to_string(),
                    line: Some(i + 1),
                    source_location: None,
                    statements: Vec::new(),
                    variable_usage: Vec::new(),
                });
            } else if let Some(ref mut para) = current_paragraph {
                let trimmed = line.trim();
                if !trimmed.is_empty() && !trimmed.starts_with('*') {
                    let mut parts = trimmed.split_whitespace();
                    let stype = parts.next().unwrap_or("").to_uppercase();
                    let operands: Vec<String> = parts.map(|s| s.to_string()).collect();
                    let stmt = Statement {
                        r#type: stype.clone(),
                        operands: operands.clone(),
                        raw: trimmed.to_string(),
                        line: Some(i + 1),
                        source_location: None,
                    };
                    para.statements.push(stmt.clone());
                    // Call graph extraction
                    if stype == "PERFORM" && !operands.is_empty() {
                        let target = if operands.iter().any(|s| s.to_uppercase() == "UNTIL") {
                            normalize_name(&operands[0])
                        } else if operands.iter().any(|s| s.to_uppercase() == "THRU") {
                            normalize_name(&operands[0])
                        } else {
                            normalize_name(&operands[0])
                        };
                        if let Some(to_name) = para_name_map.get(&target) {
                            call_graph.push(CallGraphEntry {
                                from: current_paragraph_name.clone(),
                                to: to_name.clone(),
                                r#type: if operands.iter().any(|s| s.to_uppercase() == "UNTIL") {
                                    "PERFORM VARYING".to_string()
                                } else {
                                    "PERFORM".to_string()
                                },
                                kind: "edge".to_string(),
                                line: Some(i + 1),
                                section: current_section.as_ref().map(|s| s.name.clone()),
                                source_location: None,
                            });
                        }
                    } else if (stype == "GO" && operands.get(0).map(|s| s.to_uppercase()) == Some("TO".to_string())) || stype == "GOTO" {
                        let target = if stype == "GO" { operands.get(1) } else { operands.get(0) };
                        if let Some(target) = target {
                            let target_norm = normalize_name(target);
                            if let Some(to_name) = para_name_map.get(&target_norm) {
                                call_graph.push(CallGraphEntry {
                                    from: current_paragraph_name.clone(),
                                    to: to_name.clone(),
                                    r#type: "GOTO".to_string(),
                                    kind: "edge".to_string(),
                                    line: Some(i + 1),
                                    section: current_section.as_ref().map(|s| s.name.clone()),
                                    source_location: None,
                                });
                            }
                        }
                    } else if stype == "CALL" && !operands.is_empty() {
                        let mut target = operands[0].trim_end_matches('.').to_string();
                        target = target.trim_matches('"').trim_matches('\'').to_string();
                        let target_norm = normalize_name(&target);
                        if !target_norm.is_empty() {
                            call_graph.push(CallGraphEntry {
                                from: current_paragraph_name.clone(),
                                to: target_norm,
                                r#type: "CALL".to_string(),
                                kind: "edge".to_string(),
                                line: Some(i + 1),
                                section: current_section.as_ref().map(|s| s.name.clone()),
                                source_location: None,
                            });
                        }
                    }
                }
            }
        }
    }
    // Finalize last paragraph and section
    if let Some(ref mut para) = current_paragraph {
        para.variable_usage = extract_variable_usage(&para.statements, &para_name_map);
        if let Some(ref mut sec) = current_section {
            sec.paragraphs.push(para.clone());
        } else {
            default_section_paragraphs.push(para.clone());
        }
        all_paragraphs_flat.push(para.clone());
    }
    if let Some(sec) = current_section {
        sections.push(sec);
    }
    if !default_section_paragraphs.is_empty() {
        sections.insert(0, ProcedureSection {
            name: "".to_string(),
            paragraphs: default_section_paragraphs,
        });
    }
    // Generate control flow graph
    for sec in &sections {
        for para in &sec.paragraphs {
            let mut prev_stmt: Option<&Statement> = None;
            for stmt in &para.statements {
                if let Some(prev) = prev_stmt {
                    let from_label = format!("{}:{}", para.name, prev.raw);
                    let to_label = format!("{}:{}", para.name, stmt.raw);
                    control_flow_graph.push(ControlFlowEdge {
                        from: from_label,
                        to: to_label,
                        r#type: "NEXT".to_string(),
                    });
                }
                let stype = stmt.r#type.to_uppercase();
                if stype == "PERFORM" && !stmt.operands.is_empty() {
                    let target = if stmt.operands.iter().any(|s| s.to_uppercase() == "UNTIL") {
                        normalize_name(&stmt.operands[0])
                    } else {
                        normalize_name(&stmt.operands[0])
                    };
                    if let Some(to_name) = para_name_map.get(&target) {
                        control_flow_graph.push(ControlFlowEdge {
                            from: format!("{}:{}", para.name, stmt.raw),
                            to: format!("{}:{}", to_name, stmt.raw),
                            r#type: if stmt.operands.iter().any(|s| s.to_uppercase() == "UNTIL") {
                                "PERFORM VARYING".to_string()
                            } else {
                                "PERFORM".to_string()
                            },
                        });
                    }
                } else if stype == "GOTO" && !stmt.operands.is_empty() {
                    let target = normalize_name(&stmt.operands[0]);
                    if let Some(to_name) = para_name_map.get(&target) {
                        control_flow_graph.push(ControlFlowEdge {
                            from: format!("{}:{}", para.name, stmt.raw),
                            to: format!("{}:{}", to_name, stmt.raw),
                            r#type: "GOTO".to_string(),
                        });
                    }
                }
                prev_stmt = Some(stmt);
            }
        }
    }
    (
        ProcedureDivision { sections },
        call_graph,
        control_flow_graph,
        all_paragraphs_flat,
    )
}

// Parse OPEN statements in procedure division to improve file mode detection
fn parse_open_statements(source: &str) -> HashMap<String, String> {
    let mut file_modes = HashMap::new();
    let re_open = Regex::new(r"(?i)OPEN\s+(INPUT|OUTPUT|I-O|EXTEND)\s+([A-Z0-9-]+)").unwrap();
    for line in source.lines() {
        if let Some(caps) = re_open.captures(line) {
            let mode = caps.get(1).map(|m| m.as_str().to_lowercase()).unwrap_or("unknown".to_string());
            let file = caps.get(2).map(|m| m.as_str().to_string()).unwrap_or_default();
            file_modes.insert(file, mode);
        }
    }
    file_modes
}

fn parse_input_output_section(source: &str, file_modes: &HashMap<String, String>) -> Vec<IOFile> {
    let mut files = Vec::new();
    let mut in_env = false;
    let mut in_io = false;
    let re_env_div = Regex::new(r"(?i)^\s*ENVIRONMENT DIVISION\s*\.?$").unwrap();
    let re_io_sec = Regex::new(r"(?i)^\s*INPUT-OUTPUT SECTION\s*\.?$").unwrap();
    let re_file_control = Regex::new(r"(?i)^\s*FILE-CONTROL\s*\.?$").unwrap();
    let re_select = Regex::new(r"(?i)^\s*SELECT\s+([A-Z0-9-]+)\s+ASSIGN\s+TO\s+('?\w+'?)\s*\.\s*$").unwrap();
    let re_fd = Regex::new(r"(?i)^\s*FD\s+([A-Z0-9-]+)\s*.*").unwrap();
    let re_01_level = Regex::new(r"(?i)^\s*01\s+([A-Z0-9-]+)\s*.*").unwrap();
    let mut select_to_fd: HashMap<String, String> = HashMap::new();
    let mut last_select: Option<String> = None;
    let file_section_lines = extract_section_lines(source, "FILE SECTION");
    let file_section = parse_data_items(&file_section_lines, Some("FILE SECTION"));
    let file_section_names: HashSet<String> = file_section.iter().map(|item| item.name.clone()).collect();
    let mut expecting_01 = false;

    for line in source.lines() {
        let line = line.trim_end();
        if !in_env && re_env_div.is_match(line) {
            in_env = true;
            continue;
        }
        if in_env && !in_io && re_io_sec.is_match(line) {
            in_io = true;
            continue;
        }
        if in_env && in_io {
            if re_env_div.is_match(line) && !re_io_sec.is_match(line) {
                break;
            }
            if re_file_control.is_match(line) {
                continue;
            }
            if let Some(caps) = re_select.captures(line) {
                let name = caps.get(1).map(|m| m.as_str().to_string()).unwrap_or_default();
                last_select = Some(name.clone());
                expecting_01 = false;
                let assigned = caps.get(2).map(|m| m.as_str().trim_matches('"').trim_matches('\'').to_string()).unwrap_or_default();
                let r#type = file_modes.get(&name).cloned().unwrap_or_else(|| {
                    if name.contains("IN") { "input".to_string() } else if name.contains("OUT") { "output".to_string() } else { "unknown".to_string() }
                });
                files.push(IOFile {
                    name: name.clone(),
                    r#type,
                    description: format!("Assigned to {}", assigned),
                    record_name: None,
                });
            } else if let Some(_caps) = re_fd.captures(line) {
                expecting_01 = true;
            } else if expecting_01 {
                if let Some(caps) = re_01_level.captures(line) {
                    let record_name = caps.get(1).map(|m| m.as_str().to_string()).unwrap_or_default();
                    if let Some(sel) = &last_select {
                        if file_section_names.contains(&record_name) {
                            select_to_fd.insert(sel.clone(), record_name.clone());
                            for file in files.iter_mut() {
                                if file.name == *sel {
                                    file.record_name = Some(record_name.clone());
                                }
                            }
                        }
                    }
                    expecting_01 = false;
                }
            }
        }
    }
    files
}

fn main() {
    let args = Args::parse();
    let file_content = fs::read_to_string(&args.input).unwrap_or_default();
    let (program_name, author, date_written, comments) = parse_identification_division(&file_content);
    let ws_lines = extract_section_lines(&file_content, "WORKING-STORAGE SECTION");
    let working_storage = parse_data_items(&ws_lines, Some("WORKING-STORAGE"));
    let file_lines = extract_section_lines(&file_content, "FILE SECTION");
    let file_section = parse_data_items(&file_lines, Some("FILE SECTION"));
    let (procedure_division, call_graph, control_flow_graph, paragraphs) = parse_procedure_division_and_call_graph(&file_content);
    let file_modes = parse_open_statements(&file_content);
    let io_files = parse_input_output_section(&file_content, &file_modes);
    let ir = IR {
        program_name: if !program_name.is_empty() { program_name } else { "UNKNOWN".to_string() },
        source_file: args.input.display().to_string(),
        identification_division: IdentificationDivision {
            author: if !author.is_empty() { author } else { "UNKNOWN".to_string() },
            date_written: if !date_written.is_empty() { date_written } else { Utc::now().format("%Y-%m-%d").to_string() },
            comments,
        },
        environment_division: EnvironmentDivision {
            input_output_section: InputOutputSection {
                files: io_files,
            },
        },
        data_division: DataDivision {
            working_storage,
            file_section,
        },
        paragraphs,
        procedure_division,
        call_graph,
        control_flow_graph,
    };
    if args.verbose {
        eprintln!("[VERBOSE] Parsed IR: {}", serde_json::to_string_pretty(&ir).unwrap());
    }
    println!("{}", serde_json::to_string_pretty(&ir).unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse_data_items_simple() {
        let lines = vec!["01 FOO PIC X(10).", "05 BAR PIC 9(5)."];
        let items = parse_data_items(&lines, None);
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].name, "FOO");
        assert_eq!(items[0].children.len(), 1);
        assert_eq!(items[0].children[0].name, "BAR");
    }
} 