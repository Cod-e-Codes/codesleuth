use clap::Parser;
use serde::Serialize;
use std::fs;
use std::path::PathBuf;
use chrono::Utc;
use regex::Regex;
use std::collections::HashMap;

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
        return "packed decimal (COMP-3)".to_string();
    }
    if pic.starts_with('X') || pic.contains("X(") {
        "string".to_string()
    } else if pic.contains('9') {
        if pic.contains('V') || pic.contains('.') {
            "float".to_string()
        } else if pic.contains('S') {
            "signed numeric".to_string()
        } else {
            "numeric".to_string()
        }
    } else if pic.contains("COMP") {
        "binary".to_string()
    } else {
        "unknown".to_string()
    }
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
    let re_end = Regex::new(r"(?i)^\s*\w+ DIVISION\s*\.?$").unwrap();
    let mut lines = Vec::new();
    let mut in_section = false;
    for line in source.lines() {
        if !in_section && re_start.is_match(line) {
            in_section = true;
            continue;
        }
        if in_section {
            if re_end.is_match(line) {
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

fn extract_variable_usage(statements: &[Statement]) -> Vec<VariableUsage> {
    use std::collections::HashMap;
    let mut usage: HashMap<String, (bool, bool)> = HashMap::new();
    for stmt in statements {
        let stype = stmt.r#type.to_uppercase();
        let ops = &stmt.operands;
        // Heuristic: first operand is written for MOVE, rest are read; for ADD/SUBTRACT, first is read, rest are written; for others, all are read
        match stype.as_str() {
            "MOVE" => {
                if ops.len() >= 2 {
                    usage.entry(ops[0].clone()).or_insert((true, false)); // read
                    usage.entry(ops[1].clone()).or_insert((false, true)); // written
                }
            }
            "ADD" | "SUBTRACT" => {
                if ops.len() >= 2 {
                    usage.entry(ops[0].clone()).or_insert((true, false)); // read
                    for op in &ops[1..] {
                        usage.entry(op.clone()).or_insert((false, true)); // written
                    }
                }
            }
            "READ" | "WRITE" | "OPEN" | "CLOSE" => {
                for op in ops {
                    usage.entry(op.clone()).or_insert((true, false));
                }
            }
            _ => {
                for op in ops {
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
    let re_paragraph = Regex::new(r"^\s*([A-Z0-9-]+)\.").unwrap();
    let mut in_proc = false;
    let mut sections: Vec<ProcedureSection> = Vec::new();
    let mut current_section: Option<ProcedureSection> = None;
    let mut current_paragraph: Option<Paragraph> = None;
    let mut current_paragraph_name = String::new();
    let mut call_graph = Vec::new();
    let mut all_paragraphs: Vec<String> = Vec::new();
    let mut para_name_map: HashMap<String, String> = HashMap::new();
    // Track paragraphs for default section if no explicit section
    let mut default_section_paragraphs: Vec<Paragraph> = Vec::new();
    let mut control_flow_graph = Vec::new();
    let mut all_paragraphs_flat: Vec<Paragraph> = Vec::new();
    for line in source.lines() {
        let line = line.trim_end();
        if !in_proc && re_proc_div.is_match(line) {
            in_proc = true;
            continue;
        }
        if in_proc {
            if re_division.is_match(line) && !re_proc_div.is_match(line) {
                break; // End of procedure division
            }
            if let Some(sec_caps) = re_section.captures(line) {
                // New section
                if let Some(mut sec) = current_section.take() {
                    if let Some(p) = current_paragraph.take() {
                        sec.paragraphs.push(p);
                    }
                    sections.push(sec);
                }
                current_section = Some(ProcedureSection { name: sec_caps.get(1).map(|m| m.as_str().to_string()).unwrap_or_default(), paragraphs: Vec::new() });
                current_paragraph = None;
                current_paragraph_name.clear();
            } else if let Some(para_caps) = re_paragraph.captures(line) {
                // New paragraph
                if let Some(ref mut sec) = current_section {
                    if let Some(p) = current_paragraph.take() {
                        sec.paragraphs.push(p);
                    }
                } else if let Some(p) = current_paragraph.take() {
                    default_section_paragraphs.push(p);
                }
                let name = para_caps.get(1).map(|m| m.as_str().to_string()).unwrap_or_default();
                let norm_name = normalize_name(&name);
                all_paragraphs.push(norm_name.clone());
                para_name_map.insert(norm_name.clone(), name.clone());
                current_paragraph_name = name.clone();
                current_paragraph = Some(Paragraph {
                    name,
                    section: None,
                    kind: "paragraph".to_string(),
                    line: None,
                    source_location: None,
                    statements: Vec::new(),
                    variable_usage: Vec::new(),
                });
            } else if let Some(ref mut para) = current_paragraph {
                let trimmed = line.trim();
                if !trimmed.is_empty() && !trimmed.starts_with('*') {
                    let mut parts = trimmed.split_whitespace();
                    let stype = parts.next().unwrap_or("").to_uppercase();
                    let operands: Vec<String> = parts.clone().map(|s| s.to_string()).collect();
                    // Call graph extraction
                    if stype == "PERFORM" && !operands.is_empty() {
                        // Ignore PERFORM TIMES
                        if operands.iter().any(|s| s.to_uppercase() == "TIMES") {
                            // Do not add to call graph
                        } else if operands.iter().any(|s| s.to_uppercase() == "VARYING") {
                            // PERFORM VARYING ...
                            let from_norm = current_paragraph_name.trim_end_matches('.').to_uppercase();
                            let from_name = para_name_map.get(&from_norm).unwrap_or(&from_norm);
                            if let Some(target) = operands.last() {
                                let target_norm = normalize_name(target);
                                if let Some(to_name) = para_name_map.get(&target_norm) {
                                    eprintln!("[CALL-GRAPH] from={} to={} type=PERFORM VARYING", from_name, to_name);
                                    call_graph.push(CallGraphEntry {
                                        from: from_name.clone(),
                                        to: to_name.clone(),
                                        r#type: "PERFORM VARYING".to_string(),
                                        kind: "edge".to_string(),
                                        line: None,
                                        section: None,
                                        source_location: None,
                                    });
                                } else {
                                    eprintln!("[CALL-GRAPH] Skipping PERFORM VARYING edge: {} -> {} (target not found)", from_name, target_norm);
                                }
                            }
                        } else if operands.len() >= 3 && operands[1].to_uppercase() == "THRU" {
                            // PERFORM <start> THRU <end>
                            let start = operands[0].trim_end_matches('.').to_uppercase();
                            let end = operands[2].trim_end_matches('.').to_uppercase();
                            let from_norm = current_paragraph_name.trim_end_matches('.').to_uppercase();
                            let from_name = para_name_map.get(&from_norm).unwrap_or(&from_norm);
                            let mut in_range = false;
                            for para_norm in &all_paragraphs {
                                if para_norm == &start {
                                    in_range = true;
                                }
                                if in_range {
                                    if let Some(to_name) = para_name_map.get(para_norm) {
                                        eprintln!("[CALL-GRAPH] from={} to={} type=PERFORM", from_name, to_name);
                                        call_graph.push(CallGraphEntry {
                                            from: from_name.clone(),
                                            to: to_name.clone(),
                                            r#type: "PERFORM".to_string(),
                                            kind: "edge".to_string(),
                                            line: None,
                                            section: None,
                                            source_location: None,
                                        });
                                    } else {
                                        eprintln!("[CALL-GRAPH] Skipping PERFORM THRU edge: {} -> {} (target not found)", from_name, para_norm);
                                    }
                                }
                                if para_norm == &end {
                                    break;
                                }
                            }
                        } else {
                            // Handle direct PERFORM <para> and PERFORM ... UNTIL ...
                            let target = operands[0].trim_end_matches('.').to_uppercase();
                            let from_norm = current_paragraph_name.trim_end_matches('.').to_uppercase();
                            let from_name = para_name_map.get(&from_norm).unwrap_or(&from_norm);
                            if !target.is_empty() && para_name_map.contains_key(&target) {
                                if let Some(to_name) = para_name_map.get(&target) {
                                    eprintln!("[CALL-GRAPH] from={} to={} type=PERFORM", from_name, to_name);
                                    call_graph.push(CallGraphEntry {
                                        from: from_name.clone(),
                                        to: to_name.clone(),
                                        r#type: "PERFORM".to_string(),
                                        kind: "edge".to_string(),
                                        line: None,
                                        section: None,
                                        source_location: None,
                                    });
                                } else {
                                    eprintln!("[CALL-GRAPH] Skipping PERFORM edge: {} -> {} (target not found)", from_name, target);
                                }
                            } else {
                                eprintln!("[CALL-GRAPH] Skipping PERFORM edge: {} -> {} (target not found)", from_name, target);
                            }
                        }
                    } else if (stype == "GO" && operands.get(0).map(|s| s.to_uppercase()) == Some("TO".to_string())) || stype == "GOTO" {
                        // GO TO <para> or GOTO <para>
                        let from_norm = current_paragraph_name.trim_end_matches('.').to_uppercase();
                        let from_name = para_name_map.get(&from_norm).unwrap_or(&from_norm);
                        let target = if stype == "GO" { operands.get(1) } else { operands.get(0) };
                        if let Some(target) = target {
                            let target_norm = normalize_name(target);
                            if let Some(to_name) = para_name_map.get(&target_norm) {
                                eprintln!("[CALL-GRAPH] from={} to={} type=GOTO", from_name, to_name);
                                call_graph.push(CallGraphEntry {
                                    from: from_name.clone(),
                                    to: to_name.clone(),
                                    r#type: "GOTO".to_string(),
                                    kind: "edge".to_string(),
                                    line: None,
                                    section: None,
                                    source_location: None,
                                });
                            } else {
                                eprintln!("[CALL-GRAPH] Skipping GOTO edge: {} -> {} (target not found)", from_name, target_norm);
                            }
                        }
                    } else if stype == "CALL" && !operands.is_empty() {
                        // Only handle CALL '<prog>'
                        let mut target = operands[0].trim_end_matches('.').to_string();
                        target = target.trim_matches('"').trim_matches('\'').to_string();
                        let from_norm = current_paragraph_name.trim_end_matches('.').to_uppercase();
                        let from_name = para_name_map.get(&from_norm).unwrap_or(&from_norm);
                        let target_norm = normalize_name(&target);
                        if !target_norm.is_empty() {
                            eprintln!("[CALL-GRAPH] from={} to={} type=CALL", from_name, target_norm);
                            call_graph.push(CallGraphEntry {
                                from: from_name.clone(),
                                to: target_norm.clone(),
                                r#type: "CALL".to_string(),
                                kind: "edge".to_string(),
                                line: None,
                                section: None,
                                source_location: None,
                            });
                        } else {
                            eprintln!("[CALL-GRAPH] Skipping CALL edge: {} -> {} (target not found)", from_name, target_norm);
                        }
                    }
                    // Always push the statement first, then update variable usage
                    para.statements.push(Statement {
                        r#type: stype,
                        operands,
                        raw: trimmed.to_string(),
                        line: None,
                        source_location: None,
                    });
                    let variable_usage = extract_variable_usage(&para.statements);
                    para.variable_usage = variable_usage;
                }
            }
        }
        // When a paragraph is completed (before pushing to section/default):
        if let Some(ref para) = current_paragraph {
            let para_flat = Paragraph {
                name: para.name.clone(),
                section: current_section.as_ref().map(|s| s.name.clone()),
                kind: "paragraph".to_string(),
                line: None, // Optionally parse line number if available
                source_location: None, // Optionally parse source location if available
                statements: para.statements.clone(),
                variable_usage: para.variable_usage.clone(),
            };
            all_paragraphs_flat.push(para_flat);
        }
    }
    // Push last paragraph to section or default
    if let Some(ref mut sec) = current_section {
        if let Some(p) = current_paragraph.take() {
            sec.paragraphs.push(p);
        }
        sections.push(sec.clone());
    } else if let Some(p) = current_paragraph.take() {
        default_section_paragraphs.push(p);
    }
    // If there were any paragraphs not in a section, add a default section
    if !default_section_paragraphs.is_empty() {
        sections.insert(0, ProcedureSection { name: "".to_string(), paragraphs: default_section_paragraphs });
    }
    eprintln!("Paragraph names found:");
    for p in &all_paragraphs {
        eprintln!(" - {}", p);
    }
    // After all paragraphs are parsed:
    // For each paragraph, add NEXT edges between statements, and PERFORM/GOTO edges to their targets
    for sec in &sections {
        for para in &sec.paragraphs {
            let mut prev_stmt: Option<&Statement> = None;
            for stmt in &para.statements {
                if prev_stmt.is_some() {
                    let from_label = format!("{}:{}", para.name, prev_stmt.as_ref().map(|s| s.raw.clone()).unwrap_or_default());
                    let to_label = format!("{}:{}", para.name, stmt.raw.clone());
                    control_flow_graph.push(ControlFlowEdge {
                        from: from_label,
                        to: to_label,
                        r#type: "NEXT".to_string(),
                    });
                }
                // Add PERFORM/GOTO edges
                let stype = stmt.r#type.to_uppercase();
                if stype == "PERFORM" && !stmt.operands.is_empty() {
                    let target = normalize_name(&stmt.operands[0]);
                    control_flow_graph.push(ControlFlowEdge {
                        from: format!("{}:{}", para.name, stmt.raw.clone()),
                        to: format!("{}:{}", target, stmt.raw.clone()),
                        r#type: "PERFORM".to_string(),
                    });
                } else if stype == "GOTO" && !stmt.operands.is_empty() {
                    let target = normalize_name(&stmt.operands[0]);
                    control_flow_graph.push(ControlFlowEdge {
                        from: format!("{}:{}", para.name, stmt.raw.clone()),
                        to: format!("{}:{}", target, stmt.raw.clone()),
                        r#type: "GOTO".to_string(),
                    });
                }
                prev_stmt = Some(stmt);
            }
        }
    }
    (
        ProcedureDivision {
            sections,
        },
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
    let re_select = Regex::new(r"(?i)^\s*SELECT\s+([A-Z0-9-]+)\s+ASSIGN\s+TO\s+('?\w+'?)\s*\.").unwrap();
    let re_select_simple = Regex::new(r"(?i)^\s*SELECT\s+([A-Z0-9-]+)\s+ASSIGN\s+TO\s+([A-Z0-9-]+)\s*\.").unwrap();
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
        if in_env && in_io && re_file_control.is_match(line) {
            continue; // Just a marker
        }
        if in_env && in_io {
            if re_env_div.is_match(line) && !re_io_sec.is_match(line) {
                break; // End of environment division
            }
            if let Some(caps) = re_select.captures(line) {
                let name = caps.get(1).map(|m| m.as_str().to_string()).unwrap_or_default();
                let assigned = caps.get(2).map(|m| m.as_str().trim_matches('"').trim_matches('\'').to_string()).unwrap_or_default();
                // Use file_modes if available
                let r#type = file_modes.get(&name).cloned().unwrap_or_else(|| {
                    if name.contains("IN") { "input".to_string() } else if name.contains("OUT") { "output".to_string() } else { "unknown".to_string() }
                });
                files.push(IOFile {
                    name,
                    r#type,
                    description: format!("Assigned to {}", assigned),
                });
            } else if let Some(caps) = re_select_simple.captures(line) {
                let name = caps.get(1).map(|m| m.as_str().to_string()).unwrap_or_default();
                let assigned = caps.get(2).map(|m| m.as_str().to_string()).unwrap_or_default();
                let r#type = file_modes.get(&name).cloned().unwrap_or_else(|| {
                    if name.contains("IN") { "input".to_string() } else if name.contains("OUT") { "output".to_string() } else { "unknown".to_string() }
                });
                files.push(IOFile {
                    name,
                    r#type,
                    description: format!("Assigned to {}", assigned),
                });
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