use clap::Parser;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{self, BufReader, Write};
use std::path::Path;
use regex::Regex;
use once_cell::sync::Lazy;

#[derive(Parser)]
#[command(author, version, about = "COBOL IR Markdown summary generator")]
struct Args {
    /// Input IR JSON file (default: stdin)
    #[arg(short, long)]
    input: Option<String>,
    
    /// Output Markdown file (default: stdout)
    #[arg(short, long)]
    output: Option<String>,
    
    /// Enable verbose output
    #[arg(short, long)]
    verbose: bool,
    
    /// Enable debug output
    #[arg(long)]
    debug: bool,
}

static COBOL_KEYWORDS: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    [
        "MOVE", "PERFORM", "READ", "WRITE", "DISPLAY", "IF", "ELSE", "END-IF", "UNTIL", "AT", "END", "STOP",
        "CALL", "EXEC", "SQL", "OPEN", "CLOSE", "FETCH", "COMPUTE", "SET", "IS", "NOT", "EQUAL", "THEN",
        "USING", "FROM", "BY", "TO", "VARYING", "AND", "OR", ">", "<", "=", ".", "(", ")", "FUNCTION",
        "INTO", "AFTER", "ADVANCING", "USING", "END-EXEC", "RETURN", "RUN"
    ].iter().cloned().collect()
});

static COBOL_LITERALS: Lazy<HashMap<&'static str, &'static str>> = Lazy::new(|| {
    [
        ("SPACES", "\"  \" (SPACES)"),
        ("SPACE", "\" \" (SPACE)"),
        ("ZERO", "0 (ZERO)"),
        ("ZEROS", "0 (ZEROS)"),
        ("ZEROES", "0 (ZEROES)"),
        ("HIGH-VALUE", "0xFF (HIGH-VALUE)"),
        ("LOW-VALUE", "0x00 (LOW-VALUE)"),
        ("QUOTE", "\" (QUOTE)"),
        ("QUOTES", "\" (QUOTES)"),
        ("NULL", "0 (NULL)"),
    ].iter().cloned().collect()
});

#[derive(Debug, Deserialize)]
struct IR {
    program_name: Option<String>,
    source_file: Option<String>,
    identification_division: Option<IdentificationDivision>,
    environment_division: Option<EnvironmentDivision>,
    data_division: Option<DataDivision>,
    paragraphs: Option<Vec<Paragraph>>,
    call_graph: Option<Vec<CallEdge>>,
    control_flow_graph: Option<Vec<ControlFlowEdge>>,
    procedure_division: Option<ProcedureDivision>,
}

#[derive(Debug, Deserialize)]
struct IdentificationDivision {
    author: Option<String>,
    date_written: Option<String>,
    comments: Option<Vec<String>>,
}

#[derive(Debug, Deserialize)]
struct EnvironmentDivision {
    input_output_section: Option<InputOutputSection>,
}

#[derive(Debug, Deserialize)]
struct InputOutputSection {
    files: Option<Vec<IOFile>>,
}

#[derive(Debug, Deserialize)]
struct IOFile {
    name: String,
    #[serde(rename = "type")]
    file_type: String,
    description: String,
    record_name: Option<String>,
}

#[derive(Debug, Deserialize)]
struct DataDivision {
    working_storage: Option<Vec<DataItem>>,
    file_section: Option<Vec<DataItem>>,
}

#[derive(Debug, Deserialize, Clone)]
struct DataItem {
    name: Option<serde_json::Value>,
    #[serde(rename = "type")]
    item_type: Option<serde_json::Value>,
    level: Option<u32>,
    picture: Option<serde_json::Value>,
    value: Option<serde_json::Value>,
    occurs: Option<u32>,
    redefines: Option<serde_json::Value>,
    comp3: Option<bool>,
    section: Option<serde_json::Value>,
    children: Option<Vec<DataItem>>,
}

#[derive(Debug, Deserialize, Clone)]
struct Paragraph {
    name: Option<String>,
    section: Option<String>,
    kind: Option<String>,
    line: Option<u32>,
    source_location: Option<String>,
    statements: Option<Vec<Statement>>,
    variable_usage: Option<Vec<VariableUsage>>,
}

#[derive(Debug, Deserialize, Clone)]
struct Statement {
    #[serde(rename = "type")]
    statement_type: Option<String>,
    operands: Option<Vec<String>>,
    raw: Option<String>,
    line: Option<u32>,
    source_location: Option<String>,
}

#[derive(Debug, Deserialize, Clone)]
struct VariableUsage {
    name: Option<String>,
    read: Option<bool>,
    written: Option<bool>,
}

#[derive(Debug, Deserialize)]
struct CallEdge {
    from: Option<String>,
    to: Option<String>,
    #[serde(rename = "type")]
    edge_type: Option<String>,
    kind: Option<String>,
    line: Option<u32>,
    section: Option<String>,
    source_location: Option<String>,
}

#[derive(Debug, Deserialize)]
struct ControlFlowEdge {
    from: Option<String>,
    to: Option<String>,
    #[serde(rename = "type")]
    edge_type: Option<String>,
}

#[derive(Debug, Deserialize)]
struct ProcedureDivision {
    sections: Option<Vec<ProcedureSection>>,
}

#[derive(Debug, Deserialize)]
struct ProcedureSection {
    name: Option<String>,
    paragraphs: Option<Vec<Paragraph>>,
}

fn is_valid_identifier(name: &str) -> bool {
    let name = name.trim().trim_end_matches('.').trim_matches('"').trim_matches('\'').to_uppercase();
    if name.is_empty() {
        return false;
    }
    if COBOL_KEYWORDS.contains(name.as_str()) {
        return false;
    }
    if Regex::new(r"^-?\d+(\.\d+)?$").unwrap().is_match(&name) {
        return false;
    }
    if Regex::new(r"^\W+$").unwrap().is_match(&name) {
        return false;
    }
    if !Regex::new(r"^[A-Z0-9\-_]+$").unwrap().is_match(&name) {
        return false;
    }
    true
}

fn is_literal(name: &str) -> bool {
    let n = name.trim();
    if (n.starts_with('\'') && n.ends_with('\'')) || (n.starts_with('"') && n.ends_with('"')) {
        return true;
    }
    if COBOL_LITERALS.contains_key(&n.to_uppercase().as_str()) {
        return true;
    }
    if Regex::new(r"^-?\d+(\.\d+)?$").unwrap().is_match(n) {
        return true;
    }
    if n.starts_with('\'') || n.ends_with('\'') || n.starts_with('"') || n.ends_with('"') {
        return true;
    }
    false
}

fn get_str(val: &Option<serde_json::Value>) -> &str {
    if let Some(serde_json::Value::String(s)) = val {
        s
    } else {
        ""
    }
}

fn format_value(val: &Option<serde_json::Value>) -> String {
    if let Some(v) = val {
        match v {
            serde_json::Value::String(s) => {
                let v_str = s.trim_matches('"').to_uppercase();
                if let Some(literal) = COBOL_LITERALS.get(v_str.as_str()) {
                    return literal.to_string();
                }
                if v_str.starts_with('\'') && v_str.ends_with('\'') {
                    return format!("\"{}\"", v_str.trim_matches('\''));
                }
                if v_str.starts_with('"') && v_str.ends_with('"') {
                    return v_str.to_string();
                }
                return v_str;
            }
            _ => return v.to_string(),
        }
    }
    String::new()
}

fn print_program_info<W: Write>(out: &mut W, ir: &IR) -> io::Result<()> {
    writeln!(out, "# COBOL Program Summary\n")?;
    writeln!(out, "**Program Name:** {}", ir.program_name.as_deref().unwrap_or("UNKNOWN"))?;
    writeln!(out, "**Source File:** {}", ir.source_file.as_deref().unwrap_or("UNKNOWN"))?;

    if let Some(id) = &ir.identification_division {
        writeln!(out, "**Author:** {}", id.author.as_deref().unwrap_or("UNKNOWN"))?;
        writeln!(out, "**Date Written:** {}", id.date_written.as_deref().unwrap_or("UNKNOWN"))?;
        writeln!(out, "**Comments:**")?;
        if let Some(comments) = &id.comments {
            for comment in comments {
                let cleaned = comment.trim();
                if !cleaned.is_empty() {
                    writeln!(out, "- {}", cleaned)?;
                }
            }
        } else {
            writeln!(out, "None")?;
        }
    }
    Ok(())
}

fn print_data_items<W: Write>(out: &mut W, items: &[DataItem], indent: usize, show_section: bool) -> io::Result<()> {
    let mut i = 0;
    while i < items.len() {
        if get_str(&items[i].name).to_uppercase() == "FILLER" {
            let mut count = 1;
            let mut j = i + 1;
            while j < items.len() && get_str(&items[j].name).to_uppercase() == "FILLER" {
                count += 1;
                j += 1;
            }
            if count > 1 {
                writeln!(out, "{}  - FILLER ({} items): Various strings/spaces used for layout.", "  ".repeat(indent), count)?;
            } else {
                writeln!(out, "{}  - FILLER (ignored)", "  ".repeat(indent))?;
            }
            i += count;
            continue;
        }
        let item = &items[i];
        let prefix = "  ".repeat(indent);
        let name = get_str(&item.name);
        let level = item.level.unwrap_or(0);
        let pic = get_str(&item.picture);
        let typ = get_str(&item.item_type);
        let occurs = item.occurs;
        let redefines = get_str(&item.redefines);
        let value = &item.value;
        let comp3 = item.comp3.unwrap_or(false);
        let section = get_str(&item.section);
        let array_str = if let Some(occ) = occurs { format!(" [OCCURS {}]", occ) } else { String::new() };
        let redef_str = if !redefines.is_empty() { format!(" [REDEFINES {}]", redefines) } else { String::new() };
        let mut meta = Vec::new();
        if !pic.is_empty() {
            meta.push(format!("PIC {}", pic));
        }
        if !typ.is_empty() {
            if comp3 {
                meta.push(format!("TYPE packed-decimal (COMP-3) [{}]", typ));
            } else {
                meta.push(format!("TYPE {}", typ));
            }
        }
        if let Some(val) = value {
            let formatted = format_value(&Some(val.clone()));
            if !formatted.is_empty() {
                meta.push(format!("VALUE {}", formatted));
            }
        }
        if show_section {
            if !section.is_empty() {
                meta.push(format!("SECTION {}", section));
            }
        }
        let meta_str = if meta.is_empty() { String::new() } else { format!(" - {}", meta.join("; ")) };
        let occurs_str = array_str;
        writeln!(out, "{}  - **{}** (Level {}){}{}{}", prefix, name, level, occurs_str, redef_str, meta_str)?;
        if let Some(children) = &item.children {
            print_data_items(out, children, indent + 1, show_section)?;
        }
        i += 1;
    }
    Ok(())
}

fn print_working_storage<W: Write>(out: &mut W, ws_vars: &Option<Vec<DataItem>>) -> io::Result<()> {
    writeln!(out, "\n## Working-Storage Variables\n")?;
    if let Some(vars) = ws_vars {
        for item in vars {
            if !get_str(&item.name).is_empty() {
                print_data_items(out, &[item.clone()], 0, false)?;
            }
        }
    } else {
        writeln!(out, "_Declared, but no user-defined items were found (possibly all are imported)._")?;
    }
    writeln!(out, "\n---\n")?;
    Ok(())
}

fn print_file_section<W: Write>(out: &mut W, file_sections: &Option<Vec<DataItem>>) -> io::Result<()> {
    writeln!(out, "\n## File Section\n")?;
    if let Some(sections) = file_sections {
        let level_1_items: Vec<DataItem> = sections.iter()
            .filter(|item| item.level == Some(1))
            .cloned()
            .collect();
        if !level_1_items.is_empty() {
            print_data_items(out, &level_1_items, 0, false)?;
        } else {
            writeln!(out, "_No File Section entries found._")?;
        }
    } else {
        writeln!(out, "_No File Section entries found._")?;
    }
    writeln!(out, "\n---\n")?;
    Ok(())
}

fn print_procedure_division<W: Write>(out: &mut W, ir: &IR, all_paragraphs_out: &mut Vec<(String, String, Option<u32>)>) -> io::Result<()> {
    // Use procedure_division if available, otherwise fall back to paragraphs
    let paragraphs = if let Some(pd) = &ir.procedure_division {
        if let Some(sections) = &pd.sections {
            // If we have sections, extract all paragraphs from all sections
            let mut all_paras = Vec::new();
            for section in sections {
                if let Some(para_list) = &section.paragraphs {
                    all_paras.extend(para_list.clone());
                }
            }
            Some(all_paras)
        } else {
            None
        }
    } else {
        ir.paragraphs.clone()
    };

    if let Some(paras) = paragraphs {
        if paras.is_empty() {
            writeln!(out, "\n_No Procedure Division content found._")?;
            return Ok(());
        }
        
        writeln!(out, "\n## Procedure Division\n")?;
        let mut para_map = HashMap::new();
        let paragraph_names: HashSet<String> = paras.iter()
            .filter_map(|p| p.name.as_ref().map(|n| n.to_uppercase()))
            .collect();
        
        for para in paras {
            let pname = para.name.as_deref().unwrap_or("");
            let section = para.section.as_deref().unwrap_or("");
            let line = para.line;
            let _kind = para.kind.as_deref().unwrap_or("paragraph");
            let key = (pname.to_string(), section.to_string(), line);
            para_map.insert(key.clone(), para.clone());
            if !pname.is_empty() {
                all_paragraphs_out.push((pname.to_string(), section.to_string(), line));
            }
        }
        
        for ((pname, section, line), para) in &para_map {
            let empty_statements = Vec::new();
            let empty_var_usage = Vec::new();
            let statements = para.statements.as_ref().unwrap_or(&empty_statements);
            let var_usage = para.variable_usage.as_ref().unwrap_or(&empty_var_usage);
            
            let mut filtered_vars = Vec::new();
            for vu in var_usage {
                if let Some(name) = &vu.name {
                    let clean_name = name.trim().trim_end_matches('.');
                    if !clean_name.is_empty() && is_valid_identifier(clean_name) && !is_literal(clean_name) && !paragraph_names.contains(&clean_name.to_uppercase()) {
                        filtered_vars.push(vu.clone());
                    }
                }
            }
            
            if pname.is_empty() || (statements.is_empty() && filtered_vars.is_empty()) {
                continue;
            }
            if pname.ends_with("-END") && statements.is_empty() {
                continue;
            }
            
            let line_info = if let Some(l) = line { format!(" (line {})", l) } else { String::new() };
            let src_info = if let Some(src) = &para.source_location { format!(" [{}]", src) } else { String::new() };
            let section_info = if !section.is_empty() { format!(" _(Section: {})_", section) } else { String::new() };
            let kind_info = if para.kind.as_deref().unwrap_or("paragraph") != "paragraph" { 
                format!(" _({})_", para.kind.as_deref().unwrap_or("paragraph")) 
            } else { 
                String::new() 
            };
            
            writeln!(out, "#### Paragraph: **{}**{}{}{}{}", pname, line_info, src_info, section_info, kind_info)?;
            
            if !statements.is_empty() {
                for stmt in statements {
                    let stype = stmt.statement_type.as_deref().unwrap_or("");
                    let raw = stmt.raw.as_deref().unwrap_or("");
                    let stmt_line = stmt.line;
                    let stmt_src = stmt.source_location.as_deref();
                    let empty_operands = Vec::new();
                    let operands = stmt.operands.as_ref().unwrap_or(&empty_operands);
                    
                    let stmt_info = if let Some(l) = stmt_line { format!(" (line {})", l) } else { String::new() };
                    let stmt_src_info = if let Some(src) = stmt_src { format!(" [{}]", src) } else { String::new() };
                    let operands_info = if !operands.is_empty() { format!(" [{}]", operands.join(", ")) } else { String::new() };
                    
                    writeln!(out, "- **{}**: {}{}{}{}", stype, raw, operands_info, stmt_info, stmt_src_info)?;
                }
            } else {
                writeln!(out, "_No logic here_")?;
            }
            
            if !filtered_vars.is_empty() {
                writeln!(out, "\n**Variables Used:**")?;
                writeln!(out, "| Name | Read | Written |")?;
                writeln!(out, "|------|------|---------|")?;
                for vu in &filtered_vars {
                    let name = vu.name.as_deref().unwrap_or("").trim().trim_end_matches('.');
                    let read = if vu.read.unwrap_or(false) { "Yes" } else { "" };
                    let written = if vu.written.unwrap_or(false) { "Yes" } else { "" };
                    writeln!(out, "| **{}** | {} | {} |", name, read, written)?;
                }
            } else {
                writeln!(out, "_Variables used: None._")?;
            }
        }
    }
    writeln!(out, "\n---\n")?;
    Ok(())
}

fn print_unused_paragraphs<W: Write>(out: &mut W, all_paragraphs: &[(String, String, Option<u32>)], call_graph: &Option<Vec<CallEdge>>, paragraphs: &Option<Vec<Paragraph>>) -> io::Result<()> {
    let mut called = HashSet::new();
    if let Some(cg) = call_graph {
        for edge in cg {
            if let Some(edge_type) = &edge.edge_type {
                if ["PERFORM", "GOTO", "PERFORM VARYING"].contains(&edge_type.to_uppercase().as_str()) {
                    if let Some(to) = &edge.to {
                        called.insert(to.to_uppercase());
                    }
                }
            }
        }
    }
    
    let mut used = HashSet::new();
    if !all_paragraphs.is_empty() {
        used.insert(all_paragraphs[0].0.to_uppercase());
        
        if let Some(paras) = paragraphs {
            for i in 0..paras.len().saturating_sub(1) {
                if let Some(current_name) = &paras[i].name {
                    if let Some(next_name) = &paras[i + 1].name {
                        if !current_name.is_empty() && !next_name.is_empty() {
                            called.insert(next_name.to_uppercase());
                        }
                    }
                }
            }
        }
    }
    
    let mut unused_counter = HashMap::new();
    for para in all_paragraphs {
        let pname = &para.0;
        if !called.contains(&pname.to_uppercase()) && !used.contains(&pname.to_uppercase()) {
            let key = (pname.clone(), para.1.clone(), para.2);
            *unused_counter.entry(key).or_insert(0) += 1;
        }
    }
    
    if !unused_counter.is_empty() {
        writeln!(out, "\n## Unused Paragraphs\n")?;
        writeln!(out, "**The following paragraphs are not the target of any PERFORM or GOTO:**\n")?;
        
        let mut sorted_unused: Vec<_> = unused_counter.iter().collect();
        sorted_unused.sort_by_key(|((pname, section, line), _)| (pname, section, line));
        
        for ((pname, section, line), count) in sorted_unused {
            let section_info = if !section.is_empty() { format!(" _(Section: {})_", section) } else { String::new() };
            let line_info = if let Some(l) = line { format!(" _(line {})_", l) } else { String::new() };
            let count_info = if *count > 1 { format!(" _(x{})_", count) } else { String::new() };
            writeln!(out, "- **{}**{}{}{}", pname, section_info, line_info, count_info)?;
        }
    } else {
        writeln!(out, "\n## Unused Paragraphs\n")?;
        writeln!(out, "_No unused paragraphs found._")?;
    }
    writeln!(out, "\n---\n")?;
    Ok(())
}

fn print_external_calls<W: Write>(out: &mut W, call_graph: &Option<Vec<CallEdge>>) -> io::Result<()> {
    let mut calls = HashMap::new();
    if let Some(cg) = call_graph {
        for edge in cg {
            if let Some(edge_type) = &edge.edge_type {
                if edge_type.to_uppercase() == "CALL" {
                    if let Some(prog) = &edge.to {
                        if let Some(from_para) = &edge.from {
                            calls.entry(prog.clone()).or_insert_with(Vec::new).push(from_para.clone());
                        }
                    }
                }
            }
        }
    }
    
    if !calls.is_empty() {
        writeln!(out, "\n## Subprograms / External Calls")?;
        for (prog, from_paras) in calls {
            let mut callers: Vec<_> = from_paras.into_iter().collect::<HashSet<_>>().into_iter().collect();
            callers.sort();
            let callers_str = callers.join(", ");
            writeln!(out, "- Program `{}` (Called from: {})", prog, callers_str)?;
        }
    }
    Ok(())
}

fn sanitize_node_id(name: &str) -> String {
    if name.is_empty() {
        return "UNKNOWN".to_string();
    }
    
    let parts: Vec<&str> = name.split(':').collect();
    let para_name = parts[0].chars().take(20).collect::<String>();
    let mut sanitized = para_name.chars()
        .map(|c| if c.is_alphanumeric() || c == '_' { c } else { '_' })
        .collect::<String>();
    
    if parts.len() > 1 {
        let stmt_part = parts[1].chars().take(20).collect::<String>();
        let sanitized_stmt = stmt_part.chars()
            .map(|c| if c.is_alphanumeric() || c == '_' { c } else { '_' })
            .collect::<String>();
        sanitized.push_str(&format!("_{}", sanitized_stmt));
    }
    
    if sanitized.chars().next().map_or(false, |c| c.is_numeric()) {
        sanitized.insert(0, '_');
    }
    
    sanitized
}

fn print_call_graph<W: Write>(out: &mut W, call_graph: &Option<Vec<CallEdge>>) -> io::Result<()> {
    writeln!(out, "\n## Call Graph\n")?;
    writeln!(out, "> **Legend:** Solid -> PERFORM, Dotted -.-> GOTO, Dashed --|VARYING|--> PERFORM VARYING, Double ==> CALL")?;
    
    if let Some(cg) = call_graph {
        if cg.is_empty() {
            writeln!(out, "\n_No call graph generated (missing or empty in IR)._")?;
            writeln!(out, "```mermaid")?;
            writeln!(out, "flowchart TD")?;
            writeln!(out, "    A[No call graph data]")?;
            writeln!(out, "```")?;
            writeln!(out, "\n---\n")?;
            return Ok(());
        }
        
        writeln!(out, "```mermaid")?;
        writeln!(out, "%% Dotted lines: GOTO; Solid: PERFORM or sequence; Arrows: control flow")?;
        writeln!(out, "flowchart TD")?;
        
        let mut seen_edges = HashSet::new();
        for edge in cg {
            let from_node = sanitize_node_id(edge.from.as_deref().unwrap_or(""));
            let to_node = sanitize_node_id(edge.to.as_deref().unwrap_or(""));
            let edge_type = edge.edge_type.as_deref().unwrap_or("").to_uppercase();
            
            if from_node.is_empty() || to_node.is_empty() {
                continue;
            }
            
            let key = (from_node.clone(), to_node.clone(), edge_type.clone());
            if seen_edges.contains(&key) {
                continue;
            }
            seen_edges.insert(key);
            
            match edge_type.as_str() {
                "CALL" => writeln!(out, "    {} ==>|CALL| {}", from_node, to_node)?,
                "PERFORM" => writeln!(out, "    {} --> {}", from_node, to_node)?,
                "GOTO" => writeln!(out, "    {} -.-> {}", from_node, to_node)?,
                "PERFORM VARYING" => writeln!(out, "    {} --|VARYING|--> {}", from_node, to_node)?,
                _ => writeln!(out, "    {} --> {}", from_node, to_node)?,
            }
        }
        writeln!(out, "```")?;
        
        writeln!(out, "\n### Call Graph Table\n")?;
        writeln!(out, "| **From** | **To** | **Type** | **Kind** | **Line** | **Section** | **Source** |")?;
        writeln!(out, "|------|----|------|------|------|---------|--------|")?;
        for edge in cg {
            writeln!(out, "| **{}** | **{}** | {} | {} | {} | {} | {} |", 
                edge.from.as_deref().unwrap_or(""), 
                edge.to.as_deref().unwrap_or(""), 
                edge.edge_type.as_deref().unwrap_or(""),
                edge.kind.as_deref().unwrap_or("edge"),
                edge.line.map(|l| l.to_string()).unwrap_or_else(|| "".to_string()),
                edge.section.as_deref().unwrap_or(""),
                edge.source_location.as_deref().unwrap_or(""))?;
        }
    }
    writeln!(out, "\n---\n")?;
    Ok(())
}

fn print_control_flow_graph<W: Write>(out: &mut W, cfg: &Option<Vec<ControlFlowEdge>>) -> io::Result<()> {
    writeln!(out, "\n## Control Flow Graph\n")?;
    writeln!(out, "> **Legend:** Solid -> NEXT, Dotted -.-> GOTO, Solid -> PERFORM, Dashed --|VARYING|--> PERFORM VARYING")?;
    
    if let Some(cfg_edges) = cfg {
        if cfg_edges.is_empty() {
            writeln!(out, "\n_No control flow graph generated (missing or empty in IR)._")?;
            writeln!(out, "\n---\n")?;
            return Ok(());
        }
        
        writeln!(out, "```mermaid")?;
        writeln!(out, "%% Dotted lines: GOTO; Solid: PERFORM or sequence; Arrows: control flow")?;
        writeln!(out, "flowchart TD")?;
        
        for edge in cfg_edges {
            let from_node = sanitize_node_id(edge.from.as_deref().unwrap_or(""));
            let to_node = sanitize_node_id(edge.to.as_deref().unwrap_or(""));
            let edge_type = edge.edge_type.as_deref().unwrap_or("").to_uppercase();
            let label = if edge_type != "NEXT" { format!("|{}|", edge_type) } else { String::new() };
            
            match edge_type.as_str() {
                "GOTO" => writeln!(out, "    {} -.-> {}", from_node, to_node)?,
                "PERFORM VARYING" => writeln!(out, "    {} --|VARYING|--> {}", from_node, to_node)?,
                _ => writeln!(out, "    {} --{}--> {}", from_node, label, to_node)?,
            }
        }
        writeln!(out, "```")?;
        
        writeln!(out, "\n### Control Flow Edges Table\n")?;
        writeln!(out, "| **From** | **To** | **Type** |")?;
        writeln!(out, "|------|----|------|")?;
        for edge in cfg_edges {
            writeln!(out, "| **{}** | **{}** | {} |", 
                edge.from.as_deref().unwrap_or(""), 
                edge.to.as_deref().unwrap_or(""), 
                edge.edge_type.as_deref().unwrap_or(""))?;
        }
    }
    writeln!(out, "\n---\n")?;
    Ok(())
}

fn print_io_files<W: Write>(out: &mut W, ir: &IR) -> io::Result<()> {
    if let Some(env) = &ir.environment_division {
        if let Some(io_sec) = &env.input_output_section {
            if let Some(files) = &io_sec.files {
                if !files.is_empty() {
                    writeln!(out, "\n## Environment Division - Input/Output Section\n")?;
                    writeln!(out, "| File Name | Type | Description | Record Structure |")?;
                    writeln!(out, "|-----------|------|-------------|------------------|")?;
                    for f in files {
                        writeln!(out, "| {} | {} | {} | {} |", 
                            f.name, 
                            f.file_type, 
                            f.description, 
                            f.record_name.as_deref().unwrap_or(""))?;
                    }
                } else {
                    writeln!(out, "\n_No Input/Output files found._")?;
                }
            } else {
                writeln!(out, "\n_No Input/Output files found._")?;
            }
        } else {
            writeln!(out, "\n_No Input/Output files found._")?;
        }
    } else {
        writeln!(out, "\n_No Input/Output files found._")?;
    }
    writeln!(out, "\n---\n")?;
    Ok(())
}

fn main() -> io::Result<()> {
    let args = Args::parse();
    
    let ir: IR = if let Some(input_path) = args.input {
        let file = File::open(Path::new(&input_path))?;
        let reader = BufReader::new(file);
        serde_json::from_reader(reader).expect("Failed to parse JSON input")
    } else {
        serde_json::from_reader(io::stdin()).expect("Failed to parse JSON input")
    };
    
    let mut output: Box<dyn Write> = if let Some(output_path) = args.output {
        Box::new(File::create(Path::new(&output_path))?)
    } else {
        Box::new(io::stdout())
    };
    
    if args.debug {
        eprintln!("[DEBUG] IR: {:#?}", ir);
        // Add more detailed debug output here as needed
    } else if args.verbose {
        eprintln!("[VERBOSE] Program name: {}", ir.program_name.as_deref().unwrap_or("UNKNOWN"));
    }
    
    print_program_info(&mut output, &ir)?;
    
    if let Some(data_div) = &ir.data_division {
        print_working_storage(&mut output, &data_div.working_storage)?;
        print_file_section(&mut output, &data_div.file_section)?;
    }
    
    let mut all_paragraphs = Vec::new();
    print_procedure_division(&mut output, &ir, &mut all_paragraphs)?;
    
    print_call_graph(&mut output, &ir.call_graph)?;
    print_control_flow_graph(&mut output, &ir.control_flow_graph)?;
    print_io_files(&mut output, &ir)?;
    print_unused_paragraphs(&mut output, &all_paragraphs, &ir.call_graph, &ir.paragraphs)?;
    print_external_calls(&mut output, &ir.call_graph)?;
    
    Ok(())
} 