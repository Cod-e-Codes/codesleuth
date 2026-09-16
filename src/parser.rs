//! COBOL parser.
use crate::cobol;
use crate::error::Error;
use crate::ir::{
    CallGraphEntry, ControlFlowEdge, DataDivision, DataItem, EnvironmentDivision, IOFile,
    IdentificationDivision, InputOutputSection, Paragraph, ProcedureDivision, ProcedureSection,
    Statement, VariableUsage, IR,
};
use crate::preprocess;
use once_cell::sync::Lazy;
use regex::Regex;
use std::collections::HashMap;
use std::path::Path;

static RE_COPY_ITEM: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*COPY\s+([A-Z0-9-]+)(?:\s+(.*?))?\s*$").unwrap());
static RE_DATA_START: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*(\d{1,2})\s+([A-Z0-9-]+)(?:\s+(.*))?$").unwrap());
static RE_DATA_ENTRY_START: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*\d{1,2}\s+[A-Z0-9-]+").unwrap());
static RE_REDEFINES: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)\bREDEFINES\s+([A-Z0-9-]+)").unwrap());
static RE_OCCURS: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)\bOCCURS\s+(\d+)(?:\s+TIMES)?").unwrap());
static RE_PIC: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)\bPIC(?:TURE)?(?:\s+IS)?\s+(\S+)").unwrap());
static RE_USAGE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?i)(?:\bUSAGE(?:\s+IS)?\s+)?\b(COMP-3|COMPUTATIONAL-3|PACKED-DECIMAL|COMP-1|COMP-2|COMP-4|COMP-5|COMPUTATIONAL|COMP|BINARY|DISPLAY)\b").unwrap()
});
static RE_VALUE: Lazy<Regex> = Lazy::new(|| Regex::new(r"(?i)\bVALUE(?:\s+IS)?\s+(.+)$").unwrap());
static RE_SELECT: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)SELECT\s+([A-Z0-9-]+).*?\bASSIGN\s+TO\s+(\S+)").unwrap());
static RE_FD: Lazy<Regex> = Lazy::new(|| Regex::new(r"(?i)^\s*FD\s+([A-Z0-9-]+)").unwrap());
static RE_01_LEVEL: Lazy<Regex> = Lazy::new(|| Regex::new(r"(?i)^\s*01\s+([A-Z0-9-]+)").unwrap());
static RE_PROGRAM_ID: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*PROGRAM-ID\s*\.\s*(\S+)?").unwrap());
static RE_PROC_DIV: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*PROCEDURE DIVISION\b").unwrap());
static RE_AUTHOR: Lazy<Regex> = Lazy::new(|| Regex::new(r"(?i)^\s*AUTHOR\s*\.\s*(.+)").unwrap());
static RE_DATE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*DATE-WRITTEN\s*\.\s*(.+)").unwrap());
static RE_IDENT_DIV: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*IDENTIFICATION DIVISION\s*\.?\s*$").unwrap());
static RE_END_DIV: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*\w+ DIVISION\s*\.?\s*$").unwrap());
static RE_WS_SECTION: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*WORKING-STORAGE SECTION\s*\.?\s*$").unwrap());
static RE_FILE_SECTION: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*FILE SECTION\s*\.?\s*$").unwrap());
static RE_LINKAGE_SECTION: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*LINKAGE SECTION\s*\.?\s*$").unwrap());
static RE_END_PROGRAM: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*END\s+PROGRAM\s+(\S+)\s*\.?\s*$").unwrap());
static RE_SECTION_END: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(?i)^\s*(WORKING-STORAGE SECTION|FILE SECTION|LINKAGE SECTION|PROCEDURE DIVISION|[A-Z-]+ DIVISION)\b")
        .unwrap()
});
static RE_ANY_DIVISION: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*\w+ DIVISION\s*\.?\s*$").unwrap());
static RE_SECTION_HEADER: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"^\s*([A-Z0-9-]+) SECTION\s*\.\s*$").unwrap());
static RE_PARAGRAPH: Lazy<Regex> = Lazy::new(|| Regex::new(r"^\s*([A-Z0-9-]+)\.\s*$").unwrap());
static RE_OPEN: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)OPEN\s+(INPUT|OUTPUT|I-O|EXTEND)\s+([A-Z0-9-]+)").unwrap());
static RE_ENV_DIV: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*ENVIRONMENT DIVISION\s*\.?\s*$").unwrap());
static RE_IO_SEC: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*INPUT-OUTPUT SECTION\s*\.?\s*$").unwrap());
static RE_FILE_CONTROL: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*FILE-CONTROL\s*\.?\s*$").unwrap());
static RE_ANY_DIV_PREFIX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?i)^\s*[A-Z][A-Z0-9-]* DIVISION\s*\.?").unwrap());

fn infer_type_from_pic(pic: &str) -> String {
    let pic = pic.to_uppercase();
    if pic.contains("COMP-3") || pic.contains("PACKED") {
        return "packed-decimal (COMP-3)".to_string();
    }
    if pic.starts_with('X') || pic.contains("X(") {
        return "string".to_string();
    }
    if pic.contains('9') || pic.contains('$') || pic.contains('Z') {
        return "numeric".to_string();
    }
    if pic.contains("COMP") {
        return "binary".to_string();
    }
    "unknown".to_string()
}

fn usage_is_comp3(usage: &str) -> bool {
    let u = usage.to_uppercase();
    u.contains("COMP-3") || u.contains("PACKED") || u.contains("COMPUTATIONAL-3")
}

fn infer_item_type(pic: Option<&str>, usage: Option<&str>) -> Option<String> {
    if let Some(u) = usage {
        if usage_is_comp3(u) {
            return Some("packed-decimal (COMP-3)".to_string());
        }
        let u = u.to_uppercase();
        if u.contains("COMP") || u == "BINARY" {
            return Some("binary".to_string());
        }
    }
    pic.map(infer_type_from_pic)
}

fn item_is_comp3(pic: Option<&str>, usage: Option<&str>) -> bool {
    usage.is_some_and(usage_is_comp3)
        || pic.is_some_and(|p| {
            let u = p.to_uppercase();
            u.contains("COMP-3") || u.contains("PACKED")
        })
}

fn is_cobol_comment_or_blank(line: &str) -> bool {
    cobol::is_cobol_comment_or_blank(line)
}

fn identification_comment_text(line: &str) -> Option<String> {
    if !is_cobol_comment_or_blank(line) {
        return None;
    }
    let t = line.trim();
    if t.is_empty() || !t.starts_with('*') {
        return None;
    }
    let body = t
        .trim_start_matches('*')
        .trim()
        .trim_end_matches('*')
        .trim();
    if body.is_empty() {
        return None;
    }
    if body
        .chars()
        .all(|c| matches!(c, '-' | '=' | '+' | '*' | '.' | '_' | ' '))
    {
        return None;
    }
    Some(body.to_string())
}

fn has_terminator_period(s: &str) -> bool {
    cobol::has_terminator_period(s)
}

fn strip_terminator(s: &str) -> &str {
    cobol::strip_terminator(s)
}

fn starts_data_entry(line: &str) -> bool {
    let t = line.trim_start();
    t.to_uppercase().starts_with("COPY ") || RE_DATA_ENTRY_START.is_match(t)
}

fn parse_one_data_item(text: &str, section_name: Option<&str>) -> Option<DataItem> {
    let text = strip_terminator(text).trim();
    if text.is_empty() {
        return None;
    }
    let section = section_name.map(|s| s.to_string());
    if let Some(caps) = RE_COPY_ITEM.captures(text) {
        let name = caps.get(1).map(|m| m.as_str().to_string())?;
        let rest = caps
            .get(2)
            .map(|m| m.as_str().trim())
            .filter(|s| !s.is_empty())
            .map(|s| s.to_string());
        return Some(DataItem {
            name,
            level: 1,
            picture: None,
            r#type: Some("copybook".to_string()),
            value: rest,
            occurs: None,
            redefines: None,
            comp3: false,
            section,
            children: Vec::new(),
        });
    }
    let caps = RE_DATA_START.captures(text)?;
    let level = caps.get(1)?.as_str().parse::<i32>().ok()?;
    let name = caps.get(2)?.as_str().to_string();
    let rest = caps.get(3).map(|m| m.as_str()).unwrap_or("");
    let redefines = RE_REDEFINES
        .captures(rest)
        .and_then(|c| c.get(1).map(|m| m.as_str().to_string()));
    let occurs = RE_OCCURS
        .captures(rest)
        .and_then(|c| c.get(1).and_then(|m| m.as_str().parse::<usize>().ok()));
    let picture = RE_PIC.captures(rest).and_then(|c| {
        c.get(1)
            .map(|m| m.as_str().trim_end_matches('.').to_string())
    });
    let usage = RE_USAGE
        .captures(rest)
        .and_then(|c| c.get(1).map(|m| m.as_str().to_string()));
    let value = RE_VALUE.captures(rest).and_then(|c| {
        c.get(1)
            .map(|m| m.as_str().trim().trim_end_matches('.').trim().to_string())
            .filter(|s| !s.is_empty())
    });
    let r#type = infer_item_type(picture.as_deref(), usage.as_deref());
    let comp3 = item_is_comp3(picture.as_deref(), usage.as_deref());
    Some(DataItem {
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
    })
}

fn parse_identification_division(source: &str) -> (String, String, String, Vec<String>) {
    let mut program_name = String::new();
    let mut author = String::new();
    let mut date_written = String::new();
    let mut comments = Vec::new();
    let mut in_ident = false;
    let mut pending_program_id = false;
    for line in source.lines() {
        let line = line.trim_end();
        if !in_ident && RE_IDENT_DIV.is_match(line) {
            in_ident = true;
            continue;
        }
        if in_ident {
            if RE_END_DIV.is_match(line) && !RE_IDENT_DIV.is_match(line) {
                break;
            }
            if let Some(caps) = RE_PROGRAM_ID.captures(line) {
                let name = caps
                    .get(1)
                    .map(|m| m.as_str().trim_end_matches('.').trim())
                    .filter(|s| !s.is_empty());
                if let Some(name) = name {
                    program_name = name.to_string();
                    pending_program_id = false;
                } else {
                    pending_program_id = true;
                }
            } else if pending_program_id {
                if is_cobol_comment_or_blank(line) {
                    if let Some(text) = identification_comment_text(line) {
                        comments.push(text);
                    }
                } else if let Some(tok) = line.split_whitespace().next() {
                    program_name = tok.trim_end_matches('.').to_string();
                    pending_program_id = false;
                }
            } else if let Some(caps) = RE_AUTHOR.captures(line) {
                author = caps
                    .get(1)
                    .map(|m| m.as_str().trim_end_matches('.').to_string())
                    .unwrap_or_default();
            } else if let Some(caps) = RE_DATE.captures(line) {
                date_written = caps
                    .get(1)
                    .map(|m| m.as_str().trim_end_matches('.').to_string())
                    .unwrap_or_default();
            } else if let Some(text) = identification_comment_text(line) {
                comments.push(text);
            }
        }
    }
    (program_name, author, date_written, comments)
}

fn extract_section_lines<'a>(source: &'a str, section: &str) -> Vec<&'a str> {
    let re_start = if section.eq_ignore_ascii_case("WORKING-STORAGE SECTION") {
        &RE_WS_SECTION
    } else if section.eq_ignore_ascii_case("FILE SECTION") {
        &RE_FILE_SECTION
    } else if section.eq_ignore_ascii_case("LINKAGE SECTION") {
        &RE_LINKAGE_SECTION
    } else {
        return Vec::new();
    };
    let mut lines = Vec::new();
    let mut in_section = false;
    for line in source.lines() {
        if !in_section && re_start.is_match(line) {
            in_section = true;
            continue;
        }
        if in_section {
            if RE_SECTION_END.is_match(line) && !re_start.is_match(line) {
                break;
            }
            lines.push(line);
        }
    }
    lines
}

fn parse_data_items(section_lines: &[&str], section_name: Option<&str>) -> Vec<DataItem> {
    let mut logical = Vec::new();
    let mut pending = String::new();
    for line in section_lines {
        if is_cobol_comment_or_blank(line) {
            continue;
        }
        let trimmed = line.trim();
        if pending.is_empty() {
            if starts_data_entry(trimmed) {
                pending.push_str(trimmed);
            } else {
                continue;
            }
        } else {
            pending.push(' ');
            pending.push_str(trimmed);
        }
        if has_terminator_period(&pending) {
            logical.push(std::mem::take(&mut pending));
        }
    }
    if !pending.trim().is_empty() {
        logical.push(pending);
    }
    let parsed: Vec<DataItem> = logical
        .iter()
        .filter_map(|text| parse_one_data_item(text, section_name))
        .collect();
    let mut stack: Vec<(i32, DataItem)> = Vec::new();
    let mut result: Vec<DataItem> = Vec::new();
    for item in parsed {
        if item.level == 77 {
            while let Some((_, completed)) = stack.pop() {
                if let Some((_, parent)) = stack.last_mut() {
                    parent.children.push(completed);
                } else {
                    result.push(completed);
                }
            }
            stack.push((item.level, item));
            continue;
        }
        while let Some((parent_level, _)) = stack.last() {
            if *parent_level < item.level {
                break;
            }
            let (_, completed) = stack.pop().unwrap();
            if let Some((_, parent)) = stack.last_mut() {
                parent.children.push(completed);
            } else {
                result.push(completed);
            }
        }
        stack.push((item.level, item));
    }
    while let Some((_, item)) = stack.pop() {
        if let Some((_, parent)) = stack.last_mut() {
            parent.children.push(item);
        } else {
            result.push(item);
        }
    }
    result
}

fn extract_variable_usage(
    statements: &[Statement],
    para_name_map: &HashMap<String, String>,
) -> Vec<VariableUsage> {
    let mut usage: HashMap<String, (bool, bool)> = HashMap::new();
    for stmt in statements {
        let stype = stmt.r#type.to_uppercase();
        let ops = &stmt.operands;
        let filtered_ops: Vec<_> = ops
            .iter()
            .filter(|op| {
                let opu = cobol::normalize_name(op);
                cobol::is_valid_identifier(op)
                    && !cobol::is_literal(op)
                    && !para_name_map.contains_key(&opu)
            })
            .cloned()
            .collect();
        match stype.as_str() {
            "MOVE" => {
                if filtered_ops.len() >= 2 {
                    usage
                        .entry(filtered_ops[0].clone())
                        .or_insert((true, false));
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
    let mut rows: Vec<VariableUsage> = usage
        .into_iter()
        .map(|(name, (read, written))| VariableUsage {
            name,
            read,
            written,
        })
        .collect();
    rows.sort_by(|a, b| a.name.cmp(&b.name));
    rows
}

fn procedure_region_end(line: &str) -> bool {
    RE_END_PROGRAM.is_match(line) || (RE_ANY_DIVISION.is_match(line) && !RE_PROC_DIV.is_match(line))
}

fn starts_exec_sql(line: &str) -> bool {
    let toks = cobol::split_cobol_tokens(line);
    toks.len() >= 2 && toks[0].eq_ignore_ascii_case("EXEC") && toks[1].eq_ignore_ascii_case("SQL")
}

fn ends_exec(line: &str) -> bool {
    line.to_uppercase().contains("END-EXEC")
}

fn record_exec_sql(para: &mut Paragraph, raw: &str, line_no: usize) {
    let upper = raw.to_uppercase();
    let sql = if let (Some(a), Some(b)) = (upper.find("EXEC SQL"), upper.rfind("END-EXEC")) {
        raw[a + 8..b].trim().to_string()
    } else {
        raw.trim().to_string()
    };
    para.statements.push(Statement {
        r#type: "EXEC SQL".to_string(),
        operands: vec![sql],
        raw: raw.trim().to_string(),
        line: Some(line_no),
        source_location: None,
    });
}

fn consume_procedure_header(line: &str, in_proc: &mut bool, skip_using: &mut bool) -> bool {
    if !*in_proc && RE_PROC_DIV.is_match(line) {
        *in_proc = true;
        if line.to_uppercase().contains("USING") && !has_terminator_period(line) {
            *skip_using = true;
        }
        return true;
    }
    if *skip_using {
        if has_terminator_period(line) {
            *skip_using = false;
        }
        return true;
    }
    false
}

fn implicit_paragraph_name(program_name: &str) -> String {
    if program_name.is_empty() {
        "MAIN".to_string()
    } else {
        program_name.to_string()
    }
}

fn finish_paragraph(
    current_paragraph: &mut Option<Paragraph>,
    current_section: &mut Option<ProcedureSection>,
    default_section_paragraphs: &mut Vec<Paragraph>,
    all_paragraphs_flat: &mut Vec<Paragraph>,
    para_name_map: &HashMap<String, String>,
) {
    if let Some(mut p) = current_paragraph.take() {
        p.variable_usage = extract_variable_usage(&p.statements, para_name_map);
        if let Some(sec) = current_section.as_mut() {
            sec.paragraphs.push(p.clone());
        } else {
            default_section_paragraphs.push(p.clone());
        }
        all_paragraphs_flat.push(p);
    }
}

fn ensure_paragraph(
    current_paragraph: &mut Option<Paragraph>,
    current_paragraph_name: &mut String,
    current_section: &Option<ProcedureSection>,
    program_name: &str,
    line_no: usize,
) {
    if current_paragraph.is_some() {
        return;
    }
    let name = implicit_paragraph_name(program_name);
    *current_paragraph_name = name.clone();
    *current_paragraph = Some(Paragraph {
        name,
        section: current_section.as_ref().map(|s| s.name.clone()),
        kind: "paragraph".to_string(),
        line: Some(line_no),
        source_location: None,
        statements: Vec::new(),
        variable_usage: Vec::new(),
    });
}

fn record_statement(
    para: &mut Paragraph,
    current_paragraph_name: &str,
    current_section: &Option<ProcedureSection>,
    line: &str,
    line_no: usize,
    para_name_map: &HashMap<String, String>,
    call_graph: &mut Vec<CallGraphEntry>,
) {
    let trimmed = line.trim();
    let mut parts = cobol::split_cobol_tokens(trimmed);
    let stype = parts
        .first()
        .cloned()
        .unwrap_or_default()
        .trim_end_matches('.')
        .to_uppercase();
    if !parts.is_empty() {
        parts.remove(0);
    }
    let operands: Vec<String> = parts;
    let stmt = Statement {
        r#type: stype.clone(),
        operands: operands.clone(),
        raw: trimmed.to_string(),
        line: Some(line_no),
        source_location: None,
    };
    para.statements.push(stmt);
    if stype == "PERFORM" && !operands.is_empty() {
        let target = cobol::normalize_name(&operands[0]);
        if let Some(to_name) = para_name_map.get(&target) {
            call_graph.push(CallGraphEntry {
                from: current_paragraph_name.to_string(),
                to: to_name.clone(),
                r#type: if operands.iter().any(|s| s.eq_ignore_ascii_case("UNTIL")) {
                    "PERFORM VARYING".to_string()
                } else {
                    "PERFORM".to_string()
                },
                kind: "edge".to_string(),
                line: Some(line_no),
                section: current_section.as_ref().map(|s| s.name.clone()),
                source_location: None,
            });
        }
    } else if (stype == "GO"
        && operands
            .first()
            .is_some_and(|s| s.eq_ignore_ascii_case("TO")))
        || stype == "GOTO"
    {
        let target = if stype == "GO" {
            operands.get(1)
        } else {
            operands.first()
        };
        if let Some(target) = target {
            let target_norm = cobol::normalize_name(target);
            if let Some(to_name) = para_name_map.get(&target_norm) {
                call_graph.push(CallGraphEntry {
                    from: current_paragraph_name.to_string(),
                    to: to_name.clone(),
                    r#type: "GOTO".to_string(),
                    kind: "edge".to_string(),
                    line: Some(line_no),
                    section: current_section.as_ref().map(|s| s.name.clone()),
                    source_location: None,
                });
            }
        }
    } else if stype == "CALL" && !operands.is_empty() {
        let mut target = operands[0].trim_end_matches('.').to_string();
        target = target.trim_matches('"').trim_matches('\'').to_string();
        let target_norm = cobol::normalize_name(&target);
        if !target_norm.is_empty() {
            call_graph.push(CallGraphEntry {
                from: current_paragraph_name.to_string(),
                to: target_norm,
                r#type: "CALL".to_string(),
                kind: "edge".to_string(),
                line: Some(line_no),
                section: current_section.as_ref().map(|s| s.name.clone()),
                source_location: None,
            });
        }
    }
}

fn statement_first_token(line: &str) -> String {
    cobol::split_cobol_tokens(line)
        .into_iter()
        .next()
        .map(|s| s.trim_end_matches('.').to_uppercase())
        .unwrap_or_default()
}

struct StmtCtx<'a> {
    para: &'a mut Option<Paragraph>,
    paragraph_name: &'a str,
    section: &'a Option<ProcedureSection>,
    para_name_map: &'a HashMap<String, String>,
    call_graph: &'a mut Vec<CallGraphEntry>,
}

fn flush_pending_statement(pending: &mut Option<(usize, String)>, ctx: &mut StmtCtx<'_>) {
    if let Some((line_no, raw)) = pending.take() {
        if let Some(ref mut p) = ctx.para {
            record_statement(
                p,
                ctx.paragraph_name,
                ctx.section,
                &raw,
                line_no,
                ctx.para_name_map,
                ctx.call_graph,
            );
        }
    }
}

fn accumulate_statement(
    pending: &mut Option<(usize, String)>,
    line: &str,
    line_no: usize,
    ctx: &mut StmtCtx<'_>,
) {
    let tok = statement_first_token(line);
    if pending.is_some() && cobol::is_statement_verb(&tok) {
        flush_pending_statement(pending, ctx);
    }
    if let Some((_, buf)) = pending.as_mut() {
        buf.push(' ');
        buf.push_str(line.trim());
    } else {
        *pending = Some((line_no, line.trim().to_string()));
    }
    if pending
        .as_ref()
        .is_some_and(|(_, raw)| has_terminator_period(raw))
    {
        flush_pending_statement(pending, ctx);
    }
}

fn parse_procedure_division_and_call_graph(
    source: &str,
    program_name: &str,
) -> (
    ProcedureDivision,
    Vec<CallGraphEntry>,
    Vec<ControlFlowEdge>,
    Vec<Paragraph>,
) {
    let re_section = &RE_SECTION_HEADER;
    let re_paragraph = &RE_PARAGRAPH;
    let skip_paragraphs = [
        "END-IF",
        "END-READ",
        "END-EVALUATE",
        "END-PERFORM",
        "GOBACK",
    ];
    let mut in_proc = false;
    let mut skip_using = false;
    let mut para_name_map: HashMap<String, String> = HashMap::new();
    for line in source.lines() {
        let line = line.trim_end();
        if consume_procedure_header(line, &mut in_proc, &mut skip_using) {
            continue;
        }
        if in_proc {
            if procedure_region_end(line) {
                break;
            }
            if let Some(para_caps) = re_paragraph.captures(line) {
                let name = para_caps
                    .get(1)
                    .map(|m| m.as_str().to_string())
                    .unwrap_or_default();
                if skip_paragraphs.contains(&name.as_str()) {
                    continue;
                }
                let norm_name = cobol::normalize_name(&name);
                para_name_map.insert(norm_name, name);
            }
        }
    }
    in_proc = false;
    skip_using = false;
    let mut sections: Vec<ProcedureSection> = Vec::new();
    let mut current_section: Option<ProcedureSection> = None;
    let mut current_paragraph: Option<Paragraph> = None;
    let mut current_paragraph_name = String::new();
    let mut call_graph = Vec::new();
    let mut default_section_paragraphs: Vec<Paragraph> = Vec::new();
    let mut control_flow_graph = Vec::new();
    let mut all_paragraphs_flat: Vec<Paragraph> = Vec::new();
    let mut exec_sql: Option<(usize, String)> = None;
    let mut stmt_pending: Option<(usize, String)> = None;
    for (i, line) in source.lines().enumerate() {
        let line = line.trim_end();
        if consume_procedure_header(line, &mut in_proc, &mut skip_using) {
            continue;
        }
        if in_proc {
            if procedure_region_end(line) {
                break;
            }
            if let Some((start, mut buf)) = exec_sql.take() {
                buf.push(' ');
                buf.push_str(line.trim());
                if ends_exec(line) {
                    ensure_paragraph(
                        &mut current_paragraph,
                        &mut current_paragraph_name,
                        &current_section,
                        program_name,
                        start,
                    );
                    if let Some(ref mut para) = current_paragraph {
                        record_exec_sql(para, &buf, start);
                    }
                } else {
                    exec_sql = Some((start, buf));
                }
                continue;
            }
            if starts_exec_sql(line) {
                flush_pending_statement(
                    &mut stmt_pending,
                    &mut StmtCtx {
                        para: &mut current_paragraph,
                        paragraph_name: &current_paragraph_name,
                        section: &current_section,
                        para_name_map: &para_name_map,
                        call_graph: &mut call_graph,
                    },
                );
                if ends_exec(line) {
                    ensure_paragraph(
                        &mut current_paragraph,
                        &mut current_paragraph_name,
                        &current_section,
                        program_name,
                        i + 1,
                    );
                    if let Some(ref mut para) = current_paragraph {
                        record_exec_sql(para, line, i + 1);
                    }
                } else {
                    exec_sql = Some((i + 1, line.trim().to_string()));
                }
                continue;
            }
            if let Some(sec_caps) = re_section.captures(line) {
                flush_pending_statement(
                    &mut stmt_pending,
                    &mut StmtCtx {
                        para: &mut current_paragraph,
                        paragraph_name: &current_paragraph_name,
                        section: &current_section,
                        para_name_map: &para_name_map,
                        call_graph: &mut call_graph,
                    },
                );
                finish_paragraph(
                    &mut current_paragraph,
                    &mut current_section,
                    &mut default_section_paragraphs,
                    &mut all_paragraphs_flat,
                    &para_name_map,
                );
                if let Some(sec) = current_section.take() {
                    sections.push(sec);
                }
                current_section = Some(ProcedureSection {
                    name: sec_caps
                        .get(1)
                        .map(|m| m.as_str().to_string())
                        .unwrap_or_default(),
                    paragraphs: Vec::new(),
                });
                current_paragraph_name.clear();
            } else if let Some(para_caps) = re_paragraph.captures(line) {
                let name = para_caps
                    .get(1)
                    .map(|m| m.as_str().to_string())
                    .unwrap_or_default();
                if skip_paragraphs.contains(&name.as_str()) {
                    ensure_paragraph(
                        &mut current_paragraph,
                        &mut current_paragraph_name,
                        &current_section,
                        program_name,
                        i + 1,
                    );
                    accumulate_statement(
                        &mut stmt_pending,
                        line,
                        i + 1,
                        &mut StmtCtx {
                            para: &mut current_paragraph,
                            paragraph_name: &current_paragraph_name,
                            section: &current_section,
                            para_name_map: &para_name_map,
                            call_graph: &mut call_graph,
                        },
                    );
                    continue;
                }
                flush_pending_statement(
                    &mut stmt_pending,
                    &mut StmtCtx {
                        para: &mut current_paragraph,
                        paragraph_name: &current_paragraph_name,
                        section: &current_section,
                        para_name_map: &para_name_map,
                        call_graph: &mut call_graph,
                    },
                );
                finish_paragraph(
                    &mut current_paragraph,
                    &mut current_section,
                    &mut default_section_paragraphs,
                    &mut all_paragraphs_flat,
                    &para_name_map,
                );
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
            } else if !is_cobol_comment_or_blank(line) && line.trim() != "." {
                ensure_paragraph(
                    &mut current_paragraph,
                    &mut current_paragraph_name,
                    &current_section,
                    program_name,
                    i + 1,
                );
                accumulate_statement(
                    &mut stmt_pending,
                    line,
                    i + 1,
                    &mut StmtCtx {
                        para: &mut current_paragraph,
                        paragraph_name: &current_paragraph_name,
                        section: &current_section,
                        para_name_map: &para_name_map,
                        call_graph: &mut call_graph,
                    },
                );
            }
        }
    }
    flush_pending_statement(
        &mut stmt_pending,
        &mut StmtCtx {
            para: &mut current_paragraph,
            paragraph_name: &current_paragraph_name,
            section: &current_section,
            para_name_map: &para_name_map,
            call_graph: &mut call_graph,
        },
    );
    finish_paragraph(
        &mut current_paragraph,
        &mut current_section,
        &mut default_section_paragraphs,
        &mut all_paragraphs_flat,
        &para_name_map,
    );
    if let Some(sec) = current_section {
        sections.push(sec);
    }
    if !default_section_paragraphs.is_empty() {
        sections.insert(
            0,
            ProcedureSection {
                name: "".to_string(),
                paragraphs: default_section_paragraphs,
            },
        );
    }
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
                    let target = cobol::normalize_name(&stmt.operands[0]);
                    if let Some(to_name) = para_name_map.get(&target) {
                        control_flow_graph.push(ControlFlowEdge {
                            from: format!("{}:{}", para.name, stmt.raw),
                            to: format!("{}:{}", to_name, stmt.raw),
                            r#type: if stmt
                                .operands
                                .iter()
                                .any(|s| s.eq_ignore_ascii_case("UNTIL"))
                            {
                                "PERFORM VARYING".to_string()
                            } else {
                                "PERFORM".to_string()
                            },
                        });
                    }
                } else if stype == "GOTO" && !stmt.operands.is_empty() {
                    let target = cobol::normalize_name(&stmt.operands[0]);
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

fn parse_open_statements(source: &str) -> HashMap<String, String> {
    let mut file_modes = HashMap::new();
    for line in source.lines() {
        if let Some(caps) = RE_OPEN.captures(line) {
            let mode = caps
                .get(1)
                .map(|m| m.as_str().to_lowercase())
                .unwrap_or("unknown".to_string());
            let file = caps
                .get(2)
                .map(|m| m.as_str().to_string())
                .unwrap_or_default();
            file_modes.insert(file, mode);
        }
    }
    file_modes
}

fn push_select_file(
    files: &mut Vec<IOFile>,
    select_buf: &str,
    file_modes: &HashMap<String, String>,
) {
    let text = strip_terminator(select_buf);
    let Some(caps) = RE_SELECT.captures(text) else {
        return;
    };
    let name = caps
        .get(1)
        .map(|m| m.as_str().to_string())
        .unwrap_or_default();
    let assigned = caps
        .get(2)
        .map(|m| {
            m.as_str()
                .trim_matches('"')
                .trim_matches('\'')
                .trim_end_matches(',')
                .to_string()
        })
        .unwrap_or_default();
    let r#type = file_modes.get(&name).cloned().unwrap_or_else(|| {
        if name.contains("IN") {
            "input".to_string()
        } else if name.contains("OUT") {
            "output".to_string()
        } else {
            "unknown".to_string()
        }
    });
    files.push(IOFile {
        name,
        r#type,
        description: format!("Assigned to {}", assigned),
        record_name: None,
    });
}

fn attach_record_names(files: &mut [IOFile], file_section_lines: &[&str]) {
    let mut current_fd: Option<String> = None;
    let mut waiting_01 = false;
    for line in file_section_lines {
        if is_cobol_comment_or_blank(line) {
            continue;
        }
        if let Some(caps) = RE_FD.captures(line) {
            current_fd = Some(caps[1].to_string());
            waiting_01 = true;
            continue;
        }
        if waiting_01 {
            if let Some(caps) = RE_01_LEVEL.captures(line) {
                let record_name = caps[1].to_string();
                if let Some(fd) = &current_fd {
                    for file in files.iter_mut() {
                        if file.name.eq_ignore_ascii_case(fd) {
                            file.record_name = Some(record_name.clone());
                        }
                    }
                }
                waiting_01 = false;
            } else if line.trim_start().to_uppercase().starts_with("COPY") {
                waiting_01 = false;
            }
        }
    }
}

fn parse_input_output_section(source: &str, file_modes: &HashMap<String, String>) -> Vec<IOFile> {
    let mut files = Vec::new();
    let mut in_env = false;
    let mut in_io = false;
    let mut select_buf = String::new();
    for line in source.lines() {
        let line = line.trim_end();
        if !in_env && RE_ENV_DIV.is_match(line) {
            in_env = true;
            continue;
        }
        if in_env && !in_io && RE_IO_SEC.is_match(line) {
            in_io = true;
            continue;
        }
        if in_env && in_io {
            if RE_ANY_DIV_PREFIX.is_match(line) && !RE_ENV_DIV.is_match(line) {
                if !select_buf.is_empty() {
                    push_select_file(&mut files, &select_buf, file_modes);
                }
                break;
            }
            if RE_FILE_CONTROL.is_match(line) || is_cobol_comment_or_blank(line) {
                continue;
            }
            let trimmed = line.trim();
            let upper = trimmed.to_uppercase();
            if select_buf.is_empty() {
                if upper.starts_with("SELECT ") || upper == "SELECT" {
                    select_buf.push_str(trimmed);
                }
            } else {
                select_buf.push(' ');
                select_buf.push_str(trimmed);
            }
            if !select_buf.is_empty() && has_terminator_period(&select_buf) {
                push_select_file(&mut files, &select_buf, file_modes);
                select_buf.clear();
            }
        }
    }
    if !select_buf.is_empty() {
        push_select_file(&mut files, &select_buf, file_modes);
    }
    let file_section_lines = extract_section_lines(source, "FILE SECTION");
    attach_record_names(&mut files, &file_section_lines);
    files
}

pub fn parse_cobol_file(path: impl AsRef<Path>, verbose: bool, debug: bool) -> Result<IR, Error> {
    let path = path.as_ref();
    let file_content = std::fs::read_to_string(path).map_err(|source| Error::Io {
        path: path.to_path_buf(),
        source,
    })?;
    parse_cobol_source(&file_content, path, verbose, debug)
}

pub fn parse_cobol_source(
    source: &str,
    path: impl AsRef<Path>,
    verbose: bool,
    debug: bool,
) -> Result<IR, Error> {
    let path = path.as_ref();
    let prepared = preprocess::preprocess(source, path);
    parse_program_tree(&prepared, path, verbose, debug)
}

fn parse_program_tree(source: &str, path: &Path, verbose: bool, debug: bool) -> Result<IR, Error> {
    let (this_src, nested_srcs) = split_current_and_nested(source);
    let mut ir = parse_one_program(&this_src, path, verbose, debug)?;
    for nested in nested_srcs {
        ir.nested_programs
            .push(parse_program_tree(&nested, path, verbose, debug)?);
    }
    Ok(ir)
}

fn first_program_id(source: &str) -> String {
    let (name, _, _, _) = parse_identification_division(source);
    name
}

fn split_current_and_nested(source: &str) -> (String, Vec<String>) {
    let lines: Vec<&str> = source.lines().collect();
    let mut ident_at = None;
    for (i, line) in lines.iter().enumerate() {
        if RE_IDENT_DIV.is_match(line) {
            ident_at = Some(i);
            break;
        }
    }
    let Some(ident_at) = ident_at else {
        return (source.to_string(), Vec::new());
    };
    let program_name = first_program_id(source);
    let mut current: Vec<&str> = lines[..ident_at].to_vec();
    let mut nested = Vec::new();
    let mut seen_procedure = false;
    let mut i = ident_at;
    while i < lines.len() {
        let line = lines[i];
        if seen_procedure && RE_IDENT_DIV.is_match(line) {
            let (src, consumed) = collect_nested_program(&lines[i..]);
            nested.push(src);
            i += consumed;
            continue;
        }
        if let Some(caps) = RE_END_PROGRAM.captures(line) {
            let ended = caps
                .get(1)
                .map(|m| m.as_str().trim_end_matches('.'))
                .unwrap_or("");
            if !program_name.is_empty() && ended.eq_ignore_ascii_case(&program_name) {
                current.push(line);
                i += 1;
                break;
            }
        }
        if RE_PROC_DIV.is_match(line) {
            seen_procedure = true;
        }
        current.push(line);
        i += 1;
    }
    while i < lines.len() {
        if RE_IDENT_DIV.is_match(lines[i]) {
            let (src, consumed) = collect_nested_program(&lines[i..]);
            nested.push(src);
            i += consumed;
        } else {
            i += 1;
        }
    }
    (current.join("\n"), nested)
}

fn collect_nested_program(lines: &[&str]) -> (String, usize) {
    let mut depth = 0usize;
    let mut buf = Vec::new();
    for (i, line) in lines.iter().enumerate() {
        if RE_IDENT_DIV.is_match(line) {
            depth += 1;
        }
        buf.push(*line);
        if RE_END_PROGRAM.is_match(line) {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                return (buf.join("\n"), i + 1);
            }
        }
    }
    (buf.join("\n"), lines.len())
}

fn parse_one_program(source: &str, path: &Path, verbose: bool, debug: bool) -> Result<IR, Error> {
    let path_str = path.to_string_lossy().to_string();
    let (program_name, author, date_written, comments) = parse_identification_division(source);
    let ws_lines = extract_section_lines(source, "WORKING-STORAGE SECTION");
    let working_storage = parse_data_items(&ws_lines, Some("WORKING-STORAGE"));
    let file_lines = extract_section_lines(source, "FILE SECTION");
    let file_section = parse_data_items(&file_lines, Some("FILE SECTION"));
    let linkage_lines = extract_section_lines(source, "LINKAGE SECTION");
    let linkage = parse_data_items(&linkage_lines, Some("LINKAGE"));
    let (procedure_division, call_graph, control_flow_graph, paragraphs) =
        parse_procedure_division_and_call_graph(source, &program_name);
    let file_modes = parse_open_statements(source);
    let io_files = parse_input_output_section(source, &file_modes);
    let ir = IR {
        program_name: if !program_name.is_empty() {
            program_name
        } else {
            "UNKNOWN".to_string()
        },
        source_file: path_str,
        identification_division: IdentificationDivision {
            author: if !author.is_empty() {
                author
            } else {
                "UNKNOWN".to_string()
            },
            date_written: if !date_written.is_empty() {
                date_written
            } else {
                "UNKNOWN".to_string()
            },
            comments,
        },
        environment_division: EnvironmentDivision {
            input_output_section: InputOutputSection { files: io_files },
        },
        data_division: DataDivision {
            working_storage,
            file_section,
            linkage,
        },
        paragraphs,
        procedure_division,
        call_graph,
        control_flow_graph,
        nested_programs: Vec::new(),
    };
    if verbose || debug {
        match serde_json::to_string_pretty(&ir) {
            Ok(json) => {
                if debug {
                    eprintln!("[DEBUG] Full IR: {}", json);
                } else {
                    eprintln!("[VERBOSE] Parsed IR: {}", json);
                }
            }
            Err(e) => eprintln!("[ERROR] failed to serialize IR: {}", e),
        }
    }
    Ok(ir)
}
