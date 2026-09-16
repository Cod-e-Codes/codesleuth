//! COBOL IR to Markdown.
use crate::cobol;
use crate::error::Error;
use crate::ir::{
    CallGraphEntry, ControlFlowEdge, DataItem, Paragraph, Statement, VariableUsage, IR,
};
use std::collections::{HashMap, HashSet};
use std::io::{self, Write};

fn print_program_info<W: Write>(out: &mut W, ir: &IR) -> io::Result<()> {
    writeln!(out, "# COBOL Program Summary\n")?;
    writeln!(out, "**Program Name:** {}", ir.program_name)?;
    writeln!(out, "**Source File:** {}", ir.source_file)?;
    let id = &ir.identification_division;
    writeln!(out, "**Author:** {}", id.author)?;
    writeln!(out, "**Date Written:** {}", id.date_written)?;
    writeln!(out, "**Comments:**")?;
    for comment in &id.comments {
        let cleaned = comment.trim();
        if !cleaned.is_empty() {
            writeln!(out, "- {}", cleaned)?;
        }
    }
    Ok(())
}

fn print_data_items<W: Write>(
    out: &mut W,
    items: &[DataItem],
    indent: usize,
    show_section: bool,
) -> io::Result<()> {
    let mut i = 0;
    while i < items.len() {
        if items[i].name.to_uppercase() == "FILLER" {
            let mut count = 1;
            let mut j = i + 1;
            while j < items.len() && items[j].name.to_uppercase() == "FILLER" {
                count += 1;
                j += 1;
            }
            if count > 1 {
                writeln!(out, "{}  - FILLER ({} items)", "  ".repeat(indent), count)?;
            } else {
                writeln!(out, "{}  - FILLER (ignored)", "  ".repeat(indent))?;
            }
            i += count;
            continue;
        }
        let item = &items[i];
        let prefix = "  ".repeat(indent);
        let name = &item.name;
        let level = item.level;
        let pic = item.picture.as_deref().unwrap_or("");
        let typ = item.r#type.as_deref().unwrap_or("");
        let occurs = item.occurs;
        let redefines = item.redefines.as_deref().unwrap_or("");
        let value = item.value.as_deref();
        let comp3 = item.comp3;
        let section = item.section.as_deref().unwrap_or("");
        let array_str = if let Some(occ) = occurs {
            format!(" [OCCURS {}]", occ)
        } else {
            String::new()
        };
        let redef_str = if !redefines.is_empty() {
            format!(" [REDEFINES {}]", redefines)
        } else {
            String::new()
        };
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
        let formatted = cobol::format_value(value);
        if !formatted.is_empty() {
            meta.push(format!("VALUE {}", formatted));
        }
        if show_section && !section.is_empty() {
            meta.push(format!("SECTION {}", section));
        }
        let meta_str = if meta.is_empty() {
            String::new()
        } else {
            format!(" - {}", meta.join("; "))
        };
        writeln!(
            out,
            "{}  - **{}** (Level {}){}{}{}",
            prefix, name, level, array_str, redef_str, meta_str
        )?;
        print_data_items(out, &item.children, indent + 1, show_section)?;
        i += 1;
    }
    Ok(())
}

fn print_working_storage<W: Write>(out: &mut W, ws_vars: &[DataItem]) -> io::Result<()> {
    writeln!(out, "\n## Working-Storage Variables\n")?;
    for item in ws_vars {
        if !item.name.is_empty() {
            print_data_items(out, std::slice::from_ref(item), 0, false)?;
        }
    }
    writeln!(out, "\n---\n")?;
    Ok(())
}

fn print_file_section<W: Write>(out: &mut W, file_sections: &[DataItem]) -> io::Result<()> {
    let named: Vec<&DataItem> = file_sections
        .iter()
        .filter(|item| !item.name.is_empty())
        .collect();
    if named.is_empty() {
        return Ok(());
    }
    writeln!(out, "\n## File Section\n")?;
    let owned: Vec<DataItem> = named.into_iter().cloned().collect();
    print_data_items(out, &owned, 0, false)?;
    writeln!(out, "\n---\n")?;
    Ok(())
}

fn print_linkage<W: Write>(out: &mut W, items: &[DataItem]) -> io::Result<()> {
    let named: Vec<&DataItem> = items.iter().filter(|item| !item.name.is_empty()).collect();
    if named.is_empty() {
        return Ok(());
    }
    writeln!(out, "\n## Linkage Section\n")?;
    let owned: Vec<DataItem> = named.into_iter().cloned().collect();
    print_data_items(out, &owned, 0, false)?;
    writeln!(out, "\n---\n")?;
    Ok(())
}

fn print_procedure_division<W: Write>(
    out: &mut W,
    ir: &IR,
    all_paragraphs_out: &mut Vec<(String, String, Option<usize>)>,
) -> io::Result<()> {
    let mut all_paras = Vec::new();
    for section in &ir.procedure_division.sections {
        all_paras.extend(section.paragraphs.clone());
    }
    if all_paras.is_empty() {
        all_paras = ir.paragraphs.clone();
    }

    if all_paras.is_empty() {
        writeln!(out, "\n_No Procedure Division content found._")?;
        writeln!(out, "\n---\n")?;
        return Ok(());
    }
    writeln!(out, "\n## Procedure Division\n")?;
    let mut para_map = HashMap::new();
    let paragraph_names: HashSet<String> =
        all_paras.iter().map(|p| p.name.to_uppercase()).collect();
    for para in all_paras {
        let pname = para.name.clone();
        let section = para.section.clone().unwrap_or_default();
        let line = para.line;
        let key = (pname.clone(), section.clone(), line);
        para_map.insert(key, para);
        if !pname.is_empty() {
            all_paragraphs_out.push((pname, section, line));
        }
    }
    let mut para_rows: Vec<_> = para_map.into_iter().collect();
    para_rows.sort_by(|((n1, s1, l1), _), ((n2, s2, l2), _)| (l1, n1, s1).cmp(&(l2, n2, s2)));
    for ((pname, section, line), para) in &para_rows {
        let statements = &para.statements;
        let var_usage = &para.variable_usage;
        let mut filtered_vars = Vec::new();
        for vu in var_usage {
            let clean_name = vu.name.trim().trim_end_matches('.');
            if !clean_name.is_empty()
                && cobol::is_valid_identifier(clean_name)
                && !cobol::is_literal(clean_name)
                && !paragraph_names.contains(&clean_name.to_uppercase())
            {
                filtered_vars.push(vu.clone());
            }
        }
        filtered_vars.sort_by(|a, b| a.name.cmp(&b.name));
        if statements.is_empty() && filtered_vars.is_empty() {
            continue;
        }
        if pname.ends_with("-END") && statements.is_empty() {
            continue;
        }
        let display_name = if pname.is_empty() { "unnamed" } else { pname };
        let line_info = if let Some(l) = line {
            format!(" (line {})", l)
        } else {
            String::new()
        };
        let src_info = if let Some(src) = &para.source_location {
            format!(" [{}]", src)
        } else {
            String::new()
        };
        let section_info = if !section.is_empty() {
            format!(" _(Section: {})_", section)
        } else {
            String::new()
        };
        let kind_info = if para.kind != "paragraph" {
            format!(" _({})_", para.kind)
        } else {
            String::new()
        };
        writeln!(
            out,
            "#### Paragraph: **{}**{}{}{}{}",
            display_name, line_info, src_info, section_info, kind_info
        )?;
        if !statements.is_empty() {
            for stmt in statements {
                write_statement(out, stmt)?;
            }
        } else {
            writeln!(out, "_No logic here_")?;
        }
        if !filtered_vars.is_empty() {
            writeln!(out, "\n**Variables Used:**")?;
            writeln!(out, "| Name | Read | Written |")?;
            writeln!(out, "|------|------|---------|")?;
            for vu in &filtered_vars {
                write_var_row(out, vu)?;
            }
        } else {
            writeln!(out, "_Variables used: None._")?;
        }
    }
    writeln!(out, "\n---\n")?;
    Ok(())
}

fn write_statement<W: Write>(out: &mut W, stmt: &Statement) -> io::Result<()> {
    let stype = &stmt.r#type;
    let raw = &stmt.raw;
    let operands = &stmt.operands;
    let stmt_info = if let Some(l) = stmt.line {
        format!(" (line {})", l)
    } else {
        String::new()
    };
    let stmt_src_info = if let Some(src) = &stmt.source_location {
        format!(" [{}]", src)
    } else {
        String::new()
    };
    let operands_info = if !operands.is_empty() {
        format!(" [{}]", operands.join(", "))
    } else {
        String::new()
    };
    writeln!(
        out,
        "- **{}**: {}{}{}{}",
        stype, raw, operands_info, stmt_info, stmt_src_info
    )
}

fn write_var_row<W: Write>(out: &mut W, vu: &VariableUsage) -> io::Result<()> {
    let name = vu.name.trim().trim_end_matches('.');
    let read = if vu.read { "Yes" } else { "" };
    let written = if vu.written { "Yes" } else { "" };
    writeln!(out, "| **{}** | {} | {} |", name, read, written)
}

fn print_unused_paragraphs<W: Write>(
    out: &mut W,
    all_paragraphs: &[(String, String, Option<usize>)],
    call_graph: &[CallGraphEntry],
    paragraphs: &[Paragraph],
) -> io::Result<()> {
    let mut called = HashSet::new();
    for edge in call_graph {
        if ["PERFORM", "GOTO", "PERFORM VARYING"].contains(&edge.r#type.to_uppercase().as_str()) {
            called.insert(edge.to.to_uppercase());
        }
    }
    let mut used = HashSet::new();
    if !all_paragraphs.is_empty() {
        used.insert(all_paragraphs[0].0.to_uppercase());
        for i in 0..paragraphs.len().saturating_sub(1) {
            let current_name = &paragraphs[i].name;
            let next_name = &paragraphs[i + 1].name;
            if !current_name.is_empty() && !next_name.is_empty() {
                called.insert(next_name.to_uppercase());
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
    writeln!(out, "\n## Unused Paragraphs\n")?;
    if !unused_counter.is_empty() {
        writeln!(
            out,
            "**The following paragraphs are not the target of any PERFORM or GOTO:**\n"
        )?;
        let mut sorted_unused: Vec<_> = unused_counter.iter().collect();
        sorted_unused.sort_by_key(|((pname, section, line), _)| (pname, section, line));
        for ((pname, section, line), count) in sorted_unused {
            let section_info = if !section.is_empty() {
                format!(" _(Section: {})_", section)
            } else {
                String::new()
            };
            let line_info = if let Some(l) = line {
                format!(" _(line {})_", l)
            } else {
                String::new()
            };
            let count_info = if *count > 1 {
                format!(" _(x{})_", count)
            } else {
                String::new()
            };
            writeln!(
                out,
                "- **{}**{}{}{}",
                pname, section_info, line_info, count_info
            )?;
        }
    } else {
        writeln!(out, "_No unused paragraphs found._")?;
    }
    writeln!(out, "\n---\n")?;
    Ok(())
}

fn print_external_calls<W: Write>(out: &mut W, call_graph: &[CallGraphEntry]) -> io::Result<()> {
    let mut calls = HashMap::new();
    for edge in call_graph {
        if edge.r#type.to_uppercase() == "CALL" {
            calls
                .entry(edge.to.clone())
                .or_insert_with(Vec::new)
                .push(edge.from.clone());
        }
    }
    if !calls.is_empty() {
        writeln!(out, "\n## Subprograms / External Calls")?;
        let mut programs: Vec<_> = calls.into_iter().collect();
        programs.sort_by(|a, b| a.0.cmp(&b.0));
        for (prog, from_paras) in programs {
            let mut callers: Vec<_> = from_paras
                .into_iter()
                .collect::<HashSet<_>>()
                .into_iter()
                .collect();
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
    let mut sanitized = para_name
        .chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect::<String>();
    if parts.len() > 1 {
        let stmt_part = parts[1].chars().take(20).collect::<String>();
        let sanitized_stmt = stmt_part
            .chars()
            .map(|c| {
                if c.is_alphanumeric() || c == '_' {
                    c
                } else {
                    '_'
                }
            })
            .collect::<String>();
        sanitized.push_str(&format!("_{}", sanitized_stmt));
    }
    if sanitized.chars().next().is_some_and(|c| c.is_numeric()) {
        sanitized.insert(0, '_');
    }
    sanitized
}

fn print_call_graph<W: Write>(out: &mut W, call_graph: &[CallGraphEntry]) -> io::Result<()> {
    writeln!(out, "\n## Call Graph\n")?;
    writeln!(out, "> **Legend:** Solid -> PERFORM, Dotted -.-> GOTO, Dashed --|VARYING|--> PERFORM VARYING, Double ==> CALL")?;
    if call_graph.is_empty() {
        writeln!(out, "\n_No call graph generated (missing or empty in IR)._")?;
        writeln!(out, "```mermaid")?;
        writeln!(out, "flowchart TD")?;
        writeln!(out, "    A[No call graph data]")?;
        writeln!(out, "```")?;
        writeln!(out, "\n---\n")?;
        return Ok(());
    }
    writeln!(out, "```mermaid")?;
    writeln!(
        out,
        "%% Dotted lines: GOTO; Solid: PERFORM or sequence; Arrows: control flow"
    )?;
    writeln!(out, "flowchart TD")?;
    let mut seen_edges = HashSet::new();
    for edge in call_graph {
        let from_node = sanitize_node_id(&edge.from);
        let to_node = sanitize_node_id(&edge.to);
        let edge_type = edge.r#type.to_uppercase();
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
    writeln!(
        out,
        "| **From** | **To** | **Type** | **Kind** | **Line** | **Section** | **Source** |"
    )?;
    writeln!(out, "|------|----|------|------|------|---------|--------|")?;
    for edge in call_graph {
        writeln!(
            out,
            "| **{}** | **{}** | {} | {} | {} | {} | {} |",
            edge.from,
            edge.to,
            edge.r#type,
            edge.kind,
            edge.line
                .map(|l| l.to_string())
                .unwrap_or_else(|| "".to_string()),
            edge.section.as_deref().unwrap_or(""),
            edge.source_location.as_deref().unwrap_or("")
        )?;
    }
    writeln!(out, "\n---\n")?;
    Ok(())
}

fn print_control_flow_graph<W: Write>(out: &mut W, cfg: &[ControlFlowEdge]) -> io::Result<()> {
    writeln!(out, "\n## Control Flow Graph\n")?;
    writeln!(out, "> **Legend:** Solid -> NEXT, Dotted -.-> GOTO, Solid -> PERFORM, Dashed --|VARYING|--> PERFORM VARYING")?;
    if cfg.is_empty() {
        writeln!(
            out,
            "\n_No control flow graph generated (missing or empty in IR)._"
        )?;
        writeln!(out, "\n---\n")?;
        return Ok(());
    }
    writeln!(out, "```mermaid")?;
    writeln!(
        out,
        "%% Dotted lines: GOTO; Solid: PERFORM or sequence; Arrows: control flow"
    )?;
    writeln!(out, "flowchart TD")?;
    for edge in cfg {
        let from_node = sanitize_node_id(&edge.from);
        let to_node = sanitize_node_id(&edge.to);
        let edge_type = edge.r#type.to_uppercase();
        let label = if edge_type != "NEXT" {
            format!("|{}|", edge_type)
        } else {
            String::new()
        };
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
    for edge in cfg {
        writeln!(
            out,
            "| **{}** | **{}** | {} |",
            edge.from, edge.to, edge.r#type
        )?;
    }
    writeln!(out, "\n---\n")?;
    Ok(())
}

fn print_io_files<W: Write>(out: &mut W, ir: &IR) -> io::Result<()> {
    let files = &ir.environment_division.input_output_section.files;
    if files.is_empty() {
        return Ok(());
    }
    writeln!(out, "\n## Environment Division - Input/Output Section\n")?;
    writeln!(out, "| File Name | Type | Description | Record Structure |")?;
    writeln!(out, "|-----------|------|-------------|------------------|")?;
    for f in files {
        writeln!(
            out,
            "| {} | {} | {} | {} |",
            f.name,
            f.r#type,
            f.description,
            f.record_name.as_deref().unwrap_or("")
        )?;
    }
    writeln!(out, "\n---\n")?;
    Ok(())
}

fn print_nested_programs<W: Write>(out: &mut W, nested: &[IR]) -> io::Result<()> {
    for program in nested {
        writeln!(out, "\n## Nested Program: {}\n", program.program_name)?;
        writeln!(
            out,
            "**Author:** {}",
            program.identification_division.author
        )?;
        writeln!(
            out,
            "**Date Written:** {}",
            program.identification_division.date_written
        )?;
        print_working_storage(out, &program.data_division.working_storage)?;
        print_file_section(out, &program.data_division.file_section)?;
        print_linkage(out, &program.data_division.linkage)?;
        let mut paras = Vec::new();
        print_procedure_division(out, program, &mut paras)?;
        print_nested_programs(out, &program.nested_programs)?;
    }
    Ok(())
}

pub fn render(ir: &IR, verbose: bool, debug: bool) -> Result<String, Error> {
    if debug {
        eprintln!("[DEBUG] IR: {:#?}", ir);
    } else if verbose {
        eprintln!("[VERBOSE] Program name: {}", ir.program_name);
    }
    let mut output: Vec<u8> = Vec::new();
    print_program_info(&mut output, ir).map_err(|e| Error::Report(e.to_string()))?;
    print_working_storage(&mut output, &ir.data_division.working_storage)
        .map_err(|e| Error::Report(e.to_string()))?;
    print_file_section(&mut output, &ir.data_division.file_section)
        .map_err(|e| Error::Report(e.to_string()))?;
    print_linkage(&mut output, &ir.data_division.linkage)
        .map_err(|e| Error::Report(e.to_string()))?;
    let mut all_paragraphs = Vec::new();
    print_procedure_division(&mut output, ir, &mut all_paragraphs)
        .map_err(|e| Error::Report(e.to_string()))?;
    print_call_graph(&mut output, &ir.call_graph).map_err(|e| Error::Report(e.to_string()))?;
    print_control_flow_graph(&mut output, &ir.control_flow_graph)
        .map_err(|e| Error::Report(e.to_string()))?;
    print_io_files(&mut output, ir).map_err(|e| Error::Report(e.to_string()))?;
    print_unused_paragraphs(&mut output, &all_paragraphs, &ir.call_graph, &ir.paragraphs)
        .map_err(|e| Error::Report(e.to_string()))?;
    print_external_calls(&mut output, &ir.call_graph).map_err(|e| Error::Report(e.to_string()))?;
    print_nested_programs(&mut output, &ir.nested_programs)
        .map_err(|e| Error::Report(e.to_string()))?;
    String::from_utf8(output).map_err(|e| Error::Report(e.to_string()))
}

pub fn render_json(ir_json: &str, verbose: bool, debug: bool) -> Result<String, Error> {
    let ir: IR = serde_json::from_str(ir_json)?;
    render(&ir, verbose, debug)
}
