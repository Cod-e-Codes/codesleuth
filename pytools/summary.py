import sys
import io
import json
import re
import argparse
from collections import Counter
from itertools import groupby
import hashlib

COBOL_KEYWORDS = {
    "MOVE", "PERFORM", "READ", "WRITE", "DISPLAY", "IF", "ELSE", "END-IF", "UNTIL", "AT", "END", "STOP",
    "CALL", "EXEC", "SQL", "OPEN", "CLOSE", "FETCH", "COMPUTE", "SET", "IS", "NOT", "EQUAL", "THEN",
    "USING", "FROM", "BY", "TO", "VARYING", "AND", "OR", ">", "<", "=", ".", "(", ")", "FUNCTION",
    "INTO", "AFTER", "ADVANCING", "USING", "END-EXEC", "RETURN", "RUN"
}

COBOL_LITERALS = {
    "SPACES": '"  " (SPACES)',
    "SPACE": '" " (SPACE)',
    "ZERO": '0 (ZERO)',
    "ZEROS": '0 (ZEROS)',
    "ZEROES": '0 (ZEROES)',
    "HIGH-VALUE": '0xFF (HIGH-VALUE)',
    "LOW-VALUE": '0x00 (LOW-VALUE)',
    "QUOTE": '" (QUOTE)',
    "QUOTES": '" (QUOTES)',
    "NULL": '0 (NULL)',
}

def is_valid_identifier(name):
    """Return True if name is a valid COBOL identifier (not a literal, keyword, or punctuation)."""
    name = name.strip().strip('.').strip('"').strip("'").upper()
    if not name:
        return False
    if name in COBOL_KEYWORDS:
        return False
    if re.match(r'^-?\d+(\.\d+)?$', name):  # numbers like 123 or 123.45
        return False
    if re.match(r'^\W+$', name):  # only non-word chars
        return False
    if not re.match(r'^[A-Z0-9\-_]+$', name):
        return False
    return True

def is_literal(name):
    # Exclude quoted strings and COBOL literals
    n = name.strip()
    if n.startswith("'") and n.endswith("'"):
        return True
    if n.startswith('"') and n.endswith('"'):
        return True
    if n.upper() in COBOL_LITERALS:
        return True
    if re.match(r'^-?\d+(\.\d+)?$', n):
        return True
    return False

def format_value(val):
    if not val:
        return ''
    v = val.strip().upper()
    if v in COBOL_LITERALS:
        return COBOL_LITERALS[v]
    if v.startswith("'") and v.endswith("'"):
        return f'"{val.strip()}"'
    if v.startswith('"') and v.endswith('"'):
        return val.strip()
    return val

def print_program_info(ir):
    print("# COBOL Program Summary\n")
    print(f"**Program Name:** {ir.get('program_name', 'UNKNOWN')}")
    print(f"**Source File:** {ir.get('source_file', 'UNKNOWN')}")

    id_div = ir.get('identification_division', {})
    print(f"**Author:** {id_div.get('author', 'UNKNOWN')}")
    print(f"**Date Written:** {id_div.get('date_written', 'UNKNOWN')}")

    comments = id_div.get('comments', [])
    print("**Comments:**")
    if comments:
        for comment in comments:
            cleaned = comment.strip()
            if cleaned:
                print(f"- {cleaned}")
    else:
        print("None")

def print_data_items(items, indent=0, show_section=False):
    i = 0
    while i < len(items):
        if items[i].get('name', '').upper() == 'FILLER':
            count = 1
            j = i + 1
            while j < len(items) and items[j].get('name', '').upper() == 'FILLER':
                count += 1
                j += 1
            if count > 1:
                print("  " * indent + f"- FILLER ({count} items): Various strings/spaces used for layout.")
            else:
                print("  " * indent + f"- FILLER (ignored)")
            i += count
            continue
        item = items[i]
        prefix = "  " * indent
        name = item.get('name', '')
        level = item.get('level', '')
        pic = item.get('picture', '')
        typ = item.get('type', '')
        occurs = item.get('occurs')
        redefines = item.get('redefines')
        value = item.get('value')
        comp3 = item.get('comp3', False)
        section = item.get('section', None)
        array_str = f" [OCCURS {occurs}]" if occurs else ""
        redef_str = f" [REDEFINES {redefines}]" if redefines else ""
        meta = []
        if pic:
            meta.append(f"PIC {pic}")
        if typ:
            if comp3:
                meta.append(f"TYPE packed-decimal (COMP-3) [{typ}]")
            else:
                meta.append(f"TYPE {typ}")
        if value:
            meta.append(f"VALUE {format_value(value)}")
        if show_section and section:
            meta.append(f"SECTION {section}")
        meta_str = ("; ".join(meta)) if meta else ""
        # Ensure OCCURS is always shown in the line if present
        occurs_str = array_str if array_str else ""
        print(f"{prefix}- **{name}** (Level {level}){occurs_str}{redef_str}{' - ' + meta_str if meta_str else ''}")
        if item.get('children'):
            print_data_items(item['children'], indent + 1, show_section)
        i += 1

def print_working_storage(ws_vars, file_section_names=None):
    print("\n## Working-Storage Variables\n")
    if ws_vars:
        for item in ws_vars:
            name = item.get('name')
            if not name:
                continue
            if file_section_names and name in file_section_names:
                continue
            print_data_items([item])
    else:
        print("_Declared, but no user-defined items were found (possibly all are imported)._")
    print("\n---\n")

def print_file_section(file_sections):
    # Only show file records (FD/01) in the File Section
    def is_file_record(item):
        # Heuristic: FD/01 level and has children
        return item.get('level') == 1 and item.get('children')
    filtered = [item for item in file_sections if is_file_record(item)]
    if filtered:
        print("\n## File Section\n")
        print_data_items(filtered)
    else:
        print("\n_No File Section entries found._\n")
    print("\n---\n")

def print_procedure_division(paragraphs, verbose=False, all_paragraphs_out=None):
    if not paragraphs:
        print("\n_No Procedure Division content found._")
        return
    print("\n## Procedure Division\n")
    all_paragraphs = []
    # Only keep the last instance of each paragraph (by name, section, line)
    para_map = {}
    for para in paragraphs:
        pname = para.get('name', '')
        section = para.get('section', '')
        line = para.get('line')
        key = (pname, section, line)
        para_map[key] = para  # always overwrite, so last instance is kept
    for (pname, section, line), para in para_map.items():
        # Track all paragraph names and their section/line for orphan detection
        if pname:
            all_paragraphs.append((pname, section, line))
        statements = para.get('statements', [])
        var_usage = para.get('variable_usage', [])
        filtered_vars = []
        for vu in var_usage:
            name = vu.get('name', '')
            if not name:
                continue
            name = name.strip().rstrip('.')  # Normalize variable name
            if not is_valid_identifier(name) or is_literal(name):
                continue
            filtered_vars.append({**vu, 'name': name})
        # Skip paragraphs with no name and no statements/variable usage
        if not pname or (not statements and not filtered_vars):
            continue
        if pname.endswith("-END") and not statements:
            continue
        line_info = f" (line {line})" if line else ""
        src_info = f" [{para.get('source_location')}]" if para.get('source_location') else ""
        section_info = f" _(Section: {section})_" if section else ""
        print(f"#### Paragraph: **{pname}**{line_info}{src_info}{section_info}")
        if statements:
            for stmt in statements:
                stype = stmt.get('type', '')
                raw = stmt.get('raw', '')
                stmt_line = stmt.get('line')
                stmt_src = stmt.get('source_location')
                stmt_info = f" (line {stmt_line})" if stmt_line else ""
                stmt_src_info = f" [{stmt_src}]" if stmt_src else ""
                print(f"- **{stype}**: {raw}{stmt_info}{stmt_src_info}")
        else:
            print("_No logic here_")
        if filtered_vars:
            print("\n**Variables Used:**")
            print("| Name | Read | Written |")
            print("|------|------|---------|")
            for vu in filtered_vars:
                name = vu.get('name', '').strip().rstrip('.')
                read = 'Yes' if vu.get('read') else ''
                written = 'Yes' if vu.get('written') else ''
                print(f"| **{name}** | {read} | {written} |")
        else:
            print("_Variables used: None._")
    if all_paragraphs_out is not None:
        all_paragraphs_out.extend(all_paragraphs)
    print("\n---\n")

def print_unused_paragraphs(all_paragraphs, call_graph):
    # Find all paragraphs that are not targets of any call/perform/goto
    called = set()
    for edge in call_graph:
        if edge.get('type', '').upper() in {"PERFORM", "GOTO", "PERFORM VARYING"}:
            to = edge.get('to')
            if to:
                called.add(to)
    # all_paragraphs is now a list of (name, section, line)
    unused_counter = {}
    for para in all_paragraphs:
        # Support (name, section, line) if available, else fallback
        if isinstance(para, tuple):
            if len(para) == 3:
                pname, section, line = para
            elif len(para) == 2:
                pname, section = para
                line = None
            else:
                pname = para[0]
                section = None
                line = None
        else:
            pname = para
            section = None
            line = None
        if pname not in called:
            key = (pname, section, line)
            unused_counter[key] = unused_counter.get(key, 0) + 1
    if unused_counter:
        print("\n## Unused Paragraphs\n")
        print("**The following paragraphs are not the target of any PERFORM or GOTO:**\n")
        for (pname, section, line), count in sorted(unused_counter.items()):
            section_info = f" _(Section: {section})_" if section else ""
            line_info = f" _(line {line})_" if line else ""
            count_info = f" _(x{count})_" if count > 1 else ""
            print(f"- **{pname}**{section_info}{line_info}{count_info}")
    print("\n---\n")

def print_external_calls(call_graph):
    # Find all CALL edges and group by target
    calls = {}
    for edge in call_graph:
        if edge.get('type', '').upper() == 'CALL':
            prog = edge.get('to')
            from_para = edge.get('from')
            if prog:
                if prog not in calls:
                    calls[prog] = []
                if from_para:
                    calls[prog].append(from_para)
    if calls:
        print("\n## Subprograms / External Calls")
        for prog, from_paras in calls.items():
            callers = ', '.join(sorted(set(from_paras)))
            print(f"- Program `{prog}` (Called from: {callers})")

def sanitize_node_id(name):
    if not name:
        return "UNKNOWN"
    short = name[:20]
    h = hashlib.md5(name.encode()).hexdigest()[:6]
    sanitized = re.sub(r"[^a-zA-Z0-9_]", "_", short)
    sanitized = re.sub(r"_+", "_", sanitized)
    if re.match(r"^\d", sanitized):
        sanitized = "_" + sanitized
    return f"{sanitized}_{h}"

def print_call_graph(call_graph, verbose=False):
    print("\n## Call Graph\n")
    # Add Markdown legend (ASCII only)
    print("> **Legend:** Solid -> PERFORM, Dotted -.-> GOTO, Dashed --|VARYING|--> PERFORM VARYING, Double ==> CALL")
    if not call_graph:
        # Show a minimal placeholder graph
        print("\n_No call graph generated (missing or empty in IR)._")
        print("```mermaid\nflowchart TD\n    A[No call graph data]\n```\n")
        print("\n---\n")
        return
    print("```mermaid")
    print("%% Dotted lines: GOTO; Solid: PERFORM or sequence; Arrows: control flow")
    print("flowchart TD")
    seen_edges = set()
    for edge in call_graph:
        from_node = sanitize_node_id(edge.get("from", ""))
        to_node = sanitize_node_id(edge.get("to", ""))
        edge_type = edge.get("type", "").upper()
        if not from_node or not to_node:
            continue
        key = (from_node, to_node, edge_type)
        if key in seen_edges:
            continue
        seen_edges.add(key)
        if edge_type == "CALL":
            print(f"    {from_node} ==>|CALL| {to_node}")
        elif edge_type == "PERFORM":
            print(f"    {from_node} --> {to_node}")
        elif edge_type == "GOTO":
            print(f"    {from_node} -.-> {to_node}")
        elif edge_type == "PERFORM VARYING":
            print(f"    {from_node} --|VARYING|--> {to_node}")
        else:
            print(f"    {from_node} --> {to_node}")
    print("```")
    print("\n### Call Graph Table\n")
    print("| **From** | **To** | **Type** |")
    print("|------|----|------|")
    for edge in call_graph:
        print(f"| **{edge.get('from')}** | **{edge.get('to')}** | {edge.get('type')} |")
    print("\n---\n")

def print_control_flow_graph(cfg):
    print("\n## Control Flow Graph\n")
    # Add Markdown legend (ASCII only)
    print("> **Legend:** Solid -> NEXT, Dotted -.-> GOTO, Solid -> PERFORM, Dashed --|VARYING|--> PERFORM VARYING")
    if not cfg:
        print("\n_No control flow graph generated (missing or empty in IR)._\n")
        print("\n---\n")
        return
    print("```mermaid")
    print("%% Dotted lines: GOTO; Solid: PERFORM or sequence; Arrows: control flow")
    print("flowchart TD")
    for edge in cfg:
        from_node = sanitize_node_id(edge.get("from", ""))
        to_node = sanitize_node_id(edge.get("to", ""))
        edge_type = edge.get("type", "").upper()
        label = f"|{edge_type}|" if edge_type != "NEXT" else ""
        if edge_type == "GOTO":
            print(f"    {from_node} -.-> {to_node}")
        elif edge_type == "PERFORM VARYING":
            print(f"    {from_node} --|VARYING|--> {to_node}")
        else:
            print(f"    {from_node} --{label}--> {to_node}")
    print("```")
    # Add Control Flow Edges Table
    print("\n### Control Flow Edges Table\n")
    print("| **From** | **To** | **Type** |")
    print("|------|----|------|")
    for edge in cfg:
        print(f"| **{edge.get('from')}** | **{edge.get('to')}** | {edge.get('type')} |")
    print("\n---\n")

def print_io_files(io_files, file_section=None):
    if io_files:
        print("\n## Environment Division - Input/Output Section\n")
        print("| File Name | Type | Description | Record Structure |")
        print("|-----------|------|-------------|------------------|")
        # Try to match file name to FD/record in file_section
        record_map = {}
        if file_section:
            for item in file_section:
                # Normalize FD name for better matching
                fd_name = item.get('name', '').upper().replace('-', '')
                if item.get('children'):
                    record_map[fd_name] = item['children'][0].get('name', '')
        for f in io_files:
            name = f.get('name', 'UNKNOWN')
            ftype = f.get('type', 'unknown')
            desc = f.get('description', '')
            record = record_map.get(name.upper().replace('-', ''), '')
            print(f"| {name} | {ftype} | {desc} | {record} |")
    else:
        print("\n_No Input/Output files found._")
    print("\n---\n")

def main():
    """Main entry: parse args, load IR, and print summary."""
    parser = argparse.ArgumentParser(description="COBOL IR Markdown summary generator.")
    parser.add_argument('-i', '--input', type=str, help='Input IR JSON file (default: stdin)')
    parser.add_argument('-o', '--output', type=str, help='Output Markdown file (default: stdout)')
    parser.add_argument('-v', '--verbose', action='store_true', help='Enable verbose output')
    args = parser.parse_args()

    if args.input:
        try:
            with open(args.input, encoding='utf-8') as f:
                ir = json.load(f)
        except Exception as e:
            print(f"Error: Failed to parse JSON input: {e}", file=sys.stderr)
            sys.exit(1)
    else:
        try:
            ir = json.load(sys.stdin)
        except Exception as e:
            print(f"Error: Failed to parse JSON input: {e}", file=sys.stderr)
            sys.exit(1)

    if args.output:
        out_file = open(args.output, 'w', encoding='utf-8')
    else:
        out_file = sys.stdout

    old_stdout = sys.stdout
    sys.stdout = out_file
    try:
        file_section = ir.get('data_division', {}).get('file_section', [])
        file_section_names = {item['name'] for item in file_section if 'name' in item and item['name'] and item['name'].upper() != 'FILLER'}
        print_program_info(ir)
        print_working_storage(ir.get('data_division', {}).get('working_storage', []), file_section_names)
        print_file_section(file_section)
        all_paragraphs = []
        print_procedure_division(ir.get('paragraphs', []), verbose=args.verbose, all_paragraphs_out=all_paragraphs)
        call_graph = ir.get('call_graph', [])
        print_call_graph(call_graph, verbose=args.verbose)
        print_control_flow_graph(ir.get('control_flow_graph', []))
        print_io_files(ir.get('environment_division', {}).get('input_output_section', {}).get('files', []), file_section)
        print_unused_paragraphs(all_paragraphs, call_graph)
        print_external_calls(call_graph)
    finally:
        sys.stdout = old_stdout
        if args.output:
            out_file.close()

if __name__ == "__main__":
    main()
