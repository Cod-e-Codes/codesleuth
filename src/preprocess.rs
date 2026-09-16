//! COPY, REPLACE, and fixed-form column-7 continuation.
//!
//! COPY/REPLACE matching follows IBM Enterprise COBOL 6.5: text words and
//! pseudo-text, plus dummy operands `:TAG:` / `(TAG)` as separators inside a
//! library text word. Column-7 hyphen: Language Reference SC27-8713-04,
//! https://www.ibm.com/docs/en/cobol-zos/6.5.0?topic=b-continuation-lines
use crate::cobol::{has_terminator_period, is_cobol_comment_or_blank, strip_terminator};
use std::path::{Path, PathBuf};

const COPY_DEPTH: u32 = 16;

pub fn preprocess(source: &str, origin: &Path) -> String {
    let continued = apply_continuation(source);
    let copied = expand_copies(&continued, origin, 0);
    apply_replace(&copied)
}

pub fn apply_continuation(source: &str) -> String {
    let mut out = String::new();
    for line in source.lines() {
        let ind = indicator(line);
        if ind == '-' && !out.is_empty() {
            while out.ends_with('\n') {
                out.pop();
            }
            let last_start = out.rfind('\n').map(|i| i + 1).unwrap_or(0);
            if let Some(q) = unclosed_quote(&out[last_start..]) {
                let mut last = out[last_start..].to_string();
                pad_to_col72(&mut last);
                out.truncate(last_start);
                out.push_str(&last);
                out.push_str(&literal_continuation_payload(line, q));
            } else {
                while out.ends_with(' ') {
                    out.pop();
                }
                out.push_str(&nonliteral_continuation_payload(line));
            }
            continue;
        }
        if !out.is_empty() && !out.ends_with('\n') {
            out.push('\n');
        }
        out.push_str(line);
    }
    if source.ends_with('\n') && !out.ends_with('\n') {
        out.push('\n');
    }
    out
}

fn indicator(line: &str) -> char {
    line.chars().nth(6).unwrap_or(' ')
}

fn unclosed_quote(s: &str) -> Option<char> {
    let mut in_single = false;
    let mut in_double = false;
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if !in_double && c == '\'' {
            if in_single && chars.peek() == Some(&'\'') {
                chars.next();
            } else {
                in_single = !in_single;
            }
        } else if !in_single && c == '"' {
            if in_double && chars.peek() == Some(&'"') {
                chars.next();
            } else {
                in_double = !in_double;
            }
        }
    }
    if in_double {
        Some('"')
    } else if in_single {
        Some('\'')
    } else {
        None
    }
}

fn pad_to_col72(line: &mut String) {
    let n = line.chars().count();
    if n < 72 {
        line.push_str(&" ".repeat(72 - n));
    }
}

fn area_b_through_72(line: &str) -> String {
    let mut chars: Vec<char> = line.chars().collect();
    while chars.len() < 72 {
        chars.push(' ');
    }
    chars.into_iter().skip(7).take(65).collect()
}

fn literal_continuation_payload(line: &str, quote: char) -> String {
    let body = area_b_through_72(line);
    let t = body.trim_start();
    if t.starts_with(quote) {
        t[quote.len_utf8()..].to_string()
    } else {
        t.to_string()
    }
}

fn nonliteral_continuation_payload(line: &str) -> String {
    area_b_through_72(line).trim().to_string()
}

#[derive(Clone, Debug)]
struct ReplPair {
    from: String,
    to: String,
    leading: bool,
    trailing: bool,
}

fn expand_copies(source: &str, origin: &Path, depth: u32) -> String {
    let mut out = String::new();
    let mut pending = String::new();
    for line in source.lines() {
        if pending.is_empty() {
            if is_copy_start(line) || is_sql_include_start(line) {
                pending.push_str(line);
                if has_terminator_period(&pending) {
                    out.push_str(&expand_directive(&pending, origin, depth));
                    pending.clear();
                }
            } else {
                out.push_str(line);
                out.push('\n');
            }
        } else {
            pending.push(' ');
            pending.push_str(line.trim());
            if has_terminator_period(&pending) {
                out.push_str(&expand_directive(&pending, origin, depth));
                pending.clear();
            }
        }
    }
    if !pending.is_empty() {
        out.push_str(&expand_directive(&pending, origin, depth));
    }
    out
}

fn is_copy_start(line: &str) -> bool {
    let t = line.trim_start().to_uppercase();
    t.starts_with("COPY ") || t == "COPY"
}

fn is_sql_include_start(line: &str) -> bool {
    let t = line.trim_start().to_uppercase();
    t.contains("EXEC SQL") && t.contains("INCLUDE")
}

fn expand_directive(stmt: &str, origin: &Path, depth: u32) -> String {
    let text = strip_terminator(stmt.trim());
    let upper = text.to_uppercase();
    if upper.contains("EXEC SQL") && upper.contains("INCLUDE") {
        if let Some(name) = sql_include_name(text) {
            return insert_member(&name, &[], origin, depth, stmt);
        }
        return format!("{stmt}\n");
    }
    if let Some((name, pairs)) = parse_copy_statement(text) {
        return insert_member(&name, &pairs, origin, depth, stmt);
    }
    format!("{stmt}\n")
}

fn sql_include_name(text: &str) -> Option<String> {
    let upper = text.to_uppercase();
    let idx = upper.find("INCLUDE")?;
    let after = text[idx + "INCLUDE".len()..].trim();
    let name = after
        .split_whitespace()
        .next()?
        .trim_end_matches('.')
        .to_string();
    if name.eq_ignore_ascii_case("END-EXEC") || name.is_empty() {
        None
    } else {
        Some(name)
    }
}

fn parse_copy_statement(text: &str) -> Option<(String, Vec<ReplPair>)> {
    let mut toks = split_words(text);
    if toks.is_empty() || !toks[0].eq_ignore_ascii_case("COPY") {
        return None;
    }
    toks.remove(0);
    if toks.is_empty() {
        return None;
    }
    let name = toks.remove(0);
    if !toks.is_empty()
        && (toks[0].eq_ignore_ascii_case("OF") || toks[0].eq_ignore_ascii_case("IN"))
        && toks.len() >= 2
    {
        toks.remove(0);
        toks.remove(0);
    }
    let pairs = if !toks.is_empty() && toks[0].eq_ignore_ascii_case("REPLACING") {
        parse_replacing_clause(&text[text.to_uppercase().find("REPLACING")? + 9..])
    } else {
        Vec::new()
    };
    Some((name, pairs))
}

fn split_words(s: &str) -> Vec<String> {
    s.split_whitespace()
        .map(|w| w.trim_end_matches(['.', ',']).to_string())
        .filter(|w| !w.is_empty())
        .collect()
}

fn parse_replacing_clause(s: &str) -> Vec<ReplPair> {
    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;
    let mut pairs = Vec::new();
    while i < chars.len() {
        skip_ws(&chars, &mut i);
        if i >= chars.len() {
            break;
        }
        let mut leading = false;
        let mut trailing = false;
        if match_word(&chars, &mut i, "LEADING") {
            leading = true;
        } else if match_word(&chars, &mut i, "TRAILING") {
            trailing = true;
        }
        skip_ws(&chars, &mut i);
        let Some(from) = take_operand(&chars, &mut i) else {
            break;
        };
        skip_ws(&chars, &mut i);
        if !match_word(&chars, &mut i, "BY") {
            break;
        }
        skip_ws(&chars, &mut i);
        let Some(to) = take_operand(&chars, &mut i) else {
            break;
        };
        pairs.push(ReplPair {
            from,
            to,
            leading,
            trailing,
        });
    }
    pairs
}

fn skip_ws(chars: &[char], i: &mut usize) {
    while *i < chars.len() && chars[*i].is_whitespace() {
        *i += 1;
    }
}

fn match_word(chars: &[char], i: &mut usize, word: &str) -> bool {
    let rest: String = chars[*i..].iter().collect();
    if rest.len() >= word.len()
        && rest[..word.len()].eq_ignore_ascii_case(word)
        && rest
            .chars()
            .nth(word.len())
            .is_none_or(|c| c.is_whitespace() || c == '=' || c == '.')
    {
        *i += word.len();
        true
    } else {
        false
    }
}

fn take_operand(chars: &[char], i: &mut usize) -> Option<String> {
    skip_ws(chars, i);
    if *i + 1 < chars.len() && chars[*i] == '=' && chars[*i + 1] == '=' {
        *i += 2;
        let start = *i;
        while *i + 1 < chars.len() && !(chars[*i] == '=' && chars[*i + 1] == '=') {
            *i += 1;
        }
        let text: String = chars[start..*i].iter().collect();
        if *i + 1 < chars.len() {
            *i += 2;
        }
        return Some(text);
    }
    let start = *i;
    while *i < chars.len() && !chars[*i].is_whitespace() && chars[*i] != '.' {
        *i += 1;
    }
    if start == *i {
        None
    } else {
        Some(chars[start..*i].iter().collect())
    }
}

fn insert_member(
    name: &str,
    pairs: &[ReplPair],
    origin: &Path,
    depth: u32,
    original: &str,
) -> String {
    if depth >= COPY_DEPTH {
        return format!("{original}\n");
    }
    let Some(path) = find_copybook(name, origin) else {
        return format!("{original}\n");
    };
    match std::fs::read_to_string(&path) {
        Ok(raw) => {
            let continued = apply_continuation(&raw);
            let replaced = apply_replacing(&continued, pairs);
            let nested = expand_copies(&replaced, &path, depth + 1);
            if nested.ends_with('\n') {
                nested
            } else {
                format!("{nested}\n")
            }
        }
        Err(_) => format!("{original}\n"),
    }
}

fn find_copybook(name: &str, origin: &Path) -> Option<PathBuf> {
    let candidates = member_filenames(name);
    for dir in search_dirs(origin) {
        if let Some(found) = lookup_in_dir(&dir, &candidates) {
            return Some(found);
        }
        if let Ok(rd) = std::fs::read_dir(&dir) {
            for ent in rd.flatten() {
                let p = ent.path();
                if p.is_dir() {
                    if let Some(found) = lookup_in_dir(&p, &candidates) {
                        return Some(found);
                    }
                }
            }
        }
    }
    None
}

fn lookup_in_dir(dir: &Path, candidates: &[String]) -> Option<PathBuf> {
    for cand in candidates {
        let p = dir.join(cand);
        if p.is_file() {
            return Some(p);
        }
    }
    if let Ok(rd) = std::fs::read_dir(dir) {
        for ent in rd.flatten() {
            let fname = ent.file_name();
            let s = fname.to_string_lossy();
            let stem = Path::new(&*s)
                .file_stem()
                .and_then(|x| x.to_str())
                .unwrap_or("");
            if stem.eq_ignore_ascii_case(candidates[0].trim_end_matches('.'))
                || s.eq_ignore_ascii_case(&candidates[0])
            {
                let p = ent.path();
                if p.is_file()
                    && candidates.iter().any(|c| {
                        s.eq_ignore_ascii_case(c)
                            || stem.eq_ignore_ascii_case(c)
                            || Path::new(c)
                                .file_stem()
                                .and_then(|x| x.to_str())
                                .is_some_and(|st| st.eq_ignore_ascii_case(stem))
                    })
                {
                    return Some(p);
                }
            }
        }
    }
    None
}

fn member_filenames(name: &str) -> Vec<String> {
    let n = name.trim_end_matches('.');
    vec![
        n.to_string(),
        format!("{n}.cpy"),
        format!("{n}.CPY"),
        format!("{n}.cbl"),
        format!("{n}.CBL"),
        format!("{n}.cob"),
        format!("{n}.COB"),
        format!("{n}.copy"),
        format!("{n}.COPY"),
    ]
}

fn search_dirs(origin: &Path) -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    let start = if origin.is_file() {
        origin.parent().unwrap_or(origin).to_path_buf()
    } else {
        origin.to_path_buf()
    };
    let mut cur = start;
    for _ in 0..6 {
        dirs.push(cur.clone());
        for name in [
            "COPYBOOK",
            "COPYLIB",
            "copybooks",
            "COPYLIB-MVS",
            "cpy",
            "copy",
            "Copybook",
        ] {
            dirs.push(cur.join(name));
        }
        match cur.parent() {
            Some(p) => cur = p.to_path_buf(),
            None => break,
        }
    }
    dirs
}

fn apply_replacing(text: &str, pairs: &[ReplPair]) -> String {
    if pairs.is_empty() {
        return text.to_string();
    }
    let mut out = String::new();
    for line in text.lines() {
        if is_cobol_comment_or_blank(line) {
            out.push_str(line);
            out.push('\n');
            continue;
        }
        out.push_str(&apply_pairs_line(line, pairs));
        out.push('\n');
    }
    if text.ends_with('\n') && !out.ends_with('\n') {
        out.push('\n');
    }
    out
}

fn is_dummy_tag(s: &str) -> bool {
    let t = s.trim();
    (t.starts_with(':') && t.ends_with(':') && t.len() > 2)
        || (t.starts_with('(') && t.ends_with(')') && t.len() > 2)
}

fn apply_pairs_line(line: &str, pairs: &[ReplPair]) -> String {
    let mut words = tokenize_library_line(line);
    for pair in pairs {
        if pair.leading {
            for w in &mut words {
                if !w.space && w.text.to_uppercase().starts_with(&pair.from.to_uppercase()) {
                    let rest = w.text[pair.from.len()..].to_string();
                    w.text = format!("{}{rest}", pair.to);
                }
            }
        } else if pair.trailing {
            for w in &mut words {
                if !w.space && w.text.to_uppercase().ends_with(&pair.from.to_uppercase()) {
                    let keep = w.text.len().saturating_sub(pair.from.len());
                    let head = w.text[..keep].to_string();
                    w.text = format!("{head}{}", pair.to);
                }
            }
        } else if is_dummy_tag(&pair.from) {
            for w in &mut words {
                if !w.space {
                    w.text = replace_dummy(&w.text, &pair.from, &pair.to);
                }
            }
        } else {
            words = replace_word_sequence(words, &pair.from, &pair.to);
        }
    }
    words.into_iter().map(|w| w.text).collect()
}

fn replace_dummy(text: &str, from: &str, to: &str) -> String {
    let upper = text.to_uppercase();
    let from_u = from.to_uppercase();
    let mut out = String::new();
    let mut i = 0;
    while i < text.len() {
        if upper[i..].starts_with(&from_u) {
            out.push_str(to);
            i += from.len();
        } else {
            let ch = text[i..].chars().next().unwrap();
            out.push(ch);
            i += ch.len_utf8();
        }
    }
    out
}

struct LibTok {
    text: String,
    space: bool,
}

fn tokenize_library_line(line: &str) -> Vec<LibTok> {
    let mut out = Vec::new();
    let mut cur = String::new();
    let mut space = false;
    for c in line.chars() {
        if c.is_whitespace() {
            if !cur.is_empty() {
                out.push(LibTok {
                    text: std::mem::take(&mut cur),
                    space: false,
                });
            }
            space = true;
        } else {
            if space {
                out.push(LibTok {
                    text: " ".to_string(),
                    space: true,
                });
                space = false;
            }
            cur.push(c);
        }
    }
    if space {
        out.push(LibTok {
            text: " ".to_string(),
            space: true,
        });
    }
    if !cur.is_empty() {
        out.push(LibTok {
            text: cur,
            space: false,
        });
    }
    out
}

fn replace_word_sequence(words: Vec<LibTok>, from: &str, to: &str) -> Vec<LibTok> {
    let from_words: Vec<String> = from.split_whitespace().map(|s| s.to_uppercase()).collect();
    if from_words.is_empty() {
        return words;
    }
    let mut out = Vec::new();
    let mut i = 0;
    while i < words.len() {
        if !words[i].space && word_seq_matches(&words, i, &from_words) {
            out.push(LibTok {
                text: to.to_string(),
                space: false,
            });
            i += span_for_seq(&words, i, from_words.len());
        } else {
            out.push(LibTok {
                text: words[i].text.clone(),
                space: words[i].space,
            });
            i += 1;
        }
    }
    out
}

fn word_seq_matches(words: &[LibTok], start: usize, from_words: &[String]) -> bool {
    let mut wi = start;
    let mut fi = 0;
    while fi < from_words.len() {
        while wi < words.len() && words[wi].space {
            wi += 1;
        }
        if wi >= words.len() {
            return false;
        }
        if words[wi].text.to_uppercase() != from_words[fi] {
            return false;
        }
        wi += 1;
        fi += 1;
    }
    true
}

fn span_for_seq(words: &[LibTok], start: usize, nwords: usize) -> usize {
    let mut wi = start;
    let mut seen = 0;
    while seen < nwords && wi < words.len() {
        if !words[wi].space {
            seen += 1;
        }
        wi += 1;
    }
    wi - start
}

fn apply_replace(source: &str) -> String {
    let mut out = String::new();
    let mut pending = String::new();
    let mut active: Vec<ReplPair> = Vec::new();
    for line in source.lines() {
        if pending.is_empty() {
            if is_replace_start(line) {
                pending.push_str(line);
                if has_terminator_period(&pending) {
                    apply_replace_directive(&pending, &mut active);
                    pending.clear();
                }
            } else {
                out.push_str(&apply_replacing_line_active(line, &active));
                out.push('\n');
            }
        } else {
            pending.push(' ');
            pending.push_str(line.trim());
            if has_terminator_period(&pending) {
                apply_replace_directive(&pending, &mut active);
                pending.clear();
            }
        }
    }
    if !pending.is_empty() {
        apply_replace_directive(&pending, &mut active);
    }
    out
}

fn is_replace_start(line: &str) -> bool {
    let t = line.trim_start().to_uppercase();
    t.starts_with("REPLACE ") || t == "REPLACE"
}

fn apply_replace_directive(stmt: &str, active: &mut Vec<ReplPair>) {
    let text = strip_terminator(stmt.trim());
    let upper = text.to_uppercase();
    if upper.ends_with("OFF") && upper.trim() == "REPLACE OFF" || upper == "REPLACE OFF" {
        active.clear();
        return;
    }
    if let Some(idx) = upper.find("REPLACE") {
        let rest = &text[idx + 7..];
        *active = parse_replacing_clause(rest);
    }
}

fn apply_replacing_line_active(line: &str, active: &[ReplPair]) -> String {
    if active.is_empty() {
        line.to_string()
    } else {
        apply_replacing(line, active)
            .trim_end_matches('\n')
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pad72(s: &str) -> String {
        let mut t = s.to_string();
        while t.len() < 72 {
            t.push(' ');
        }
        t.truncate(72);
        t
    }

    fn area_b(line: &str) -> String {
        let mut chars: Vec<char> = line.chars().collect();
        while chars.len() < 72 {
            chars.push(' ');
        }
        chars.truncate(72);
        chars[7..].iter().collect()
    }

    #[test]
    fn ibm_65_unclosed_literal_drops_continuation_quote_keeps_col72_spaces() {
        // IBM Enterprise COBOL 6.5 Language Reference SC27-8713-04,
        // "Continuation of alphanumeric and national literals":
        // https://www.ibm.com/docs/en/cobol-zos/6.5.0?topic=b-continuation-lines
        // Example letters A-M. Spaces through column 72 of each continued line
        // are in the literal. The quotation mark that starts the continuation
        // is not.
        let a_to_e = "AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEE";
        let g_to_k = "GGGGGGGGGGHHHHHHHHHHIIIIIIIIIIJJJJJJJJJJKKKKKKKKKK";
        let l_to_m = "LLLLLLLLLLMMMMMMMMMM";
        let line1 = pad72(&format!("000001 \"{a_to_e}"));
        let line2 = pad72(&format!("      - \"{g_to_k}"));
        let line3 = pad72(&format!("      - \"{l_to_m}\""));
        assert_eq!(line1.len(), 72);
        assert_eq!(line2.len(), 72);
        assert_eq!(line3.len(), 72);

        let content1: String = line1.chars().skip(8).collect();
        let after_q2 = area_b(&line2).trim_start()[1..].to_string();
        let after_q3 = area_b(&line3).trim_start()[1..].to_string();
        let expected = format!("000001 \"{content1}{after_q2}{after_q3}");

        let got = apply_continuation(&format!("{line1}\n{line2}\n{line3}\n"));
        let got_line = got.trim_end_matches('\n');
        assert_eq!(got_line, expected);
        let inner = got_line
            .split_once('"')
            .unwrap()
            .1
            .rsplit_once('"')
            .unwrap()
            .0;
        assert!(
            !inner.contains('"'),
            "continuation quotes must not be in the value: {inner:?}"
        );
        assert!(inner.starts_with(a_to_e), "{inner:?}");
        assert!(inner.contains(g_to_k), "{inner:?}");
        assert!(inner.ends_with(l_to_m), "{inner:?}");
        assert_eq!(inner.matches(a_to_e).count(), 1);
        assert!(inner.len() > a_to_e.len() + g_to_k.len() + l_to_m.len());
    }
}
