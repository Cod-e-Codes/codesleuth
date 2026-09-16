use once_cell::sync::Lazy;
use regex::Regex;
use std::collections::{HashMap, HashSet};

pub static KEYWORDS: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    HashSet::from([
        "MOVE",
        "PERFORM",
        "READ",
        "WRITE",
        "DISPLAY",
        "IF",
        "ELSE",
        "END-IF",
        "UNTIL",
        "AT",
        "END",
        "STOP",
        "CALL",
        "EXEC",
        "SQL",
        "OPEN",
        "CLOSE",
        "FETCH",
        "COMPUTE",
        "SET",
        "IS",
        "NOT",
        "EQUAL",
        "THEN",
        "USING",
        "FROM",
        "BY",
        "TO",
        "VARYING",
        "AND",
        "OR",
        ">",
        "<",
        "=",
        ".",
        "(",
        ")",
        "FUNCTION",
        "INTO",
        "AFTER",
        "ADVANCING",
        "END-EXEC",
        "RETURN",
        "RUN",
        "INPUT",
        "OUTPUT",
        "I-O",
        "EXTEND",
        "CURRENT-DATE",
        "GOBACK",
    ])
});

pub static LITERALS: Lazy<HashMap<&'static str, &'static str>> = Lazy::new(|| {
    HashMap::from([
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
    ])
});

static RE_NUMERIC: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"^-?\d+(\.\d+)?$").expect("numeric literal regex"));
static RE_NONWORD: Lazy<Regex> = Lazy::new(|| Regex::new(r"^\W+$").expect("nonword regex"));
static RE_IDENT: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"^[A-Z0-9\-_]+$").expect("identifier regex"));

pub fn normalize_name(s: &str) -> String {
    s.trim().trim_end_matches('.').to_uppercase()
}

pub fn is_literal(name: &str) -> bool {
    let n = name.trim();
    if (n.starts_with('\'') && n.ends_with('\'')) || (n.starts_with('"') && n.ends_with('"')) {
        return true;
    }
    if LITERALS.contains_key(n.to_uppercase().as_str()) {
        return true;
    }
    if n.parse::<f64>().is_ok() || RE_NUMERIC.is_match(n) {
        return true;
    }
    n.starts_with('\'') || n.ends_with('\'') || n.starts_with('"') || n.ends_with('"')
}

pub fn is_valid_identifier(name: &str) -> bool {
    let name = name
        .trim()
        .trim_end_matches('.')
        .trim_matches('"')
        .trim_matches('\'')
        .to_uppercase();
    if name.is_empty() {
        return false;
    }
    if KEYWORDS.contains(name.as_str()) {
        return false;
    }
    if RE_NUMERIC.is_match(&name) {
        return false;
    }
    if RE_NONWORD.is_match(&name) {
        return false;
    }
    RE_IDENT.is_match(&name)
}

pub fn is_cobol_comment_or_blank(line: &str) -> bool {
    let t = line.trim();
    t.is_empty() || t.starts_with('*')
}

pub fn terminator_period_index(s: &str) -> Option<usize> {
    let bytes = s.as_bytes();
    let mut i = 0;
    let mut in_single = false;
    let mut in_double = false;
    while i < bytes.len() {
        let c = bytes[i];
        if !in_double && c == b'\'' {
            if in_single && i + 1 < bytes.len() && bytes[i + 1] == b'\'' {
                i += 2;
                continue;
            }
            in_single = !in_single;
        } else if !in_single && c == b'"' {
            if in_double && i + 1 < bytes.len() && bytes[i + 1] == b'"' {
                i += 2;
                continue;
            }
            in_double = !in_double;
        } else if !in_single
            && !in_double
            && c == b'.'
            && s[i + 1..].chars().all(char::is_whitespace)
        {
            return Some(i);
        }
        i += 1;
    }
    None
}

pub fn has_terminator_period(s: &str) -> bool {
    terminator_period_index(s).is_some()
}

pub fn strip_terminator(s: &str) -> &str {
    if let Some(i) = terminator_period_index(s) {
        s[..i].trim_end()
    } else {
        s.trim_end()
    }
}

pub fn split_cobol_tokens(s: &str) -> Vec<String> {
    let s = strip_terminator(s).trim();
    let mut tokens = Vec::new();
    let mut cur = String::new();
    let mut chars = s.chars().peekable();
    let mut in_single = false;
    let mut in_double = false;
    while let Some(c) = chars.next() {
        if !in_double && c == '\'' {
            cur.push(c);
            if in_single && chars.peek() == Some(&'\'') {
                cur.push(chars.next().unwrap());
            } else {
                in_single = !in_single;
            }
        } else if !in_single && c == '"' {
            cur.push(c);
            if in_double && chars.peek() == Some(&'"') {
                cur.push(chars.next().unwrap());
            } else {
                in_double = !in_double;
            }
        } else if !in_single && !in_double && c.is_whitespace() {
            if !cur.is_empty() {
                tokens.push(std::mem::take(&mut cur));
            }
        } else {
            cur.push(c);
        }
    }
    if !cur.is_empty() {
        tokens.push(cur);
    }
    tokens
}

pub fn format_value(raw: Option<&str>) -> String {
    let Some(s) = raw else {
        return String::new();
    };
    let v_str = s.trim_matches('"').to_uppercase();
    if let Some(literal) = LITERALS.get(v_str.as_str()) {
        return (*literal).to_string();
    }
    if v_str.starts_with('\'') && v_str.ends_with('\'') {
        return format!("\"{}\"", v_str.trim_matches('\''));
    }
    if v_str.starts_with('"') && v_str.ends_with('"') {
        return v_str;
    }
    v_str
}
