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
