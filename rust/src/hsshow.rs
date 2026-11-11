//! Reproduces Haskell's derived `Show` output for the values that appear in
//! error messages (strings, lexeme lists, expressions).

use std::fmt::Write;

const ASCII_NAMES: [&str; 33] = [
    "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "BS", "HT", "LF", "VT", "FF", "CR",
    "SO", "SI", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "EM", "SUB", "ESC",
    "FS", "GS", "RS", "US", "SP",
];

/// `show :: String -> String`, including the surrounding quotes.
pub fn show_string(s: &str) -> String {
    let mut out = String::from("\"");
    let chars: Vec<char> = s.chars().collect();
    for (i, &c) in chars.iter().enumerate() {
        let next = chars.get(i + 1).copied();
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\u{7f}' => out.push_str("\\DEL"),
            c if (c as u32) > 0x7f => {
                write!(out, "\\{}", c as u32).unwrap();
                if next.is_some_and(|n| n.is_ascii_digit()) {
                    out.push_str("\\&");
                }
            }
            c if c >= ' ' => out.push(c),
            '\u{7}' => out.push_str("\\a"),
            '\u{8}' => out.push_str("\\b"),
            '\u{c}' => out.push_str("\\f"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{b}' => out.push_str("\\v"),
            '\u{e}' => {
                out.push_str("\\SO");
                if next == Some('H') {
                    out.push_str("\\&");
                }
            }
            c => {
                out.push('\\');
                out.push_str(ASCII_NAMES[c as usize]);
            }
        }
    }
    out.push('"');
    out
}

/// `show :: [String] -> String`.
pub fn show_string_list<S: AsRef<str>>(xs: &[S]) -> String {
    let items: Vec<String> = xs.iter().map(|x| show_string(x.as_ref())).collect();
    format!("[{}]", items.join(","))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strings() {
        assert_eq!(show_string("abc"), r#""abc""#);
        assert_eq!(show_string("a\"b\\c"), r#""a\"b\\c""#);
        assert_eq!(show_string("héllo"), r#""h\233llo""#);
        assert_eq!(show_string("é1"), r#""\233\&1""#);
        assert_eq!(show_string("\n\t\u{0}\u{7f}"), r#""\n\t\NUL\DEL""#);
        assert_eq!(show_string("\u{e}H"), r#""\SO\&H""#);
    }
}
