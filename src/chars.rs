//! Character classes and text splitting with the exact semantics of the
//! Haskell implementation (`Data.Char`, `Data.Text.lines`/`words`).
//!
//! Rust's own helpers (`char::is_whitespace`, `char::is_alphanumeric`,
//! `str::lines`, `str::split_whitespace`) differ in edge cases, so don't use
//! them for anything that affects parsing or output.

use unicode_general_category::{GeneralCategory as G, get_general_category};

/// `Data.Char.isSpace`: the Latin-1 spaces (`' '`, `\t\n\v\f\r`, U+00A0), plus
/// every other `Zs` character. Unlike `char::is_whitespace`, this excludes
/// U+0085, U+2028 and U+2029.
pub fn is_space(c: char) -> bool {
    let u = c as u32;
    if u <= 0x377 {
        u == 32 || (9..=13).contains(&u) || u == 0xa0
    } else {
        get_general_category(c) == G::SpaceSeparator
    }
}

/// `isAlphaNum c || isSymbol c || isPunctuation c`, minus `\`, `(` and `)`.
/// These are the characters an identifier (term name / file name) may contain.
pub fn is_ident_char(c: char) -> bool {
    if c == '\\' || c == '(' || c == ')' {
        return false;
    }
    matches!(
        get_general_category(c),
        // isAlpha
        G::UppercaseLetter
            | G::LowercaseLetter
            | G::TitlecaseLetter
            | G::ModifierLetter
            | G::OtherLetter
            // isNumber
            | G::DecimalNumber
            | G::LetterNumber
            | G::OtherNumber
            // isSymbol
            | G::MathSymbol
            | G::CurrencySymbol
            | G::ModifierSymbol
            | G::OtherSymbol
            // isPunctuation
            | G::ConnectorPunctuation
            | G::DashPunctuation
            | G::OpenPunctuation
            | G::ClosePunctuation
            | G::InitialPunctuation
            | G::FinalPunctuation
            | G::OtherPunctuation
    )
}

/// `Data.Text.lines`: split on `\n` only (`\r` is kept). A trailing newline
/// doesn't produce an empty final line, and the empty string has no lines.
pub fn lines(s: &str) -> Vec<String> {
    if s.is_empty() {
        return Vec::new();
    }
    let body = s.strip_suffix('\n').unwrap_or(s);
    body.split('\n').map(str::to_owned).collect()
}

/// `Data.Text.unlines`: every line followed by `\n`.
pub fn unlines<S: AsRef<str>>(ls: &[S]) -> String {
    let mut out = String::new();
    for l in ls {
        out.push_str(l.as_ref());
        out.push('\n');
    }
    out
}

/// `Data.Text.words`: split on [`is_space`], dropping empty pieces.
pub fn words(s: &str) -> Vec<&str> {
    s.split(is_space).filter(|w| !w.is_empty()).collect()
}

/// `Data.Text.unwords`.
pub fn unwords<S: AsRef<str>>(ws: &[S]) -> String {
    ws.iter().map(AsRef::as_ref).collect::<Vec<_>>().join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lines_match_haskell() {
        assert_eq!(lines(""), Vec::<String>::new());
        assert_eq!(lines("\n"), vec![""]);
        assert_eq!(lines("a"), vec!["a"]);
        assert_eq!(lines("a\n"), vec!["a"]);
        assert_eq!(lines("a\n\n"), vec!["a", ""]);
        assert_eq!(lines("a\r\nb"), vec!["a\r", "b"]);
    }

    #[test]
    fn space_matches_haskell() {
        for c in [
            ' ', '\t', '\n', '\u{b}', '\u{c}', '\r', '\u{a0}', '\u{3000}',
        ] {
            assert!(is_space(c), "{c:?}");
        }
        for c in ['\u{85}', '\u{2028}', '\u{2029}', 'a', '\u{1c}'] {
            assert!(!is_space(c), "{c:?}");
        }
    }

    #[test]
    fn ident_chars() {
        for c in [
            'a', 'Z', '0', '-', '>', '.', '/', '{', '"', 'é', 'α', '→', '✓', '«',
        ] {
            assert!(is_ident_char(c), "{c:?}");
        }
        for c in ['\\', '(', ')', ' ', '\n', '\u{301}'] {
            assert!(!is_ident_char(c), "{c:?}");
        }
    }
}
