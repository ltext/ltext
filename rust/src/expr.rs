//! Expressions: the AST, lexer, parser and pretty printer.
//!
//! Port of `LText.Expr`. The lexer and parser reproduce the Haskell behaviour
//! exactly, quirks included; see `docs/rust-port.md`.

use std::fmt;

use crate::chars::{is_ident_char, is_space};
use crate::hsshow::{show_string, show_string_list};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Abs(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Var(String),
    Lit {
        content: Vec<String>,
        source: String,
        in_error: bool,
    },
    Concat {
        left: Box<Expr>,
        right: Box<Expr>,
        source: String,
        in_error: bool,
    },
}

impl Expr {
    pub fn abs(n: impl Into<String>, e: Expr) -> Expr {
        Expr::Abs(n.into(), Box::new(e))
    }
    pub fn app(f: Expr, a: Expr) -> Expr {
        Expr::App(Box::new(f), Box::new(a))
    }
    pub fn var(n: impl Into<String>) -> Expr {
        Expr::Var(n.into())
    }
    pub fn lit(content: Vec<String>, source: impl Into<String>, in_error: bool) -> Expr {
        Expr::Lit {
            content,
            source: source.into(),
            in_error,
        }
    }
    pub fn concat(left: Expr, right: Expr, source: impl Into<String>, in_error: bool) -> Expr {
        Expr::Concat {
            left: Box::new(left),
            right: Box::new(right),
            source: source.into(),
            in_error,
        }
    }

    /// Haskell's derived `showsPrec`.
    fn shows_prec(&self, d: u8, out: &mut String) {
        let paren = d >= 11;
        if paren {
            out.push('(');
        }
        match self {
            Expr::Abs(n, e) => {
                out.push_str("Abs ");
                out.push_str(&show_string(n));
                out.push(' ');
                e.shows_prec(11, out);
            }
            Expr::App(f, a) => {
                out.push_str("App ");
                f.shows_prec(11, out);
                out.push(' ');
                a.shows_prec(11, out);
            }
            Expr::Var(n) => {
                out.push_str("Var ");
                out.push_str(&show_string(n));
            }
            Expr::Lit {
                content,
                source,
                in_error,
            } => {
                out.push_str("Lit {litContent = ");
                out.push_str(&show_string_list(content));
                out.push_str(", litSource = ");
                out.push_str(&show_string(source));
                out.push_str(", litInError = ");
                out.push_str(show_bool(*in_error));
                out.push('}');
            }
            Expr::Concat {
                left,
                right,
                source,
                in_error,
            } => {
                out.push_str("Concat {concatLeft = ");
                left.shows_prec(0, out);
                out.push_str(", concatRight = ");
                right.shows_prec(0, out);
                out.push_str(", concatSource = ");
                out.push_str(&show_string(source));
                out.push_str(", concatInError = ");
                out.push_str(show_bool(*in_error));
                out.push('}');
            }
        }
        if paren {
            out.push(')');
        }
    }

    /// Haskell `show`, used in error messages.
    pub fn hs_show(&self) -> String {
        let mut s = String::new();
        self.shows_prec(0, &mut s);
        s
    }
}

fn show_bool(b: bool) -> &'static str {
    if b { "True" } else { "False" }
}

// ---------------------------------------------------------------------------
// Pretty printing

/// `ppExpr`. The Haskell version uses `Text.PrettyPrint`, but every layout it
/// builds renders on a single line, with pieces separated by one space.
pub fn pp_expr(e: &Expr) -> String {
    let mut out = String::new();
    pp(e, &mut out);
    out
}

fn pp(e: &Expr, out: &mut String) {
    match e {
        Expr::Abs(x, body) => {
            out.push('\\');
            out.push_str(x);
            out.push_str(" -> ");
            pp(body, out);
        }
        Expr::App(f, a) => {
            pp_parens(f, matches!(**f, Expr::Abs(..)), out);
            out.push(' ');
            pp_parens(a, matches!(**a, Expr::Abs(..) | Expr::App(..)), out);
        }
        Expr::Var(x) => out.push_str(x),
        Expr::Lit {
            source,
            in_error: true,
            ..
        }
        | Expr::Concat {
            source,
            in_error: true,
            ..
        } => {
            out.push_str("[text from \"");
            out.push_str(source);
            out.push_str("\"]");
        }
        Expr::Lit { content, .. } => out.push_str(&crate::chars::unlines(content)),
        Expr::Concat { left, right, .. } => {
            pp(left, out);
            out.push(' ');
            pp(right, out);
        }
    }
}

fn pp_parens(e: &Expr, parens: bool, out: &mut String) {
    if parens {
        out.push('(');
    }
    pp(e, out);
    if parens {
        out.push(')');
    }
}

// ---------------------------------------------------------------------------
// Lexing

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lexeme {
    Lambda,
    Arrow,
    Ident(String),
    Bracketed(Vec<Lexeme>),
}

impl Lexeme {
    fn hs_show(&self) -> String {
        match self {
            Lexeme::Lambda => "Lambda".into(),
            Lexeme::Arrow => "Arrow".into(),
            Lexeme::Ident(x) => format!("Ident {}", show_string(x)),
            Lexeme::Bracketed(ls) => format!("Bracketed {{getBracketed = {}}}", show_lexemes(ls)),
        }
    }
}

fn show_lexemes(ls: &[Lexeme]) -> String {
    let items: Vec<String> = ls.iter().map(Lexeme::hs_show).collect();
    format!("[{}]", items.join(","))
}

/// `many (lambda <|> arrow <|> bracketed <|> ident)` run with `parseOnly`:
/// lexes as many tokens as possible and silently ignores whatever is left.
pub fn lex(input: &str) -> Vec<Lexeme> {
    let chars: Vec<char> = input.chars().collect();
    lex_many(&chars, 0).0
}

fn lex_many(s: &[char], mut pos: usize) -> (Vec<Lexeme>, usize) {
    let mut out = Vec::new();
    while let Some((lexeme, next)) = lex_token(s, pos) {
        out.push(lexeme);
        pos = next;
    }
    (out, pos)
}

fn skip_space(s: &[char], mut pos: usize) -> usize {
    while pos < s.len() && is_space(s[pos]) {
        pos += 1;
    }
    pos
}

/// One token. On failure the caller's position is left untouched, which matches
/// attoparsec's backtracking across `<|>` alternatives.
fn lex_token(s: &[char], pos: usize) -> Option<(Lexeme, usize)> {
    let p = skip_space(s, pos);
    match s.get(p) {
        Some('\\') => return Some((Lexeme::Lambda, p + 1)),
        Some('-') if s.get(p + 1) == Some(&'>') => return Some((Lexeme::Arrow, p + 2)),
        Some('(') => {
            let (inner, q) = lex_many(s, p + 1);
            let q = skip_space(s, q);
            if s.get(q) == Some(&')') {
                return Some((Lexeme::Bracketed(inner), q + 1));
            }
            // Fall through to `ident`, which can't start with '(' either.
            return None;
        }
        _ => {}
    }
    let start = p;
    let mut end = p;
    while end < s.len() && is_ident_char(s[end]) {
        end += 1;
    }
    (end > start).then(|| (Lexeme::Ident(s[start..end].iter().collect()), end))
}

// ---------------------------------------------------------------------------
// Parsing

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    BracketsInsideLambda(Vec<Lexeme>),
    LambdaInsideLambda(Vec<Lexeme>),
    LambdaInStaleScope(Vec<Lexeme>, Expr),
    ArrowWithoutLambda(Vec<Lexeme>),
    ArrowInScope(Vec<Lexeme>),
    EmptyExpression,
    LexerError(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::BracketsInsideLambda(ls) => write!(
                f,
                "[Parse Error] Brackets are inside a lambda declaration, with trailing token stream: {}",
                show_lexemes(ls)
            ),
            ParseError::LambdaInsideLambda(ls) => write!(
                f,
                "[Parse Error] A lambda is inside a lambda declaration, with trailing token stream: {}",
                show_lexemes(ls)
            ),
            ParseError::LambdaInStaleScope(ls, e) => write!(
                f,
                "[Parse Error] A lambda is inside a stale scope, with trailing token stream: {} and parse state {}",
                show_lexemes(ls),
                e.hs_show()
            ),
            ParseError::ArrowWithoutLambda(ls) => write!(
                f,
                "[Parse Error] An arrow was found without a preceding lambda, with trailing token stream: {}",
                show_lexemes(ls)
            ),
            ParseError::ArrowInScope(ls) => write!(
                f,
                "[Parse Error] An arrow alone was found inside a function body, with trailing token stream: {}",
                show_lexemes(ls)
            ),
            ParseError::EmptyExpression => write!(f, "[Parse Error] Empty expression"),
            ParseError::LexerError(err) => write!(f, "[Lexer Error] {err}"),
        }
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ParseState {
    InsideLambda,
    Fresh,
    Stale(Expr),
}

/// `runParse`: lex and parse an expression with a fresh parser state.
pub fn parse_expr(input: &str) -> Result<Expr, ParseError> {
    let lexemes = lex(input);
    let mut state = ParseState::Fresh;
    expr(&lexemes, &mut state)
}

fn expr(ls: &[Lexeme], st: &mut ParseState) -> Result<Expr, ParseError> {
    let Some((head, rest)) = ls.split_first() else {
        return match st {
            ParseState::Stale(e) => Ok(e.clone()),
            _ => Err(ParseError::EmptyExpression),
        };
    };
    match head {
        Lexeme::Lambda => match st {
            ParseState::InsideLambda => Err(ParseError::LambdaInsideLambda(ls.to_vec())),
            ParseState::Stale(e) => Err(ParseError::LambdaInStaleScope(ls.to_vec(), e.clone())),
            ParseState::Fresh => {
                *st = ParseState::InsideLambda;
                expr(rest, st)
            }
        },
        Lexeme::Arrow => match st {
            ParseState::InsideLambda => {
                *st = ParseState::Fresh;
                expr(rest, st)
            }
            _ => Err(ParseError::ArrowInScope(ls.to_vec())),
        },
        Lexeme::Ident(x) => match st {
            ParseState::InsideLambda => Ok(Expr::abs(x.clone(), expr(rest, st)?)),
            ParseState::Fresh => {
                *st = ParseState::Stale(Expr::var(x.clone()));
                expr(rest, st)
            }
            ParseState::Stale(f) => {
                *st = ParseState::Stale(Expr::app(f.clone(), Expr::var(x.clone())));
                expr(rest, st)
            }
        },
        Lexeme::Bracketed(bs) => match st {
            ParseState::InsideLambda => Err(ParseError::BracketsInsideLambda(ls.to_vec())),
            ParseState::Fresh => {
                let e = expr(bs, st)?;
                *st = ParseState::Stale(e);
                expr(rest, st)
            }
            ParseState::Stale(f) => {
                let f = f.clone();
                *st = ParseState::Fresh;
                let e = expr(bs, st)?;
                *st = ParseState::Stale(Expr::app(f, e));
                expr(rest, st)
            }
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(x: &str) -> Expr {
        Expr::var(x)
    }

    #[test]
    fn parses_application_left_assoc() {
        assert_eq!(
            parse_expr("f a b"),
            Ok(Expr::app(Expr::app(v("f"), v("a")), v("b")))
        );
        assert_eq!(
            parse_expr("f (a b)"),
            Ok(Expr::app(v("f"), Expr::app(v("a"), v("b"))))
        );
    }

    #[test]
    fn parses_lambdas() {
        assert_eq!(
            parse_expr(r"\x y -> x"),
            Ok(Expr::abs("x", Expr::abs("y", v("x"))))
        );
        assert_eq!(parse_expr(r"\x ->x"), Ok(Expr::abs("x", v("x"))));
        assert_eq!(parse_expr(r"\x->x"), Err(ParseError::EmptyExpression));
    }

    #[test]
    fn lexer_quirks() {
        assert_eq!(parse_expr("foo bar )"), Ok(Expr::app(v("foo"), v("bar"))));
        assert_eq!(parse_expr("foo (bar"), Ok(v("foo")));
        assert_eq!(parse_expr("(foo bar"), Err(ParseError::EmptyExpression));
        assert_eq!(parse_expr("()"), Err(ParseError::EmptyExpression));
    }

    #[test]
    fn error_messages() {
        assert_eq!(
            parse_expr(r"\(x) -> x").unwrap_err().to_string(),
            r#"[Parse Error] Brackets are inside a lambda declaration, with trailing token stream: [Bracketed {getBracketed = [Ident "x"]},Arrow,Ident "x"]"#
        );
        assert_eq!(
            parse_expr(r"foo \x -> x").unwrap_err().to_string(),
            r#"[Parse Error] A lambda is inside a stale scope, with trailing token stream: [Lambda,Ident "x",Arrow,Ident "x"] and parse state Var "foo""#
        );
    }

    #[test]
    fn pretty_prints() {
        let e = parse_expr(r"(\y -> g y) ((\y -> g y) x)").unwrap();
        assert_eq!(pp_expr(&e), r"(\y -> g y) ((\y -> g y) x)");
    }
}
