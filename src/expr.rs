//! Expressions: the AST, lexer, parser and pretty printer.
//!
//! Grammar (application is left-associative, lambdas extend to the right):
//!
//! ```text
//! expr  ::= '\' name+ '->' expr | atom+
//! atom  ::= name | '(' expr ')'
//! ```
//!
//! A name is one or more letters, digits, symbols or punctuation characters,
//! except `\`, `(` and `)`. A name ends before `->`.

use std::fmt;

use crate::chars::{is_ident_char, is_space, unlines};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Abs(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Var(String),
    /// Lines of text from `source`. `in_error` marks text that is reported as
    /// `[text from "SOURCE"]` in error messages.
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
}

// ---------------------------------------------------------------------------
// Pretty printing

/// Render an expression as source text. For expressions built only from `Abs`,
/// `App` and `Var`, `parse_expr(&pp_expr(e)) == Ok(e)`.
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
        Expr::Lit { content, .. } => out.push_str(&unlines(content)),
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

/// Render tokens back as source text, for error messages.
fn render(ls: &[Lexeme]) -> String {
    let mut out = String::new();
    for (i, l) in ls.iter().enumerate() {
        if i > 0 && ls[i - 1] != Lexeme::Lambda {
            out.push(' ');
        }
        match l {
            Lexeme::Lambda => out.push('\\'),
            Lexeme::Arrow => out.push_str("->"),
            Lexeme::Ident(x) => out.push_str(x),
            Lexeme::Bracketed(inner) => {
                out.push('(');
                out.push_str(&render(inner));
                out.push(')');
            }
        }
    }
    out
}

pub fn lex(input: &str) -> Result<Vec<Lexeme>, ParseError> {
    let chars: Vec<char> = input.chars().collect();
    let (ls, pos) = lex_many(&chars, 0)?;
    match chars.get(pos) {
        None => Ok(ls),
        Some(')') => Err(ParseError::UnmatchedParen(pos + 1)),
        Some(&c) => Err(ParseError::UnexpectedChar(c, pos + 1)),
    }
}

/// Lex tokens until the end of input or a character that can't start one.
/// Returns the position of that character (after any spaces).
fn lex_many(s: &[char], mut pos: usize) -> Result<(Vec<Lexeme>, usize), ParseError> {
    let mut out = Vec::new();
    loop {
        pos = skip_space(s, pos);
        let Some(&c) = s.get(pos) else { break };
        let lexeme = match c {
            '\\' => {
                pos += 1;
                Lexeme::Lambda
            }
            '-' if s.get(pos + 1) == Some(&'>') => {
                pos += 2;
                Lexeme::Arrow
            }
            '(' => {
                let (inner, q) = lex_many(s, pos + 1)?;
                if s.get(q) != Some(&')') {
                    return Err(match s.get(q) {
                        None => ParseError::UnclosedParen(pos + 1),
                        Some(&c) => ParseError::UnexpectedChar(c, q + 1),
                    });
                }
                pos = q + 1;
                Lexeme::Bracketed(inner)
            }
            c if is_ident_char(c) => {
                let start = pos;
                while pos < s.len()
                    && is_ident_char(s[pos])
                    && !(s[pos] == '-' && s.get(pos + 1) == Some(&'>'))
                {
                    pos += 1;
                }
                Lexeme::Ident(s[start..pos].iter().collect())
            }
            _ => break,
        };
        out.push(lexeme);
    }
    Ok((out, pos))
}

fn skip_space(s: &[char], mut pos: usize) -> usize {
    while pos < s.len() && is_space(s[pos]) {
        pos += 1;
    }
    pos
}

// ---------------------------------------------------------------------------
// Parsing

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    /// `\(x) -> …`: lambda parameters must be names.
    BracketsInsideLambda(Vec<Lexeme>),
    /// `\x \y -> …`
    LambdaInsideLambda(Vec<Lexeme>),
    /// `f \x -> …`: a lambda argument needs parentheses. Holds the function.
    LambdaInStaleScope(Vec<Lexeme>, Expr),
    /// `f -> …`
    ArrowInScope(Vec<Lexeme>),
    /// `\x y` with no `->`.
    LambdaWithoutArrow,
    EmptyExpression,
    /// Column (1-based) of a `(` that is never closed.
    UnclosedParen(usize),
    /// Column of a `)` with no matching `(`.
    UnmatchedParen(usize),
    /// A character that can't appear in an expression, and its column.
    UnexpectedChar(char, usize),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::BracketsInsideLambda(ls) => write!(
                f,
                "[Parse Error] Lambda parameters must be names, but found brackets at: {}",
                render(ls)
            ),
            ParseError::LambdaInsideLambda(ls) => write!(
                f,
                "[Parse Error] A lambda can't start inside another lambda's parameters, at: {}",
                render(ls)
            ),
            ParseError::LambdaInStaleScope(ls, e) => write!(
                f,
                "[Parse Error] A lambda used as an argument must be in parentheses: {} ({})",
                pp_expr(e),
                render(ls)
            ),
            ParseError::ArrowInScope(ls) => write!(
                f,
                "[Parse Error] Found an arrow without a lambda, at: {}",
                render(ls)
            ),
            ParseError::LambdaWithoutArrow => {
                write!(f, "[Parse Error] A lambda is missing its arrow and body")
            }
            ParseError::EmptyExpression => write!(f, "[Parse Error] Empty expression"),
            ParseError::UnclosedParen(col) => {
                write!(f, "[Parse Error] Unclosed parenthesis at column {col}")
            }
            ParseError::UnmatchedParen(col) => {
                write!(
                    f,
                    "[Parse Error] Unmatched closing parenthesis at column {col}"
                )
            }
            ParseError::UnexpectedChar(c, col) => write!(
                f,
                "[Parse Error] Unexpected character {c:?} (U+{:04X}) at column {col}",
                *c as u32
            ),
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

pub fn parse_expr(input: &str) -> Result<Expr, ParseError> {
    let lexemes = lex(input)?;
    let mut state = ParseState::Fresh;
    expr(&lexemes, &mut state)
}

/// Parse lexemes in state `st`. On success, the caller replaces `st`.
///
/// This is a loop rather than recursion over the lexemes, and it moves the
/// expression built so far instead of copying it, so a long expression parses
/// in linear time and constant stack. Lambda parameters are collected and
/// wrapped around the body at the end, since a lambda extends to the right.
fn expr(mut ls: &[Lexeme], st: &mut ParseState) -> Result<Expr, ParseError> {
    let mut params: Vec<String> = Vec::new();
    loop {
        let Some((head, rest)) = ls.split_first() else {
            let body = match std::mem::replace(st, ParseState::Fresh) {
                ParseState::Stale(e) => e,
                ParseState::InsideLambda => return Err(ParseError::LambdaWithoutArrow),
                ParseState::Fresh => return Err(ParseError::EmptyExpression),
            };
            return Ok(params.into_iter().rev().fold(body, |e, n| Expr::abs(n, e)));
        };
        match head {
            Lexeme::Lambda => match st {
                ParseState::InsideLambda => {
                    return Err(ParseError::LambdaInsideLambda(ls.to_vec()));
                }
                ParseState::Stale(e) => {
                    return Err(ParseError::LambdaInStaleScope(ls.to_vec(), e.clone()));
                }
                ParseState::Fresh => *st = ParseState::InsideLambda,
            },
            Lexeme::Arrow => match st {
                ParseState::InsideLambda => *st = ParseState::Fresh,
                _ => return Err(ParseError::ArrowInScope(ls.to_vec())),
            },
            Lexeme::Ident(x) => match std::mem::replace(st, ParseState::Fresh) {
                ParseState::InsideLambda => {
                    params.push(x.clone());
                    *st = ParseState::InsideLambda;
                }
                ParseState::Fresh => *st = ParseState::Stale(Expr::var(x.clone())),
                ParseState::Stale(f) => {
                    *st = ParseState::Stale(Expr::app(f, Expr::var(x.clone())));
                }
            },
            Lexeme::Bracketed(bs) => match std::mem::replace(st, ParseState::Fresh) {
                ParseState::InsideLambda => {
                    return Err(ParseError::BracketsInsideLambda(ls.to_vec()));
                }
                ParseState::Fresh => {
                    let e = expr(bs, st)?;
                    *st = ParseState::Stale(e);
                }
                ParseState::Stale(f) => {
                    let e = expr(bs, st)?;
                    *st = ParseState::Stale(Expr::app(f, e));
                }
            },
        }
        ls = rest;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(x: &str) -> Expr {
        Expr::var(x)
    }

    fn err(src: &str) -> String {
        parse_expr(src).unwrap_err().to_string()
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
        let id = Expr::abs("x", v("x"));
        assert_eq!(
            parse_expr(r"\x y -> x"),
            Ok(Expr::abs("x", Expr::abs("y", v("x"))))
        );
        assert_eq!(parse_expr(r"\x ->x"), Ok(id.clone()));
        assert_eq!(parse_expr(r"\x->x"), Ok(id.clone()));
        assert_eq!(parse_expr(r"(\x->x)"), Ok(id));
        assert_eq!(
            parse_expr("a-b c->d"),
            Err(ParseError::ArrowInScope(vec![
                Lexeme::Arrow,
                Lexeme::Ident("d".into())
            ]))
        );
    }

    #[test]
    fn rejects_leftover_input() {
        assert_eq!(
            err("foo bar )"),
            "[Parse Error] Unmatched closing parenthesis at column 9"
        );
        assert_eq!(
            err("foo (bar"),
            "[Parse Error] Unclosed parenthesis at column 5"
        );
        assert_eq!(
            err("(foo (bar)"),
            "[Parse Error] Unclosed parenthesis at column 1"
        );
        assert_eq!(
            err("foo \u{301}"),
            r"[Parse Error] Unexpected character '\u{301}' (U+0301) at column 5"
        );
        assert_eq!(err("()"), "[Parse Error] Empty expression");
    }

    #[test]
    fn error_messages() {
        assert_eq!(
            err(r"\(x) -> x"),
            r"[Parse Error] Lambda parameters must be names, but found brackets at: (x) -> x"
        );
        assert_eq!(
            err(r"foo \x -> x"),
            r"[Parse Error] A lambda used as an argument must be in parentheses: foo (\x -> x)"
        );
        assert_eq!(
            err(r"\x"),
            "[Parse Error] A lambda is missing its arrow and body"
        );
    }

    #[test]
    fn pretty_prints() {
        let e = parse_expr(r"(\y -> g y) ((\y -> g y) x)").unwrap();
        assert_eq!(pp_expr(&e), r"(\y -> g y) ((\y -> g y) x)");
    }
}
