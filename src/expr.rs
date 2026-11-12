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

use smallvec::smallvec;

use crate::Stack;
use std::fmt;

use crate::chars::{is_ident_char, is_space, unlines};

// None of the code below recurses once per node: documents and generated
// expressions can be hundreds of thousands of nodes deep, which would
// overflow the stack. Traversals use explicit stacks instead, and so do
// `Clone` and `Drop`.

#[derive(Debug, PartialEq, Eq)]
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

/// An expression taken apart, with its children unboxed. `Expr` implements
/// `Drop`, so it can't be destructured by value; [`Expr::into_node`] does it
/// instead.
pub enum Node {
    Abs(String, Expr),
    App(Expr, Expr),
    Var(String),
    Lit {
        content: Vec<String>,
        source: String,
        in_error: bool,
    },
    Concat {
        left: Expr,
        right: Expr,
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

    /// A placeholder left behind when a child is moved out; it owns nothing.
    fn hole() -> Expr {
        Expr::Var(String::new())
    }

    /// Take the expression apart, moving its children out.
    pub fn into_node(mut self) -> Node {
        let take = |b: &mut Box<Expr>| std::mem::replace(&mut **b, Expr::hole());
        match &mut self {
            Expr::Abs(n, b) => Node::Abs(std::mem::take(n), take(b)),
            Expr::App(f, a) => Node::App(take(f), take(a)),
            Expr::Var(n) => Node::Var(std::mem::take(n)),
            Expr::Lit {
                content,
                source,
                in_error,
            } => Node::Lit {
                content: std::mem::take(content),
                source: std::mem::take(source),
                in_error: *in_error,
            },
            Expr::Concat {
                left,
                right,
                source,
                in_error,
            } => Node::Concat {
                left: take(left),
                right: take(right),
                source: std::mem::take(source),
                in_error: *in_error,
            },
        }
    }
}

impl From<Node> for Expr {
    fn from(n: Node) -> Expr {
        match n {
            Node::Abs(n, b) => Expr::abs(n, b),
            Node::App(f, a) => Expr::app(f, a),
            Node::Var(n) => Expr::Var(n),
            Node::Lit {
                content,
                source,
                in_error,
            } => Expr::lit(content, source, in_error),
            Node::Concat {
                left,
                right,
                source,
                in_error,
            } => Expr::concat(left, right, source, in_error),
        }
    }
}

impl Expr {
    fn is_leaf(&self) -> bool {
        matches!(self, Expr::Var(_) | Expr::Lit { .. })
    }
}

impl Drop for Expr {
    fn drop(&mut self) {
        // Move the children onto a stack, so that dropping them doesn't
        // recurse; each one is emptied the same way before it's dropped.
        // Leaves, and nodes whose children are leaves, need none of this.
        fn push_children(e: &mut Expr, stack: &mut Vec<Expr>) {
            let mut take = |b: &mut Box<Expr>| {
                if !b.is_leaf() {
                    stack.push(std::mem::replace(&mut **b, Expr::hole()));
                }
            };
            match e {
                Expr::Abs(_, b) => take(b),
                Expr::App(l, r)
                | Expr::Concat {
                    left: l, right: r, ..
                } => {
                    take(l);
                    take(r);
                }
                Expr::Var(_) | Expr::Lit { .. } => {}
            }
        }
        let deep = match self {
            Expr::Abs(_, b) => !b.is_leaf(),
            Expr::App(l, r)
            | Expr::Concat {
                left: l, right: r, ..
            } => !l.is_leaf() || !r.is_leaf(),
            Expr::Var(_) | Expr::Lit { .. } => false,
        };
        if !deep {
            return;
        }
        let mut stack = Vec::new();
        push_children(self, &mut stack);
        while let Some(mut e) = stack.pop() {
            push_children(&mut e, &mut stack);
        }
    }
}

/// How deep the fast, recursive paths may go before switching to an
/// explicit stack. Small enough to be safe on any thread's stack.
const RECURSION_LIMIT: usize = 128;

impl Clone for Expr {
    fn clone(&self) -> Expr {
        clone_at(self, 0)
    }
}

/// Clone recursively near the root, which is fastest, and with an explicit
/// stack below `RECURSION_LIMIT`.
fn clone_at(e: &Expr, depth: usize) -> Expr {
    if depth == RECURSION_LIMIT {
        return clone_deep(e);
    }
    match e {
        Expr::Abs(n, b) => Expr::abs(n.clone(), clone_at(b, depth + 1)),
        Expr::App(f, a) => Expr::app(clone_at(f, depth + 1), clone_at(a, depth + 1)),
        Expr::Var(n) => Expr::Var(n.clone()),
        Expr::Lit {
            content,
            source,
            in_error,
        } => Expr::lit(content.clone(), source.clone(), *in_error),
        Expr::Concat {
            left,
            right,
            source,
            in_error,
        } => Expr::concat(
            clone_at(left, depth + 1),
            clone_at(right, depth + 1),
            source.clone(),
            *in_error,
        ),
    }
}

/// Clone with an explicit stack.
fn clone_deep(e: &Expr) -> Expr {
    enum Step<'a> {
        Visit(&'a Expr),
        Abs(&'a str),
        App,
        Concat(&'a str, bool),
    }
    let mut steps: Stack<_> = smallvec![Step::Visit(e)];
    let mut done: Stack<Expr> = Stack::new();
    while let Some(step) = steps.pop() {
        match step {
            Step::Visit(e) => match e {
                Expr::Abs(n, b) => {
                    steps.push(Step::Abs(n));
                    steps.push(Step::Visit(b));
                }
                Expr::App(f, a) => {
                    steps.push(Step::App);
                    steps.push(Step::Visit(a));
                    steps.push(Step::Visit(f));
                }
                Expr::Concat {
                    left,
                    right,
                    source,
                    in_error,
                } => {
                    steps.push(Step::Concat(source, *in_error));
                    steps.push(Step::Visit(right));
                    steps.push(Step::Visit(left));
                }
                Expr::Var(n) => done.push(Expr::Var(n.clone())),
                Expr::Lit {
                    content,
                    source,
                    in_error,
                } => done.push(Expr::lit(content.clone(), source.clone(), *in_error)),
            },
            Step::Abs(n) => {
                let b = done.pop().expect("body");
                done.push(Expr::abs(n, b));
            }
            Step::App => {
                let a = done.pop().expect("argument");
                let f = done.pop().expect("function");
                done.push(Expr::app(f, a));
            }
            Step::Concat(source, in_error) => {
                let r = done.pop().expect("right");
                let l = done.pop().expect("left");
                done.push(Expr::concat(l, r, source, in_error));
            }
        }
    }
    done.pop().expect("result")
}

// ---------------------------------------------------------------------------
// Pretty printing

/// Render an expression as source text. For expressions built only from `Abs`,
/// `App` and `Var`, `parse_expr(&pp_expr(e)) == Ok(e)`.
pub fn pp_expr(e: &Expr) -> String {
    enum Item<'a> {
        Expr(&'a Expr),
        Str(&'a str),
    }
    let mut out = String::new();
    // Items still to print, last first.
    let mut todo: Stack<_> = smallvec![Item::Expr(e)];
    fn parens<'a>(todo: &mut Stack<Item<'a>>, e: &'a Expr, yes: bool) {
        if yes {
            todo.push(Item::Str(")"));
        }
        todo.push(Item::Expr(e));
        if yes {
            todo.push(Item::Str("("));
        }
    }
    while let Some(item) = todo.pop() {
        let e = match item {
            Item::Str(s) => {
                out.push_str(s);
                continue;
            }
            Item::Expr(e) => e,
        };
        match e {
            Expr::Abs(x, body) => {
                out.push('\\');
                out.push_str(x);
                out.push_str(" -> ");
                todo.push(Item::Expr(body));
            }
            Expr::App(f, a) => {
                parens(&mut todo, a, matches!(**a, Expr::Abs(..) | Expr::App(..)));
                todo.push(Item::Str(" "));
                parens(&mut todo, f, matches!(**f, Expr::Abs(..)));
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
                todo.push(Item::Expr(right));
                todo.push(Item::Str(" "));
                todo.push(Item::Expr(left));
            }
        }
    }
    out
}

// ---------------------------------------------------------------------------
// Lexing

/// A token. Parentheses are tokens of their own; the lexer checks that they
/// balance.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lexeme {
    Lambda,
    Arrow,
    Ident(String),
    Open,
    Close,
}

/// Render tokens back as source text, for error messages.
fn render(ls: &[Lexeme]) -> String {
    let mut out = String::new();
    for (i, l) in ls.iter().enumerate() {
        let space =
            i > 0 && !matches!(ls[i - 1], Lexeme::Lambda | Lexeme::Open) && *l != Lexeme::Close;
        if space {
            out.push(' ');
        }
        match l {
            Lexeme::Lambda => out.push('\\'),
            Lexeme::Arrow => out.push_str("->"),
            Lexeme::Ident(x) => out.push_str(x),
            Lexeme::Open => out.push('('),
            Lexeme::Close => out.push(')'),
        }
    }
    out
}

pub fn lex(input: &str) -> Result<Vec<Lexeme>, ParseError> {
    let s: Vec<char> = input.chars().collect();
    let mut out = Vec::new();
    // Positions of the parentheses that are still open.
    let mut open: Vec<usize> = Vec::new();
    let mut pos = 0;
    loop {
        pos = skip_space(&s, pos);
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
                open.push(pos);
                pos += 1;
                Lexeme::Open
            }
            ')' if !open.is_empty() => {
                open.pop();
                pos += 1;
                Lexeme::Close
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
    match (s.get(pos), open.last()) {
        (None, None) => Ok(out),
        // The innermost unclosed parenthesis.
        (None, Some(&p)) => Err(ParseError::UnclosedParen(p + 1)),
        (Some(')'), _) => Err(ParseError::UnmatchedParen(pos + 1)),
        (Some(&c), _) => Err(ParseError::UnexpectedChar(c, pos + 1)),
    }
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
    parse(&lex(input)?)
}

/// The tokens from `i` to the end of the parenthesised group that contains
/// it (or to the end of the input), for error messages.
fn rest_of_group(ls: &[Lexeme], i: usize) -> Vec<Lexeme> {
    let mut depth = 0usize;
    for (j, l) in ls.iter().enumerate().skip(i) {
        match l {
            Lexeme::Open => depth += 1,
            Lexeme::Close if depth == 0 => return ls[i..j].to_vec(),
            Lexeme::Close => depth -= 1,
            _ => {}
        }
    }
    ls[i..].to_vec()
}

/// One parenthesised group being parsed: its state, and the lambda
/// parameters seen so far, which wrap its body at the end since a lambda
/// extends to the right.
struct Group {
    st: ParseState,
    params: Vec<String>,
}

impl Group {
    fn new() -> Group {
        Group {
            st: ParseState::Fresh,
            params: Vec::new(),
        }
    }

    fn finish(self) -> Result<Expr, ParseError> {
        let body = match self.st {
            ParseState::Stale(e) => e,
            ParseState::InsideLambda => return Err(ParseError::LambdaWithoutArrow),
            ParseState::Fresh => return Err(ParseError::EmptyExpression),
        };
        Ok(self
            .params
            .into_iter()
            .rev()
            .fold(body, |e, n| Expr::abs(n, e)))
    }
}

/// Parse balanced tokens. Groups are kept on a stack rather than parsed
/// recursively, so deep nesting can't overflow the call stack.
fn parse(ls: &[Lexeme]) -> Result<Expr, ParseError> {
    // The enclosing groups, each waiting for the group inside it.
    let mut outer: Vec<Group> = Vec::new();
    let mut g = Group::new();
    for (i, l) in ls.iter().enumerate() {
        match l {
            Lexeme::Lambda => match &g.st {
                ParseState::InsideLambda => {
                    return Err(ParseError::LambdaInsideLambda(rest_of_group(ls, i)));
                }
                ParseState::Stale(e) => {
                    return Err(ParseError::LambdaInStaleScope(
                        rest_of_group(ls, i),
                        e.clone(),
                    ));
                }
                ParseState::Fresh => g.st = ParseState::InsideLambda,
            },
            Lexeme::Arrow => match g.st {
                ParseState::InsideLambda => g.st = ParseState::Fresh,
                _ => return Err(ParseError::ArrowInScope(rest_of_group(ls, i))),
            },
            Lexeme::Ident(x) => match std::mem::replace(&mut g.st, ParseState::Fresh) {
                ParseState::InsideLambda => {
                    g.params.push(x.clone());
                    g.st = ParseState::InsideLambda;
                }
                ParseState::Fresh => g.st = ParseState::Stale(Expr::var(x.clone())),
                ParseState::Stale(f) => {
                    g.st = ParseState::Stale(Expr::app(f, Expr::var(x.clone())));
                }
            },
            Lexeme::Open => {
                if matches!(g.st, ParseState::InsideLambda) {
                    return Err(ParseError::BracketsInsideLambda(rest_of_group(ls, i)));
                }
                // The enclosing group keeps its state: `Fresh`, or `Stale(f)`
                // to be applied to this group.
                outer.push(std::mem::replace(&mut g, Group::new()));
            }
            Lexeme::Close => {
                let e = std::mem::replace(&mut g, outer.pop().expect("balanced")).finish()?;
                g.st = match std::mem::replace(&mut g.st, ParseState::Fresh) {
                    ParseState::Stale(f) => ParseState::Stale(Expr::app(f, e)),
                    _ => ParseState::Stale(e),
                };
            }
        }
    }
    g.finish()
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
