//! Documents: text files with an optional arity header. Port of `LText.Document`.

use smallvec::smallvec;

use crate::Stack;
use std::fmt;

use crate::Error;
use crate::chars::{lines, unlines, unwords, words};
use crate::expr::{Expr, ParseError, parse_expr, pp_expr};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Document {
    /// The document's parameters, from its header line.
    pub arity: Vec<String>,
    pub body: Vec<DocumentBody>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DocumentBody {
    RawText(Vec<String>),
    Expression(Expr),
}

/// Merge adjacent `RawText` blocks.
pub fn repack_document(ds: Vec<DocumentBody>) -> Vec<DocumentBody> {
    let mut acc: Vec<DocumentBody> = Vec::new();
    for d in ds {
        match (acc.last_mut(), d) {
            (Some(DocumentBody::RawText(t)), DocumentBody::RawText(t2)) => t.extend(t2),
            (_, d) => acc.push(d),
        }
    }
    acc
}

/// Words of a line, if it has at least three: (first, middle, last).
fn delimited(line: &str) -> Option<(&str, Vec<&str>, &str)> {
    let ws = words(line);
    if ws.len() < 3 {
        return None;
    }
    Some((ws[0], ws[1..ws.len() - 1].to_vec(), ws[ws.len() - 1]))
}

/// A document's left and right delimiters.
pub type Delims = (String, String);

/// A parse error in an expression line of a document.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocParseError {
    /// 1-based line number.
    pub line: usize,
    /// The expression text between the delimiters.
    pub expr: String,
    pub err: ParseError,
}

/// Parse a document, returning its left and right delimiters if it has a
/// header.
pub fn parse_document(text: &str) -> Result<(Document, Option<Delims>), DocParseError> {
    let all = lines(text);
    let Some((head, body)) = all.split_first() else {
        return Ok((
            Document {
                arity: vec![],
                body: vec![],
            },
            None,
        ));
    };
    let Some((l, params, r)) = delimited(head) else {
        // No header: arity 0, and the whole file is text.
        return Ok((
            Document {
                arity: vec![],
                body: vec![DocumentBody::RawText(all.clone())],
            },
            None,
        ));
    };

    let mut out: Vec<DocumentBody> = Vec::new();
    for (i, line) in body.iter().enumerate() {
        match delimited(line) {
            Some((l2, inner, r2)) if l2 == l && r2 == r => {
                out.push(DocumentBody::Expression({
                    let src = unwords(&inner);
                    parse_expr(&src).map_err(|err| DocParseError {
                        line: i + 2,
                        expr: src.clone(),
                        err,
                    })?
                }));
            }
            _ => match out.last_mut() {
                Some(DocumentBody::RawText(t)) => t.push(line.clone()),
                _ => out.push(DocumentBody::RawText(vec![line.clone()])),
            },
        }
    }
    let arity = params.into_iter().map(str::to_owned).collect();
    Ok((
        Document { arity, body: out },
        Some((l.to_owned(), r.to_owned())),
    ))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrintError {
    /// An expression line that contains text, which can't be written to a
    /// file. Holds the offending expression, with its text marked.
    ConcatExprText(Expr),
    NoExplicitDelimiters,
}

impl fmt::Display for PrintError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrintError::ConcatExprText(e) => write!(
                f,
                "[Print Error] Can't print textual data while inside an expression:\n\n    {}\n\n...cannot be rendered to a file.",
                pp_expr(e)
            ),
            PrintError::NoExplicitDelimiters => write!(
                f,
                "[Print Error] Can't render a document with arity without explicit --left and --right delimiters"
            ),
        }
    }
}

impl std::error::Error for PrintError {}

pub fn print_document(delims: Option<(&str, &str)>, doc: &Document) -> Result<String, PrintError> {
    let mut bs: Vec<String> = Vec::new();
    for b in &doc.body {
        match b {
            DocumentBody::RawText(t) => bs.extend(t.iter().cloned()),
            DocumentBody::Expression(e) => {
                let (ld, rd) = delims.ok_or(PrintError::NoExplicitDelimiters)?;
                bs.push(format!("{ld} {} {rd}", pp_expr(e)));
            }
        }
    }
    if doc.arity.is_empty() {
        return Ok(unlines(&bs));
    }
    let (ld, rd) = delims.ok_or(PrintError::NoExplicitDelimiters)?;
    let mut header = vec![ld.to_owned()];
    header.extend(doc.arity.iter().cloned());
    header.push(rd.to_owned());
    let mut all = vec![unwords(&header)];
    all.extend(bs);
    Ok(unlines(&all))
}

/// Turn a document into an expression: one `Abs` per parameter, wrapped around
/// the body joined with `Concat`.
pub fn from_document(source: &str, doc: &Document) -> Expr {
    let block = |b: &DocumentBody| match b {
        DocumentBody::RawText(t) => Expr::lit(t.clone(), source, false),
        DocumentBody::Expression(e) => e.clone(),
    };
    // A right-nested chain, built from the end.
    let body = match doc.body.split_last() {
        None => Expr::lit(vec![], source, false),
        Some((last, init)) => init.iter().rev().fold(block(last), |acc, b| {
            Expr::concat(block(b), acc, source, false)
        }),
    };
    doc.arity
        .iter()
        .rev()
        .fold(body, |e, n| Expr::abs(n.clone(), e))
}

/// Does `e` contain any text? Text can't appear inside an expression line,
/// because it would end up between the delimiters.
fn contains_text(e: &Expr) -> bool {
    let mut todo: Stack<_> = smallvec![e];
    while let Some(e) = todo.pop() {
        match e {
            Expr::Lit { .. } | Expr::Concat { .. } => return true,
            Expr::Abs(_, b) => todo.push(b),
            Expr::App(f, a) => {
                todo.push(a);
                todo.push(f);
            }
            Expr::Var(_) => {}
        }
    }
    false
}

/// Replace text inside an expression with a `[text from "FILE"]` marker, for
/// error messages.
fn mark_text(e: &Expr) -> Expr {
    enum Step<'a> {
        Visit(&'a Expr),
        Abs(&'a str),
        App,
    }
    let mut steps: Stack<_> = smallvec![Step::Visit(e)];
    let mut done: Stack<Expr> = Stack::new();
    while let Some(step) = steps.pop() {
        match step {
            Step::Visit(e) => match e {
                Expr::Lit {
                    content, source, ..
                } => done.push(Expr::lit(content.clone(), source.clone(), true)),
                Expr::Concat {
                    left,
                    right,
                    source,
                    ..
                } => done.push(Expr::concat(
                    (**left).clone(),
                    (**right).clone(),
                    source.clone(),
                    true,
                )),
                Expr::Abs(n, b) => {
                    steps.push(Step::Abs(n));
                    steps.push(Step::Visit(b));
                }
                Expr::App(f, a) => {
                    steps.push(Step::App);
                    steps.push(Step::Visit(a));
                    steps.push(Step::Visit(f));
                }
                Expr::Var(_) => done.push(e.clone()),
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
        }
    }
    done.pop().expect("result")
}

/// Turn an evaluated expression back into a document: leading lambdas become
/// the header, text becomes raw lines, and anything else becomes an
/// expression line, which must not contain text.
pub fn to_document(e: &Expr) -> Result<Document, PrintError> {
    let mut arity = Vec::new();
    let mut e = e;
    while let Expr::Abs(n, b) = e {
        arity.push(n.clone());
        e = b;
    }
    let mut body = Vec::new();
    // The parts of a `Concat` tree, in order, last on top.
    let mut todo: Stack<_> = smallvec![e];
    while let Some(e) = todo.pop() {
        match e {
            Expr::Lit { content, .. } => body.push(DocumentBody::RawText(content.clone())),
            Expr::Concat { left, right, .. } => {
                todo.push(right);
                todo.push(left);
            }
            e if contains_text(e) => return Err(PrintError::ConcatExprText(mark_text(e))),
            e => body.push(DocumentBody::Expression(e.clone())),
        }
    }
    Ok(Document { arity, body })
}

fn read_file(path: &str) -> Result<String, Error> {
    std::fs::read_to_string(path).map_err(|e| Error::Io(path.to_owned(), e))
}

/// Read and parse a document file into an expression.
pub fn fetch_document(path: &str) -> Result<Expr, Error> {
    let (doc, _) = parse_document(&read_file(path)?)
        .map_err(|err| Error::DocParse(path.to_owned(), Box::new(err)))?;
    Ok(from_document(path, &doc))
}

/// Read a file as plain text, with no header.
pub fn raw_document(path: &str) -> Result<Expr, Error> {
    Ok(Expr::lit(lines(&read_file(path)?), path, false))
}
