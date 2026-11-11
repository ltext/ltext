//! Documents: text files with an optional arity header. Port of `LText.Document`.

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

/// Parse a document, returning its left and right delimiters if it has a header.
pub fn parse_document(text: &str) -> Result<(Document, Option<(String, String)>), ParseError> {
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
    for line in body {
        match delimited(line) {
            Some((l2, inner, r2)) if l2 == l && r2 == r => {
                out.push(DocumentBody::Expression(parse_expr(&unwords(&inner))?));
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
    /// Text inside an `Abs` or `App`, which can't be written to a file.
    ConcatExprText(Expr),
    NoExplicitDelimiters,
}

impl fmt::Display for PrintError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrintError::ConcatExprText(e) => write!(
                f,
                "[Print Error] Can't print textual data while inside an expression: \n\n{}\n\n...cannot be rendered to a file.",
                pp_expr(&decorate_unprintable(e))
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
    fn go(source: &str, body: &[DocumentBody]) -> Expr {
        match body {
            [] => Expr::lit(vec![], source, false),
            [DocumentBody::RawText(t)] => Expr::lit(t.clone(), source, false),
            [DocumentBody::Expression(e)] => e.clone(),
            [DocumentBody::RawText(t), rest @ ..] => Expr::concat(
                Expr::lit(t.clone(), source, false),
                go(source, rest),
                source,
                false,
            ),
            [DocumentBody::Expression(e), rest @ ..] => {
                Expr::concat(e.clone(), go(source, rest), source, false)
            }
        }
    }
    doc.arity
        .iter()
        .rev()
        .fold(go(source, &doc.body), |e, n| Expr::abs(n.clone(), e))
}

#[derive(Clone, Copy)]
enum Mode {
    Top,
    InsideConcat,
    InsideExpr,
}

/// Mark text that sits inside an expression, so it can be reported.
pub fn decorate_unprintable(e: &Expr) -> Expr {
    go(Mode::Top, e)
}

fn go(mode: Mode, e: &Expr) -> Expr {
    match (mode, e) {
        (
            Mode::Top | Mode::InsideConcat,
            Expr::Concat {
                left,
                right,
                source,
                ..
            },
        ) => Expr::concat(
            go(Mode::InsideConcat, left),
            go(Mode::InsideConcat, right),
            source.clone(),
            false,
        ),
        (Mode::Top, Expr::Abs(n, b)) => Expr::abs(n.clone(), go(Mode::Top, b)),
        (Mode::Top, Expr::App(f, a)) => Expr::app(go(Mode::Top, f), go(Mode::Top, a)),
        (Mode::InsideConcat | Mode::InsideExpr, Expr::Abs(n, b)) => {
            Expr::abs(n.clone(), go(Mode::InsideExpr, b))
        }
        (Mode::InsideConcat | Mode::InsideExpr, Expr::App(f, a)) => {
            Expr::app(go(Mode::InsideExpr, f), go(Mode::InsideExpr, a))
        }
        (
            Mode::InsideExpr,
            Expr::Concat {
                left,
                right,
                source,
                ..
            },
        ) => Expr::concat((**left).clone(), (**right).clone(), source.clone(), true),
        (
            Mode::InsideExpr,
            Expr::Lit {
                content, source, ..
            },
        ) => Expr::lit(content.clone(), source.clone(), true),
        _ => e.clone(),
    }
}

pub fn is_any_unprintable(e: &Expr) -> bool {
    match e {
        Expr::Concat { in_error: true, .. } | Expr::Lit { in_error: true, .. } => true,
        Expr::Concat { left, right, .. } | Expr::App(left, right) => {
            is_any_unprintable(left) || is_any_unprintable(right)
        }
        Expr::Abs(_, b) => is_any_unprintable(b),
        _ => false,
    }
}

pub fn to_document(e: &Expr) -> Result<Document, PrintError> {
    if is_any_unprintable(&decorate_unprintable(e)) {
        return Err(PrintError::ConcatExprText(e.clone()));
    }
    let mut arity = Vec::new();
    let mut e = e;
    while let Expr::Abs(n, b) = e {
        arity.push(n.clone());
        e = b;
    }
    fn body(e: &Expr, out: &mut Vec<DocumentBody>) {
        match e {
            Expr::Lit { content, .. } => out.push(DocumentBody::RawText(content.clone())),
            Expr::Concat { left, right, .. } => {
                body(left, out);
                body(right, out);
            }
            e => out.push(DocumentBody::Expression(e.clone())),
        }
    }
    let mut b = Vec::new();
    body(e, &mut b);
    Ok(Document { arity, body: b })
}

fn read_file(path: &str) -> Result<String, Error> {
    std::fs::read_to_string(path).map_err(|e| Error::Io(path.to_owned(), e))
}

/// Read and parse a document file into an expression.
pub fn fetch_document(path: &str) -> Result<Expr, Error> {
    let (doc, _) = parse_document(&read_file(path)?)?;
    Ok(from_document(path, &doc))
}

/// Read a file as plain text, with no header.
pub fn raw_document(path: &str) -> Result<Expr, Error> {
    Ok(Expr::lit(lines(&read_file(path)?), path, false))
}
