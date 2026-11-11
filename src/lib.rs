//! λtext: a general-purpose templating utility that treats text files as
//! higher-order functions.

pub mod chars;
pub mod document;
pub mod eval;
pub mod expr;
pub mod files;
pub mod typecheck;

use std::fmt;

/// Any error that ends a run.
#[derive(Debug)]
pub enum Error {
    Parse(expr::ParseError),
    /// A parse error in an expression line of a document.
    DocParse(String, Box<document::DocParseError>),
    Print(document::PrintError),
    Type(typecheck::TypeError),
    Io(String, std::io::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Parse(e) => e.fmt(f),
            Error::DocParse(file, e) => {
                let msg = e.err.to_string();
                let msg = msg.strip_prefix("[Parse Error] ").unwrap_or(&msg);
                write!(
                    f,
                    "[Parse Error] In {file:?}, line {}, expression {:?}: {msg}",
                    e.line, e.expr
                )
            }
            Error::Print(e) => e.fmt(f),
            Error::Type(e) => e.fmt(f),
            Error::Io(path, e) => write!(f, "[IO Error] Can't read {path:?}: {e}"),
        }
    }
}

impl std::error::Error for Error {}

impl From<expr::ParseError> for Error {
    fn from(e: expr::ParseError) -> Self {
        Error::Parse(e)
    }
}
impl From<document::PrintError> for Error {
    fn from(e: document::PrintError) -> Self {
        Error::Print(e)
    }
}
impl From<typecheck::TypeError> for Error {
    fn from(e: typecheck::TypeError) -> Self {
        Error::Type(e)
    }
}
