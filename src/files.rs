//! Loading the files that free variables in the command-line expression refer to.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::Error;
use crate::document::{fetch_document, raw_document};
use crate::eval::{free_vars, substitute};
use crate::expr::Expr;

/// Reads and parses each referenced file once, shared by type checking and
/// evaluation.
pub struct Loader {
    raw: HashSet<String>,
    raw_canonical: HashSet<PathBuf>,
    cache: HashMap<String, Expr>,
    verbose: bool,
}

impl Loader {
    /// `raw` lists the files to read as plain text (no header). A file counts as
    /// raw if it is named exactly as given, or if both paths resolve to the same
    /// file (`-r ./notes` matches `notes`).
    pub fn new(raw: &[String], verbose: bool) -> Loader {
        Loader {
            raw: raw.iter().cloned().collect(),
            raw_canonical: raw
                .iter()
                .filter_map(|r| Path::new(r).canonicalize().ok())
                .collect(),
            cache: HashMap::new(),
            verbose,
        }
    }

    /// Does `name` refer to an existing file?
    pub fn exists(&self, name: &str) -> bool {
        Path::new(name).is_file()
    }

    pub fn is_raw(&self, name: &str) -> bool {
        self.raw.contains(name)
            || Path::new(name)
                .canonicalize()
                .is_ok_and(|p| self.raw_canonical.contains(&p))
    }

    /// The expression for a file: a document, or plain text if it is raw.
    pub fn load(&mut self, name: &str) -> Result<Expr, Error> {
        if let Some(e) = self.cache.get(name) {
            return Ok(e.clone());
        }
        let raw = self.is_raw(name);
        if self.verbose {
            let kind = if raw { "plain text" } else { "document" };
            eprintln!("[ltext] reading {name:?} as {kind}");
        }
        let e = if raw {
            raw_document(name)?
        } else {
            fetch_document(name)?
        };
        self.cache.insert(name.to_owned(), e.clone());
        Ok(e)
    }

    /// Replace each free variable of a command-line expression with the
    /// contents of the file it names.
    pub fn resolve(&mut self, e: &Expr) -> Result<Expr, Error> {
        let mut out = e.clone();
        for f in free_vars(e) {
            out = substitute(&f, &self.load(&f)?, &out);
        }
        Ok(out)
    }
}
