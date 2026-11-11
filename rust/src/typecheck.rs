//! Type inference. A literal port of `LText.Type`, including the order in
//! which fresh type variables are created (it shows in `-t` output) and the
//! known scope leak in `Abs` (docs/rust-port.md, bug #3).
//!
//! `quantify` is never called in the Haskell code, so every scheme has an
//! empty quantifier set. The context therefore maps names straight to types.

use std::collections::{HashMap, HashSet};
use std::fmt;

use crate::Error;
use crate::document::fetch_document;
use crate::expr::Expr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Text,
    TVar(String),
    Arrow(Box<Type>, Box<Type>),
}

impl Type {
    fn arrow(a: Type, b: Type) -> Type {
        Type::Arrow(Box::new(a), Box::new(b))
    }

    fn free_tvars(&self, out: &mut HashSet<String>) {
        match self {
            Type::Text => {}
            Type::TVar(n) => {
                out.insert(n.clone());
            }
            Type::Arrow(a, b) => {
                a.free_tvars(out);
                b.free_tvars(out);
            }
        }
    }

    fn contains_tvar(&self, n: &str) -> bool {
        let mut s = HashSet::new();
        self.free_tvars(&mut s);
        s.contains(n)
    }
}

/// Renames type variables to `a0`, `a1`, … in order of first appearance, so
/// printed types don't depend on how many fresh variables inference used.
#[derive(Default)]
struct Renamer(HashMap<String, String>);

impl Renamer {
    fn pp(&mut self, t: &Type) -> String {
        match t {
            Type::Text => "Text".into(),
            Type::TVar(n) => {
                let next = format!("a{}", self.0.len());
                self.0.entry(n.clone()).or_insert(next).clone()
            }
            Type::Arrow(a, b) => {
                let l = match **a {
                    Type::Arrow(..) => format!("({})", self.pp(a)),
                    _ => self.pp(a),
                };
                format!("{l} -> {}", self.pp(b))
            }
        }
    }
}

/// Pretty-print a type, e.g. `(a0 -> Text) -> a0 -> Text`.
pub fn pp_type(t: &Type) -> String {
    Renamer::default().pp(t)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    CantUnify(Type, Type),
    UnboundVariable(String),
    /// A type variable that would have to contain itself.
    OccursCheckFailure(Type, Type),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut r = Renamer::default();
        match self {
            TypeError::CantUnify(a, b) => {
                let (a, b) = (r.pp(a), r.pp(b));
                write!(f, "[Type Error] Can't unify type {a} with {b}")
            }
            TypeError::UnboundVariable(n) => write!(
                f,
                "[Type Error] Unbound variable \"{n}\" (not a lambda parameter or an existing file)"
            ),
            TypeError::OccursCheckFailure(v, t) => {
                let (v, t) = (r.pp(v), r.pp(t));
                write!(
                    f,
                    "[Type Error] Occurs check failure: type variable {v} in {t}. Note: recursion is forbidden"
                )
            }
        }
    }
}

impl std::error::Error for TypeError {}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Level {
    /// The command-line expression: free variables are file names.
    TopLevel,
    /// Inside a document: every variable must be bound by its header.
    DocLevel,
}

/// Hindley-Milner inference (algorithm W) over a single global substitution.
/// The language has no `let`, so there's no generalisation. Each reference to a
/// file is inferred afresh, which makes files polymorphic per use.
struct Checker<'a> {
    subst: HashMap<String, Type>,
    fresh: usize,
    raw_files: &'a HashSet<String>,
}

impl Checker<'_> {
    fn fresh_tvar(&mut self) -> Type {
        let t = Type::TVar(format!("t{}", self.fresh));
        self.fresh += 1;
        t
    }

    /// Apply the current substitution completely.
    fn resolve(&self, t: &Type) -> Type {
        match t {
            Type::Text => Type::Text,
            Type::TVar(n) => match self.subst.get(n) {
                Some(t2) => self.resolve(t2),
                None => t.clone(),
            },
            Type::Arrow(a, b) => Type::arrow(self.resolve(a), self.resolve(b)),
        }
    }

    fn unify(&mut self, a: &Type, b: &Type) -> Result<(), TypeError> {
        let (a, b) = (self.resolve(a), self.resolve(b));
        match (&a, &b) {
            (Type::Text, Type::Text) => Ok(()),
            (Type::TVar(n), Type::TVar(m)) if n == m => Ok(()),
            (Type::TVar(n), t) | (t, Type::TVar(n)) => {
                if t.contains_tvar(n) {
                    return Err(TypeError::OccursCheckFailure(
                        Type::TVar(n.clone()),
                        t.clone(),
                    ));
                }
                self.subst.insert(n.clone(), t.clone());
                Ok(())
            }
            (Type::Arrow(a1, a2), Type::Arrow(b1, b2)) => {
                self.unify(a1, b1)?;
                self.unify(a2, b2)
            }
            _ => Err(TypeError::CantUnify(a, b)),
        }
    }

    fn infer(
        &mut self,
        level: Level,
        ctx: &mut HashMap<String, Type>,
        e: &Expr,
    ) -> Result<Type, Error> {
        match e {
            Expr::Lit { .. } => Ok(Type::Text),
            Expr::Concat { left, right, .. } => {
                let tl = self.infer(level, ctx, left)?;
                self.unify(&tl, &Type::Text).map_err(|e| self.finish(e))?;
                let tr = self.infer(level, ctx, right)?;
                self.unify(&tr, &Type::Text).map_err(|e| self.finish(e))?;
                Ok(Type::Text)
            }
            Expr::Var(x) => match ctx.get(x) {
                Some(t) => Ok(t.clone()),
                None if level == Level::TopLevel && std::path::Path::new(x).is_file() => {
                    if self.raw_files.contains(x) {
                        Ok(Type::Text)
                    } else {
                        // Documents are closed: check them in an empty context.
                        let d = fetch_document(x)?;
                        self.infer(Level::DocLevel, &mut HashMap::new(), &d)
                    }
                }
                None => Err(TypeError::UnboundVariable(x.clone()).into()),
            },
            Expr::Abs(n, body) => {
                let t = self.fresh_tvar();
                let shadowed = ctx.insert(n.clone(), t.clone());
                let tb = self.infer(level, ctx, body);
                match shadowed {
                    Some(old) => ctx.insert(n.clone(), old),
                    None => ctx.remove(n),
                };
                Ok(Type::arrow(t, tb?))
            }
            Expr::App(f, a) => {
                let tf = self.infer(level, ctx, f)?;
                let ta = self.infer(level, ctx, a)?;
                let r = self.fresh_tvar();
                self.unify(&tf, &Type::arrow(ta, r.clone()))
                    .map_err(|e| self.finish(e))?;
                Ok(r)
            }
        }
    }

    /// Resolve the types in an error message as far as is known.
    fn finish(&self, e: TypeError) -> Error {
        match e {
            TypeError::CantUnify(a, b) => TypeError::CantUnify(self.resolve(&a), self.resolve(&b)),
            TypeError::OccursCheckFailure(a, b) => {
                TypeError::OccursCheckFailure(self.resolve(&a), self.resolve(&b))
            }
            e => e,
        }
        .into()
    }
}

/// Infer the type of a command-line expression, whose free variables are file
/// names. Files in `raw_files` are plain text.
pub fn type_of_top_level(e: &Expr, raw_files: &HashSet<String>) -> Result<Type, Error> {
    let mut c = Checker {
        subst: HashMap::new(),
        fresh: 0,
        raw_files,
    };
    let t = c.infer(Level::TopLevel, &mut HashMap::new(), e)?;
    Ok(c.resolve(&t))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::parse_expr;

    fn type_of(src: &str) -> Result<String, String> {
        type_of_top_level(&parse_expr(src).unwrap(), &HashSet::new())
            .map(|t| pp_type(&t))
            .map_err(|e| e.to_string())
    }

    #[test]
    fn infers_combinators() {
        assert_eq!(type_of(r"\x -> x").unwrap(), "a0 -> a0");
        assert_eq!(type_of(r"\x y -> x").unwrap(), "a0 -> a1 -> a0");
        assert_eq!(
            type_of(r"\f g x -> f (g x)").unwrap(),
            "(a0 -> a1) -> (a2 -> a0) -> a2 -> a1"
        );
        assert_eq!(type_of(r"(\x -> x) (\y -> y)").unwrap(), "a0 -> a0");
    }

    #[test]
    fn lambda_scope_is_restored() {
        // `x` is only bound inside the first lambda.
        assert!(
            type_of(r"(\x -> x) x")
                .unwrap_err()
                .contains("Unbound variable \"x\"")
        );
        assert_eq!(type_of(r"\x -> (\x -> x) x").unwrap(), "a0 -> a0");
    }

    #[test]
    fn rejects_self_application() {
        assert!(type_of(r"\x -> x x").unwrap_err().contains("Occurs check"));
    }

    #[test]
    fn unification_composes_substitutions() {
        // Unifying `a -> a` with `b -> Text` must solve both variables.
        let raw = HashSet::new();
        let mut c = Checker {
            subst: HashMap::new(),
            fresh: 0,
            raw_files: &raw,
        };
        let (a, b) = (Type::TVar("a".into()), Type::TVar("b".into()));
        c.unify(
            &Type::arrow(a.clone(), a.clone()),
            &Type::arrow(b.clone(), Type::Text),
        )
        .unwrap();
        assert_eq!(c.resolve(&a), Type::Text);
        assert_eq!(c.resolve(&b), Type::Text);
    }
}
