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
use crate::hsshow::show_string;

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

/// `ppType`.
pub fn pp_type(t: &Type) -> String {
    match t {
        Type::Text => "Text".into(),
        Type::TVar(n) => n.clone(),
        Type::Arrow(a, b) => {
            let l = match **a {
                Type::Arrow(..) => format!("({})", pp_type(a)),
                _ => pp_type(a),
            };
            format!("{l} -> {}", pp_type(b))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    CantUnify(Type, Type),
    UnboundVariable(String),
    OccursCheckFailure(String, Type),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::CantUnify(a, b) => {
                write!(
                    f,
                    "[Type Error] Can't unify type {} with {}",
                    pp_type(a),
                    pp_type(b)
                )
            }
            TypeError::UnboundVariable(n) => {
                write!(f, "[Type Error] Unbound variable {}", show_string(n))
            }
            TypeError::OccursCheckFailure(n, t) => write!(
                f,
                "[Type Error] Occurs check failure: type variable {} in {}. Note: recursion is forbidden",
                show_string(n),
                pp_type(t)
            ),
        }
    }
}

impl std::error::Error for TypeError {}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Subst(HashMap<String, Type>);

impl Subst {
    fn single(n: String, t: Type) -> Subst {
        Subst(HashMap::from([(n, t)]))
    }

    fn apply(&self, t: &Type) -> Type {
        match t {
            Type::Text => Type::Text,
            Type::TVar(n) => self.0.get(n).cloned().unwrap_or_else(|| t.clone()),
            Type::Arrow(a, b) => Type::arrow(self.apply(a), self.apply(b)),
        }
    }

    /// `self <> other` = `fmap (apply self) other <> self` (left-biased union).
    fn compose(&self, other: &Subst) -> Subst {
        let mut out = self.0.clone();
        for (k, v) in &other.0 {
            out.insert(k.clone(), self.apply(v));
        }
        Subst(out)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Level {
    TopLevel,
    DocLevel,
}

struct Checker<'a> {
    context: HashMap<String, Type>,
    fresh: usize,
    raw_files: &'a HashSet<String>,
}

impl Checker<'_> {
    fn fresh_tvar(&mut self) -> Type {
        let t = Type::TVar(format!("a{}", self.fresh));
        self.fresh += 1;
        t
    }

    fn infer(&mut self, level: Level, e: &Expr) -> Result<(Subst, Type), Error> {
        match e {
            Expr::Lit { .. } => Ok((Subst::default(), Type::Text)),
            Expr::Concat { left, right, .. } => {
                let (s1, t1) = self.infer(level, left)?;
                let (s2, t2) = self.infer(level, right)?;
                let s3 = mgu(&t1, &t2)?;
                let s4 = mgu(&s3.apply(&t1), &Type::Text)?;
                // `s4 <> s3 <> s2 <> s1`, with `<>` right-associative.
                Ok((s4.compose(&s3.compose(&s2.compose(&s1))), Type::Text))
            }
            Expr::Var(x) => match self.context.get(x) {
                Some(t) => Ok((Subst::default(), t.clone())),
                None if level == Level::TopLevel && std::path::Path::new(x).is_file() => {
                    if self.raw_files.contains(x) {
                        Ok((Subst::default(), Type::Text))
                    } else {
                        let d = fetch_document(x)?;
                        self.infer(Level::DocLevel, &d)
                    }
                }
                None => Err(TypeError::UnboundVariable(x.clone()).into()),
            },
            Expr::Abs(n, body) => {
                let t = self.fresh_tvar();
                // Bug-for-bug: the binding isn't removed again afterwards.
                self.context.insert(n.clone(), t.clone());
                let (s, tb) = self.infer(level, body)?;
                let ta = s.apply(&t);
                Ok((s, Type::arrow(ta, tb)))
            }
            Expr::App(f, a) => {
                let t = self.fresh_tvar();
                let (s1, t1) = self.infer(level, f)?;
                for v in self.context.values_mut() {
                    *v = s1.apply(v);
                }
                let (s2, t2) = self.infer(level, a)?;
                let s3 = mgu(&s2.apply(&t1), &Type::arrow(t2, t.clone()))?;
                let result = s3.apply(&t);
                Ok((s3.compose(&s2.compose(&s1)), result))
            }
        }
    }
}

fn mgu(a: &Type, b: &Type) -> Result<Subst, TypeError> {
    match (a, b) {
        (Type::Arrow(l1, l2), Type::Arrow(r1, r2)) => {
            let s1 = mgu(l1, r1)?;
            let s2 = mgu(&s1.apply(l2), &s1.apply(r2))?;
            Ok(s1.compose(&s2))
        }
        (Type::TVar(n), t) | (t, Type::TVar(n)) => var_bind(n, t),
        (Type::Text, Type::Text) => Ok(Subst::default()),
        _ => Err(TypeError::CantUnify(a.clone(), b.clone())),
    }
}

fn var_bind(n: &str, t: &Type) -> Result<Subst, TypeError> {
    if *t == Type::TVar(n.to_owned()) {
        Ok(Subst::default())
    } else if t.contains_tvar(n) {
        Err(TypeError::OccursCheckFailure(n.to_owned(), t.clone()))
    } else {
        Ok(Subst::single(n.to_owned(), t.clone()))
    }
}

/// `typeOfTopLevel`: infer the type of a command-line expression, whose free
/// variables are file names. Files in `raw_files` are plain text.
pub fn type_of_top_level(e: &Expr, raw_files: &HashSet<String>) -> Result<Type, Error> {
    let mut c = Checker {
        context: HashMap::new(),
        fresh: 0,
        raw_files,
    };
    let (s, t) = c.infer(Level::TopLevel, e)?;
    Ok(s.apply(&t))
}
