//! Type inference. A literal port of `LText.Type`, including the order in
//! which fresh type variables are created (it shows in `-t` output) and the
//! known scope leak in `Abs` (docs/rust-port.md, bug #3).
//!
//! `quantify` is never called in the Haskell code, so every scheme has an
//! empty quantifier set. The context therefore maps names straight to types.

use smallvec::smallvec;

use crate::Stack;
use std::collections::HashMap;
use std::fmt;

use crate::Error;
use crate::expr::Expr;
use crate::files::Loader;

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Text,
    TVar(String),
    Arrow(Box<Type>, Box<Type>),
}

// Types can be as deep as the expressions they describe, so, like `Expr`,
// `Type` is cloned, dropped and traversed with explicit stacks.

impl Type {
    fn arrow(a: Type, b: Type) -> Type {
        Type::Arrow(Box::new(a), Box::new(b))
    }

    /// The two sides of an arrow, moved out.
    fn into_arrow(mut self) -> (Type, Type) {
        let Type::Arrow(a, b) = &mut self else {
            panic!("not an arrow")
        };
        let take = |b: &mut Box<Type>| std::mem::replace(&mut **b, Type::Text);
        (take(a), take(b))
    }
}

impl Drop for Type {
    fn drop(&mut self) {
        fn push_children(t: &mut Type, stack: &mut Stack<Type>) {
            if let Type::Arrow(a, b) = t {
                for c in [a, b] {
                    if let Type::Arrow(..) = **c {
                        stack.push(std::mem::replace(&mut **c, Type::Text));
                    }
                }
            }
        }
        let mut stack: Stack<_> = Stack::new();
        push_children(self, &mut stack);
        while let Some(mut t) = stack.pop() {
            push_children(&mut t, &mut stack);
        }
    }
}

impl Clone for Type {
    fn clone(&self) -> Type {
        enum Step<'a> {
            Visit(&'a Type),
            Arrow,
        }
        let mut steps: Stack<_> = smallvec![Step::Visit(self)];
        let mut done: Stack<Type> = Stack::new();
        while let Some(step) = steps.pop() {
            match step {
                Step::Visit(Type::Arrow(a, b)) => {
                    steps.push(Step::Arrow);
                    steps.push(Step::Visit(b));
                    steps.push(Step::Visit(a));
                }
                Step::Visit(Type::Text) => done.push(Type::Text),
                Step::Visit(Type::TVar(n)) => done.push(Type::TVar(n.clone())),
                Step::Arrow => {
                    let b = done.pop().expect("result");
                    let a = done.pop().expect("argument");
                    done.push(Type::arrow(a, b));
                }
            }
        }
        done.pop().expect("type")
    }
}

/// Renames type variables to `a0`, `a1`, … in order of first appearance, so
/// printed types don't depend on how many fresh variables inference used.
#[derive(Default)]
struct Renamer(HashMap<String, String>);

impl Renamer {
    fn pp(&mut self, t: &Type) -> String {
        enum Item<'a> {
            Type(&'a Type),
            Str(&'static str),
        }
        let mut out = String::new();
        let mut todo: Stack<_> = smallvec![Item::Type(t)];
        while let Some(item) = todo.pop() {
            match item {
                Item::Str(s) => out.push_str(s),
                Item::Type(Type::Text) => out.push_str("Text"),
                Item::Type(Type::TVar(n)) => {
                    let next = format!("a{}", self.0.len());
                    out.push_str(self.0.entry(n.clone()).or_insert(next));
                }
                Item::Type(Type::Arrow(a, b)) => {
                    todo.push(Item::Type(b));
                    todo.push(Item::Str(" -> "));
                    if let Type::Arrow(..) = **a {
                        todo.push(Item::Str(")"));
                        todo.push(Item::Type(a));
                        todo.push(Item::Str("("));
                    } else {
                        todo.push(Item::Type(a));
                    }
                }
            }
        }
        out
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
    files: &'a mut Loader,
}

impl Checker<'_> {
    fn fresh_tvar(&mut self) -> Type {
        let t = Type::TVar(format!("t{}", self.fresh));
        self.fresh += 1;
        t
    }

    /// Follow bound type variables, without changing anything.
    fn follow<'t>(&'t self, mut t: &'t Type) -> &'t Type {
        while let Type::TVar(n) = t {
            match self.subst.get(n) {
                Some(bound) => t = bound,
                None => break,
            }
        }
        t
    }

    /// Apply the current substitution completely.
    fn resolve(&self, t: &Type) -> Type {
        enum Step<'a> {
            Visit(&'a Type),
            Arrow,
        }
        let mut steps: Stack<_> = smallvec![Step::Visit(t)];
        let mut done: Stack<Type> = Stack::new();
        while let Some(step) = steps.pop() {
            match step {
                Step::Visit(t) => match self.follow(t) {
                    Type::Arrow(a, b) => {
                        steps.push(Step::Arrow);
                        steps.push(Step::Visit(b));
                        steps.push(Step::Visit(a));
                    }
                    t => done.push(t.clone()),
                },
                Step::Arrow => {
                    let b = done.pop().expect("result");
                    let a = done.pop().expect("argument");
                    done.push(Type::arrow(a, b));
                }
            }
        }
        done.pop().expect("type")
    }

    /// Follow bound type variables until reaching an unbound variable or a
    /// type constructor. Chains of variables bound to variables are shortened
    /// on the way, so repeated lookups stay cheap.
    fn shallow(&mut self, t: Type) -> Type {
        let Type::TVar(start) = &t else {
            return t;
        };
        // The variables passed through that are bound to other variables.
        let mut chain: Vec<String> = Vec::new();
        let mut n = start.clone();
        let r = loop {
            match self.subst.get(&n) {
                None if chain.is_empty() => return t,
                None => break Type::TVar(n),
                Some(Type::TVar(m)) => {
                    let m = m.clone();
                    chain.push(std::mem::replace(&mut n, m));
                }
                Some(bound) => break bound.clone(),
            }
        };
        for v in chain {
            self.subst.insert(v, r.clone());
        }
        r
    }

    /// Does type variable `n` occur in `t`, under the current substitution?
    fn occurs(&self, n: &str, t: &Type) -> bool {
        let mut todo: Stack<_> = smallvec![t];
        while let Some(t) = todo.pop() {
            match t {
                Type::Text => {}
                Type::TVar(m) if m == n => return true,
                Type::TVar(m) => todo.extend(self.subst.get(m)),
                Type::Arrow(a, b) => {
                    todo.push(b);
                    todo.push(a);
                }
            }
        }
        false
    }

    /// Unify two types, resolving only as far as each step needs. Errors hold
    /// partly resolved types; `finish` resolves them for the message.
    fn unify(&mut self, a: &Type, b: &Type) -> Result<(), TypeError> {
        enum Case {
            Equal,
            /// Bind the variable on the left (true) or right (false).
            Bind(bool),
            Arrows,
            Clash,
        }
        // Pairs still to unify, first on top.
        let mut todo: Stack<_> = smallvec![(a.clone(), b.clone())];
        while let Some((a, b)) = todo.pop() {
            let (a, b) = (self.shallow(a), self.shallow(b));
            let case = match (&a, &b) {
                (Type::Text, Type::Text) => Case::Equal,
                (Type::TVar(n), Type::TVar(m)) if n == m => Case::Equal,
                (Type::TVar(_), _) => Case::Bind(true),
                (_, Type::TVar(_)) => Case::Bind(false),
                (Type::Arrow(..), Type::Arrow(..)) => Case::Arrows,
                _ => Case::Clash,
            };
            match case {
                Case::Equal => {}
                Case::Bind(left) => {
                    let (v, t) = if left { (a, b) } else { (b, a) };
                    let Type::TVar(n) = &v else { unreachable!() };
                    if self.occurs(n, &t) {
                        return Err(TypeError::OccursCheckFailure(v, t));
                    }
                    self.subst.insert(n.clone(), t);
                }
                Case::Arrows => {
                    let ((a1, a2), (b1, b2)) = (a.into_arrow(), b.into_arrow());
                    todo.push((a2, b2));
                    todo.push((a1, b1));
                }
                Case::Clash => return Err(TypeError::CantUnify(a, b)),
            }
        }
        Ok(())
    }

    fn infer(
        &mut self,
        level: Level,
        ctx: &mut HashMap<String, Type>,
        e: &Expr,
    ) -> Result<Type, Error> {
        /// What to do with a type once it's inferred.
        enum Then<'e> {
            /// It's the left side of a `Concat`; this is the right side.
            ConcatLeft(&'e Expr),
            ConcatRight,
            /// It's the body of a lambda: its parameter, the parameter's type,
            /// and whatever the parameter shadowed in the context.
            Abs(&'e str, Type, Option<Type>),
            /// It's a function; this is its argument.
            AppFun(&'e Expr),
            /// It's an argument; this is the function's type.
            AppArg(Type),
        }
        let mut stack: Stack<Then<'_>> = Stack::new();
        let mut cur = e;
        loop {
            // Go down to a leaf.
            let mut ty = loop {
                match cur {
                    Expr::Lit { .. } => break Type::Text,
                    Expr::Concat { left, right, .. } => {
                        stack.push(Then::ConcatLeft(right));
                        cur = left;
                    }
                    Expr::Var(x) => {
                        break match ctx.get(x) {
                            Some(t) => t.clone(),
                            None if level == Level::TopLevel && self.files.exists(x) => {
                                if self.files.is_raw(x) {
                                    Type::Text
                                } else {
                                    // Documents are closed: check them in an
                                    // empty context. They can't refer to
                                    // files, so this nests only once.
                                    let d = self.files.load(x)?;
                                    self.infer(Level::DocLevel, &mut HashMap::new(), &d)?
                                }
                            }
                            None => return Err(TypeError::UnboundVariable(x.clone()).into()),
                        };
                    }
                    Expr::Abs(n, body) => {
                        let t = self.fresh_tvar();
                        let shadowed = ctx.insert(n.clone(), t.clone());
                        stack.push(Then::Abs(n, t, shadowed));
                        cur = body;
                    }
                    Expr::App(f, a) => {
                        stack.push(Then::AppFun(a));
                        cur = f;
                    }
                }
            };
            // Go back up, until another subexpression needs inferring.
            cur = loop {
                match stack.pop() {
                    None => return Ok(ty),
                    Some(Then::ConcatLeft(right)) => {
                        self.unify(&ty, &Type::Text).map_err(|e| self.finish(e))?;
                        stack.push(Then::ConcatRight);
                        break right;
                    }
                    Some(Then::ConcatRight) => {
                        self.unify(&ty, &Type::Text).map_err(|e| self.finish(e))?;
                        ty = Type::Text;
                    }
                    Some(Then::Abs(n, t, shadowed)) => {
                        match shadowed {
                            Some(old) => ctx.insert(n.to_owned(), old),
                            None => ctx.remove(n),
                        };
                        ty = Type::arrow(t, ty);
                    }
                    Some(Then::AppFun(a)) => {
                        stack.push(Then::AppArg(ty));
                        break a;
                    }
                    Some(Then::AppArg(tf)) => {
                        let r = self.fresh_tvar();
                        self.unify(&tf, &Type::arrow(ty, r.clone()))
                            .map_err(|e| self.finish(e))?;
                        ty = r;
                    }
                }
            };
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
/// names.
pub fn type_of_top_level(e: &Expr, files: &mut Loader) -> Result<Type, Error> {
    let mut c = Checker {
        subst: HashMap::new(),
        fresh: 0,
        files,
    };
    let t = c.infer(Level::TopLevel, &mut HashMap::new(), e)?;
    Ok(c.resolve(&t))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::parse_expr;

    fn type_of(src: &str) -> Result<String, String> {
        type_of_top_level(&parse_expr(src).unwrap(), &mut Loader::new(&[], false))
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
        let mut files = Loader::new(&[], false);
        let mut c = Checker {
            subst: HashMap::new(),
            fresh: 0,
            files: &mut files,
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
