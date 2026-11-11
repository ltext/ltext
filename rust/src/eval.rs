//! Evaluation. A literal port of `LText.Eval`: this is *not* full
//! β-normalisation. The result of a substitution isn't evaluated again, and
//! substitution doesn't avoid capture.

use std::collections::BTreeSet;

use crate::expr::Expr;

pub fn evaluate(e: &Expr) -> Expr {
    match e {
        Expr::Abs(n, body) => Expr::abs(n.clone(), evaluate(body)),
        Expr::Concat {
            left,
            right,
            source,
            in_error,
        } => match (evaluate(left), evaluate(right)) {
            (
                Expr::Lit {
                    content: mut t1, ..
                },
                Expr::Lit { content: t2, .. },
            ) => {
                t1.extend(t2);
                Expr::lit(t1, source.clone(), *in_error)
            }
            (l, r) => Expr::concat(l, r, source.clone(), *in_error),
        },
        Expr::App(f, a) => match evaluate(f) {
            Expr::Abs(n, body) => substitute(&n, &evaluate(a), &evaluate(&body)),
            f2 => Expr::app(f2, evaluate(a)),
        },
        _ => e.clone(),
    }
}

/// Replace free occurrences of `n` in `e` with `x`.
pub fn substitute(n: &str, x: &Expr, e: &Expr) -> Expr {
    match e {
        Expr::Concat {
            left,
            right,
            source,
            in_error,
        } => Expr::concat(
            substitute(n, x, left),
            substitute(n, x, right),
            source.clone(),
            *in_error,
        ),
        Expr::Var(m) if m == n => x.clone(),
        Expr::App(f, a) => Expr::app(substitute(n, x, f), substitute(n, x, a)),
        Expr::Abs(m, body) if m != n => Expr::abs(m.clone(), substitute(n, x, body)),
        _ => e.clone(),
    }
}

/// Free variables. Sorted so that file resolution happens in a deterministic
/// order. (Haskell iterates a `HashSet`; the order only affects which error is
/// reported first when several files are unreadable.)
pub fn free_vars(e: &Expr) -> BTreeSet<String> {
    match e {
        Expr::Abs(n, body) => {
            let mut s = free_vars(body);
            s.remove(n);
            s
        }
        Expr::App(a, b)
        | Expr::Concat {
            left: a, right: b, ..
        } => {
            let mut s = free_vars(a);
            s.extend(free_vars(b));
            s
        }
        Expr::Var(n) => BTreeSet::from([n.clone()]),
        Expr::Lit { .. } => BTreeSet::new(),
    }
}
