//! Evaluation: reduce an expression to β-normal form.
//!
//! Only well-typed expressions are evaluated. The type system has no recursion
//! (self-application fails the occurs check), so every well-typed expression
//! has a normal form and evaluation terminates.

use std::collections::BTreeSet;

use crate::expr::Expr;

/// Reduce to normal form, including under lambdas, and merge adjacent text.
pub fn evaluate(e: &Expr) -> Expr {
    match e {
        Expr::Abs(n, body) => Expr::abs(n.clone(), evaluate(body)),
        Expr::App(f, a) => match evaluate(f) {
            Expr::Abs(n, body) => evaluate(&substitute(&n, &evaluate(a), &body)),
            f2 => Expr::app(f2, evaluate(a)),
        },
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
        Expr::Var(_) | Expr::Lit { .. } => e.clone(),
    }
}

/// Replace the free occurrences of `n` in `e` with `x`, renaming bound
/// variables of `e` where they would capture a free variable of `x`.
pub fn substitute(n: &str, x: &Expr, e: &Expr) -> Expr {
    match e {
        Expr::Var(m) if m == n => x.clone(),
        Expr::Var(_) | Expr::Lit { .. } => e.clone(),
        Expr::App(f, a) => Expr::app(substitute(n, x, f), substitute(n, x, a)),
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
        Expr::Abs(m, _) if m == n => e.clone(),
        Expr::Abs(m, body) => {
            let fv_x = free_vars(x);
            if fv_x.contains(m) && free_vars(body).contains(n) {
                let mut avoid = fv_x;
                avoid.extend(free_vars(body));
                avoid.insert(n.to_owned());
                let m2 = fresh_name(m, &avoid);
                let body = substitute(m, &Expr::var(m2.clone()), body);
                Expr::abs(m2, substitute(n, x, &body))
            } else {
                Expr::abs(m.clone(), substitute(n, x, body))
            }
        }
    }
}

/// `base1`, `base2`, … : the first one not in `avoid`.
fn fresh_name(base: &str, avoid: &BTreeSet<String>) -> String {
    (1..)
        .map(|i| format!("{base}{i}"))
        .find(|c| !avoid.contains(c))
        .expect("unbounded")
}

/// Free variables, sorted so that file resolution happens in a stable order.
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::{parse_expr, pp_expr};

    fn eval(src: &str) -> String {
        pp_expr(&evaluate(&parse_expr(src).unwrap()))
    }

    #[test]
    fn reduces_redexes_created_by_substitution() {
        assert_eq!(eval(r"\y -> (\x -> x y) (\z -> z)"), r"\y -> y");
        assert_eq!(
            eval(r"\g -> (\f x -> f (f x)) (\y -> g y)"),
            r"\g -> \x -> g (g x)"
        );
    }

    #[test]
    fn substitution_avoids_capture() {
        assert_eq!(eval(r"\b -> (\a b -> a) b"), r"\b -> \b1 -> b");
        assert_eq!(
            eval(r"\b b1 -> (\a b -> a b1) b"),
            r"\b -> \b1 -> \b2 -> b b1"
        );
    }

    #[test]
    fn shadowing_stops_substitution() {
        assert_eq!(eval(r"(\x -> \x -> x) y"), r"\x -> x");
    }
}
