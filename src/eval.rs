//! Evaluation: reduce an expression to β-normal form.
//!
//! Only well-typed expressions are evaluated. The type system has no recursion
//! (self-application fails the occurs check), so every well-typed expression
//! has a normal form and evaluation terminates.

use std::collections::BTreeSet;

use crate::expr::Expr;

/// Reduce to normal form, including under lambdas, and merge adjacent text.
pub fn evaluate(e: &Expr) -> Expr {
    eval(e.clone())
}

/// [`evaluate`], taking ownership so that text is moved rather than copied.
pub fn eval(e: Expr) -> Expr {
    match e {
        Expr::Abs(n, body) => Expr::abs(n, eval(*body)),
        Expr::App(f, a) => match eval(*f) {
            Expr::Abs(n, body) => {
                let a = eval(*a);
                eval(substitute_owned(&n, a, *body))
            }
            f2 => Expr::app(f2, eval(*a)),
        },
        Expr::Concat { .. } => eval_concat(e),
        Expr::Var(_) | Expr::Lit { .. } => e,
    }
}

/// Evaluate a chain of `Concat`s. Documents are right-nested chains, one link
/// per text block or expression line, so this walks the chain with a loop
/// instead of recursion, and builds merged text back to front so that each
/// line is moved a bounded number of times.
fn eval_concat(e: Expr) -> Expr {
    // The left operands, with each link's source and error flag.
    let mut links = Vec::new();
    let mut tail = e;
    while let Expr::Concat {
        left,
        right,
        source,
        in_error,
    } = tail
    {
        links.push((*left, source, in_error));
        tail = *right;
    }

    // Merged text at the front of the result so far, in reverse line order,
    // with the source and error flag of the link that produced it.
    struct Text {
        rev: Vec<String>,
        source: String,
        in_error: bool,
    }
    impl Text {
        fn into_lit(mut self) -> Expr {
            self.rev.reverse();
            Expr::lit(self.rev, self.source, self.in_error)
        }
    }
    enum Acc {
        Text(Text),
        Other(Expr),
    }
    impl Acc {
        fn new(e: Expr) -> Acc {
            match e {
                Expr::Lit {
                    mut content,
                    source,
                    in_error,
                } => {
                    content.reverse();
                    Acc::Text(Text {
                        rev: content,
                        source,
                        in_error,
                    })
                }
                e => Acc::Other(e),
            }
        }
        fn into_expr(self) -> Expr {
            match self {
                Acc::Text(t) => t.into_lit(),
                Acc::Other(e) => e,
            }
        }
    }

    let mut acc = Acc::new(eval(tail));
    for (left, source, in_error) in links.into_iter().rev() {
        acc = match (eval(left), acc) {
            (Expr::Lit { content, .. }, Acc::Text(mut t)) => {
                t.rev.extend(content.into_iter().rev());
                t.source = source;
                t.in_error = in_error;
                Acc::Text(t)
            }
            (l, acc) => Acc::Other(Expr::concat(l, acc.into_expr(), source, in_error)),
        };
    }
    acc.into_expr()
}

/// Replace the free occurrences of `n` in `e` with `x`, renaming bound
/// variables of `e` where they would capture a free variable of `x`.
pub fn substitute(n: &str, x: &Expr, e: &Expr) -> Expr {
    substitute_owned(n, x.clone(), e.clone())
}

/// [`substitute`], taking ownership. `x` is moved into its last occurrence
/// instead of being copied, so substituting a large argument for a variable
/// used once costs nothing extra.
pub fn substitute_owned(n: &str, x: Expr, e: Expr) -> Expr {
    let remaining = count_free(n, &e);
    Subst {
        n,
        x: Some(x),
        remaining,
        fv_x: None,
    }
    .go(e)
}

/// The number of free occurrences of `n` in `e`.
fn count_free(n: &str, e: &Expr) -> usize {
    match e {
        Expr::Var(m) => usize::from(m == n),
        Expr::Lit { .. } => 0,
        Expr::Abs(m, _) if m == n => 0,
        Expr::Abs(_, body) => count_free(n, body),
        Expr::App(a, b)
        | Expr::Concat {
            left: a, right: b, ..
        } => count_free(n, a) + count_free(n, b),
    }
}

struct Subst<'a> {
    n: &'a str,
    /// The replacement, until its last use moves it out.
    x: Option<Expr>,
    /// Free occurrences of `n` not yet replaced.
    remaining: usize,
    /// The free variables of `x`, computed when first needed.
    fv_x: Option<BTreeSet<String>>,
}

impl Subst<'_> {
    fn replacement(&mut self) -> Expr {
        self.remaining -= 1;
        if self.remaining == 0 {
            self.x.take()
        } else {
            self.x.clone()
        }
        .expect("used after the last occurrence")
    }

    fn go(&mut self, e: Expr) -> Expr {
        if self.remaining == 0 {
            return e;
        }
        let n = self.n;
        match e {
            Expr::Var(m) if m == n => self.replacement(),
            Expr::Var(_) | Expr::Lit { .. } => e,
            Expr::App(f, a) => {
                let f = self.go(*f);
                Expr::app(f, self.go(*a))
            }
            Expr::Concat {
                left,
                right,
                source,
                in_error,
            } => {
                let left = self.go(*left);
                Expr::concat(left, self.go(*right), source, in_error)
            }
            Expr::Abs(ref m, _) if m == n => e,
            Expr::Abs(m, body) => {
                // `remaining > 0`, so `x` hasn't been moved out yet.
                let x = self.x.as_ref().expect("x is present");
                let fvs = self.fv_x.get_or_insert_with(|| free_vars(x));
                if fvs.contains(&m) && count_free(n, &body) > 0 {
                    let mut avoid = fvs.clone();
                    avoid.extend(free_vars(&body));
                    avoid.insert(n.to_owned());
                    let m2 = fresh_name(&m, &avoid);
                    let body = substitute_owned(&m, Expr::var(m2.clone()), *body);
                    Expr::abs(m2, self.go(body))
                } else {
                    Expr::abs(m, self.go(*body))
                }
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
    fn substitutes_every_occurrence() {
        assert_eq!(eval(r"(\x -> x (x y)) f"), "f (f y)");
        assert_eq!(eval(r"(\x -> \z -> x z x) (g b)"), r"\z -> g b z (g b)");
        // A capturing lambda between occurrences.
        assert_eq!(eval(r"(\x -> x (\b -> x b)) (f b)"), r"f b (\b1 -> f b b1)");
    }

    #[test]
    fn merges_long_text_chains() {
        let lit = |s: &str| Expr::lit(vec![s.into()], "f", false);
        let chain = (0..1_000).rev().fold(lit("end"), |acc, i| {
            Expr::concat(lit(&i.to_string()), acc, "f", false)
        });
        match evaluate(&chain) {
            Expr::Lit { content, .. } => {
                assert_eq!(content.len(), 1_001);
                assert_eq!(content[0], "0");
                assert_eq!(content[999], "999");
                assert_eq!(content[1_000], "end");
            }
            e => panic!("not merged: {e:?}"),
        }
    }

    #[test]
    fn shadowing_stops_substitution() {
        assert_eq!(eval(r"(\x -> \x -> x) y"), r"\x -> x");
    }
}
