//! Evaluation: reduce an expression to β-normal form.
//!
//! Only well-typed expressions are evaluated. The type system has no recursion
//! (self-application fails the occurs check), so every well-typed expression
//! has a normal form and evaluation terminates.
//!
//! Like the rest of the crate, these functions use explicit stacks instead of
//! recursing once per node, so deep expressions can't overflow the stack.

use smallvec::smallvec;

use crate::Stack;
use std::collections::{BTreeSet, HashMap};

use crate::expr::{Expr, Node};

/// Reduce to normal form, including under lambdas, and merge adjacent text.
pub fn evaluate(e: &Expr) -> Expr {
    eval(e.clone())
}

/// [`evaluate`], taking ownership so that text is moved rather than copied.
pub fn eval(e: Expr) -> Expr {
    /// What to do with a value once it's computed.
    enum Then {
        /// Wrap it in a lambda with this parameter.
        Abs(String),
        /// It's a function: evaluate this argument next.
        Apply(Expr),
        /// It's the argument to a lambda with this parameter and body.
        Beta(String, Expr),
        /// It's the argument to this function, which isn't a lambda.
        App(Expr),
        /// It's part of a chain of text.
        Concat(Box<Chain>),
    }
    let mut stack: Stack<Then> = Stack::new();
    let mut todo = e;
    loop {
        // Go down to the first part of `todo` that is already a value.
        let mut v = loop {
            todo = match todo {
                Expr::Abs(..) | Expr::App(..) | Expr::Concat { .. } => todo,
                v => break v,
            };
            match todo.into_node() {
                Node::Abs(n, body) => {
                    stack.push(Then::Abs(n));
                    todo = body;
                }
                Node::App(f, a) => {
                    stack.push(Then::Apply(a));
                    todo = f;
                }
                Node::Concat {
                    left,
                    right,
                    source,
                    in_error,
                } => {
                    let (chain, tail) = Chain::new(left, right, source, in_error);
                    stack.push(Then::Concat(Box::new(chain)));
                    todo = tail;
                }
                Node::Var(_) | Node::Lit { .. } => unreachable!("handled above"),
            }
        };
        // Go back up, until something else needs evaluating.
        todo = loop {
            match stack.pop() {
                None => return v,
                Some(Then::Abs(n)) => v = Expr::abs(n, v),
                Some(Then::Apply(a)) => {
                    if let Expr::Abs(..) = v {
                        let Node::Abs(n, body) = v.into_node() else {
                            unreachable!()
                        };
                        stack.push(Then::Beta(n, body));
                    } else {
                        stack.push(Then::App(v));
                    }
                    break a;
                }
                Some(Then::Beta(n, body)) => break substitute_owned(&n, v, body),
                Some(Then::App(f)) => v = Expr::app(f, v),
                Some(Then::Concat(mut chain)) => {
                    chain.add(v);
                    match chain.next() {
                        Some(left) => {
                            stack.push(Then::Concat(chain));
                            break left;
                        }
                        None => v = chain.finish(),
                    }
                }
            }
        };
    }
}

/// Evaluating a chain of `Concat`s. Documents are right-nested chains, one
/// link per text block or expression line. The tail is evaluated first, then
/// the left operands from last to first, and text is merged back to front so
/// that each line is moved a bounded number of times.
struct Chain {
    /// The left operands not yet evaluated, with each link's source and error
    /// flag.
    links: Vec<(Expr, String, bool)>,
    /// The source and error flag of the link whose left operand is being
    /// evaluated.
    pending: Option<(String, bool)>,
    /// The result so far, for the tail of the chain.
    acc: Option<Acc>,
}

enum Acc {
    /// Merged text, in reverse line order, with the source and error flag of
    /// the link that produced it.
    Text {
        rev: Vec<String>,
        source: String,
        in_error: bool,
    },
    Other(Expr),
}

impl Acc {
    fn into_expr(self) -> Expr {
        match self {
            Acc::Text {
                mut rev,
                source,
                in_error,
            } => {
                rev.reverse();
                Expr::lit(rev, source, in_error)
            }
            Acc::Other(e) => e,
        }
    }
}

impl Chain {
    /// Split a chain into its links and the expression at the end.
    fn new(left: Expr, right: Expr, source: String, in_error: bool) -> (Chain, Expr) {
        let mut links = vec![(left, source, in_error)];
        let mut tail = right;
        while let Expr::Concat { .. } = tail {
            let Node::Concat {
                left,
                right,
                source,
                in_error,
            } = tail.into_node()
            else {
                unreachable!()
            };
            links.push((left, source, in_error));
            tail = right;
        }
        let chain = Chain {
            links,
            pending: None,
            acc: None,
        };
        (chain, tail)
    }

    /// Add an evaluated operand to the front of the result: first the tail,
    /// then each left operand.
    fn add(&mut self, v: Expr) {
        self.acc = Some(match (self.acc.take(), self.pending.take()) {
            (None, _) => match v.into_node() {
                Node::Lit {
                    mut content,
                    source,
                    in_error,
                } => {
                    content.reverse();
                    Acc::Text {
                        rev: content,
                        source,
                        in_error,
                    }
                }
                v => Acc::Other(v.into()),
            },
            (Some(acc), Some((source, in_error))) => match (v, acc) {
                (
                    v @ Expr::Lit { .. },
                    Acc::Text {
                        mut rev,
                        source: _,
                        in_error: _,
                    },
                ) => {
                    let Node::Lit { content, .. } = v.into_node() else {
                        unreachable!()
                    };
                    rev.extend(content.into_iter().rev());
                    Acc::Text {
                        rev,
                        source,
                        in_error,
                    }
                }
                (v, acc) => Acc::Other(Expr::concat(v, acc.into_expr(), source, in_error)),
            },
            (Some(_), None) => unreachable!("an operand without a link"),
        });
    }

    /// The next left operand to evaluate, if any.
    fn next(&mut self) -> Option<Expr> {
        let (left, source, in_error) = self.links.pop()?;
        self.pending = Some((source, in_error));
        Some(left)
    }

    fn finish(self) -> Expr {
        self.acc.expect("the tail was added").into_expr()
    }
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
    .run(e)
}

/// The number of free occurrences of `n` in `e`.
fn count_free(n: &str, e: &Expr) -> usize {
    let mut count = 0;
    let mut todo: Stack<_> = smallvec![e];
    while let Some(e) = todo.pop() {
        match e {
            Expr::Var(m) => count += usize::from(m == n),
            Expr::Lit { .. } => {}
            Expr::Abs(m, _) if m == n => {}
            Expr::Abs(_, body) => todo.push(body),
            Expr::App(a, b)
            | Expr::Concat {
                left: a, right: b, ..
            } => {
                todo.push(b);
                todo.push(a);
            }
        }
    }
    count
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

    /// Substitute in place. Only the replaced variables and renamed
    /// lambdas change; everything else stays where it is.
    fn run(&mut self, mut e: Expr) -> Expr {
        let n = self.n;
        let mut todo: Stack<&mut Expr> = smallvec![&mut e];
        while let Some(e) = todo.pop() {
            if self.remaining == 0 {
                break;
            }
            match e {
                Expr::Var(m) if m == n => *e = self.replacement(),
                Expr::Var(_) | Expr::Lit { .. } => {}
                Expr::App(f, a)
                | Expr::Concat {
                    left: f, right: a, ..
                } => {
                    todo.push(a);
                    todo.push(f);
                }
                // `n` is shadowed.
                Expr::Abs(m, _) if m == n => {}
                Expr::Abs(m, body) => {
                    // `remaining > 0`, so `x` hasn't been moved out yet.
                    let x = self.x.as_ref().expect("x is present");
                    let fvs = self.fv_x.get_or_insert_with(|| free_vars(x));
                    if fvs.contains(m) && count_free(n, body) > 0 {
                        let mut avoid = fvs.clone();
                        avoid.extend(free_vars(body));
                        avoid.insert(n.to_owned());
                        let m2 = fresh_name(m, &avoid);
                        // A separate substitution, which only nests again if
                        // the body binds the fresh name too.
                        let old = std::mem::replace(&mut **body, Expr::var(""));
                        **body = substitute_owned(m, Expr::var(m2.clone()), old);
                        *m = m2;
                    }
                    todo.push(body);
                }
            }
        }
        drop(todo);
        e
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
    enum Step<'a> {
        Visit(&'a Expr),
        Unbind(&'a str),
    }
    let mut out = BTreeSet::new();
    // How many enclosing lambdas bind each name.
    let mut bound: HashMap<&str, usize> = HashMap::new();
    let mut steps: Stack<_> = smallvec![Step::Visit(e)];
    while let Some(step) = steps.pop() {
        match step {
            Step::Visit(Expr::Abs(n, body)) => {
                *bound.entry(n).or_default() += 1;
                steps.push(Step::Unbind(n));
                steps.push(Step::Visit(body));
            }
            Step::Visit(
                Expr::App(a, b)
                | Expr::Concat {
                    left: a, right: b, ..
                },
            ) => {
                steps.push(Step::Visit(b));
                steps.push(Step::Visit(a));
            }
            Step::Visit(Expr::Var(n)) => {
                if bound.get(n.as_str()).is_none_or(|&k| k == 0) {
                    out.insert(n.clone());
                }
            }
            Step::Visit(Expr::Lit { .. }) => {}
            Step::Unbind(n) => *bound.get_mut(n).expect("bound") -= 1,
        }
    }
    out
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
        match &evaluate(&chain) {
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
