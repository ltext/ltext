//! Ports of the QuickCheck properties in `test/Main.hs`.

use ltext::chars::is_ident_char;
use ltext::document::{Document, DocumentBody, parse_document, print_document, repack_document};
use ltext::eval::{evaluate, free_vars};
use ltext::expr::{Expr, parse_expr, pp_expr};
use ltext::files::Loader;
use ltext::typecheck::{Type, pp_type, type_of_top_level};
use proptest::prelude::*;
use std::collections::HashMap;

/// A term name, like the Haskell generator's `listOf1 (arbitrary `suchThat` isFilename)`.
///
/// Names can't contain `->`: the lexer ends a name before an arrow.
fn name() -> impl Strategy<Value = String> {
    prop::collection::vec(
        any::<char>().prop_filter("ident char", |c| is_ident_char(*c)),
        1..6,
    )
    .prop_map(|cs| cs.into_iter().collect::<String>())
    .prop_filter("contains an arrow", |s| !s.contains("->"))
}

/// Expressions built from `Abs`, `App` and `Var` only, as in the Haskell `Arbitrary` instance.
fn expr() -> impl Strategy<Value = Expr> {
    name()
        .prop_map(Expr::var)
        .prop_recursive(4, 10, 2, |inner| {
            prop_oneof![
                (name(), inner.clone()).prop_map(|(x, e)| Expr::abs(x, e)),
                (inner.clone(), inner).prop_map(|(f, a)| Expr::app(f, a)),
            ]
        })
}

/// Expressions over a small pool of names, so that shadowing and potential
/// captures are common. Free variables are closed over with lambdas, and only
/// well-typed terms are kept.
fn typed_expr() -> impl Strategy<Value = Expr> {
    let name = prop::sample::select(vec!["x", "y", "z", "f"]);
    let leaf = name.clone().prop_map(Expr::var);
    leaf.prop_recursive(5, 24, 2, move |inner| {
        prop_oneof![
            (name.clone(), inner.clone()).prop_map(|(x, e)| Expr::abs(x, e)),
            (inner.clone(), inner).prop_map(|(f, a)| Expr::app(f, a)),
        ]
    })
    .prop_map(|e| free_vars(&e).into_iter().fold(e, |e, x| Expr::abs(x, e)))
    .prop_filter("well-typed", |e| type_of(e).is_some())
}

fn type_of(e: &Expr) -> Option<Type> {
    type_of_top_level(e, &mut Loader::new(&[], false)).ok()
}

/// Is `specific` an instance of `general` (by substituting `general`'s
/// type variables)?
fn is_instance(general: &Type, specific: &Type, m: &mut HashMap<String, Type>) -> bool {
    match (general, specific) {
        (Type::TVar(n), t) => match m.get(n) {
            Some(bound) => bound == t,
            None => {
                m.insert(n.clone(), t.clone());
                true
            }
        },
        (Type::Text, Type::Text) => true,
        (Type::Arrow(a1, b1), Type::Arrow(a2, b2)) => {
            is_instance(a1, a2, m) && is_instance(b1, b2, m)
        }
        _ => false,
    }
}

/// No application of a lambda remains anywhere.
fn is_normal(e: &Expr) -> bool {
    match e {
        Expr::App(f, a) => !matches!(**f, Expr::Abs(..)) && is_normal(f) && is_normal(a),
        Expr::Abs(_, b) => is_normal(b),
        _ => true,
    }
}

fn alnum_word() -> impl Strategy<Value = String> {
    "[a-zA-Z0-9]{1,6}"
}

fn document() -> impl Strategy<Value = Document> {
    let body = prop_oneof![
        prop::collection::vec(alnum_word(), 1..4).prop_map(DocumentBody::RawText),
        expr().prop_map(DocumentBody::Expression),
    ];
    (
        prop::collection::vec(alnum_word(), 1..4),
        prop::collection::vec(body, 1..6),
    )
        .prop_map(|(arity, body)| Document { arity, body })
}

proptest! {
    #[test]
    fn print_parse_iso(e in expr()) {
        prop_assert_eq!(parse_expr(&pp_expr(&e)), Ok(e));
    }

    #[test]
    fn eval_reaches_normal_form(e in typed_expr()) {
        let v = evaluate(&e);
        prop_assert!(is_normal(&v), "{} evaluated to {}", pp_expr(&e), pp_expr(&v));
        prop_assert_eq!(evaluate(&v), v.clone());
    }

    #[test]
    fn eval_preserves_types(e in typed_expr()) {
        let v = evaluate(&e);
        // Evaluation can only make the type more general (e.g. by dropping an
        // unused argument and its constraints).
        let (te, tv) = (type_of(&e).unwrap(), type_of(&v).expect("result is well-typed"));
        prop_assert!(
            is_instance(&tv, &te, &mut HashMap::new()),
            "{} : {} evaluated to {} : {}",
            pp_expr(&e), pp_type(&te), pp_expr(&v), pp_type(&tv)
        );
    }

    #[test]
    fn print_parse_iso_doc(
        doc in document(),
        l in "[a-zA-Z0-9]{2,5}",
        r in "[a-zA-Z0-9]{2,5}",
    ) {
        let doc = Document { arity: doc.arity, body: repack_document(doc.body) };
        let txt = print_document(Some((&l, &r)), &doc).unwrap();
        let (parsed, delims) = parse_document(&txt).unwrap();
        prop_assert_eq!(parsed, doc);
        if let Some((l2, r2)) = delims {
            prop_assert_eq!((l2, r2), (l, r));
        }
    }
}
