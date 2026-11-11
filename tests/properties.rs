//! Ports of the QuickCheck properties in `test/Main.hs`.

use ltext::chars::is_ident_char;
use ltext::document::{Document, DocumentBody, parse_document, print_document, repack_document};
use ltext::eval::evaluate;
use ltext::expr::{Expr, parse_expr, pp_expr};
use proptest::prelude::*;

/// A term name, like the Haskell generator's `listOf1 (arbitrary `suchThat` isFilename)`.
///
/// Names starting with `->` are excluded: they print as an arrow, so they don't
/// survive a print/parse round trip in either implementation.
fn name() -> impl Strategy<Value = String> {
    prop::collection::vec(
        any::<char>().prop_filter("ident char", |c| is_ident_char(*c)),
        1..6,
    )
    .prop_map(|cs| cs.into_iter().collect::<String>())
    .prop_filter("prints as an arrow", |s| !s.starts_with("->"))
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
    fn idempotent_eval(e in expr()) {
        let once = evaluate(&e);
        prop_assert_eq!(evaluate(&once), once);
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
