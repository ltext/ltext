//! Very deep inputs. Each test runs on a thread with a small stack, so any
//! code that recursed once per node would overflow it.

use std::process::Command;

use ltext::document::{print_document, to_document};
use ltext::eval::eval;
use ltext::expr::{parse_expr, pp_expr};
use ltext::files::Loader;
use ltext::typecheck::{pp_type, type_of_top_level};

const DEPTH: usize = 100_000;

fn on_small_stack(f: impl FnOnce() + Send + 'static) {
    std::thread::Builder::new()
        .stack_size(256 * 1024)
        .spawn(f)
        .unwrap()
        .join()
        .unwrap();
}

/// Type-check, evaluate and print a closed expression.
fn run(src: &str) -> (String, String) {
    let e = parse_expr(src).unwrap();
    let ty = type_of_top_level(&e, &mut Loader::new(&[], false)).unwrap();
    (pp_type(&ty), pp_expr(&eval(e)))
}

#[test]
fn nested_parentheses() {
    on_small_stack(|| {
        // \f x -> f (f (… x))
        let src = format!(r"\f x -> {}x{}", "f (".repeat(DEPTH), ")".repeat(DEPTH));
        let (ty, out) = run(&src);
        assert_eq!(ty, "(a0 -> a0) -> a0 -> a0");
        let expected = format!(
            r"\f -> \x -> {}f x{}",
            "f (".repeat(DEPTH - 1),
            ")".repeat(DEPTH - 1)
        );
        assert!(out == expected);
    });
}

#[test]
fn long_application() {
    on_small_stack(|| {
        // \f x -> f x x … x
        let src = format!(r"\f x -> f{}", " x".repeat(DEPTH));
        let (ty, out) = run(&src);
        assert!(ty.starts_with("(a0 -> a0 -> "));
        assert!(out == src.replacen(r"\f x", r"\f -> \x", 1));
    });
}

#[test]
fn many_parameters() {
    on_small_stack(|| {
        // \x0 x1 … -> x0, whose type is an arrow chain as long as the input.
        let params: Vec<String> = (0..DEPTH).map(|i| format!("x{i}")).collect();
        let src = format!(r"\{} -> x0", params.join(" "));
        let (ty, out) = run(&src);
        assert_eq!(ty.matches(" -> ").count(), DEPTH);
        assert!(out.starts_with(r"\x0 -> \x1 -> "));
    });
}

#[test]
fn deep_normal_form() {
    on_small_stack(|| {
        // 2^(2^(2^2)) as Church numerals: a normal form 65,536 levels deep,
        // from a short expression.
        let two = r"(\f x -> f (f x))";
        let e = parse_expr(&[two; 4].join(" ")).unwrap();
        let out = pp_expr(&eval(e));
        assert_eq!(out.matches(" (").count(), 65_535);
    });
}

#[test]
fn unclosed_deep_parentheses() {
    on_small_stack(|| {
        let src = "(".repeat(DEPTH);
        assert_eq!(
            parse_expr(&src).unwrap_err().to_string(),
            format!("[Parse Error] Unclosed parenthesis at column {DEPTH}")
        );
    });
}

/// A template with one expression line per hole.
fn template(holes: usize) -> String {
    let mut s = String::from("{{ x }}\n");
    for i in 0..holes {
        s.push_str(&format!("line {i}\n{{{{ x }}}}\n"));
    }
    s
}

#[test]
fn template_with_many_holes() {
    // File names are variables in the expression, so use a relative path with
    // forward slashes: a Windows absolute path's backslashes would be lambdas.
    // Tests run in the package directory.
    let rel = format!("target/ltext-deep-{}", std::process::id());
    let dir = std::path::PathBuf::from(&rel);
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("tpl"), template(100_000)).unwrap();
    std::fs::write(dir.join("val"), "v\n").unwrap();

    // Through the library, on a small stack.
    on_small_stack(move || {
        let tpl = format!("{rel}/tpl");
        let val = format!("{rel}/val");
        let e = parse_expr(&format!("{tpl} {val}")).unwrap();
        let mut files = Loader::new(&[], false);
        type_of_top_level(&e, &mut files).unwrap();
        let resolved = files.resolve(&e).unwrap();
        let out = print_document(None, &to_document(&eval(resolved)).unwrap()).unwrap();
        assert_eq!(out.lines().count(), 200_000);
        // A partial application keeps every expression line.
        let e = parse_expr(&tpl).unwrap();
        let resolved = files.resolve(&e).unwrap();
        let doc = to_document(&eval(resolved)).unwrap();
        let out = print_document(Some(("{{", "}}")), &doc).unwrap();
        assert_eq!(out.lines().count(), 200_001);
    });

    // Through the binary.
    let out = Command::new(env!("CARGO_BIN_EXE_ltext"))
        .arg("tpl val")
        .current_dir(&dir)
        .output()
        .unwrap();
    std::fs::remove_dir_all(&dir).unwrap();
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(out.stdout.iter().filter(|&&b| b == b'\n').count(), 200_000);
}
