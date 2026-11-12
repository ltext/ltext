//! Benchmarks for λtext. Run with `cargo bench`; `cargo bench -- render` runs
//! one group. See `benches/README.md`.
//!
//! Each group scales one input dimension so that the growth rate (linear,
//! quadratic, …) is visible, not just the absolute time.

use std::fmt::Write as _;
use std::hint::black_box;
use std::path::PathBuf;
use std::process::Command;
use std::time::Duration;

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};

use ltext::document::{parse_document, print_document, to_document};
use ltext::eval::{eval, evaluate};
use ltext::expr::{Expr, parse_expr};
use ltext::files::Loader;
use ltext::typecheck::type_of_top_level;

// ---------------------------------------------------------------------------
// Inputs

/// A Church numeral: `\f x -> f (f (… x))`.
fn church(n: usize) -> String {
    format!(r"(\f x -> {}x{})", "f (".repeat(n), ")".repeat(n))
}

/// A template with `holes` expression lines, each preceded by three lines of
/// text, all filled by the one parameter `x`.
fn template(holes: usize) -> String {
    let mut s = String::from("{{ x }}\n");
    for i in 0..holes {
        writeln!(
            s,
            "line {i} of the template\nsome more text\nand a third line\n{{{{ x }}}}"
        )
        .unwrap();
    }
    s
}

/// `lines` lines of plain text. The first line has fewer than three words, so
/// it isn't a header and the file has no parameters.
fn text(lines: usize) -> String {
    let mut s = String::from("Notes\n");
    for i in 1..lines {
        writeln!(s, "line {i}: the quick brown fox jumps over the lazy dog").unwrap();
    }
    s
}

/// A document whose first line looks like a header (three or more words), so
/// that every line with the same first and last words is an expression line.
fn expression_lines(lines: usize) -> String {
    let mut s = String::new();
    for i in 0..lines {
        writeln!(s, "line {i}: the quick brown fox jumps over the lazy dog").unwrap();
    }
    s
}

/// A scratch directory holding the files that the rendering benchmarks
/// read. The benchmarks run with it as the working directory, since file
/// names are variables in the command-line expression.
struct Fixtures {
    dir: PathBuf,
}

impl Fixtures {
    fn new() -> Fixtures {
        let dir = std::env::temp_dir().join(format!("ltext-bench-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let f = Fixtures { dir };
        f.write("value", "hello, world\n");
        f.write("id", "{{ x }}\n{{ x }}\n");
        f.write("greet", "{{ name }}\nHello,\n{{ name }}\n!\n");
        f.write("name", "World\n");
        f.write("wrap", "{{ x }}\n<div>\n{{ x }}\n</div>\n");
        f.write("pair", "[[ x y ]]\nfirst: x\n[[ x ]]\nsecond:\n[[ y ]]\n");
        f
    }

    fn write(&self, name: &str, contents: &str) {
        std::fs::write(self.dir.join(name), contents).unwrap();
    }

    fn enter(&self) {
        std::env::set_current_dir(&self.dir).unwrap();
    }
}

impl Drop for Fixtures {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.dir);
    }
}

/// What the `ltext` binary does with an expression, minus argument parsing
/// and writing to stdout (see `run` in src/main.rs).
fn render(expression: &str, raw: &[String]) -> String {
    let expr = parse_expr(expression).unwrap();
    let mut files = Loader::new(raw, false);
    type_of_top_level(&expr, &mut files).unwrap();
    let resolved = files.resolve(&expr).unwrap();
    let doc = to_document(&eval(resolved)).unwrap();
    print_document(None, &doc).unwrap()
}

fn closed(src: &str) -> Expr {
    parse_expr(src).unwrap()
}

// ---------------------------------------------------------------------------
// Front end: lexing, parsing, type checking

fn bench_parse(c: &mut Criterion) {
    let mut g = c.benchmark_group("parse_expr");
    for n in [10, 100, 1000] {
        // `f a0 a1 … an`
        let app: String = (0..n).fold("f".into(), |s, i| format!("{s} a{i}"));
        g.throughput(Throughput::Bytes(app.len() as u64));
        g.bench_with_input(BenchmarkId::new("application", n), &app, |b, s| {
            b.iter(|| parse_expr(black_box(s)).unwrap())
        });
        let num = church(n);
        g.throughput(Throughput::Bytes(num.len() as u64));
        g.bench_with_input(BenchmarkId::new("nested_parens", n), &num, |b, s| {
            b.iter(|| parse_expr(black_box(s)).unwrap())
        });
    }
    g.finish();

    let mut g = c.benchmark_group("parse_document");
    for holes in [10, 100, 1000] {
        let t = template(holes);
        g.throughput(Throughput::Bytes(t.len() as u64));
        g.bench_with_input(BenchmarkId::new("template", holes), &t, |b, s| {
            b.iter(|| parse_document(black_box(s)).unwrap())
        });
    }
    for lines in [1_000, 10_000, 100_000] {
        let t = text(lines);
        g.throughput(Throughput::Bytes(t.len() as u64));
        g.bench_with_input(BenchmarkId::new("plain_text", lines), &t, |b, s| {
            b.iter(|| parse_document(black_box(s)).unwrap())
        });
    }
    for lines in [1_000, 10_000] {
        let t = expression_lines(lines);
        g.throughput(Throughput::Bytes(t.len() as u64));
        g.bench_with_input(BenchmarkId::new("expression_lines", lines), &t, |b, s| {
            b.iter(|| parse_document(black_box(s)).unwrap())
        });
    }
    g.finish();
}

fn bench_typecheck(c: &mut Criterion) {
    let mut g = c.benchmark_group("typecheck");
    let mut files = Loader::new(&[], false);
    for n in [10, 100, 1000] {
        let e = closed(&church(n));
        g.bench_with_input(BenchmarkId::new("church_numeral", n), &e, |b, e| {
            b.iter(|| type_of_top_level(black_box(e), &mut files).unwrap())
        });
    }
    // `(\f x -> f x) (\f x -> f x) …`: every application unifies a fresh
    // arrow type, so the substitution grows with n.
    for n in [10, 100, 1000] {
        let e = closed(&format!(r"\y -> {}y", r"(\x -> x) ".repeat(n)));
        g.bench_with_input(BenchmarkId::new("identity_chain", n), &e, |b, e| {
            b.iter(|| type_of_top_level(black_box(e), &mut files).unwrap())
        });
    }
    g.finish();
}

// ---------------------------------------------------------------------------
// Evaluation of closed expressions

fn bench_eval(c: &mut Criterion) {
    let mut g = c.benchmark_group("evaluate");
    // m^n with Church numerals: the normal form has m^n applications of f.
    for (m, n) in [(2, 4), (2, 8), (2, 10), (3, 6)] {
        let e = closed(&format!(r"(\m n -> n m) {} {}", church(m), church(n)));
        g.bench_with_input(
            BenchmarkId::new("church_power", format!("{m}^{n}")),
            &e,
            |b, e| b.iter(|| evaluate(black_box(e))),
        );
    }
    // `(\x -> x) ((\x -> x) (… y))`: n β-reductions.
    for n in [10, 100, 1000] {
        let e = closed(&format!(
            r"\y -> {}y{}",
            r"(\x -> x) (".repeat(n),
            ")".repeat(n)
        ));
        g.bench_with_input(BenchmarkId::new("identity_chain", n), &e, |b, e| {
            b.iter(|| evaluate(black_box(e)))
        });
    }
    g.finish();
}

// ---------------------------------------------------------------------------
// End to end, from files on disk

fn bench_render(c: &mut Criterion, fixtures: &Fixtures) {
    fixtures.enter();
    let mut g = c.benchmark_group("render");

    g.bench_function("greet_name", |b| {
        b.iter(|| render(black_box("greet name"), &[]))
    });

    // A template with many holes, all filled with one value.
    for holes in [10, 100, 1000, 4000] {
        let name = format!("template{holes}");
        fixtures.write(&name, &template(holes));
        let expr = format!("{name} value");
        g.throughput(Throughput::Elements(holes as u64));
        g.bench_with_input(BenchmarkId::new("template_holes", holes), &expr, |b, e| {
            b.iter(|| render(black_box(e), &[]))
        });
    }

    // `wrap (wrap (… value))`: templates applied to templates.
    for depth in [10, 100, 1000] {
        let expr = format!("{}value{}", "wrap (".repeat(depth), ")".repeat(depth));
        g.throughput(Throughput::Elements(depth as u64));
        g.bench_with_input(BenchmarkId::new("nested_wrap", depth), &expr, |b, e| {
            b.iter(|| render(black_box(e), &[]))
        });
    }

    // A large plain-text file passed through `id`.
    for lines in [1_000, 10_000, 100_000] {
        let name = format!("text{lines}");
        let t = text(lines);
        fixtures.write(&name, &t);
        let raw = vec![name.clone()];
        let expr = format!("id {name}");
        g.throughput(Throughput::Bytes(t.len() as u64));
        g.bench_with_input(
            BenchmarkId::new("raw_text_through_id", lines),
            &expr,
            |b, e| b.iter(|| render(black_box(e), &raw)),
        );
    }
    g.finish();
}

/// Whole-process time for a small template, including startup.
fn bench_cli(c: &mut Criterion, fixtures: &Fixtures) {
    let mut g = c.benchmark_group("cli");
    g.bench_function("greet_name", |b| {
        b.iter(|| {
            let out = Command::new(env!("CARGO_BIN_EXE_ltext"))
                .arg("greet name")
                .current_dir(&fixtures.dir)
                .output()
                .unwrap();
            assert!(out.status.success());
        })
    });
    g.finish();
}

fn bench_files(c: &mut Criterion) {
    let fixtures = Fixtures::new();
    bench_render(c, &fixtures);
    bench_cli(c, &fixtures);
}

fn config() -> Criterion {
    Criterion::default()
        .warm_up_time(Duration::from_secs(1))
        .measurement_time(Duration::from_secs(3))
        .sample_size(30)
}

criterion_group! {
    name = benches;
    config = config();
    targets = bench_parse, bench_typecheck, bench_eval, bench_files
}
criterion_main!(benches);
