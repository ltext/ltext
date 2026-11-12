# Benchmarks

```bash
cargo bench                         # everything (about 3 minutes)
cargo bench -- render               # one group, or any substring of a name
cargo bench -- --test               # run each benchmark once, as a smoke test
```

The suite uses [Criterion](https://docs.rs/criterion). Most inputs are
generated at several sizes, so the reports show how the time grows (linear,
quadratic, …) as well as the time itself.

| Group | What it measures |
| --- | --- |
| `parse_expr` | Lexing and parsing a command-line expression: a long application `f a0 a1 …`, and deeply nested parentheses (a Church numeral). |
| `parse_document` | Splitting a document into text and expression lines: a template with many holes, a plain-text file, and a file where every line is an expression line. |
| `typecheck` | Type inference on closed expressions: a Church numeral and a chain of identity applications. |
| `evaluate` | Reduction to normal form: Church-numeral exponentiation, and a chain of β-reductions. |
| `render` | The whole pipeline from files on disk, without process startup: `greet name`, a template with many holes, templates nested inside each other, and a large raw file passed through `id`. |
| `cli` | Running the `ltext` binary on `greet name`, including process startup. |

The `render` and `cli` groups write their input files to a temporary
directory, which is removed afterwards.

## Comparing changes

Save a baseline before a change, then compare against it:

```bash
git switch master && cargo bench -- --save-baseline before
git switch my-branch && cargo bench -- --baseline before
```

Criterion reports the change for each benchmark and whether it is
statistically significant. Results are in `target/criterion/`.
