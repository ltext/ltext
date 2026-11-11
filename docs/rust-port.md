# Porting λtext to Rust

Status: **preparation**. The Haskell implementation is still the reference.
This document covers the behaviour the port has to reproduce, the quirks that
are easy to get wrong, the known bugs, and the plan.

The parity spec is the golden corpus in `golden/` (83 CLI cases recorded from
the Haskell build; see `golden/README.md`). Source size is about 900 lines of
Haskell (`src/LText/{Expr,Document,Eval,Type}.hs`, `app/Main.hs`); expect
1,000–1,500 lines of Rust.

## Pipeline

```
argv ──parse──▶ Expr ──typecheck (reads files)──▶ Type ──(-t)──▶ print type
                  │
                  └─ resolve free vars as files ─▶ evaluate ─▶ toDocument ─▶ printDocument ─▶ stdout
```

Every file is read twice: once during type checking and once during resolution.

## Module map

| Haskell                  | Rust (proposed)    | Contents |
|--------------------------|--------------------|----------|
| `LText.Expr`             | `expr.rs`          | `Expr`, lexer, parser state machine, pretty printer, parse errors |
| `LText.Document`         | `document.rs`      | document parse/print, `from_document` / `to_document`, printability check |
| `LText.Eval`             | `eval.rs`          | `evaluate`, `substitute`, `free_vars` |
| `LText.Type`             | `typecheck.rs`     | types, substitutions, unification, inference |
| `Application.Types`, `app/Main.hs` | `main.rs` | CLI (clap), env, error reporting |
| `test/Main.hs` (QuickCheck) | `tests/` (proptest) | print/parse round trip, eval idempotence, document round trip |

Suggested crates: `clap` (derive), `unicode-general-category`, and `proptest`
(dev). The lexer is small enough to write by hand, with no parser combinators.
Use `thiserror` for the error enums if you like.

## Data types

```rust
enum Expr {
    Abs(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Var(String),
    Lit    { lines: Vec<String>, source: PathBuf, in_error: bool },
    Concat { left: Box<Expr>, right: Box<Expr>, source: PathBuf, in_error: bool },
}
enum Type { Text, TVar(String), Arrow(Box<Type>, Box<Type>) }
```

`in_error` exists only so error messages can show `[text from "file"]`.

## Behaviour to reproduce exactly

### Character classes (use Unicode general categories, not Rust's helpers)

- **Identifier char**: anything except `\`, `(` and `)` that is alphanumeric
  (L* or N*), a symbol (Sm, Sc, Sk, So) or punctuation (Pc, Pd, Ps, Pe, Pi, Pf,
  Po). `-`, `>`, `.`, `/` and `{` are all identifier chars.
  `char::is_alphanumeric` is **not** equivalent (it uses the Alphabetic
  property, which includes some marks).
- **Space** (attoparsec `skipSpace`, `Text.words`): space, `\t \n \v \f \r`,
  U+00A0, and any other Zs character. Unlike `char::is_whitespace`, this does
  **not** include U+0085, U+2028 or U+2029.
- **Lines** (`Text.lines`): split on `\n` only. A final `\n` doesn't produce an
  empty last line, `""` has zero lines, and `\r` is kept. Don't use
  `str::lines`, which strips `\r\n`.

### Lexer (`lex = many (lambda <|> arrow <|> bracketed <|> ident)`)

At each token, skip spaces and then try, in this order: `\` → Lambda;
`->` → Arrow; `(` lex `)` → Bracketed; one or more identifier chars → Ident.
If every alternative fails, **lexing stops silently and the rest of the input is
ignored**. There's no end-of-input check, and `LexerError` is effectively
unreachable. An alternative that fails partway backtracks to where the token
started (including the skipped spaces). Consequences, all covered by golden
cases:

- `foo bar )` → `foo bar`
- `foo (bar` → `foo` (unclosed paren: the bracket fails and the rest is dropped)
- `(foo bar` → `[]` → EmptyExpression
- `\x->x` → Lambda, Ident `x->x` → EmptyExpression (identifiers are greedy)
- `\x ->x` → `\x -> x`

### Parser (state machine in `expr`)

State is `InsideLambda | Scope(Fresh) | Scope(Stale(expr))`, starting at
`Scope(Fresh)`. Port the transitions literally from `LText/Expr.hs:200-247`.
Application is left-associative. `\a b -> e` is sugar for nested `Abs`. The
state is a single mutable cell shared across the recursion into brackets.

### Pretty printer (`ppExpr`, uses `Text.PrettyPrint`)

Everything here renders on one line, and pieces are joined with **one space**
(`<+>`), even when a piece is empty text.

- `Abs x e` → `\x -> ` + body. (The `$$`/`nest` layout always overlaps onto the
  same line.)
- `App f a`: parenthesise `f` if it is an `Abs`; parenthesise `a` if it is an
  `Abs` or an `App`.
- `Lit` → the lines joined with `\n` after each one (`unlines`). If `in_error`
  is set, `[text from "SRC"]` instead.
- `Concat l r` → `pp(l) + " " + pp(r)`, or `[text from "SRC"]` if `in_error` is
  set.

A trailing empty `Lit` gives a trailing space (`printerr-*` cases). If any golden
case disagrees with this simple model, port the needed subset of HughesPJ
instead of special-casing it.

`ppType`: `Text`, a variable name, or `t1 -> t2`, with `t1` parenthesised when
it is itself an arrow.

### Documents

- **Header** = first line, if it has ≥ 3 whitespace-separated words: the left
  delimiter, the parameter names, then the right delimiter. Any file whose first
  line has three or more words is treated as having a header, prose included
  (`eval-nonl`). That is why `--raw` exists.
- With no header, the whole file, first line included, is one `RawText` block
  and the arity is 0.
- **Expression line** = a body line with ≥ 3 words whose first word is the left
  delimiter and last word is the right delimiter. The inner words are joined
  with single spaces and parsed with a fresh parser state. Indentation and extra
  spacing don't matter, and `\r` is lost (`eval-spacing`, `eval-crlf`).
- Any other line is raw text, and consecutive raw lines merge into one block.
- `from_document`: wrap the body in `Abs` for each parameter, right to left. The
  body becomes `[]` → `Lit []`, `[x]` → `x`, and `x : rest` → `Concat x (go rest)`.
- `print_document`: `unlines` of the body lines. With arity, the header line
  `L p1 … pn R` comes first. Each expression renders as `L <pp> R`. Arity or an
  expression without both `--left` and `--right` → NoExplicitDelimiters.
- Raw files (`--raw`) become a single `Lit` holding all of their lines.

### Evaluation (port verbatim — it is *not* full β-normalisation)

```
evaluate(Abs n e)        = Abs n (evaluate e)
evaluate(Concat a b ..)  = if both evaluate to Lit: Lit (lines a ++ lines b) [keep outer source/flag]
                           else Concat a' b'
evaluate(App f a)        = match evaluate f { Abs n b => substitute n (evaluate a) (evaluate b),
                                              f'       => App f' (evaluate a) }
```

The result of `substitute` is **not** evaluated again. Substitution doesn't
avoid capture (it stops at a binder with the same name). That's safe in
practice only because file documents are closed terms.

### Type inference (port verbatim, including the counter order)

Fresh variables are `a0`, `a1`, … from one counter shared across the whole run,
**documents included**. The numbers show up in `-t` output, so the order of
`fresh()` calls has to match exactly:

- `Abs`: fresh, then bind it in the shared context **without restoring it
  afterwards** (see the bugs below).
- `App`: fresh **before** inferring either side. Apply `s1` to the context
  between the two sides, then unify `s2(t1)` with `t2 -> fresh`.
- `Concat`: infer both sides, unify them, then unify with `Text`; compose as
  `s4 ∘ s3 ∘ s2 ∘ s1`.
- `Var` at top level: if it isn't in the context and is an existing **file**,
  it has type `Text` when it's in `--raw`; otherwise infer the parsed document
  at document level. At document level, an unbound variable is an error.
- Composition `f <> g = map (apply f) g ∪ f`, with the left side winning on
  conflicts.
- `quantify` is never called, so every scheme has an empty quantifier set.
  Schemes are plain types, and `HashSet` iteration order never affects output.
- Unifier argument order matters for error messages
  (`Can't unify type X with Y`).

### CLI and exit codes

- `ltext EXPRESSION [--version] [-t|--type] [-v|--verbose] [-r|--raw FILE]... [--left L] [--right R]`
- Success → exit 0, with output on stdout and no trailing additions.
- Type, print and in-document parse errors → one line on stderr
  (`[Type Error] …`, `[Print Error] …`, `[Parse Error] …`), exit 1.
- Usage errors → exit 1. An argument starting with `-`, such as `->`, is taken
  as an option.
- Only the exit code and stdout are normative for usage and help output. clap's
  wording will differ.
- `--left` without `--right` (or the reverse) is silently ignored.

## Known bugs (recorded as-is in the golden corpus)

| # | Bug | Golden case(s) |
|---|-----|----------------|
| 1 | Top-level parse errors aren't caught. They escape as a GHC exception dump instead of `[Parse Error] …` (only parse errors *inside documents* get the formatted message). | `parse-*` |
| 2 | `--version` can't run: EXPRESSION is required. It would also print the stale `0.1.3`. | `cli-version` |
| 3 | **Type-checker scope leak.** `Abs` never restores the context, so a parameter name used inside a referenced file overwrites the caller's binding. `\x -> apply x` is rejected and `\f -> apply f` gets the **unsound** type `a0 -> a3 -> Text`; `\z -> apply z` is correct. | `type-partial`, `type-scope-leak-*` |
| 4 | Text spliced into an argument position isn't flagged as unprintable. It gets printed inside the delimiters and produces a broken document. | `partial-free-in-body`, `partial-text-in-arg` |
| 5 | `-v/--verbose` does nothing. | `cli-verbose` |
| 6 | Files are read and parsed twice (type check, then resolution). | — |

## Plan

1. **Scaffold** a Cargo crate in `rust/`. It can't go at the repo root yet
   because Haskell already uses `src/`. Set up the module layout above, and run
   `cargo test` plus `golden/run.sh rust/target/release/ltext` in CI next to the
   Haskell jobs.
2. **Port the core** in dependency order — `expr` → `eval` → `document` →
   `typecheck` → `main` — with unit tests per module. Port the three QuickCheck
   properties to proptest.
3. **Parity**: make `golden/run.sh` pass on stdout and exit codes. Use
   `--strict-stderr` for the `[… Error]` lines too, except the bug #1 cases.
   Optionally add a differential fuzz harness that runs both binaries on random
   expressions and documents.
4. **Cut over**: move the crate to the root and delete the Haskell sources. Then
   switch Nix to `rustPlatform.buildRustPackage`, switch the snap to the `rust`
   plugin (re-check the classic/strict confinement plugs), and update the
   README. Release as 0.2.0.
5. **Fix the bugs above** in separate commits, re-recording only the affected
   golden cases (`golden/run.sh --update --only …` on the Rust binary). Each
   diff then shows exactly what behaviour changed.

## Open decisions

- **Bug-for-bug parity first, then fixes** (recommended, as in the plan), or fix
  while porting? Parity first means the corpus is an exact oracle. Fixing while
  porting means the corpus has to be edited by hand.
- **Error message format** for the bug #1 cases: use the in-document
  `[Parse Error] …` format (recommended). Should the lexeme dumps keep Haskell's
  `show` syntax (`[Ident "x",Arrow]`) or switch to something friendlier?
- **Haskell's future**: delete it at cut-over (recommended), or keep it as a
  reference implementation for a while.
- **Library API**: is anything besides the CLI a public interface? If not, keep
  it a single binary crate with internal modules.
