![ltext](https://cdn.rawgit.com/ltext/ltext.github.io/master/logo/ltext.png)

λtext
=====

> General-Purpose Templating Utility

# Overview

λtext turns text files into higher-order functions, featuring a Hindley-Milner /
prenex polymorphic type system. See the [github.io page](http://ltext.github.io/).

## Building

λtext is written in Rust (1.85 or newer).

```bash
$> git clone https://github.com/ltext/ltext
$> cd ltext
$> cargo install --path .
```

### Using Nix

With flakes enabled:

```bash
$> nix build          # binary at ./result/bin/ltext
$> nix run . -- --help
$> nix develop        # shell with cargo, clippy, rustfmt and rust-analyzer
```

Without flakes, `nix-build` builds the executable from your `<nixpkgs>` channel.

### Testing

```bash
$> cargo test                                   # unit and property tests
$> cargo build --release
$> golden/run.sh target/release/ltext           # CLI golden tests
```

See `golden/README.md` for the golden test corpus.

## Usage

```bash
$> ltext -h
λtext - parameterized file evaluator

Usage: ltext [OPTIONS] <EXPRESSION>

Arguments:
  <EXPRESSION>  Expression to evaluate, e.g. `greet name` or `\x -> wrap x`

Options:
  -t, --type           Print the type of EXPRESSION instead of evaluating it
  -v, --verbose        Report what ltext is doing on stderr
  -r, --raw <FILE>     Treat FILE as plain text, even if its first line looks like a header
      --left <LEFT>    Left delimiter for rendering a partially applied result
      --right <RIGHT>  Right delimiter for rendering a partially applied result
  -h, --help           Print help (see more with '--help')
  -V, --version        Print version

$> ltext --type "\a -> a"
a0 -> a0
```

For example, given a file `greet`:

```
{{ name }}
Hello,
{{ name }}
!
```

and a file `name` containing `World`, `ltext "greet name"` prints:

```
Hello,
World
!
```

Applying fewer arguments than a template takes leaves a template.
`--left` and `--right` give the delimiters to render it with:

```bash
$> ltext --left '<<' --right '>>' "\n -> greet n"
<< n >>
Hello,
<< n >>
!
```

## How It Works

From λtext's point of view, _any text file_ can be a template (given that
it's utf-8 encoded). Just declare parameters in the first line of
your files (usually in a different syntax than the file's native tongue,
via comments or obscure delimiters), then use those variables somewhere in
the content. With the `ltext` command you can then apply the function-y
files to each other.

### The CLI

There will be two primary uses of the `ltext` command - evaluating template
expressions, and querying for the type signature of a template/expression.

#### Type Queries

Just like the `:t` command in GHCi, you can find out the type of a template or
expression with `-t`.

#### Expression Evaluation

All files have __closed scope__, meaning they only have access to the variables
declared in the file. For instance:

```
{{ foo }}

...
```

Will _only_ have access to the variable `foo`, while using the delimiters `{{`
and `}}` to escape your expression.

#### Variable Recognition

When we use a parameter in a file, we need it to be easily recognized by a parser;
a different syntax than to the language you're working with.

The first line in a file will be parsed to see if it qualifies as a
lambda header. If you don't want a file have recognized arity, just invoke `ltext`
with the `--raw` argument listing the file:

```bash
$> ltext "foo bar" --raw "bar"
```


# Credits

All credits go to Martin Grabmueller's [AlgorithmW](https://hackage.haskell.org/package/AlgorithmW) package for the type inference algorithm used in λtext.
