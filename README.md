![ltext](https://cdn.rawgit.com/ltext/ltext.github.io/master/logo/ltext.png)

λtext
=====

> General-Purpose Templating Utility

# Overview

λtext turns text files into higher-order functions, featuring a Hindley-Milner /
prenex polymorphic type system. See the [github.io page](http://ltext.github.io/).

## Installing

Each [release](https://github.com/ltext/ltext/releases/latest) has
ready-made builds. Every asset name includes the version, shown as `VERSION`
below.

| Platform | Download |
| --- | --- |
| Debian, Ubuntu | `ltext_VERSION-1_amd64.deb` / `_arm64.deb`: `sudo apt install ./ltext_*.deb` |
| Fedora, RHEL, openSUSE | `ltext-VERSION-1.x86_64.rpm` / `.aarch64.rpm`: `sudo dnf install ./ltext-*.rpm` |
| Any Linux (static binary) | `ltext-VERSION-x86_64-unknown-linux-musl.tar.gz` / `aarch64-…` |
| macOS (Apple silicon, Intel) | `ltext-VERSION-aarch64-apple-darwin.tar.gz` / `x86_64-…` |
| Windows (x64, ARM64) | `ltext-VERSION-x86_64-pc-windows-msvc.exe` or `.zip` / `aarch64-…` |

Unpack an archive and put `ltext` somewhere on your `PATH`. `SHA256SUMS`
lists the checksum of every asset. macOS may quarantine a downloaded binary;
`xattr -d com.apple.quarantine ltext` clears that.

With Cargo: `cargo install ltext` builds it from [crates.io](https://crates.io/crates/ltext);
`cargo binstall ltext` fetches the prebuilt binary instead.

With Nix: `nix run github:ltext/ltext -- --help`, or `nix profile add github:ltext/ltext`.

On Arch Linux, from the AUR: [`ltext`](https://aur.archlinux.org/packages/ltext) (builds from source) or
[`ltext-bin`](https://aur.archlinux.org/packages/ltext-bin) (prebuilt), e.g. `yay -S ltext-bin`.

`ltext --version` prints the release version, e.g. `ltext 0.2.0`. A build of
an untagged commit prints the `git describe` form instead, e.g.
`ltext 0.2.0-3-g1a2b3c4`: 3 commits after 0.2.0, at commit `1a2b3c4`.

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
