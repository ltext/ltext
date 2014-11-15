λtext
=====

Lambdas as Templates

<img src="https://raw.githubusercontent.com/athanclark/ltext/master/nonsense/ltext.png" align="right" style="z-index: 1000; position: relative;">

## Installation

```bash
~> git clone https://github.com/athanclark/ltext.git
~> cd ltext/ && cabal sandbox init && cabal install
~> .cabal-sandbox/bin/ltext --help
```

## Usage

Normally, unproceessed files will have an appended `.ltext` extension to them,
like `index.js.ltext` - this helps identify files to be referenced. When declaring
parameters and using variables for a certain file type, it's important to pick
a syntax that doesn't clash with the origional file's. Ideally, everything should
just be regular expressions in the end, so choosing a distinguished syntax is
very useful. For instance, in JavaScript, we use `%# ... #%` to wrap all ltext
variables.

When invoking `ltext`, simply supply an expression and the output will be piped to
stdout by default (this can be overridden with `--output`). Expressions look like
this:

```bash
~> ltext 'index (foo bar) baz`
```

Where `(foo bar)` and `baz` are parameters applied to `index`, and `bar` is a parameter
applied to `foo`.

## Types

The value that references finalized textual content is `Content`. Only the values with a
type `Content` can actually be included in text verbatim.

Polymorphic arity can be inferred from the use of variables with traditional Hindley-Milner
parametric polymorphism. However, top-level type declarations of a template are indeed
possible, so templates can have their results be more rigorously defined. `finalize` has a
type signature of `(a -> Content) -> a -> Content`, for instance. Any template that uses
`finalize`, or has a signature similar to it, will output _rendered_ content and nothing more.

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

TODO: Write contribution instructions here
