![ltext](https://cdn.rawgit.com/ltext/ltext.github.io/master/logo/ltext.png)

λtext
=====

> General-Purpose Templating Utility

# Overview

λtext turns text files into higher-order functions, featuring a Hindley-Milner /
prenex polymorphic type system. See the [github.io page](http://ltext.github.io/).

## Building

```bash
$> git clone git@github.com/ltext/ltext
$> cd ltext
$> stack install ltext
```

This should install in one pass; all the non-stackage dependencies are included
in `stack.yaml`.

## Usage

```bash
$> ltext --help

λtext - parameterized file evaluator

Usage: ltext EXPRESSION [--version] [-t|--type] [-v|--verbose] [-r|--raw FILE]
             [--left LEFT] [--right RIGHT]
  Evaluate EXPRESSION and send the substitution to stdout. See
  http://ltext.github.io/ for more details.

Available options:
  -h,--help                Show this help text
  --version                Print the version number
  -t,--type                Perform type inference on an expression
  -v,--verbose             Be verbose, sending info through stderr
  -r,--raw FILE            Treat these files as plaintext without an arity
                           header
  --left LEFT              The left delimiter to use for rendering partially
                           applied files
  --right RIGHT            The right delimiter to use for rendering partially
                           applied files

$> ltext --type "\a -> a"

a0 -> a0
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
