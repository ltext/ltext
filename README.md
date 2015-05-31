![ltext](https://cdn.rawgit.com/ltext/ltext.github.io/master/logo/ltext.png)

λtext
=====

[![Stories in Ready](https://badge.waffle.io/ltext/ltext.png?label=ready&title=Ready)](https://waffle.io/ltext/ltext)

> General-Purpose Templating Utility

# Overview

λtext turns text files into higher-order functions, featuring a Hindley-Milner /
prenex polymorphic type system.

## Installation

```bash
$> cabal install ltext
```

## Usage

You can supply options manually with flags:

```bash
$> ltext --help

ltext - higher-order file applicator

Usage: ltext EXPRESSION [-t|--type] [-o|--output OUTPUT] [-l|--left LEFTDELIM]
             [-r|--right RIGHTDELIM] [-c|--config CONFIG]
  Evaluate EXPRESSION and send to OUTPUT

Available options:
  -h,--help                Show this help text
  -t,--type                query the type signature of an expression
  -o,--output OUTPUT       output destination
  -l,--left LEFTDELIM      left delimiter
  -r,--right RIGHTDELIM    right delimiter
  -c,--config CONFIG       location of config file

# as an example
$> ltext "foo bar" --left "{{" --right "}}" -o "baz"
```

or, you can store commonly used ones in a YAML file under `./.ltext/config.yaml`:

```bash
$> cat ./.ltext/config.yaml

typeQuery: false
left: "//{{"
right: "}}"
```

```bash
$> cat foo

## x ##

asdf
birds

## x ##

fsda
```

```bash
# keeping delimiters in-line
$> ltext "foo"

//{{ x }}

asdf
bird

//{{ x }}

fsda

$> cat bar

hashtagtrashswag

# application
$> ltext "foo bar"

hashtagtrashswag

asdf
bird

hashtagtrashswag

fsda
```

## How It Works

From λtext's point of view, _any file_ can be a template. Just declare parameters
in your files (usually in a different syntax than the file's native tongue,
via comments or obscure delimiters), use those variables somewhere in
the content, then with the `ltext` command you apply the function-y files to each other.

### The CLI

There will be two primary uses of the `ltext` command - evaluating template
expressions, and querying for the type signature of a template/expression.

#### Expression Evaluation

When we evaluate a template expression, normally we just feed the result to
`stdout`. But there is also the `-o` flag, for explicitly stating an output file.

> __Note__: all output from `ltext` will be UTF-8.

#### Type Queries

Just like the `:t` command in GHCi, you can find out the type of a template or
expression with `-t`.

#### Variable Recognition

When we use a parameter in a file, we need it to be easily recognized by a parser -
either a different syntax than to the language you're working with - an explicit delimitation.

The first line in a file will be tested against to see if it qualifies as a
lambda header. If you don't want a file have recognized arity, just throw in a
newline.
