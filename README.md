[![Stories in Ready](https://badge.waffle.io/ltext/ltext.png?label=ready&title=Ready)](https://waffle.io/ltext/ltext)
![ltext](nonsense/ltext.png)

λtext
=====

Functional Templates

> __WARNING:__ Partially applying files doesn't allow them to be parsable again
> (yet) - __only use this if you're fully applying templates__.

# Overview

λtext turns text files into higher-order functions, featuring a Hindley-Milner /
prenex polymorphic type system.

## How It Will Work

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
