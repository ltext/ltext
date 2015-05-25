[![Stories in Ready](https://badge.waffle.io/ltext/ltext.png?label=ready&title=Ready)](https://waffle.io/ltext/ltext)
![ltext](nonsense/ltext.png)

λtext
=====

Functional Templates

> __WARNING__ This project is still in it's infancy, so don't expect it to work!

# Overview

λtext turns text files into higher-order functions, featuring a Hindley-Milner /
prenex polymorphic type system.

## How It Will Work

From λtext's point of view, _any file_ can be a template. Just delcare parameters
in your files (usually in a different syntax than the file's native tongue,
via comments or obscure delimiters), use those variables somewhere in
the content, then with the `ltext` command you apply the function-y files to each other.

### The System

λtext has polymorphic types and monomorphic types.
You're free to use type annotations (just like Haskell), or
let the type system infer the most general one (just like Haskell).

There will not be user-definable types, if-statements, pattern matching, or
`undefined`. Just good 'ol λ-calculus.

### The CLI

There will be two primary uses of the `ltext` command - evaluating template
expressions, and querying for the type signature of a template/expression.

#### Expression Evaluation

When we evaluate a template expression, normally we just feed the result to
`stdout`. But there will be configuration options, like a flag for specifying
an output file.

There will also be options to pad or clip whitespace at variable use sites or the result
of a template. This will let you make sure content is substituted just how you
need it.

> __Note__: all output from `ltext` will be UTF-8.

#### Type Queries

Just like the `:t` command in GHCi, you can find out the type of
a template or expression. We also allow people to declare type signature, giving
them variable precision for each expression. This is useful for finalization steps
in a compilation process, for instance - just constrain the result to be `:: Text`.

#### Variable Recognition

When we use a parameter in a file, we need it to be easily recognized by a parser -
either a different syntax than to the language you're working with,
or a very explicit delimitation.

For instance, if we are working in javascript, we would want the variables to
look _completely_ different from javascript - something
like this might work:

```javascript
// foo.js x y =
function Foo (x, y) {
  // {{x}}
  this.bar = {{bar}};
  this.x = x;
  this.y = y;
  /* --y-- */
}
```

- It's unobtrusive to the existing language (ie; can still `jshint`)
- delimitation is explicit

The only issue we face is the detection of unobtrusive spans of content in different
languages - the `/* ... */` could be used as a delimiter in pattern recognition,
while `//` serves as a more correct initializer for "unobtrusive content". Still a work in-progress.

## Template Files

Unproceessed files should have `.ltext` appended to them,
like `index.js.ltext` - this helps identify files to be referenced, and keeps people
from using them in production. But, it's not necessary to adopt the practice.

The choices for parsing & formatting may reside in a
`.ltext/config/` folder. Here, we have files such as `default.js.yml`,
`default1.js.yml`, etc. Rules should cascade - we might try our formatting rules
in `default.*`, then cascade alphabetically until a
match is found that works with the template. Generally speaking, this will all
"just work" under the hood, but just in case it's tricky, you can supply
your own override via `-o`.

### Constraining Templates

Imagine you have a rat's nest of templates of all shapes and kinds - markdown, HTML,
javascript & coffeescript templates, tons of stuff. How do you make sure your
expressions "make sense" beyond λ-checking? This is where qualified types can come in handy.

For instance: inserting content in HTML - there are only three languages that can
be legally inserted: other HTML, CSS, and JavaScript. How do we establish this
constraint?

```haskell
foo.html.ltext :: (a.html + a.css + a.js) => a -> Content
bar.js.ltext :: Content
baz.md.ltext :: Content

foo bar -- Type checks
foo baz -- Does not type check
```

> ...note the consice syntax of `foo bar` - if the file names are unique, we can
> write _just enough_ to be identified.

File extension polymorphism - `(+)` adds, `(-)` excludes
