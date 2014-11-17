λtext
=====

![ltext](nonsense/ltext.png)

Lambdas as Templates

> __WARNING__ This project is still in it's infancy, so don't expect it to work!

## Overview

λtext is a text templating engine that uses a variant of the polymorphic lambda 
calculus (System F) as a means to represent templates.

## How It Will Work

From λtext's point of view, _every file_ is a template (or just raw content). We 
delcare parameters in our text files _within_ the text file itself (the variable 
turns it into a template), and we use the λtext cli to apply the templates to 
each other.

We emply a strong typing scheme, similar to Haskell's. The polymorphic type 
inference in λtext allows us to monkey-patch templates together intuitively - 
for instance, if I have a template `foo` with a type `a -> a`, we can apply 
content to `foo` just as easily as extending `foo` with another template:

```haskell
foo :: forall a. a -> a
bar :: forall a. a -> a
baz :: Content

foo baz :: Content
(foo bar) baz :: Content
```

Just like Haskell, the `forall a.` isn't necessary.

### The Type System

There are two kinds of types in λtext - polymorphic types represented by type 
variables declared with `forall`, and the `Content` (unit) type. There is no 
`curry` or tuple, nor sum types or any algebraic data types. The only means we 
have for declaring a type are  _type aliases_ (taken from Haskell), which is 
fine - all we are doing here is applying templates together. If this need ever 
arises, we'll assess it then, but for now it's unneccesary.

### The CLI

There will be three main uses of the `ltext` command - evaluating template 
expressions, querying for the type signature of a template or template 
expression, and checking the integrity of a template file.

#### Expression Evaluation

When we evaluate a template expression, normally we just feed the result to 
`stdout` and pipe in our shell to a result file or what have you. But, just in 
case, there's a flag for specifying an output file.

We will also supply a few utility functions, like `nolines` and `nowords`, which 
eliminate newline characters and spaces, respectively. Note that these are 
_forgetful_ transformations, unline `unlines` and `unwords` from Haskell 
(because there's no list type in λtext). We will also include serializers and 
escaping (possibly even including the serializers implicitly).

Note that all output from expression evaluations will become UTF-8. Please 
prepare accordingly.

#### Type Queries

Type queries are just like the `:t` command in GHCi - they infer the type of a 
template based on it's content. We also allow people to specify the type signature of 
templates and their individual parameters, giving them the ability to strictly 
require a parameter to have a flat `Content` type, for instance. This is useful 
for finalization steps in a compilation process. This also lets us practically 
see if our expression will work, or what else is needed in our expression to get 
what we want.

#### Integrity Checks

Template integrity checks are a technical necessity. When we declare parameters 
in our file, we want these parameters to be easily seen by our parser - that is, 
it uses an _alien syntax_ compared to the syntax of the content you're 
templating. For instance, if we are working in javascript, we would want our 
template variables to look completely different from actual javascript, else 
we could shoot ourselves in the foot later and lose the variable 
somewhere in the template body. Integrity checks make sure that our variable is 
found in the template body, and will also try to find human errors - similar 
names found in the body (forgot to make the variable look alien), free variables 
(every template only has scope to it's parameter declarations), and other 
lexical errors.

Later, we'll give facilities to the CLI for adding and removing variables from a 
template header, hopefully to elimonate human errors.

## Template Files

Normally, unproceessed files will have an appended `.ltext` extension to them,
like `index.js.ltext` - this helps identify files to be referenced. When 
declaring parameters and using variables for a certain file type, it's important 
to pick a syntax that doesn't clash with the origional file's. Ideally, 
everything should just be regular expressions in the end, so choosing a 
distinguished syntax is very useful. For instance, in JavaScript, we use
`%# ... #%` to wrap all ltext variables.

The actual mechanisms for parsing & formatting for each file type are in the 
`config/` folder. Here, we have files such as `default.js.yml`, 
`default1.js.yml`, etc. First, we try our formatting rules in `default.*` 
(matching our template's content type), then cascade alphabetically until a 
match is found that works with the template. Generally speaking, this will all 
"just work" under the hood, but just in case you're being tricky, you can supply 
your own override via the `-o` flag.

### Stabilizing Templates

Imagine you have an orchestra of templates of all kinds and types - markdown 
templates, higher-order HTML templates, javascript & coffeescript templates, 
tons of stuff. How do you make sure your template applications "make sense"? 
This is where qualified types come in handy.

One of the main use cases I've had is including content to HTML - there are only 
three types of content that can be included: other HTML, CSS, and (rendered) 
JavaScript. So how do we establish this constraint? Like typeclasses!

```haskell
foo.html.ltext :: (a.html + a.css + a.js) => a -> Content
bar.js.ltext :: Content
baz.md.ltext :: Content

foo.html.ltext bar.js.ltext -- Type checks ✓
foo baz                     -- Does not type check
```

> ...note the more consice syntax of `foo baz` - if the file names are unique, we can 
> write _just enough_ to be identified.

Anyway, we have two facilities for qualifying a polymorphic type in λtext - 
inclusion and disclusion. Inclusion means you can be specific of the file 
extensions allowed, and combine them with union (`+`), and disclusion means you 
can specify what file extensions _should not_ be allowed (`a/.js` means a file 
that doesn't end in `.js`) and combine them with product. Here is a table of 
accepted syntax:

| Product | Union | Only  | Without |
|:-------:|:-----:|:-----:|:-------:|
| `*`     | `+`   | `.foo`| `/.foo` |
| `&`     | `|`   |       | `-.foo` |
| `,`     |       |       |         |

So, `(a.foo | a/.bar) => ...` means that a can either be a `.foo`, or not a 
`.bar`, and `(a-.bar & a-.baz) => ...` means that a cannot be either a `.foo` or 
a `.bar`. Also note that nonsensical qualifications (ie, constraints that have 
no inhabitants) can exist: `(a/.foo & a.foo) => ...`, `(a.foo & a.bar) => ...`, 
for instance.

### Example: JavaScript Template

Let's make a JavaScript template that takes in two parameters - one that's a 
template and one that gets applied to the template. We're going to format the 
header by wrapping our parameter declarations in JavaScript comments `/* ... 
*/`, and identify it with "alien" horizontal delimiters `----------`. We also 
prepend all variables with `%`, and we also throw a `\` before the parameter's 
delcaration for lambda's sake. Here's the example so far:

```javascript
/*-----------
\ %foo %bar
-----------*/

module.exports = function () {
  %foo %bar
};
```

Let's try it out! `ltext check example.js.ltext` gives us the O.K., so let's see 
what the type is:

```bash
~> ltext type example

➥ (a -> b) -> a -> b
```

Let's make it a little more rigorous:

```javascript
/*-----------
\ (%foo :: b.js => a -> b) (%bar :: Content)
-----------*/

...
```

Now, we can be sure that `%foo` will only return javascript, and `%bar` is flat 
content and not a parametric template.

