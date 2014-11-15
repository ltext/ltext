TODO
====

This document is for development organizational & record-keeping purposes.

## Current issues

### Text Parsing and Context

#### Text Substitution

We need to prepare for __massive__ files, which means 2-phase evaluation:

1. Parse the template body to find the positions where the template variables 
   occour, and record their character placement
2. When content is applied to the template, jump the cursor position to the 
   variable's
    1. eliminate the variable text
    2. insert the content stream into the variable's position

For the file content streaming, Conduit will be used. For actual content 
insertion, we may require temporary files.

#### BiDirectional Lenses as Formatting Rules

We will want template headers to be automatically constructed. This can be a 
major task, with a number of gotcha's:
- Varying horizontal width of the horizontal delimiters
- What syntaxes are necessary? What aren't? (`\` for example)
- Do the delimiters _wrap_ identifiers, or just prepend?

These issues will just grow over time. My temporary solution is to separate 
"getting" and "putting" - getters are just regular expressions, and putters are 
just text concatenation. Works well enough for now.

#### Alien Syntax Inference

Ideally, we would like our alien syntax _inferred_. This is a really difficult 
task. So far, I've got two major ideas in mind for (potentially contextual)
lexical recognition:

- accounted recognition
    - For instance, alien syntax of template variables. We need to recognize 
      `%foo` itself as it's own distinguished element, - as we see 
      `%`, we need to think "oop, here comes a distinguished element".
- neglected recognition
    - This deals with header delimiters and (unnecessary) parameter 
      "initializers" like `\` (lambda). When we see this, we need to realize 
      that it should be _neglected_ from our object's identification, but still 
      see it as the initialization of a context to change our state of thinking.

This is still very vague, I'm not even sure if it's possible.
