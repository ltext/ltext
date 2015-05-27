Notes
=====

## Table of Contents

- Parser
  - Comments / Unobtrusive Text
    - Delimeters
  - Variable Identification
    - Delimeters
- Renderer
  - Padding and Clipping
- Lookup

## Parser

### Comments / Unobtrusive Text

This will grow to a noisy catelog, but it would still be convenient to have confidence
that a span of text will not affect the meaning of a file.

#### Delimeters

##### C-Style

- `//` and `\n`
- `/*` and `*/`

##### Haskell-Style

- `--` and `\n`
- `{-` and `-}`

### Variable Identification

#### Delimeters

We need delimiters that occur in the text normally.

- Should be palindome / mirrored
- Should be the same for all variables in a template
- Should delete replace variable delimiters, __and__ up-to surrounding
  unobtrusive delimiters as well, if they exist

##### Jade-Style

- `{{foo}}`

> _Shouldn't_ be used in Jade files

##### Others

- `#foo#`


## File Names

- `\`
- Parens: `(` or `)`
- arrow: `->`
    - `-` or `>` is OK
