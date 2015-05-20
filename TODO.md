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

##### Jade-Style

- `{{foo}}`

> _Shouldn't_ be used in Jade files

##### Others

- `#foo#`

## Renderer

### Padding and Clipping

Can be declared at either variable call sites (`v`), or template definition (`t`).

> Shouldn't remove content of the parent - ie, can't remove container whitespace from a `t`.

#### Padding

- make new lines (`t`, `v`)
- make new spaces (`t`, `v`)
- indentation (`v`)

#### Clipping

- remove inserted new lines (`v`)
- remove inserted new spaces (`v`)

## Lookup
