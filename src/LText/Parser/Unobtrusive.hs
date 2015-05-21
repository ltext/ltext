module LText.Parser.Unobtrusive where

import Data.Map as Map

-- Specifies a catalog of unobtrusive text (between delimiters) for different languages.

type DelimitedSpan = (String, String)

doubleBackslash   = ("//",   "\n")
backslashAsterisk = ("/*",   "*/")
doubleDash        = ("--",   "\n")
curlyDash         = ("{-",   "-}")
angleBangDash     = ("<!--", "-->")

data Language =
    C
  | Cpp
  | Haskell
  | JavaScript
  | Css
  | Sass
  | Less
  | Html
  deriving (Ord, Eq, Show)

unobSpans :: Map Language [DelimitedSpan]
unobSpans = Map.fromList $
  [ (C, [doubleBackslash, backslashAsterisk])
  , (Cpp, [doubleBackslash, backslashAsterisk])
  , (Haskell, [doubleDash, curlyDash])
  , (JavaScript, [doubleBackslash, backslashAsterisk])
  , (Css, [backslashAsterisk])
  , (Sass, [doubleBackslash, backslashAsterisk])
  , (Less, [doubleBackslash, backslashAsterisk])
  , (Html, [angleBangDash])
  ]
