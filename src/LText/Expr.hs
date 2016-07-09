{-# LANGUAGE
    RecursiveDo
  #-}

module LText.Expr where

{-
Represents the expression usable from the command line, and within
a delimitation.
-}

import Control.Applicative
import Text.Earley
import Data.Char


data Expr
  = Abs String Expr
  | App Expr Expr
  | Var String
  deriving (Show, Eq)

expr :: Grammar r (Prod r String Char Expr)
expr = mdo
  whitespace <- rule . many $ token ' ' <?> "whitespace"
  let whitespace1 = token ' ' *> whitespace <?> "whitespace1"

  let lamb   = whitespace1 *> token '\\' <?> "lambda"
      arr    = whitespace1 *> token '-' *> token '>' <?> "arrow"

      leftP  = whitespace1 *> token '(' <?> "left paren"
      rightP = token ')' <?> "right paren"

      word   = (:) <$> satisfy (not . isSpace)
                   <*> many (satisfy $ not . isSpace)
                   <?> "identifier"
      ident  = whitespace *> word

  x1 <- rule $  (Var <$> ident)
            <|> (leftP *> x3 <* rightP)

  x2 <- rule $  Abs <$> (lamb *> ident <* arr) <*> x2
            <|> x1

  x3 <- rule $  App <$> x3 <*> (whitespace1 *> x2)
            <|> x2

  pure $ x3 <* whitespace
